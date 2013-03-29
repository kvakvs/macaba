%%%------------------------------------------------------------------------
%%% @doc Board data model, is used by board client (macaba_board_cli) and then
%%% formatted to HTML or JSON etc
%%% Created: 2013-02-17 Dmytro Lytovchenko <kvakvs@yandex.ru>
%%%------------------------------------------------------------------------
-module(macaba_board).

-export([ start/0
        , load_board_dynamics/0
        , get/1
        , get_dynamic/1, get_dynamic_riak/1
        , get_site_config/0
        , set_site_config/1
        , get_boards/0
        , add_thread/2
        , thread_bump_if_no_sage/4
        , next_post_id/1
        , get_threads/1
        , touch_board_dynamic/1
        ]).

-include_lib("macaba/include/macaba_types.hrl").
-define(DEFAULT_SITE, <<"default">>).

%%%-----------------------------------------------------------------------------
start() -> ok.

%%%-----------------------------------------------------------------------------
add_thread(BoardId, ThreadId) ->
  %% add thread to board
  F = fun(BD = #mcb_board_dynamic{ threads=T }) ->
          T2 = [ThreadId | T],
          lager:debug("add_thread ~p", [T2]),
          touch_board_dynamic(BD#mcb_board_dynamic{ threads = T2 })
      end,
  {atomic, _NewD} = macaba_db_mnesia:update(mcb_board_dynamic, BoardId, F),
  check_board_threads_limit(BoardId).

%%%-----------------------------------------------------------------------------
%% @doc Returns list of configured boards
-spec get_boards() -> [#mcb_board{}].
get_boards() ->
  #mcb_site_config{ boards=B } = get_site_config(),
  B.

%%%-----------------------------------------------------------------------------
get_site_config() ->
  SiteKey = ?DEFAULT_SITE,
  case macaba_db_mnesia:read(mcb_site_config, SiteKey) of
    {ok, #mcb_site_config{} = Conf1} ->
      Conf1;
    {error, not_found} ->
      case macaba_db_riak:read(mcb_site_config, SiteKey) of
        {ok, #mcb_site_config{} = Conf2} ->
          macaba_db_mnesia:write(mcb_site_config, Conf2),
          Conf2;
        {error, not_found} ->
          Default = fake_default_site_config(),
          macaba_db_mnesia:write(mcb_site_config, Default),
          macaba_db_riak:write(mcb_site_config, Default),
          Default
      end
  end.

%%%-----------------------------------------------------------------------------
set_site_config(Site = #mcb_site_config{}) ->
  macaba_db_riak:write(mcb_site_config, Site),
  %% ensure board dynamics created for new boards
  Boards = Site#mcb_site_config.boards,
  [internal_create_dynamic(BId) || #mcb_board{board_id=BId} <- Boards].

%% @private
internal_create_dynamic(BoardId) when is_binary(BoardId) ->
  BD = case ?MODULE:get_dynamic(BoardId) of
         {error, not_found} -> #mcb_board_dynamic{board_id = BoardId};
         {ok, Value} -> Value
       end,
  BD2 = touch_board_dynamic(BD),
  macaba_db_riak:write(mcb_board_dynamic, BD2),
  macaba_db_mnesia:write(mcb_board_dynamic, BD2).

%%%-----------------------------------------------------------------------------
%% @doc Returns board by name
-spec get(BoardId :: binary()) -> {ok, #mcb_board{}} | {error, not_found}.
get(BoardId) ->
  case lists:keysearch(BoardId, #mcb_board.board_id, get_boards()) of
    {value, X} -> {ok, X};
    false -> {error, not_found}
  end.

%%%-----------------------------------------------------------------------------
%% @private
fake_default_site_config() ->
  M = <<"Site is temporarily offline, come back in 15 min">>,
  #mcb_site_config{
          site_id = ?DEFAULT_SITE,
          boards = fake_default_boards(),
          offline = false,
          offline_message = M
         }.

%% @private
fake_default_boards() ->
  {ok, DefaultAnon} = macaba_conf:get([<<"board">>,
                                       <<"default_anonymous_name">>]),
  [#mcb_board{
        board_id       = <<"unconfigured">>
      , short_name     = <<"default_board">>
      , category       = <<"no_category">>
      , title          = <<"Board not configured">>
      , anonymous_name = DefaultAnon
      , max_threads    = 20 * 10
     }].

%%%-----------------------------------------------------------------------------
%% @doc Returns list of threads in board (only info headers, no contents!), also
%% a proplist with board contents (first post and X last posts - configurable)
-spec get_threads(BoardId :: binary()) ->
                     {ok, [#mcb_thread{}], [#mcb_thread{}]} | {error, any()}.

get_threads(BoardId) when is_binary(BoardId) ->
  case macaba_db_mnesia:read(mcb_board_dynamic, BoardId) of
    {error, not_found} ->
      {error, dynamic_not_found};
    {ok, #mcb_board_dynamic{
       pinned_threads = PThreadIds,
       threads        = ThreadIds
      }} ->
      PThreads = load_threads(BoardId, PThreadIds, []),
      Threads = load_threads(BoardId, ThreadIds, []),
      {ok, PThreads, Threads}
  end.

%% @private
load_threads(_BoardId, [], Accum) -> lists:reverse(Accum);
load_threads(BoardId, [ThreadId | Tail], Accum) -> 
  case macaba_thread:get(BoardId, ThreadId) of
    {ok, Thread} ->
      load_threads(BoardId, Tail, [Thread | Accum]);
    {error, not_found} ->
      lager:error("board: get_threads thread ~p not found", [ThreadId]),
      load_threads(BoardId, Tail, Accum)
  end.

%%%-----------------------------------------------------------------------------
%% @private
%% @doc Reads board info and cuts extra threads in the end according to board
%% settings.
check_board_threads_limit(BoardId) ->
  {ok, Board} = ?MODULE:get(BoardId),
  F = fun(BD = #mcb_board_dynamic{ threads=T }) ->
          Cut = min(Board#mcb_board.max_threads, length(T)),
          {T2, Delete} = lists:split(Cut, T),
          %% send messages to delete sunken threads
          [macaba_board_worker:thread_delete(BoardId, ThreadId)
           || ThreadId <- Delete],
          touch_board_dynamic(BD#mcb_board_dynamic{ threads = T2 })
      end,
  case macaba_db_mnesia:update(mcb_board_dynamic, BoardId, F) of
    {atomic, _} ->
      ok;
    Err ->
      lager:error("board: thread limits check error ~p", [Err])
  end.

%%%-----------------------------------------------------------------------------
%% @private
%% @doc Checks email field of the new post, if it contains no <<"sage">>, and if
%% thread is shorter than SoftPostLimit - bumps thread to become first on board
-spec thread_bump_if_no_sage(BoardId :: binary(),
                             ThreadId :: binary(),
                             SoftPostLimit :: integer(),
                             Post :: #mcb_post{}) -> boolean().

thread_bump_if_no_sage(BoardId, ThreadId, SoftPostLimit,
                       #mcb_post{email = Email}) ->
  Sink = case Email of
           <<"sage">> ->
             {ok, Sink0} = macaba_conf:get([<<"board">>, <<"sage_sink">>], 3),
             Sink0;
           _ -> 0
         end,
  %% sage is not set - bump up
  {ok, TD} = macaba_thread:get_dynamic(BoardId, ThreadId),
  case length(TD#mcb_thread_dynamic.post_ids) > SoftPostLimit of
    true ->
      false; % over soft limit, no bumping
    false ->
      BumpF = fun(BD = #mcb_board_dynamic{ threads=T0 }) ->
                  T = bump_or_sink(Sink, Email, ThreadId, T0),
                  touch_board_dynamic(BD#mcb_board_dynamic{ threads = T })
              end,
      {atomic, _} = macaba_db_mnesia:update(mcb_board_dynamic, BoardId, BumpF),
      true
  end.

%%%-----------------------------------------------------------------------------
%% @private
%% @doc Bumps thread if Sage set to false, or sinks thread by Sink positions
%% if Sage set to true. ThreadId is deleted from ThreadsList then inserted at
%% first position or at new (sunken) position
bump_or_sink(Sink, <<"sage">>, ThreadId, Threads0) ->
  %% split thread list at the ThreadId value
  {Head, Tail0} = lists:splitwith(fun(X) -> X =/= ThreadId end, Threads0),
  Tail = erlang:tl(Tail0),
  %% split tail Sink positions lower
  {Tail1, Tail2} = lists:split(erlang:min(length(Tail), Sink), Tail),
  %% insert value between parts of split tail
  lists:flatten([Head, Tail1, ThreadId, Tail2]);

bump_or_sink(_Sink, _Email, ThreadId, Threads0) ->
  %% sage=false, insert at 1st position
  [ThreadId | lists:delete(ThreadId, Threads0)].

%%%-----------------------------------------------------------------------------
%% @doc Generates new post_id for creating thread on the board
next_post_id(BoardId) when is_binary(BoardId) ->
  F = fun(BD = #mcb_board_dynamic{ last_post_id=L }) ->
          touch_board_dynamic(BD#mcb_board_dynamic{ last_post_id = L+1 });
         ({error, not_found}) ->
          touch_board_dynamic(#mcb_board_dynamic{ last_post_id = 0 })
      end,
  {atomic, NewD} = macaba_db_mnesia:update(mcb_board_dynamic, BoardId, F),
  Next = macaba:as_binary(NewD#mcb_board_dynamic.last_post_id),
  lager:debug("board: next_post_id board=~p result=~s", [BoardId, Next]),
  Next.

%%%-----------------------------------------------------------------------------
get_dynamic(BoardId) when is_binary(BoardId) ->
  case macaba_db_mnesia:read(mcb_board_dynamic, BoardId) of
    {ok, Value} -> {ok, Value};
    {error, not_found} -> get_dynamic_riak(BoardId)
  end.

get_dynamic_riak(BoardId) when is_binary(BoardId) ->
  macaba_db_riak:read(mcb_board_dynamic, BoardId).

%%%-----------------------------------------------------------------------------
%% @doc May be SLOW! Enumerates RIAK keys in board bucket, and calculates thread
%% lists for boards. Do this only on one node of the macaba cluster.
%% This is called from macaba_masternode:handle_leader_call after startup been
%% finished, call initiated by macaba_startup temporary module
load_board_dynamics() ->
  lager:info("[load_board_dynamics] enumerating threads and caching..."),
  %% TODO: if record in Mnesia exists, we have this job on >1 node, fatal!
  Boards = get_boards(),
  update_dynamics_for_board(Boards).

%% @private
%% @doc Reloads saved dynamics for each board from RIAK on startup
update_dynamics_for_board([]) -> ok;
update_dynamics_for_board([B = #mcb_board{} | Boards]) ->
  BoardId = B#mcb_board.board_id,
  BD = case ?MODULE:get_dynamic(BoardId) of
         {error, not_found} -> #mcb_board_dynamic{board_id = BoardId};
         {ok, Value} -> Value
       end,
  lager:debug("{{dbinit}} upd_dyn_b bd=~p", [BD]),
  macaba_db_mnesia:write(mcb_board_dynamic, touch_board_dynamic(BD)),
  update_dynamics_for_threads(BoardId, BD#mcb_board_dynamic.threads),
  update_dynamics_for_board(Boards).

%% @private
update_dynamics_for_threads(_BoardId, []) -> ok;
update_dynamics_for_threads(BoardId, [ThreadId | Threads])
  when is_binary(ThreadId) ->
  {ok, TD} = macaba_thread:get_dynamic_riak(BoardId, ThreadId),
  lager:debug("{{dbinit}} upd_dyn_t td=~p", [TD]),
  macaba_db_mnesia:write(mcb_thread_dynamic, TD),
  update_dynamics_for_threads(BoardId, Threads).

%%%-----------------------------------------------------------------------------
%% @doc Updates last_modified and etag for Board Dynamic
%% You have to update board dynamic if board is changed
-spec touch_board_dynamic(#mcb_board_dynamic{}) -> #mcb_board_dynamic{}.

touch_board_dynamic(BD = #mcb_board_dynamic{ pinned_threads=PThreads
                             , threads=Threads
                             , last_post_id=LastPost }) ->
  MTime = calendar:local_time(),
  Modified = erlang:localtime_to_universaltime(MTime),
  ETag = mcweb:create_and_format_etag({PThreads, Threads, LastPost}),
  BD#mcb_board_dynamic{
      last_modified = Modified
    , etag          = ETag
   }.

%%% Local Variables:
%%% erlang-indent-level: 2
%%% End:
