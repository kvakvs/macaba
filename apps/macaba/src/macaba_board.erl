%%%------------------------------------------------------------------------
%%% @doc Board data model, is used by board client (macaba_board_cli) and then
%%% formatted to HTML or JSON etc
%%% Created: 2013-02-17 Dmytro Lytovchenko <kvakvs@yandex.ru>
%%%------------------------------------------------------------------------
-module(macaba_board).

-export([ start/0
        , load_board_dynamics/0
        %% , detect_content_type/1

        , get/1
        , get_boards/0
        , add_thread/2
        , thread_bump_if_no_sage/4
        , next_post_id/1
        , get_threads/1
        ]).

-include_lib("macaba/include/macaba_types.hrl").

%%%-----------------------------------------------------------------------------
start() -> ok.

%%%-----------------------------------------------------------------------------
add_thread(BoardId, ThreadId) ->
  %% add thread to board
  F = fun(BD = #mcb_board_dynamic{ threads=T }) ->
          T2 = [ThreadId | T],
          BD#mcb_board_dynamic{ threads = T2}
      end,
  {atomic, _NewD} = macaba_db_mnesia:update(mcb_board_dynamic, BoardId, F),

  check_board_threads_limit(BoardId).

%%%-----------------------------------------------------------------------------
%% @doc Returns list of configured boards
-spec get_boards() -> [#mcb_board{}].
get_boards() ->
  case macaba_db_riak:read(mcb_site_config, <<"default">>) of
    #mcb_site_config{ boards=B } -> B;
    {error, not_found} -> fake_default_boards()
  end.

%%%-----------------------------------------------------------------------------
%% @doc Returns board by name
-spec get(BoardId :: binary()) -> #mcb_board{} | {error, not_found}.
get(BoardId) ->
  case lists:keysearch(BoardId, #mcb_board.board_id, get_boards()) of
    {value, X} -> X;
    false -> {error, not_found}
  end.

%%%-----------------------------------------------------------------------------
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
-spec get_threads(BoardId :: binary()) -> [#mcb_thread{}].

get_threads(BoardId) when is_binary(BoardId) ->
  Ids = case macaba_db_mnesia:read(mcb_board_dynamic, BoardId) of
          {error, not_found} -> [];
          #mcb_board_dynamic{threads=T} -> T
        end,
  Threads0 = [begin
                TKey = macaba_db:key_for(mcb_thread, {BoardId, T}),
                case macaba_db_riak:read(mcb_thread, TKey) of
                  #mcb_thread{}=Value -> Value;
                  {error, _} -> []
                end
              end || T <- Ids],
  Threads = lists:flatten(Threads0),
  Threads.

%%%-----------------------------------------------------------------------------
%% @private
%% @doc Reads board info and cuts extra threads in the end according to board
%% settings.
check_board_threads_limit(BoardId) ->
  Board = ?MODULE:get(BoardId),
  F = fun(BD = #mcb_board_dynamic{ threads=T }) ->
          Cut = min(Board#mcb_board.max_threads, length(T)),
          {T2, Delete} = lists:split(Cut, T),
          %% send messages to delete sunken threads
          [macaba_board_worker:thread_delete(BoardId, ThreadId)
           || ThreadId <- Delete],
          BD#mcb_board_dynamic{ threads = T2 }
      end,
  case macaba_db_mnesia:update(mcb_board_dynamic, BoardId, F) of
    {atomic, _} ->
      ok;
    Err ->
      lager:error("board: thread limits check error ~p", [Err])
  end.

%% @private
%% @doc Checks email field of the new post, if it contains no <<"sage">>, and if
%% thread is shorter than SoftPostLimit - bumps thread to become first on board
-spec thread_bump_if_no_sage(BoardId :: binary(),
                             ThreadId :: binary(),
                             SoftPostLimit :: integer(),
                             Post :: #mcb_post{}) -> boolean().

thread_bump_if_no_sage(_BoardId, _ThreadId, _SoftPostLimit,
                       #mcb_post{email = <<"sage">>}) -> false;

thread_bump_if_no_sage(BoardId, ThreadId, SoftPostLimit, _Post) ->
  TD = macaba_thread:get_dynamic(BoardId, ThreadId),
  case length(TD#mcb_thread_dynamic.post_ids) > SoftPostLimit of
    true ->
      false; % over soft limit, no bumping
    false ->
      BumpF = fun(BD = #mcb_board_dynamic{ threads=T }) ->
                  BD#mcb_board_dynamic{
                    threads = [ThreadId | lists:delete(ThreadId, T)]
                   }
              end,
      {atomic, _} = macaba_db_mnesia:update(mcb_board_dynamic, BoardId, BumpF),
      true
  end.

%%%-----------------------------------------------------------------------------
%% @doc Generates new post_id for creating thread on the board
next_post_id(BoardId) when is_binary(BoardId) ->
  F = fun(BD = #mcb_board_dynamic{ last_post_id=L }) ->
          BD#mcb_board_dynamic{ last_post_id = L+1 };
         ({error, not_found}) ->
          #mcb_board_dynamic{ last_post_id = 1 }
      end,
  {atomic, NewD} = macaba_db_mnesia:update(mcb_board_dynamic, BoardId, F),
  Next = macaba:as_binary(NewD#mcb_board_dynamic.last_post_id),
  lager:debug("board: next_post_id board=~p result=~s", [BoardId, Next]),
  Next.

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
  BD = case macaba_db_riak:read(mcb_board_dynamic, BoardId) of
         {error, not_found} -> #mcb_board_dynamic{board_id = BoardId};
         Value -> Value
       end,
  lager:debug("{{dbinit}} upd_dyn_b bd=~p", [BD]),
  %%T = fun() -> mnesia:write(mcb_board_dynamic, BD, write) end,
  %%{atomic, _} = mnesia:transaction(T),
  macaba_db_mnesia:write(mcb_board_dynamic, BD),
  update_dynamics_for_threads(BoardId, BD#mcb_board_dynamic.threads),
  update_dynamics_for_board(Boards).

%% @private
update_dynamics_for_threads(_BoardId, []) -> ok;
update_dynamics_for_threads(BoardId, [ThreadId | Threads])
  when is_binary(ThreadId) ->
  TD = macaba_thread:get_dynamic(BoardId, ThreadId),
  lager:debug("{{dbinit}} upd_dyn_t td=~p", [TD]),
  %% T = fun() -> mnesia:write(mcb_thread_dynamic, TD, write) end,
  %% {atomic, _} = mnesia:transaction(T),
  macaba_db_mnesia:write(mcb_thread_dynamic, TD),
  update_dynamics_for_threads(BoardId, Threads).

%%% Local Variables:
%%% erlang-indent-level: 2
%%% End:
