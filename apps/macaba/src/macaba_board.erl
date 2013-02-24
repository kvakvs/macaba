%%%------------------------------------------------------------------------
%%% @doc Board data model, is used by board client (macaba_board_cli) and then
%%% formatted to HTML or JSON etc
%%% Created: 2013-02-17 Dmytro Lytovchenko <kvakvs@yandex.ru>
%%%------------------------------------------------------------------------
-module(macaba_board).

-export([ construct_post/2
        , get_board/1
        , get_boards/0
        , get_threads/1
        , get_thread_contents/2
        , new_post/2
        , new_thread/3
        , start/0
        , load_board_dynamics/0
        ]).

-include_lib("macaba/include/macaba_types.hrl").

%%%-----------------------------------------------------------------------------
start() ->
  ok.

%%%-----------------------------------------------------------------------------
%% @private
%% @doc May be SLOW! Enumerates RIAK keys in board bucket, and calculates thread
%% lists for boards. Do this only on one node of the macaba cluster.
%% This is called from macaba_masternode:handle_leader_call after startup been
%% finished, call initiated by macaba_startup temporary module
load_board_dynamics() ->
  lager:info("[load_board_dynamics] enumerating threads and caching..."),
  %% TODO: if record in Mnesia exists, we have this job on >1 node, fatal!
  Boards = get_boards(),
  update_dynamics_for_board(Boards).
  %% {ok, ThreadIds} = riak_pool_auto:list_keys(
  %%                     macaba_db_riak:bucket_for(mcb_thread_dynamic)).

%% @private
%% @doc Reloads saved dynamics for each board from RIAK on startup
update_dynamics_for_board([]) -> ok;
update_dynamics_for_board([B = #mcb_board{} | Boards]) ->
  BoardId = B#mcb_board.board_id,
  BD = case macaba_db_riak:read(mcb_board_dynamic, BoardId) of
         {error, not_found} -> #mcb_board_dynamic{board_id = BoardId};
         Value -> Value
       end,
  lager:debug("upd_dyn_b bd=~p", [BD]),
  %%T = fun() -> mnesia:write(mcb_board_dynamic, BD, write) end,
  %%{atomic, _} = mnesia:transaction(T),
  macaba_db_mnesia:write(mcb_board_dynamic, BD),
  update_dynamics_for_threads(BD#mcb_board_dynamic.threads),
  update_dynamics_for_board(Boards).

%% @private
update_dynamics_for_threads([]) -> ok;
update_dynamics_for_threads([ThreadId | Threads]) when is_binary(ThreadId) ->
  TD = case macaba_db_riak:read(mcb_thread_dynamic, ThreadId) of
         {error, not_found} -> #mcb_thread_dynamic{thread_id = ThreadId};
         Value -> Value
       end,
  lager:debug("upd_dyn_t td=~p", [TD]),
  %% T = fun() -> mnesia:write(mcb_thread_dynamic, TD, write) end,
  %% {atomic, _} = mnesia:transaction(T),
  macaba_db_mnesia:write(mcb_thread_dynamic, TD),
  update_dynamics_for_threads(Threads).

%%%-----------------------------------------------------------------------------
%% @!private
%% @!doc SLOW! Enumerates RIAK keys in thread bucket, and calculates post lists
%% for threads. Do this only on one node of the macaba cluster.
%% This is called from macaba_masternode:handle_leader_call after startup been
%% finished, call initiated by macaba_startup temporary module
%% load_thread_dynamics() ->
%% lager:info("[load_thread_dynamics] enumerating thread posts and caching..."),
%%   %% TODO: if record in Mnesia exists, we have this job  on >1 node, fatal!
%%   ok.

%%%-----------------------------------------------------------------------------
%% @doc Returns list of configured boards
-spec get_boards() -> [#mcb_board{}].
get_boards() ->
  case macaba_db_riak:read(mcb_site_config, <<"default">>) of
    #mcb_site_config{ boards=B } -> B;
    {error, not_found} -> fake_default_boards()
  end.

%%%-----------------------------------------------------------------------------
%% @doc Returns list of configured boards (TODO: cache in memory?)
-spec get_board(BoardId :: binary()) -> #mcb_board{} | {error, not_found}.
get_board(BoardId) ->
  case lists:keysearch(BoardId, #mcb_board.board_id, get_boards()) of
    {value, X} -> X;
    false -> {error, not_found}
  end.

%%%-----------------------------------------------------------------------------
%% @private
fake_default_boards() ->
  [#mcb_board{
      board_id= <<"default">>,
      category="default",
      title="Default board"
     }].

%%%-----------------------------------------------------------------------------
%% @doc Returns list of threads in board (only info headers, no contents!), also
%% a proplist with board contents (first post and X last posts - configurable)
-spec get_threads(BoardId :: binary()) ->
                     {[#mcb_thread{}], [{binary(), [#mcb_post{}]}]}.
get_threads(BoardId) when is_binary(BoardId) ->
  Ids = case macaba_db_mnesia:read(mcb_board_dynamic, BoardId) of
          {error, not_found} -> [];
          #mcb_board_dynamic{threads=T} -> T
        end,
  Threads0 = [case macaba_db_riak:read(mcb_thread, T) of
                #mcb_thread{}=Value -> Value;
                {error, _} -> []
              end || T <- Ids],
  Threads = lists:flatten(Threads0),
  Threads.

%%%-----------------------------------------------------------------------------
%% @doc By thread id reads thread dynamic, to get ids of posts, loads first and
%% configured amount of last posts into a proplist. Give 'all' for LastCount to
%% load whole thread
-spec get_thread_contents(ThreadId :: binary(),
                          LastCount0 :: integer() | 'all') -> [#mcb_post{}].

get_thread_contents(ThreadId, LastCount0) when is_binary(ThreadId) ->
  TD = case macaba_db_mnesia:read(mcb_thread_dynamic, ThreadId) of
         {error, not_found} -> #mcb_thread_dynamic{};
         D -> D
       end,
  PostIds = TD#mcb_thread_dynamic.post_ids,

  %% if lastcount was set to 'all' - change it to thread length
  LastCount1 = case LastCount0 of
                 all -> length(PostIds);
                 _ -> LastCount0
               end,
  LastCount = min(LastCount1, length(PostIds)),

  %% get first and cut last
  First = case PostIds of [] -> []; [F|_] -> get_post(F) end,
  %% FIXME: this may run slow on large threads >1000 posts?
  PostIds2 = tl(PostIds),
  T = min(length(PostIds2), max(0, length(PostIds2) - LastCount)),
  LastIds = lists:nthtail(T, PostIds2),
  Last = lists:map(fun get_post/1, LastIds),
  lists:flatten([First | Last]).

%%%-----------------------------------------------------------------------------
%% @doc Loads post info
-spec get_post(PostId :: binary()) -> [#mcb_post{}] | {error, not_found}.
get_post(PostId) when is_binary(PostId) ->
  macaba_db_riak:read(mcb_post, PostId).

%%%-----------------------------------------------------------------------------
%% @doc Creates a new thread with a single post, thread_id is set to the first
%% post id. Writes both thread and post to database.
new_thread(BoardId, ThreadOpts, PostOpts) when is_binary(BoardId) ->
  Post0    = construct_post(BoardId, PostOpts),
  PostId   = Post0#mcb_post.post_id,

  Hidden   = macaba:propget(hidden,    ThreadOpts, false),
  Pinned   = macaba:propget(pinned,    ThreadOpts, false),
  ReadOnly = macaba:propget(read_only, ThreadOpts, false),

  Thread = #mcb_thread{
      thread_id = PostId
    , hidden    = Hidden
    , pinned    = Pinned
    , read_only = ReadOnly
    %%, author    = Post0#mcb_post.author
    %%, subject   = Post0#mcb_post.subject
    %%, created   = Post0#mcb_post.created
   },
  ThreadDyn = #mcb_thread_dynamic{
    thread_id = PostId,
    post_ids  = [PostId]
   },
  macaba_db_mnesia:write(mcb_thread_dynamic, ThreadDyn),
  %% link post to thread
  Post = Post0#mcb_post{ thread_id = PostId },
  macaba_db_riak:write(mcb_post, Post),
  macaba_db_riak:write(mcb_thread, Thread),
  %% add thread to board
  F = fun(BD = #mcb_board_dynamic{ threads=T }) ->
          BD#mcb_board_dynamic{ threads = T ++ [PostId]}
      end,
  {atomic, _NewD} = macaba_db_mnesia:update(mcb_board_dynamic, BoardId, F),
  {Thread, Post}.

%%%-----------------------------------------------------------------------------
%% @doc Creates new post, writes to database
new_post(BoardId, Opts) ->
  Post = construct_post(BoardId, Opts),
  macaba_db_riak:write(mcb_post, Post),

  %%-----------------------------------------------
  %% TODO vector clocks and conflict resolution
  %%-----------------------------------------------
  ThreadId = macaba:propget(thread_id, Opts),

  %% update thread post list
  F = fun(TD = #mcb_thread_dynamic{ post_ids=L }) ->
          TD#mcb_thread_dynamic{ post_ids = L++[Post#mcb_post.post_id] }
      end,
  {atomic, _} = macaba_db_mnesia:update(mcb_thread_dynamic, ThreadId, F),
  %% ThreadD0 = macaba_db_riak:read(mcb_thread, ThreadId),
  %% NewIds = Thread0#mcb_thread.post_ids ++ [Post#mcb_post.post_id],
  %% Thread = Thread0#mcb_thread{ post_ids=NewIds },
  %% macaba_db_riak:write(mcb_thread, Thread),
  Post.

%%%-----------------------------------------------------------------------------
%% @doc Creates structure for a new post, returns it. Does not write.
construct_post(BoardId, Opts) ->
  ThreadId  = macaba:propget(thread_id, Opts),
  Author    = macaba:propget(author,    Opts),
  Subject   = macaba:propget(subject,   Opts),
  Message   = macaba:propget(message,   Opts),
  Attach    = macaba:propget(attach,    Opts),
  AttachId  = write_attachment(Attach),

  PostId = next_board_post_id(BoardId),
  #mcb_post{
    thread_id = ThreadId,
    post_id   = PostId,
    subject   = Subject,
    author    = Author,
    message   = Message,
    created   = get_now_utc(),
    attach_id = AttachId,
    sage      = false
   }.

%%%-----------------------------------------------------------------------------
%% @private
%% @doc Gets unix time for now
-define(SECONDS_1970, 1970*365*24*3600).
get_now_utc() ->
  {UTCD, UTCT} = calendar:universal_time(),
  calendar:datetime_to_gregorian_seconds({UTCD, UTCT}) - ?SECONDS_1970.

%%%-----------------------------------------------------------------------------
%% @doc Generates new post_id for creating thread on the board
next_board_post_id(BoardId) when is_binary(BoardId) ->
  F = fun(BD = #mcb_board_dynamic{ last_post_id=L }) ->
          BD#mcb_board_dynamic{ last_post_id = L+1 }
      end,
  {atomic, NewD} = macaba_db_mnesia:update(mcb_board_dynamic, BoardId, F),
  macaba:as_binary(NewD#mcb_board_dynamic.last_post_id).

%%%-----------------------------------------------------------------------------
%% @private
%% @doc Searches database for existing file, if so - returns {error, exists}
write_attachment(undefined) -> undefined;
write_attachment(_A) -> undefined.

%%% Local Variables:
%%% erlang-indent-level: 2
%%% End:
