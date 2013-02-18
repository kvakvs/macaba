%%%------------------------------------------------------------------------
%%% @doc Board and board list handling
%%% Created: 2013-02-17 Dmytro Lytovchenko <kvakvs@yandex.ru>
%%%------------------------------------------------------------------------
-module(macaba_board).

-export([ get_board/1
        , get_boards/0
        , get_threads/1
        , start/0
        ]).

-include_lib("macaba/include/macaba_types.hrl").

%%%-----------------------------------------------------------------------------
start() ->
  ets:new(macaba_boards_tab
          , [ set, protected
            , {keypos, #mcb_board.board_id}
            , named_table
            , {write_concurrency, true}
            , {read_concurrency, true}
            ]),
  {ok, Boards} = application:get_env(macaba, boards),
  internal_cache_boards(Boards).

%%%-----------------------------------------------------------------------------
%% @private
%% @doc Loads boards dynamic data from database and stores in ETS
internal_cache_boards([]) -> ok;
internal_cache_boards([{Id, Def} | Boards]) ->
  DynamicData = case riak_pool_auto:get(<<"board">>, macaba:as_binary(Id)) of
                  {error, notfound}   -> [];
                  X when is_binary(X) -> binary_to_term(X, [safe])
                end,
  B = #mcb_board{
    board_id = macaba:as_binary(Id),
    category = macaba:propget(category, Def, ""),
    title    = macaba:propget(title, Def, atom_to_list(Id)),
    post_id  = macaba:propget(post_id, DynamicData, 1)
   },
  ets:insert(macaba_boards_tab, B),
  internal_cache_boards(Boards).

%%%-----------------------------------------------------------------------------
%% @doc Returns list of configured boards + dynamic data from ETS
get_boards() ->
  [ macaba:record_to_proplist(B) || B <- ets:tab2list(macaba_boards_tab)].

%%%-----------------------------------------------------------------------------
%% @doc Returns list of configured boards
-spec get_board(BoardId :: integer() | binary()) ->
                   orddict:orddict() | {error, not_found}.
get_board(FindId) ->
  case ets:lookup(macaba_boards_tab, macaba:as_binary(FindId)) of
    [] -> {error, not_found};
    [B] -> macaba:record_to_proplist(B)
  end.

%%%-----------------------------------------------------------------------------
%% @doc Returns list of threads in board (only info headers, no contents!)
get_threads(BoardId) when is_list(BoardId) ->
  get_threads(list_to_binary(BoardId));

get_threads(BoardId) when is_binary(BoardId) ->
  [db_get_thread_header(ThreadId) || ThreadId <- get_threads_index(BoardId)].

%%%-----------------------------------------------------------------------------
%% @doc Creates a new thread, and writes it to database. Thread is given an
%% unique thread_id from given board
new_thread(undefined, _Opts) -> {error, board_id_not_found};
new_thread(BoardId, Opts) ->
  ThreadId = macaba_board:next_board_post_id(BoardId),
  Hidden   = macaba:propget(hidden,    Opts, false),
  Pinned   = macaba:propget(pinned,    Opts, false),
  ReadOnly = macaba:propget(read_only, Opts, false),
  T = #mcb_thread{
    thread_id = ThreadId,
    %% first post id equals to thread id but we never display thread id
    post_ids = [ThreadId],
    hidden = Hidden,
    pinned = Pinned,
    read_only = ReadOnly
   },
  riak_pool_auto:put(<<"thread">>, macaba:as_binary(ThreadId)).

%%%-----------------------------------------------------------------------------
%% @doc Creates structure for a new thread post, writes to database.
%% If thread_id equals <<"new">> also creates new thread header and adds a
%% new thread too
%% @return ok or {error, Reason}
new_post(Opt) ->
  BoardId   = macaba:propget(board_id,  Opt),
  ThreadId0 = macaba:propget(thread_id, Opt),
  Name      = macaba:propget(name,      Opt),
  Subject   = macaba:propget(subject,   Opt),
  Message   = macaba:propget(message,   Opt),
  Attach    = macaba:propget(attach,    Opt),
  AttachId  = macaba_board:write_attachment(Attach),

  TOpts = [],
  {ok, ThreadId} = macaba_board:find_or_create_thread(BoardId, ThreadId0, TOpts),
  {ok, PostId} = case ThreadId0 of
                   <<"new">> -> ThreadId;
                   _         -> macaba_board:next_board_post_id(BoardId)
                 end,
  Post = #mcb_post{
    thread_id = ThreadId,
    post_id = PostId
   }.

%%%-----------------------------------------------------------------------------
next_board_post_id(undefined) -> {error, board_id_not_found};
next_board_post_id(BoardId) ->
  %% TODO: Make sync with RIAK as fast as possible but skipping writes which
  %% come too soon after previous sync
  ets:update_counter(macaba_boards_tab, macaba:as_binary(BoardId),
                     {#mcb_board.post_id, 1}).

%%%-----------------------------------------------------------------------------
%% @private
%% @doc Searches for existing thread if the parameter is a number.
%% If parameter is <<"new">> creates a new thread
find_or_create_thread(undefined, _, _Opts) -> {error, thread_id_not_found};
find_or_create_thread(_, undefined, _Opts) -> {error, board_not_found};
find_or_create_thread(<<"new">>, BoardId, ThreadOpts) ->
  T = new_thread(BoardId, ThreadOpts),
  {ok, T}.

%%%-----------------------------------------------------------------------------
%% @private
%% @doc Searches database for existing file, if so - returns {error, exists}
db_write_attachment(undefined) -> undefined;
db_write_attachment(_A) -> undefined.

%%%-----------------------------------------------------------------------------
%% @private
%% @doc Reads thread index from memory ETS cache
-spec get_threads_index(BoardId :: binary()) -> [integer()].
get_threads_index(BoardId) ->
  {ok, B} = get_board(BoardId),
  B#mcb_board.threads.

%%%-----------------------------------------------------------------------------
%% @private
%% -spec db_get_threads_index(BoardId :: binary()) -> [integer()].
%% db_get_threads_index(BoardId) ->
%%   case riak_pool_auto:get(<<"board-index">>, BoardId) of
%%     {error, notfound} -> [];
%%     X when is_binary(X) ->
%%       {Version, Value} = binary_to_term(X, [safe]),
%%       upgrade(mcb_thread_list, Version, Value)
%%   end.

%%%-----------------------------------------------------------------------------
%% @private
%% @doc TODO: insert memory caching here
-spec db_get_thread_header(ThreadId :: integer()) ->
                              orddict:orddict() | {error, not_found}.
db_get_thread_header(ThreadId) ->
  case riak_pool_auto:get(<<"thread-head">>, ThreadId) of
    {error, notfound} -> [];
    X when is_binary(X) ->
      {Version, Value} = binary_to_term(X, [safe]),
      upgrade(mcb_thread, Version, Value)
  end.

%%%-----------------------------------------------------------------------------
%% @doc Stub for data upgrade function, to support multiple versions
%% of the same data. On successful upgrade data is written back too!
-spec upgrade(Type :: macaba_dbobject(), integer(), any()) -> any().
upgrade(_Type, _Version, X) -> X.

%%% Local Variables:
%%% erlang-indent-level: 2
%%% End:
