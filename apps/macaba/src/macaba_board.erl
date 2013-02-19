%%%------------------------------------------------------------------------
%%% @doc Board and board list handling
%%% Created: 2013-02-17 Dmytro Lytovchenko <kvakvs@yandex.ru>
%%%------------------------------------------------------------------------
-module(macaba_board).

-export([ get_board/1
        , get_boards/0
        , get_threads/1
        , new_post/1
        , new_thread/2
        , start/0
        ]).

-include_lib("macaba/include/macaba_types.hrl").
%% TODO: change this to {p, g, K} for gproc distributed mode
%% change this to {p, l, K} for local only mode
-define(GPROC_PROP(K), {p, l, K}).
-define(GPROC_COUNTER(K), {c, l, K}).

%%%-----------------------------------------------------------------------------
start() ->
  %% ets:new(macaba_boards_tab
  %%         , [ set, protected
  %%           , {keypos, #mcb_board.board_id}
  %%           , named_table
  %%           , {write_concurrency, true}
  %%           , {read_concurrency, true}
  %%           ]),
  {ok, Boards} = application:get_env(macaba, boards),
  internal_cache_boards(Boards).

%%%-----------------------------------------------------------------------------
%% @private
%% @doc Loads boards dynamic data from database and stores in ETS
internal_cache_boards([]) -> ok;
internal_cache_boards([{Id, Def} | Boards]) ->
  BoardId = macaba:as_binary(Id),
  B = #mcb_board{
    board_id = BoardId,
    category = macaba:propget(category, Def, ""),
    title    = macaba:propget(title, Def, atom_to_list(Id))
   },
  %% ets:insert(macaba_boards_tab, B),
  gproc:reg(?GPROC_PROP({mcb_board, BoardId}), B),

  %% Load dynamic data from RIAK and create a global counter (if not exists)
  case get_global_counter(mcb_board_id, BoardId) of
    X when is_integer(X) -> ok;
    {error, not_found} ->
      Dynamic = case riak_pool_auto:get(<<"board">>, BoardId) of
                  {error, notfound}   -> [];
                  X when is_binary(X) -> decode(mcb_board_dynamic, X)
                end,
      %% This will succeed if counter did not exist, this will fail silently
      %% (return {error, badarg}) if counter already existed. We do nothing
      %% if counter exists
      create_global_counter(mcb_board_id, BoardId,
                         Dynamic#mcb_board_dynamic.post_id)
  end,
  internal_cache_boards(Boards).

%%%-----------------------------------------------------------------------------
%% @doc Attempts to write to gproc global aggregated counter, fails with
%% {error, exists} of counter exists (you should abort attempt to create)
-spec create_global_counter(T :: atom(), Key :: any(), Value :: integer()) ->
                               ok | {error, atom()}.
create_global_counter(T, Key, Value) ->
  try add_global_counter({T, BoardId}, Value),
      gproc:add_global_aggr_counter(?GPROC_COUNTER({T, Key}))
  catch error:badarg -> {error, exists}
  end.

%%%-----------------------------------------------------------------------------
%% @private
%% @doc Searches gproc for global counter with type and name
-spec get_global_counter(T :: atom(), Key :: any()) ->
                            integer() | {error, not_found}.
get_global_counter(T, BoardId) ->
  try gproc:lookup_global_aggr_counter({T, BoardId})
  catch error:badarg -> {error, not_found}
  end.

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
  ThreadId = next_board_post_id(BoardId),
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
  TBin = encode(mcb_thread, T),
  riak_pool_auto:put(<<"thread">>, macaba:as_binary(ThreadId), TBin).

%%%-----------------------------------------------------------------------------
%% @doc Creates structure for a new thread post, writes to database.
%% If thread_id equals <<"new">> also creates new thread header and adds a
%% new thread too
%% @return ok or {error, Reason}
new_post(Opt) ->
  BoardId   = macaba:propget(board_id,  Opt),
  ThreadId0 = macaba:propget(thread_id, Opt),
  Author    = macaba:propget(author,    Opt),
  Subject   = macaba:propget(subject,   Opt),
  Message   = macaba:propget(message,   Opt),
  Attach    = macaba:propget(attach,    Opt),
  AttachId  = macaba_board:write_attachment(Attach),

  TOpts = [],
  {ok, ThreadId} = macaba_board:find_or_create_thread(BoardId, ThreadId0,
                                                      TOpts),
  {ok, PostId} = case ThreadId0 of
                   <<"new">> -> ThreadId;
                   _         -> macaba_board:next_board_post_id(BoardId)
                 end,
  P = #mcb_post{
    thread_id = ThreadId,
    post_id = PostId,
    subject = Subject,
    author = Author,
    message = Message,
    created = get_created_utc(),
    attach_id = AttachId,
    sage = false
   },
  PBin = encode(mcb_post, P),
  riak_pool_auto:put(<<"post">>, macaba:as_binary(PostId), PBin).

%%%-----------------------------------------------------------------------------
%% @private
%% @doc Gets unix time for now
-define(SECONDS_1970, 1970*365*24*3600).
get_created_utc() ->
  {UTCD, UTCT} = calendar:universal_time(),
  calendar:datetime_to_gregorian_seconds({UTCD, UTCT}) - ?SECONDS_1970.

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
%% @private
%% @doc Stub for data upgrade function, to support multiple versions
%% of the same data. On successful upgrade data is written back too!
-spec upgrade(Type :: macaba_dbobject(), integer(), any()) -> any().
upgrade(mcb_board_dynamic, ?MCB_BOARD_DYNAMIC_VER, X) -> X;
upgrade(mcb_thread, ?MCB_THREAD_VER, X) -> X;
upgrade(mcb_post, ?MCB_POST_VER, X) -> X.

%% @internal
%% @doc Decodes database object, as a tuple of {version, binaryencoded}
%% if version is too low, the object is filtered through upgrade/3
decode(T, Bin) ->
  {Version, Value} = binary_to_term(Bin, [safe]),
  upgrade(T, Version, Value).

%% @internal
%% @doc Encodes database object with current version. If on read version is too
%% low, its gets filtered through upgrade/3
encode(mcb_board_dynamic, P) -> term_to_binary( {?MCB_BOARD_DYNAMIC_VER, P} );
encode(mcb_thread, P) -> term_to_binary( {?MCB_THREAD_VER, P} );
encode(mcb_post, P) -> term_to_binary( {?MCB_POST_VER, P} ).

%%% Local Variables:
%%% erlang-indent-level: 2
%%% End:
