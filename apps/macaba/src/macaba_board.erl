%%%------------------------------------------------------------------------
%%% @doc Board and board list handling
%%% Created: 2013-02-17 Dmytro Lytovchenko <kvakvs@yandex.ru>
%%%------------------------------------------------------------------------
-module(macaba_board).

-export([ get_board/1
        , get_boards/0
        , get_threads/1
        ]).

-include_lib("macaba/include/macaba_types.hrl").

%%%-----------------------------------------------------------------------------
%% @doc Returns list of configured boards
get_boards() ->
  {ok, Boards} = application:get_env(macaba, boards),
  [ [{id, Id} | Def] || {Id, Def} <- Boards].

%%%-----------------------------------------------------------------------------
%% @doc Returns list of configured boards
%% May crash if board not found TODO: move this to ETS?
-spec get_board(BoardId :: integer() | binary()) ->
                   orddict:orddict() | {error, not_found}.
get_board(FindId) when is_binary(FindId) ->
  get_board(binary_to_existing_atom(FindId, latin1));

get_board(FindId) when is_atom(FindId) ->
  {ok, Boards} = application:get_env(macaba, boards),
  case macaba:propget(FindId, Boards) of
    undefined -> {error, not_found};
    B -> [{id, FindId} | B]
  end.

%%%-----------------------------------------------------------------------------
%% @doc Returns list of threads in board (only info headers, no contents!)
get_threads(BoardId) when is_list(BoardId) ->
  get_threads(list_to_binary(BoardId));

get_threads(BoardId) when is_binary(BoardId) ->
  ThreadIndex = [db_get_thread_header() ||
                  ThreadId <- db_get_threads_index(BoardId)].

%%%-----------------------------------------------------------------------------
%% @private
%% @doc TODO: insert memory caching here
-spec db_get_threads_index(BoardId :: binary()) -> [integer()].
db_get_threads_index(BoardId) ->
  case riak_pool_auto:get(<<"board-index">>, BoardId) of
    {error, notfound} -> [];
    X when is_binary(X) ->
      upgrade(mcb_thread_list, Version, binary_to_term(X, [safe]))
  end.

%%%-----------------------------------------------------------------------------
%% @private
%% @doc TODO: insert memory caching here
-spec db_get_thread_header(ThreadId :: integer()) ->
                              orddict:orddict() | {error, not_found}.
db_get_thread_header(ThreadId) ->
  case riak_pool_auto:get(<<"thread-head">>, ThreadId) of
    {error, notfound} -> [];
    {Version, X} when is_binary(X) ->
      upgrade(mcb_thread, Version, binary_to_term(X, [safe]))
  end.

%%%-----------------------------------------------------------------------------
%% @doc Stub for data version management function, to support multiple versions
%% of the same data
-spec upgrade(Type :: macaba_dbobject(), integer(), any()) -> any().
upgrade(_Type, _Version, X) -> X.

%%% Local Variables:
%%% erlang-indent-level: 2
%%% End:
