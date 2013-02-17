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

%% @doc Returns list of configured boards
get_boards() ->
  {ok, Boards} = application:get_env(macaba, boards),
  [ [{id, Id} | Def] || {Id, Def} <- Boards].

%% @doc Returns list of configured boards
%% May crash if board not found
get_board(FindId) when is_binary(FindId) ->
  get_board(binary_to_existing_atom(FindId, latin1));

get_board(FindId) when is_atom(FindId) ->
  {ok, Boards} = application:get_env(macaba, boards),
  B = macaba:propget(FindId, Boards),
  [{id, FindId} | B].

%% @doc Returns list of threads in board (only info headers, no contents!)
get_threads(BoardId) ->
  [].

%%% Local Variables:
%%% erlang-indent-level: 2
%%% End:
