%%%------------------------------------------------------------------------
%%% @doc Board client, requests data from macaba_board and formats it to
%%% proplists according to board needs
%%% Created: 2013-02-24 Dmytro Lytovchenko <kvakvs@yandex.ru>
%%%------------------------------------------------------------------------
-module(macaba_board_cli).

-export([ get_boards/0
        , get_board/1
        , get_threads/3
        , get_thread_previews/2
        , get_thread_preview/2
        ]).

-include_lib("macaba/include/macaba_types.hrl").

%%%-----------------------------------------------------------------------------
%% @doc Returns list of boards as proplists
-spec get_boards() -> [proplist_t()].
get_boards() ->
  B = macaba_board:get_boards(),
  lists:map(fun macaba:record_to_proplist/1, B).

%%%-----------------------------------------------------------------------------
%% @doc Returns board header formatted as proplist
-spec get_board(BoardId :: binary()) -> proplist_t().
get_board(BoardId) ->
  macaba:record_to_proplist(macaba_board:get_board(BoardId)).

%%%-----------------------------------------------------------------------------
%% @doc Returns board contents paginated
-spec get_threads(BoardId :: binary(),
                  {Page :: integer(), PageSize :: integer()},
                  PreviewSize :: integer()) ->
                         [proplist_t()].
get_threads(BoardId, {undefined, PageSize}, PreviewSize) ->
  get_threads(BoardId, {1, PageSize}, PreviewSize);
get_threads(BoardId, {Page, PageSize}, PreviewSize) ->
  Threads = macaba:pagination(
              macaba_board:get_threads(BoardId), Page, PageSize),
  Proplists = lists:map(fun macaba:record_to_proplist/1, Threads),
  case PreviewSize of
    X when X > 1->
      [ [{preview, get_thread_preview(
                     macaba:propget(thread_id, T), PreviewSize)
         } | T] || T <- Proplists];
    _ -> Proplists
  end.

%%%-----------------------------------------------------------------------------
%% @doc Returns thread preview (first post plus few last posts) or full thread
%% contents
-spec get_thread_previews(ThreadIds :: [binary()],
                          PreviewSize :: non_neg_integer()) ->
                                 [proplist_of(proplist_t())].
get_thread_previews(ThreadIdList, PreviewSize) ->
  %% for each thread get preview
  lager:debug("cli: get_thread_previews ids=~p psize=~p", [ThreadIdList, PreviewSize]),
  [{T, get_thread_preview(T, PreviewSize)} || T <- ThreadIdList].

%%%-----------------------------------------------------------------------------
-spec get_thread_preview(ThreadId :: binary(),
                         PreviewSize :: non_neg_integer()) ->
                            [proplist_t()].
get_thread_preview(ThreadId, PreviewSize) ->
  lager:debug("cli: get_thread_preview id=~p", [ThreadId]),
  Posts = macaba_board:get_thread_contents(ThreadId, PreviewSize),
  %% convert each record in preview to proplist
  [macaba:record_to_proplist(P) || P <- Posts].

%%% Local Variables:
%%% erlang-indent-level: 2
%%% End:
