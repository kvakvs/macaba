%%%------------------------------------------------------------------------
%%% @doc Board client, requests data from macaba_board and formats it to
%%% proplists according to board needs
%%% Created: 2013-02-24 Dmytro Lytovchenko <kvakvs@yandex.ru>
%%%------------------------------------------------------------------------
-module(macaba_board_cli).

-export([ get_boards/0
        , get_board/1
        , get_thread/2
        , get_threads/3
        , get_thread_previews/3
        , get_thread_preview/3
        , anonymous_delete_post/4
        ]).

-include_lib("macaba/include/macaba_types.hrl").

%%%-----------------------------------------------------------------------------
%% @doc Attempts to delete post if passwords match
-spec anonymous_delete_post(BoardId :: binary(),
                            PostId :: binary(),
                            FileOnly :: boolean(),
                            Password :: binary()) -> ok | {error, any()}.
anonymous_delete_post(BoardId, PostId, FileOnly, Password) ->
  lager:info("board_cli: anonymous_delete_post B=~s P=~s Pass=~s",
             [BoardId, PostId, Password]),
  P = macaba_board:get_post(BoardId, PostId),
  case P#mcb_post.delete_pass of
    Password when byte_size(Password) > 0 ->
      case FileOnly of
        true -> macaba_board:delete_post_attach(BoardId, PostId);
        false -> macaba_board:delete_post(BoardId, PostId)
      end;
    _ -> {error, password}
  end.

%%%-----------------------------------------------------------------------------
%% @doc Returns list of boards as proplists
-spec get_boards() -> [proplist_t()].
get_boards() ->
  B = macaba_board:get_boards(),
  lists:map(fun macaba:record_to_proplist/1, B).

%%%-----------------------------------------------------------------------------
%% @doc Returns board header formatted as proplist
-spec get_board(BoardId :: binary()) -> proplist_t() | {error, not_found}.
get_board(BoardId) ->
  case macaba_board:get_board(BoardId) of
    {error, not_found} -> {error, not_found};
    B -> macaba:record_to_proplist(B)
  end.

%%%-----------------------------------------------------------------------------
%% @doc Returns thread header formatted as proplist
-spec get_thread(BoardId :: binary(), ThreadId :: binary()) ->
                    proplist_t() | {error, not_found}.
get_thread(BoardId, ThreadId) ->
  case macaba_board:get_thread(BoardId, ThreadId) of
    {error, not_found} -> {error, not_found};
    T -> macaba:record_to_proplist(T)
  end.

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
      [additional_fields_for_thread(T, PreviewSize) || T <- Proplists];
    _ -> Proplists
  end.

%%%-----------------------------------------------------------------------------
%% @private
additional_fields_for_thread(T, PreviewSize) ->
  BoardId     = macaba:propget(board_id, T),
  ThreadId    = macaba:propget(thread_id, T),
  PreviewList = get_thread_preview(BoardId, ThreadId, PreviewSize),
  Preview = {preview, PreviewList},
  TD = macaba_board:get_thread_dynamic(BoardId, ThreadId),
  PostIds = TD#mcb_thread_dynamic.post_ids,
  SkippedP = {skipped_posts, max(0, length(PostIds) - PreviewSize - 1)},
  PreviewListTl = case PreviewList of
                    [] -> [];
                    _ -> tl(PreviewList)
                  end,
  SkippedI = {skipped_images, count_images(PreviewListTl, PreviewSize)},
  [Preview, SkippedP, SkippedI | T].

%%%-----------------------------------------------------------------------------
count_images(Posts0, TailSize) ->
  {Posts, _} = lists:split(max(0, length(Posts0) - TailSize), Posts0),
  lists:foldl(fun(X, Accum) ->
                  case macaba:propget(attach_id, X) of
                    [] -> Accum;
                    L when is_list(L) -> Accum+1
                  end
              end, 0, Posts).

%%%-----------------------------------------------------------------------------
%% @doc Returns thread preview (first post plus few last posts) or full thread
%% contents
-spec get_thread_previews(BoardId :: binary(),
                          ThreadIds :: [binary()],
                          PreviewSize :: non_neg_integer()) ->
                                 [proplist_of(proplist_t())].
get_thread_previews(BoardId, ThreadIdList, PreviewSize) ->
  %% for each thread get preview
  %% lager:debug("cli: get_thr_previews ids=~p size=~p",
  %%             [ThreadIdList, PreviewSize]),
  [{T, get_thread_preview(BoardId, T, PreviewSize)} || T <- ThreadIdList].

%%%-----------------------------------------------------------------------------
-spec get_thread_preview(BoardId :: binary(),
                         ThreadId :: binary(),
                         PreviewSize :: non_neg_integer() | 'all') ->
                            [proplist_t()].
get_thread_preview(BoardId, ThreadId, PreviewSize) ->
  %% lager:debug("cli: get_thread_preview id=~p", [ThreadId]),
  Posts = macaba_board:get_thread_contents(BoardId, ThreadId, PreviewSize),
  %% convert each record in preview to proplist
  [additional_fields_for_post(P, macaba:record_to_proplist(P)) || P <- Posts].

additional_fields_for_post(P = #mcb_post{}, PropList) ->
  %% load attachment headers
  AttachMod = macaba_plugins:mod(attachments),
  Att0 = lists:map(fun(AttId) ->
                       AttachMod:read_header(AttId)
                   end, P#mcb_post.attach_ids),
  %% filter out only existing attachments
  Att1 = lists:filter(fun(#mcb_attachment{}) -> true; (_) -> false end, Att0),
  Att = lists:map(fun macaba:record_to_proplist/1, Att1),
  [{attach_info, Att} | PropList].

%%% Local Variables:
%%% erlang-indent-level: 2
%%% End:
