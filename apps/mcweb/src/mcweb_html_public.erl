%%%-----------------------------------------------------------------------------
%%% @doc This module has few predefined handlers (init, handle and terminate)
%%% which are called by cowboy on incoming HTTP request.
%%% Serves HTML templates, and provides basic HTTP access to the board.
%%% Created: 2013-02-16 Dmytro Lytovchenko <kvakvs@yandex.ru>
%%%-----------------------------------------------------------------------------
-module(mcweb_html_public).

-export([ init/3
        , handle/2
        , terminate/3]).
-export([ macaba_handle_index/3
        , macaba_handle_board/3
        , macaba_handle_thread/3
        , macaba_handle_thread_new/3
        , macaba_handle_thread_manage/3
        , macaba_handle_post_new/3
        , macaba_handle_attach/3
        , macaba_handle_attach_thumb/3
        ]).
-export([ chain_get_boards/2
        , chain_get_board_info/2
        , chain_get_threads/2
        , chain_get_thread_posts/2
        , chain_get_thread_info/2
        , chain_check_post_attach/2
        , chain_thread_new/2
        , chain_get_attach/2
        , chain_attach_send/2
        ]).

-include_lib("macaba/include/macaba_types.hrl").
-include_lib("mcweb/include/mcweb.hrl").

%%%-----------------------------------------------------------------------------
init({_Transport, http}, Req, [Mode]) ->
  {ok, Req, #mcb_html_state{
         mode = Mode
        }}.

-spec handle(cowboy_req:req(), mcweb:html_state()) ->
                {ok, cowboy_req:req(), mcweb:html_state()}.

handle(Req0, State0) ->
  mcweb:handle_helper(?MODULE, Req0, State0).

terminate(_Reason, _Req, _State) ->
  ok.

%%%-----------------------------------------------------------------------------
%% @doc GET /
%%%-----------------------------------------------------------------------------
-spec macaba_handle_index(Method :: binary(),
                          Req :: cowboy_req:req(),
                          State :: mcweb:html_state()) ->
                             mcweb:handler_return().

macaba_handle_index(<<"GET">>, Req0, State0) ->
  lager:debug("http GET /"),
  Boards = macaba_board_cli:get_boards(),
  State1 = mcweb:state_set_var(boards, Boards, State0),
  mcweb:render_page("index", Req0, State1).

%%%-----------------------------------------------------------------------------
%% @doc Do GET board/id/
%%%-----------------------------------------------------------------------------
-spec macaba_handle_board(Method :: binary(),
                          Req :: cowboy_req:req(),
                          State :: mcweb:html_state()) ->
                             mcweb:handler_return().

macaba_handle_board(<<"GET">>, Req0, State0) ->
  {_, Req, State} = mcweb:chain_run(
                      [ fun chain_get_boards/2
                      , fun chain_get_board_info/2
                      , fun chain_get_threads/2
                      ], Req0, State0),
  mcweb:render_page("board", Req, State).

chain_get_boards(Req, State0) ->
  Boards = macaba_board_cli:get_boards(),
  State = mcweb:state_set_var(boards, Boards, State0),
  mcweb:chain_success(Req, State).

%% @private
%% @doc get current board info
chain_get_board_info(Req0, State0) ->
  {BoardId, Req} = cowboy_req:binding(mcb_board, Req0),
  lager:debug("http GET board ~s", [BoardId]),
  case macaba_board_cli:get_board(BoardId) of
    {error, not_found} ->
      {Req1, State1} = mcweb:render_page(404, "board_404", Req, State0),
      mcweb:chain_fail(Req1, State1);
    BoardInfo ->
      State = mcweb:state_set_var(board_info, BoardInfo, State0),
      mcweb:chain_success(Req, State)
  end.

%% @private
%% @doc get visible threads
chain_get_threads(Req0, State0) ->
  {BoardId, Req1} = cowboy_req:binding(mcb_board, Req0),
  {Page0, Req} = cowboy_req:binding(mcb_page, Req1),
  {ok, PageSize0} = macaba_conf:get([<<"board">>, <<"page_size">>]),
  {ok, PreviewSize0} = macaba_conf:get([<<"board">>, <<"thread">>,
                                        <<"preview_last_posts">>]),
  Page = macaba:as_integer(Page0, 1),
  PageSize = macaba:as_integer(PageSize0),
  PreviewSize = macaba:as_integer(PreviewSize0),
  {ok, PinnedThreads, Threads, PageNums} = macaba_board_cli:get_threads(
                                             BoardId,
                                             {Page, PageSize},
                                             PreviewSize),
  State1 = mcweb:state_set_var(threads, Threads, State0),
  State2 = mcweb:state_set_var(pinned_threads, PinnedThreads, State1),
  State  = mcweb:state_set_var(page_nums, PageNums, State2),
  mcweb:chain_success(Req, State).

%%%-----------------------------------------------------------------------------
%% @doc HTTP POST: Create thread on board/id/new
%%%-----------------------------------------------------------------------------
-spec macaba_handle_thread_new(Method :: binary(),
                               Req :: cowboy_req:req(),
                               State :: mcweb:html_state()) ->
                                  mcweb:handler_return().

macaba_handle_thread_new(<<"POST">>, Req0, State0) ->
  {_, Req1, State1} = mcweb:chain_run(
                        [ fun chain_check_post_attach/2
                        , fun chain_thread_new/2
                        ], Req0, State0),
  {Req1, State1}.

%%%---------------------------------------------------
%% @private
%% @doc Checks that attach is recognized content type and doesn't exist in db
chain_check_post_attach(Req0, State0=#mcb_html_state{post_data=PD}) ->
  Attach = macaba:propget(<<"attach">>, PD, <<>>),
  case macaba_attach:detect_content_type(Attach) of
    {error, no_idea} ->
      mcweb:chain_fail(
        mcweb:render_error(<<"bad_attach_format">>, Req0, State0));
    {error, empty} ->
      %% do nothing for empty attach
      %% State1 = state_set_var(attach_key, <<>>, State0),
      mcweb:chain_success(Req0, State0);
    {ok, _} ->
      %% FIXME: this is calculated twice, here and in macaba_attach:write
      AttachKey = crypto:sha(Attach),
      AttachMod = macaba_plugins:mod(attachments),
      case AttachMod:exists(AttachKey) of
        true ->
          mcweb:chain_fail(
            mcweb:render_error(<<"duplicate_image">>, Req0, State0));
        false ->
          %% State1 = state_set_var(attach_key, AttachKey, State0),
          mcweb:chain_success(Req0, State0)
      end
  end.

%%%---------------------------------------------------
%% @private
%% @doc Creates new thread
chain_thread_new(Req0, State0) ->
  {BoardId, Req} = cowboy_req:binding(mcb_board, Req0),
  %% lager:debug("http POST new thread, board=~s", [BoardId]),
  PostOpt = get_post_create_options(Req, State0),
  ThreadOpt = orddict:from_list([]),
  case macaba_thread:new(BoardId, ThreadOpt, PostOpt) of
    {ok, _Thread, _Post} ->
      {BoardId, Req1} = cowboy_req:binding(mcb_board, Req0),
      mcweb:chain_success(
        mcweb:redirect("/board/" ++ macaba:as_string(BoardId), Req1, State0));
    {error, E} ->
      EStr = iolist_to_binary(io_lib:format("~p", [E])),
      mcweb:chain_fail(mcweb:render_error(EStr, Req, State0))
  end.

%%%-----------------------------------------------------------------------------
%% @doc Create post in thread on board/b_id/thread/t_id/post/new
%%%-----------------------------------------------------------------------------
-spec macaba_handle_post_new(Method :: binary(),
                             Req :: cowboy_req:req(),
                             State :: mcweb:html_state()) ->
                                mcweb:handler_return().

macaba_handle_post_new(<<"POST">>, Req0, State0) ->
  {_, Req1, State1} = mcweb:chain_run(
                        [ fun chain_check_thread_exists/2
                        , fun chain_check_post_attach/2
                        , fun chain_post_new/2
                        , fun chain_post_new_redirect/2
                        ], Req0, State0),
  {Req1, State1}.

chain_post_new_redirect(Req0, State0) ->
  {BoardId, Req1} = cowboy_req:binding(mcb_board, Req0),
  Post     = mcweb:state_get_var(created_post, State0),
  ThreadId = Post#mcb_post.thread_id,
  PostId   = Post#mcb_post.post_id,
  %% lager:debug("http POST reply, board=~s thread=~s", [BoardId, ThreadId]),
  {Req, State} = mcweb:redirect_to_thread_and_post(
                   BoardId, ThreadId, PostId, Req1, State0),
  mcweb:chain_success(Req, State).

%%%---------------------------------------------------
%% @private
%% @doc Creates new post reply in thread
chain_post_new(Req0, State0) ->
  {BoardId, Req} = cowboy_req:binding(mcb_board, Req0),
  PostOpt = get_post_create_options(Req, State0),
  case macaba_post:new(BoardId, PostOpt) of
    {ok, Post} ->
      State = mcweb:state_set_var(created_post, Post, State0),
      mcweb:chain_success(Req, State);
    {error, E0} ->
      E = iolist_to_binary(io_lib:format("~p", [E0])),
      mcweb:chain_fail(mcweb:render_error(E, Req, State0))
  end.

%%%---------------------------------------------------
%% @private
%% @doc Ensure that threadid posted exists
chain_check_thread_exists(Req0, State0=#mcb_html_state{post_data=PD}) ->
  {BoardId, Req1} = cowboy_req:binding(mcb_board, Req0),
  ThreadId = macaba:propget(<<"thread_id">>, PD, ""),
  case macaba_thread:get(BoardId, ThreadId) of
    {error, not_found} ->
      mcweb:chain_fail(
        mcweb:render_page(404, "thread_404", Req1, State0));
    _ ->
      mcweb:chain_success(Req1, State0)
  end.

%%%---------------------------------------------------
%% @private
get_post_create_options(Req0, #mcb_html_state{post_data=PD}) ->
  %% {ok, PostVals, Req1} = cowboy_req:body_qs(Req0),
  ThreadId = macaba:propget(<<"thread_id">>, PD, ""),
  Author   = macaba:propget(<<"author">>,    PD, ""),
  Email    = macaba:propget(<<"email">>,     PD, ""),
  Subject  = macaba:propget(<<"subject">>,   PD, ""),
  Message  = macaba:propget(<<"message">>,   PD, ""),
  Attach   = macaba:propget(<<"attach">>,    PD, ""),
  DeletePw = macaba:propget(<<"deletepw">>,  PD, ""),

  %% user identification and poster_id
  UserId   = mcweb:get_user_identification(Req0),
  PosterId = mcweb:get_poster_id(UserId),

  orddict:from_list([ {thread_id,  ThreadId}
                    , {author,     Author}
                    , {email,      Email}
                    , {subject,    Subject}
                    , {message,    Message}
                    , {attach,     Attach}
                    , {ident,      UserId}
                    , {poster_id,  PosterId}
                    , {deletepw,   DeletePw}
                    ]).

%%%-----------------------------------------------------------------------------
%% @doc Do POST board/b_id/thread/t_id/manage - delete posts by password
%%%-----------------------------------------------------------------------------
-spec macaba_handle_thread_manage(Method :: binary(),
                                  Req :: cowboy_req:req(),
                                  State :: mcweb:html_state()) ->
                                     mcweb:handler_return().

macaba_handle_thread_manage(<<"POST">>, Req0, State0) ->
  {_, Req1, State} = mcweb:chain_run(
                       [ fun chain_thread_manage_delete/2
                       ], Req0, State0),
  {ThreadId, Req2} = cowboy_req:binding(mcb_thread, Req1),
  {BoardId, Req3}  = cowboy_req:binding(mcb_board,  Req2),
  mcweb:redirect_to_thread(BoardId, ThreadId, Req3, State).

chain_thread_manage_delete(Req0, State0=#mcb_html_state{post_data=PD}) ->
  %%lager:debug("manage_delete post=~p", [PD]),
  MarkedPosts = macaba:propget(<<"array_mark">>, PD),
  Password = macaba:propget(<<"pass">>, PD),
  FileOnly = macaba:as_bool(macaba:propget(<<"fileonly">>, PD, false)),
  {BoardId, Req1}  = cowboy_req:binding(mcb_board,  Req0),
  lists:foreach(fun(M) ->
                    macaba_board_cli:anonymous_delete_post(
                      BoardId, M, FileOnly, Password)
                end, MarkedPosts),
  mcweb:chain_success(Req1, State0).

%%%-----------------------------------------------------------------------------
%% @doc Do GET board/b_id/thread/t_id - show thread contents
%%%-----------------------------------------------------------------------------
-spec macaba_handle_thread(Method :: binary(),
                           Req :: cowboy_req:req(),
                           State :: mcweb:html_state()) ->
                              mcweb:handler_return().

macaba_handle_thread(<<"GET">>, Req0, State0) ->
  {_, Req, State} = mcweb:chain_run(
                      [ fun chain_get_boards/2
                      , fun chain_get_board_info/2
                      , fun chain_get_thread_info/2
                      , fun chain_get_thread_posts/2
                      ], Req0, State0),
  mcweb:render_page("thread", Req, State).

%% @private
%% @doc get thread info if thread exists
chain_get_thread_info(Req0, State0) ->
  {ThreadId, Req} = cowboy_req:binding(mcb_thread, Req0),
  {BoardId, Req}  = cowboy_req:binding(mcb_board,  Req0),
  lager:debug("http GET thread ~s", [ThreadId]),
  case macaba_board_cli:get_thread(BoardId, ThreadId) of
    {error, not_found} ->
      mcweb:chain_fail(
        mcweb:render_page(404, "thread_404", Req, State0));
    ThreadInfo ->
      State = mcweb:state_set_var(thread_info, ThreadInfo, State0),
      mcweb:chain_success(Req, State)
  end.

%% @private
%% @doc get all posts in thread
chain_get_thread_posts(Req0, State0) ->
  {ThreadId, Req} = cowboy_req:binding(mcb_thread, Req0),
  {BoardId, Req}  = cowboy_req:binding(mcb_board,  Req0),
  Posts = macaba_board_cli:get_thread_preview(BoardId, ThreadId, all),
  State1 = mcweb:state_set_var(posts, Posts, State0),
  case Posts of
    [] ->
      mcweb:chain_fail(
        mcweb:render_page(404, "thread_404", Req0, State1));
    _ ->
      State2 = mcweb:state_set_var(first_post, hd(Posts), State1),
      mcweb:chain_success(Req, State2)
  end.

%%%-----------------------------------------------------------------------------
%% @doc Do GET attach/att_id
%%%-----------------------------------------------------------------------------
-spec macaba_handle_attach(Method :: binary(),
                           Req :: cowboy_req:req(),
                           State :: mcweb:html_state()) ->
                              mcweb:handler_return().

macaba_handle_attach(<<"GET">>, Req0, State0) ->
  State1 = mcweb:state_set_var(thumbnail, false, State0),
  {_, Req, State} = mcweb:chain_run(
                      [ fun chain_get_attach/2
                      , fun chain_attach_send/2
                      ], Req0, State1),
  {Req, State}.

%%%-----------------------------------------------------------------------------
%% @doc Do GET attach/att_id/thumb
%%%-----------------------------------------------------------------------------
-spec macaba_handle_attach_thumb(Method :: binary(),
                                 Req :: cowboy_req:req(),
                                 State :: mcweb:html_state()) ->
                                    mcweb:handler_return().

macaba_handle_attach_thumb(<<"GET">>, Req0, State0) ->
  State1 = mcweb:state_set_var(thumbnail, true, State0),
  {_, Req, State} = mcweb:chain_run(
                      [ fun chain_get_attach/2
                      , fun chain_attach_send/2
                      ], Req0, State1),
  {Req, State}.

%%%---------------------------------------------------
%% @private
%% @doc get thread info if thread exists
chain_get_attach(Req0, State0) ->
  {AttachId0, Req} = cowboy_req:binding(mcb_attach, Req0),
  AttachId = macaba:hexstr_to_bin(binary_to_list(AttachId0)),
  %% for primary image data attachid=attach_body_id
  State1 = mcweb:state_set_var(body_id, AttachId, State0),

  AttachMod = macaba_plugins:mod(attachments),
  case AttachMod:read_header(AttachId) of
    {error, not_found} ->
      mcweb:chain_fail(
        mcweb:render_page(404, "attach_404", Req, State1));
    {ok, Att = #mcb_attachment{}} ->
      %% for thumbnail attachid.thumbnail_hash=attach_body_id
      State2 = case mcweb:state_get_var(thumbnail, State1) of
                 false ->
                   lager:debug("http GET attach ~s",
                               [bin_to_hex:bin_to_hex(AttachId)]),
                   State1;
                 true ->
                   lager:debug("http GET thumb ~s",
                               [bin_to_hex:bin_to_hex(AttachId)]),
                   mcweb:state_set_var(
                     body_id, Att#mcb_attachment.thumbnail_hash,
                     State1)
               end,
      State = mcweb:state_set_var(attach, Att, State2),
      %% assume if header exists, then body exists too
      mcweb:chain_success(Req, State)
  end.

%%%---------------------------------------------------
%% @private
chain_attach_send(Req0, State0) ->
  %% TODO: Etag/if modified since support
  case State0#mcb_html_state.already_rendered of
    true ->
      mcweb:chain_success(Req0, State0);
    false ->
      Att       = mcweb:state_get_var(attach, State0),
      AttachId  = mcweb:state_get_var(body_id, State0),
      Headers   = [ {<<"Content-Type">>, Att#mcb_attachment.content_type} ],
      AttachMod = macaba_plugins:mod(attachments),

      {ok, AttBody} = AttachMod:read_body(AttachId),
      {ok, Req}     = cowboy_req:reply(
                        200, Headers, AttBody#mcb_attachment_body.data, Req0),
      State = State0#mcb_html_state{already_rendered=true},
      mcweb:chain_success(Req, State)
  end.

%%%-----------------------------------------------------------------------------
%%% HELPER FUNCTIONS
%%%-----------------------------------------------------------------------------


%%% Local Variables:
%%% erlang-indent-level: 2
%%% End:
