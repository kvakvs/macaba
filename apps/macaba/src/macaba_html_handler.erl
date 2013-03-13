%%%-----------------------------------------------------------------------------
%%% @doc This module has few predefined handlers (init, handle and terminate)
%%% which are called by cowboy on incoming HTTP request.
%%% Serves HTML templates, and provides basic HTTP access to the board.
%%% Created: 2013-02-16 Dmytro Lytovchenko <kvakvs@yandex.ru>
%%%-----------------------------------------------------------------------------
-module(macaba_html_handler).

-export([ init/3
        , handle/2
        , terminate/3]).
-export([ macaba_handle_index/2
        , macaba_handle_board/2
        , macaba_handle_thread/2
        , macaba_handle_thread_new/2
        , macaba_handle_thread_manage/2
        , macaba_handle_post_new/2
        , macaba_handle_attach/2
        , macaba_handle_attach_thumb/2
        %% , macaba_handle_admin/2
        , macaba_handle_util_preview/2
        ]).
-export([ chain_get_boards/1
        , chain_get_board_info/1
        , chain_get_threads/1
        , chain_get_thread_posts/1
        , chain_get_thread_info/1
        , chain_check_post_attach/1
        , chain_thread_new/1
        , chain_get_attach/1
        , chain_attach_send/1
        ]).

-include_lib("macaba/include/macaba_types.hrl").

%%%-----------------------------------------------------------------------------
init({_Transport, http}, Req, [Mode]) ->
  {ok, Req, #mcb_html_state{
         mode = Mode
        }}.

%%%-----------------------------------------------------------------------------
%% TODO: merge this with macaba_html_admin
handle(Req0, State0 = #mcb_html_state{ mode=Mode }) ->
  try
    {Method, Req1} = cowboy_req:method(Req0),

    %% parse request body as multipart, this will not work for POST urlencoded
    {Req2, State1} = case macaba_web:is_POST_and_multipart(Req1) of
                       {true, true}  ->
                         macaba_web:parse_multipart_form_data(Req1, State0);
                       {true, false}  ->
                         macaba_web:parse_body_qs(Req1, State0);
                       {false, _} ->
                         {Req1, State0}
                     end,
    {Req3, State2} = macaba_web:get_user(Req2, State1),
    FnName = macaba:as_atom("macaba_handle_" ++ macaba:as_string(Mode)),
    {Req4, State3} = apply(?MODULE, FnName, [Method, {Req3, State2}]),
    {ok, Req4, State3}
  catch
    E -> T = lists:flatten(io_lib:format("handle error: ~p ~p",
                                         [E, erlang:get_stacktrace()])),
         lager:error(E),
         {ReqE, StateE} = macaba_web:response_text(500, T, Req0, State0),
         {ok, ReqE, StateE}
  end.

%%%-----------------------------------------------------------------------------
terminate(_Reason, _Req, _State) ->
  ok.

%% %%%-----------------------------------------------------------------------------
%% %% @doc GET/POST: /admin
%% %%%-----------------------------------------------------------------------------
%% macaba_handle_admin(<<"GET">>, {Req0, State0}) ->
%%   lager:debug("http GET admin"),
%%   %% Boards = macaba_board_cli:get_boards(),
%%   %% State1 = state_set_var(boards, Boards, State0),
%%   {_, {Req, State}} = macaba_web:chain_run(
%%                         [ fun chain_get_boards/1
%%                         , fun(X) -> chain_fail_if_user(X, anon) end
%%                         ], {Req0, State0}),
%%   render_page("admin", Req, State);

%% macaba_handle_admin(<<"POST">>, {Req0, State0}) ->
%%   lager:debug("http POST admin"),
%%   {_, {Req, State}} = macaba_web:chain_run(
%%                         [ fun chain_check_admin_login/1
%%                         , fun chain_check_mod_login/1
%%                         ], {Req0, State0}),
%%   redirect("/admin/", Req, State).

%% %% @private
%% %% @doc Checks admin login and password from macaba.config
%% chain_check_admin_login({Req0, State0=#mcb_html_state{ post_data=PD }}) ->
%%   {ok, ALogin} = macaba_conf:get([<<"board">>, <<"admin_login">>]),
%%   {ok, APassword} = macaba_conf:get([<<"board">>, <<"admin_password">>]),
%%   Login = macaba:propget(<<"login">>, PD),
%%   Password = macaba:propget(<<"password">>, PD),
%%   %% lager:debug("L=~s:P=~s AL=~s:AP=~s", [Login, Password, ALogin, APassword]),
%%   case {ALogin =:= Login, APassword =:= Password} of
%%     {true, true} ->
%%       {Req, State} = create_session_for(#mcb_user{type=admin}, Req0, State0),
%%       %% stop checking passwords right here
%%       {error, {Req, State}};
%%     _ ->
%%       {ok, {Req0, State0}}
%%   end.

%% %% @private
%% %% @doc Checks mod login and password from database
%% chain_check_mod_login({Req0, State0}) ->
%%   {ok, {Req0, State0}}.

%% %% @private
%% %% @doc Gets user from ses cookie, checks if its type is Role, changes to login
%% %% page if user.type=Role
%% chain_fail_if_user({Req0, State0}, Role) ->
%%   {Req, State} = get_user(Req0, State0),
%%   #mcb_user{type=Type} = State#mcb_html_state.user,
%%   case Type of
%%     Role ->
%%       %% Login required if accessing this as anonymous
%%       {error, render_page("admin_login", Req, State)};
%%     _ ->
%%       {ok, {Req, State}}
%%   end.

%%%-----------------------------------------------------------------------------
%%% Utility: Preview markup
%%%-----------------------------------------------------------------------------
macaba_handle_util_preview(<<"POST">>, {Req0, State0}) ->
  lager:debug("http POST util/preview"),
  PD = State0#mcb_html_state.post_data,
  Message = macaba:propget(<<"markup">>, PD, <<>>),
  MessageProcessed = macaba_plugins:call(markup, [Message]),
  macaba_web:response_text(200, MessageProcessed, Req0, State0).

%%%-----------------------------------------------------------------------------
%% @doc GET /
%%%-----------------------------------------------------------------------------
macaba_handle_index(<<"GET">>, {Req0, State0}) ->
  lager:debug("http GET root"),
  Boards = macaba_board_cli:get_boards(),
  State1 = macaba_web:state_set_var(boards, Boards, State0),
  macaba_web:render_page("index", Req0, State1).

%%%-----------------------------------------------------------------------------
%% @doc Do GET board/id/
%%%-----------------------------------------------------------------------------
macaba_handle_board(<<"GET">>, {Req0, State0}) ->
  {_, {Req, State}} = macaba_web:chain_run(
                        [ fun chain_get_boards/1
                        , fun chain_get_board_info/1
                        , fun chain_get_threads/1
                        ], {Req0, State0}),
  macaba_web:render_page("board", Req, State).

chain_get_boards({Req, State0}) ->
  Boards = macaba_board_cli:get_boards(),
  State = macaba_web:state_set_var(boards, Boards, State0),
  {ok, {Req, State}}.

%% @private
%% @doc get current board info
chain_get_board_info({Req0, State0}) ->
  {BoardId, Req} = cowboy_req:binding(mcb_board, Req0),
  lager:debug("http GET board ~s", [BoardId]),
  case macaba_board_cli:get_board(BoardId) of
    {error, not_found} ->
      {Req1, State1} = macaba_web:render_page(404, "board_404", Req, State0),
      {error, {Req1, State1}};
    BoardInfo ->
      State = macaba_web:state_set_var(board_info, BoardInfo, State0),
      {ok, {Req, State}}
  end.

%% @private
%% @doc get visible threads
chain_get_threads({Req0, State0}) ->
  {BoardId, Req1} = cowboy_req:binding(mcb_board, Req0),
  {Page, Req} = cowboy_req:binding(mcb_page, Req1),
  {ok, BPageSize} = macaba_conf:get([<<"board">>, <<"page_size">>]),
  {ok, PreviewSize} = macaba_conf:get([<<"board">>, <<"thread">>,
                                       <<"preview_last_posts">>]),
  Threads = macaba_board_cli:get_threads(
              BoardId, {macaba:as_integer(Page, 1),
                        macaba:as_integer(BPageSize)},
              macaba:as_integer(PreviewSize)),
  State = macaba_web:state_set_var(threads, Threads, State0),
  {ok, {Req, State}}.

%%%-----------------------------------------------------------------------------
%% @doc HTTP POST: Create thread on board/id/new
%%%-----------------------------------------------------------------------------
macaba_handle_thread_new(<<"POST">>, {Req0, State0}) ->
  {_, {Req1, State1}} = macaba_web:chain_run(
                        [ fun chain_check_post_attach/1
                        , fun chain_thread_new/1
                        ], {Req0, State0}),
  {BoardId, Req2} = cowboy_req:binding(mcb_board, Req1),
  macaba_web:redirect("/board/" ++ macaba:as_string(BoardId), Req2, State1).

%%%---------------------------------------------------
%% @private
%% @doc Checks that attach is recognized content type and doesn't exist in db
chain_check_post_attach({Req0, State0=#mcb_html_state{post_data=PD}}) ->
  Attach = macaba:propget(<<"attach">>, PD, <<>>),
  case macaba_attach:detect_content_type(Attach) of
    {error, no_idea} ->
      macaba_web:render_error(<<"bad_attach_format">>, Req0, State0);
    {error, empty} ->
      %% do nothing for empty attach
      %% State1 = state_set_var(attach_key, <<>>, State0),
      {ok, {Req0, State0}};
    {ok, _} ->
      %% FIXME: this is calculated twice, here and in macaba_attach:write
      AttachKey = crypto:sha(Attach),
      AttachMod = macaba_plugins:mod(attachments),
      case AttachMod:exists(AttachKey) of
        true ->
          macaba_web:render_error(<<"duplicate_image">>, Req0, State0);
        false ->
          %% State1 = state_set_var(attach_key, AttachKey, State0),
          {ok, {Req0, State0}}
      end
  end.

%%%---------------------------------------------------
%% @private
%% @doc Creates new thread
chain_thread_new({Req0, State0}) ->
  {BoardId, Req} = cowboy_req:binding(mcb_board, Req0),
  lager:debug("http POST new thread, board=~s", [BoardId]),
  PostOpt = get_post_create_options(Req, State0),
  ThreadOpt = orddict:from_list([]),
  macaba_thread:new(BoardId, ThreadOpt, PostOpt),
  {ok, {Req, State0}}.

%%%-----------------------------------------------------------------------------
%% @doc Create post in thread on board/b_id/thread/t_id/post/new
%%%-----------------------------------------------------------------------------
macaba_handle_post_new(<<"POST">>, {Req0, State0}) ->
  {_, {Req1, State1}} = macaba_web:chain_run(
                        [ fun chain_check_thread_exists/1
                        , fun chain_check_post_attach/1
                        , fun chain_post_new/1
                        , fun chain_post_new_redirect/1
                        ], {Req0, State0}),
  {Req1, State1}.

chain_post_new_redirect({Req0, State0}) ->
  {BoardId, Req1} = cowboy_req:binding(mcb_board, Req0),
  Post     = macaba_web:state_get_var(created_post, State0),
  ThreadId = Post#mcb_post.thread_id,
  PostId   = Post#mcb_post.post_id,
  lager:debug("http POST reply, board=~s thread=~s", [BoardId, ThreadId]),
  {Req, State} = macaba_web:redirect_to_thread_and_post(
                   BoardId, ThreadId, PostId, Req1, State0),
  {ok, {Req, State}}.

%%%---------------------------------------------------
%% @private
%% @doc Creates new post reply in thread
chain_post_new({Req0, State0}) ->
  {BoardId, Req} = cowboy_req:binding(mcb_board, Req0),
  PostOpt = get_post_create_options(Req, State0),
  case macaba_post:new(BoardId, PostOpt) of
    {ok, Post} ->
      State = macaba_web:state_set_var(created_post, Post, State0),
      {ok, {Req, State}};
    {error, E} ->
      macaba_web:render_error(E, Req, State0)
  end.

%%%---------------------------------------------------
%% @private
%% @doc Ensure that threadid posted exists
chain_check_thread_exists({Req0, State0=#mcb_html_state{post_data=PD}}) ->
  {BoardId, Req1} = cowboy_req:binding(mcb_board, Req0),
  ThreadId = macaba:propget(<<"thread_id">>, PD, ""),
  case macaba_thread:get(BoardId, ThreadId) of
    {error, not_found} ->
      {Req2, State1} = macaba_web:render_page(404, "thread_404", Req1, State0),
      {error, {Req2, State1}};
    _ ->
      {ok, {Req1, State0}}
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
  PosterId = macaba_web:get_poster_id(Req0),

  orddict:from_list([ {thread_id,  ThreadId}
                    , {author,     Author}
                    , {email,      Email}
                    , {subject,    Subject}
                    , {message,    Message}
                    , {attach,     Attach}
                    , {poster_id,  PosterId}
                    %% , {attach_key, state_get_var(attach_key, State)}
                    , {deletepw,   DeletePw}
                    ]).

%%%-----------------------------------------------------------------------------
%% @doc Do POST board/b_id/thread/t_id/manage - delete posts by password
%%%-----------------------------------------------------------------------------
macaba_handle_thread_manage(<<"POST">>, {Req0, State0}) ->
  {_, {Req1, State}} = macaba_web:chain_run(
                         [ fun chain_thread_manage_delete/1
                         ], {Req0, State0}),
  {ThreadId, Req2} = cowboy_req:binding(mcb_thread, Req1),
  {BoardId, Req3}  = cowboy_req:binding(mcb_board,  Req2),
  macaba_web:redirect_to_thread(BoardId, ThreadId, Req3, State).

chain_thread_manage_delete({Req0, State0=#mcb_html_state{post_data=PD}}) ->
  %%lager:debug("manage_delete post=~p", [PD]),
  MarkedPosts = macaba:propget(<<"array_mark">>, PD),
  Password = macaba:propget(<<"pass">>, PD),
  FileOnly = macaba:as_bool(macaba:propget(<<"fileonly">>, PD, false)),
  {BoardId, Req1}  = cowboy_req:binding(mcb_board,  Req0),
  lists:foreach(fun(M) ->
                    macaba_board_cli:anonymous_delete_post(
                      BoardId, M, FileOnly, Password)
                end, MarkedPosts),
  {ok, {Req1, State0}}.

%%%-----------------------------------------------------------------------------
%% @doc Do GET board/b_id/thread/t_id - show thread contents
%%%-----------------------------------------------------------------------------
macaba_handle_thread(<<"GET">>, {Req0, State0}) ->
  {_, {Req, State}} = macaba_web:chain_run(
                         [ fun chain_get_boards/1
                         , fun chain_get_board_info/1
                         , fun chain_get_thread_info/1
                         , fun chain_get_thread_posts/1
                         ], {Req0, State0}),
  macaba_web:render_page("thread", Req, State).

%% @private
%% @doc get thread info if thread exists
chain_get_thread_info({Req0, State0}) ->
  {ThreadId, Req} = cowboy_req:binding(mcb_thread, Req0),
  {BoardId, Req}  = cowboy_req:binding(mcb_board,  Req0),
  lager:debug("http GET thread ~s", [ThreadId]),
  case macaba_board_cli:get_thread(BoardId, ThreadId) of
    {error, not_found} ->
      {error, macaba_web:render_page(404, "thread_404", Req, State0)};
    ThreadInfo ->
      State = macaba_web:state_set_var(thread_info, ThreadInfo, State0),
      {ok, {Req, State}}
  end.

%% @private
%% @doc get all posts in thread
chain_get_thread_posts({Req0, State0}) ->
  {ThreadId, Req} = cowboy_req:binding(mcb_thread, Req0),
  {BoardId, Req}  = cowboy_req:binding(mcb_board,  Req0),
  Posts = macaba_board_cli:get_thread_preview(BoardId, ThreadId, all),
  State1 = macaba_web:state_set_var(posts, Posts, State0),
  case Posts of
    [] ->
      {error, macaba_web:render_page(404, "thread_404", Req0, State1)};
    _ ->
      State2 = macaba_web:state_set_var(first_post, hd(Posts), State1),
      {ok, {Req, State2}}
  end.


%%%-----------------------------------------------------------------------------
%% @doc Do GET attach/att_id
%%%-----------------------------------------------------------------------------
macaba_handle_attach(<<"GET">>, {Req0, State0}) ->
  State1 = macaba_web:state_set_var(thumbnail, false, State0),
  {_, {Req, State}} = macaba_web:chain_run(
                         [ fun chain_get_attach/1
                         , fun chain_attach_send/1
                         ], {Req0, State1}),
  {Req, State}.

%%%-----------------------------------------------------------------------------
%% @doc Do GET attach/att_id/thumb
%%%-----------------------------------------------------------------------------
macaba_handle_attach_thumb(<<"GET">>, {Req0, State0}) ->
  State1 = macaba_web:state_set_var(thumbnail, true, State0),
  {_, {Req, State}} = macaba_web:chain_run(
                         [ fun chain_get_attach/1
                         , fun chain_attach_send/1
                         ], {Req0, State1}),
  {Req, State}.

%%%---------------------------------------------------
%% @private
%% @doc get thread info if thread exists
chain_get_attach({Req0, State0}) ->
  {AttachId0, Req} = cowboy_req:binding(mcb_attach, Req0),
  AttachId = macaba:hexstr_to_bin(binary_to_list(AttachId0)),
  %% for primary image data attachid=attach_body_id
  State1 = macaba_web:state_set_var(body_id, AttachId, State0),

  AttachMod = macaba_plugins:mod(attachments),
  case AttachMod:read_header(AttachId) of
    {error, not_found} ->
      {Req1, State} = macaba_web:render_page(404, "attach_404", Req, State1),
      {error, {Req1, State}};
    {ok, Att = #mcb_attachment{}} ->
      %% for thumbnail attachid.thumbnail_hash=attach_body_id
      State2 = case macaba_web:state_get_var(thumbnail, State1) of
                 false ->
                   lager:debug("http GET attach ~s",
                               [bin_to_hex:bin_to_hex(AttachId)]),
                   State1;
                 true ->
                   lager:debug("http GET thumb ~s",
                               [bin_to_hex:bin_to_hex(AttachId)]),
                   macaba_web:state_set_var(
                     body_id, Att#mcb_attachment.thumbnail_hash,
                     State1)
               end,
      State = macaba_web:state_set_var(attach, Att, State2),
      %% assume if header exists, then body exists too
      {ok, {Req, State}}
  end.

%%%---------------------------------------------------
%% @private
chain_attach_send({Req0, State0}) ->
  %% TODO: Etag/if modified since support
  case State0#mcb_html_state.already_rendered of
    true ->
      {ok, {Req0, State0}};

    false ->
      Att       = macaba_web:state_get_var(attach, State0),
      AttachId  = macaba_web:state_get_var(body_id, State0),
      Headers   = [ {<<"Content-Type">>, Att#mcb_attachment.content_type} ],
      AttachMod = macaba_plugins:mod(attachments),

      {ok, AttBody} = AttachMod:read_body(AttachId),
      {ok, Req}     = cowboy_req:reply(
                        200, Headers, AttBody#mcb_attachment_body.data, Req0),
      State = State0#mcb_html_state{already_rendered=true},
      {ok, {Req, State}}
  end.

%%%-----------------------------------------------------------------------------
%%% HELPER FUNCTIONS
%%%-----------------------------------------------------------------------------


%%% Local Variables:
%%% erlang-indent-level: 2
%%% End:
