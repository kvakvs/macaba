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
        , macaba_handle_admin/2
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

-type board_cmd_t() :: index | board | thread | thread_new.
-record(mcb_html_state, {
          mode           :: board_cmd_t(),
          page_vars = [] :: orddict:orddict(),
          already_rendered = false :: boolean(),
          post_data = [] :: orddict:orddict(),
          user :: #mcb_user{}
         }).

%%%-----------------------------------------------------------------------------
init({_Transport, http}, Req, [Mode]) ->
  {ok, Req, #mcb_html_state{
         mode = Mode
        }}.

%%%-----------------------------------------------------------------------------
handle(Req0, State0 = #mcb_html_state{ mode=Mode }) ->
  try
    {Method, Req1} = cowboy_req:method(Req0),

    %% parse request body as multipart, this will not work for POST urlencoded
    {Req2, State1} = case is_POST_and_multipart(Req1) of
                       {true, true}  -> parse_multipart_form_data(Req1, State0);
                       {true, false}  -> parse_body_qs(Req1, State0);
                       {false, _} -> {Req1, State0}
                     end,
    {Req3, State2} = get_user(Req2, State1),
    FnName = macaba:as_atom("macaba_handle_" ++ macaba:as_string(Mode)),
    {Req4, State3} = apply(?MODULE, FnName, [Method, {Req3, State2}]),
    {ok, Req4, State3}
  catch
    E -> T = lists:flatten(io_lib:format("handle error: ~p ~p",
                                         [E, erlang:get_stacktrace()])),
         lager:error(E),
         {ReqE, StateE} = response_text(500, T, Req0, State0),
         {ok, ReqE, StateE}
  end.

%%%-----------------------------------------------------------------------------
terminate(_Reason, _Req, _State) ->
  ok.

%%%-----------------------------------------------------------------------------
%% @doc GET/POST: /admin
%%%-----------------------------------------------------------------------------
macaba_handle_admin(<<"GET">>, {Req0, State0}) ->
  lager:debug("http GET admin"),
  %% Boards = macaba_board_cli:get_boards(),
  %% State1 = state_set_var(boards, Boards, State0),
  {_, {Req, State}} = macaba_web:chain_run(
                        [ fun chain_get_boards/1
                        , fun(X) -> chain_fail_if_user(X, anon) end
                        ], {Req0, State0}),
  render_page("admin", Req, State);

macaba_handle_admin(<<"POST">>, {Req0, State0}) ->
  lager:debug("http POST admin"),
  {_, {Req, State}} = macaba_web:chain_run(
                        [ fun chain_check_admin_login/1
                        , fun chain_check_mod_login/1
                        ], {Req0, State0}),
  redirect("/admin/", Req, State).

%% @private
%% @doc Checks admin login and password from macaba.config
chain_check_admin_login({Req0, State0=#mcb_html_state{ post_data=PD }}) ->
  {ok, ALogin} = macaba_conf:get([<<"board">>, <<"admin_login">>]),
  {ok, APassword} = macaba_conf:get([<<"board">>, <<"admin_password">>]),
  Login = macaba:propget(<<"login">>, PD),
  Password = macaba:propget(<<"password">>, PD),
  case {ALogin =:= Login, APassword = Password} of
    {true, true} ->
      {Req, State} = create_session_for(#mcb_user{type=admin}, Req0, State0),
      {ok, {Req, State}};
    _ ->
      {ok, {Req0, State0}}
  end.

%% @private
%% @doc Checks mod login and password from database
chain_check_mod_login({Req0, State0}) ->
  {ok, {Req0, State0}}.

%% @private
%% @doc Gets user from ses cookie, checks if its type is Role, changes to login
%% page if user.type=Role
chain_fail_if_user({Req0, State0}, Role) ->
  {Req, State} = get_user(Req0, State0),
  #mcb_user{type=Type} = State#mcb_html_state.user,
  case Type of
    Role ->
      %% Login required if accessing this as anonymous
      {error, render_page("admin_login", Req, State)};
    _ ->
      {ok, {Req, State}}
  end.

%%%-----------------------------------------------------------------------------
%% @doc GET /
%%%-----------------------------------------------------------------------------
macaba_handle_index(<<"GET">>, {Req0, State0}) ->
  lager:debug("http GET root"),
  Boards = macaba_board_cli:get_boards(),
  State1 = state_set_var(boards, Boards, State0),
  render_page("index", Req0, State1).

%%%-----------------------------------------------------------------------------
%% @doc Do GET board/id/
%%%-----------------------------------------------------------------------------
macaba_handle_board(<<"GET">>, {Req0, State0}) ->
  {_, {Req, State}} = macaba_web:chain_run(
                        [ fun chain_get_boards/1
                        , fun chain_get_board_info/1
                        , fun chain_get_threads/1
                        ], {Req0, State0}),
  render_page("board", Req, State).

chain_get_boards({Req, State0}) ->
  Boards = macaba_board_cli:get_boards(),
  State = state_set_var(boards, Boards, State0),
  {ok, {Req, State}}.

%% @private
%% @doc get current board info
chain_get_board_info({Req0, State0}) ->
  {BoardId, Req} = cowboy_req:binding(mcb_board, Req0),
  lager:debug("http GET board ~s", [BoardId]),
  case macaba_board_cli:get_board(BoardId) of
    {error, not_found} ->
      {Req1, State1} = render_page(404, "board_404", Req, State0),
      {error, {Req1, State1}};
    BoardInfo ->
      State = state_set_var(board_info, BoardInfo, State0),
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
              BoardId, {macaba:as_integer(Page, 0),
                        macaba:as_integer(BPageSize)},
              macaba:as_integer(PreviewSize)),
  State = state_set_var(threads, Threads, State0),
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
  redirect("/board/" ++ macaba:as_string(BoardId), Req2, State1).

%%%---------------------------------------------------
%% @private
%% @doc Checks that attach is recognized content type and doesn't exist in db
chain_check_post_attach({Req0, State0=#mcb_html_state{post_data=PD}}) ->
  Attach = macaba:propget(<<"attach">>, PD, <<>>),
  case macaba_attach:detect_content_type(Attach) of
    {error, no_idea} ->
      render_error(<<"bad_attach_format">>, Req0, State0);
    {error, empty} ->
      %% do nothing for empty attach
      State1 = state_set_var(attach_key, <<>>, State0),
      {ok, {Req0, State1}};
    {ok, _} ->
      AttachKey = crypto:sha(Attach),
      AttachMod = macaba_plugins:mod(attachments),
      case AttachMod:exists(AttachKey) of
        true ->
          render_error(<<"duplicate_image">>, Req0, State0);
        false ->
          State1 = state_set_var(attach_key, AttachKey, State0),
          {ok, {Req0, State1}}
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
  {BoardId, Req} = cowboy_req:binding(mcb_board, Req0),
  Post           = state_get_var(created_post, State0),
  ThreadId       = Post#mcb_post.thread_id,
  PostId         = Post#mcb_post.post_id,
  lager:debug("http POST reply, board=~s thread=~s", [BoardId, ThreadId]),
  {ok, redirect_to_thread_and_post(BoardId, ThreadId, PostId, Req, State0)}.

%%%---------------------------------------------------
%% @private
%% @doc Creates new post reply in thread
chain_post_new({Req0, State0}) ->
  {BoardId, Req} = cowboy_req:binding(mcb_board, Req0),
  PostOpt = get_post_create_options(Req, State0),
  case macaba_post:new(BoardId, PostOpt) of
    {ok, Post} ->
      State = state_set_var(created_post, Post, State0),
      {ok, {Req, State}};
    {error, E} ->
      render_error(E, Req, State0)
  end.

%%%---------------------------------------------------
%% @private
%% @doc Ensure that threadid posted exists
chain_check_thread_exists({Req0, State0=#mcb_html_state{post_data=PD}}) ->
  {BoardId, Req1} = cowboy_req:binding(mcb_board, Req0),
  ThreadId = macaba:propget(<<"thread_id">>, PD, ""),
  case macaba_thread:get(BoardId, ThreadId) of
    {error, not_found} ->
      {Req2, State1} = render_page(404, "thread_404", Req1, State0),
      {error, {Req2, State1}};
    _ ->
      {ok, {Req1, State0}}
  end.

%%%---------------------------------------------------
%% @private
get_post_create_options(_Req0, State=#mcb_html_state{post_data=PD}) ->
  %% {ok, PostVals, Req1} = cowboy_req:body_qs(Req0),
  ThreadId = macaba:propget(<<"thread_id">>, PD, ""),
  Author   = macaba:propget(<<"author">>,    PD, ""),
  Email    = macaba:propget(<<"email">>,     PD, ""),
  Subject  = macaba:propget(<<"subject">>,   PD, ""),
  Message  = macaba:propget(<<"message">>,   PD, ""),
  Attach   = macaba:propget(<<"attach">>,    PD, ""),
  DeletePw = macaba:propget(<<"deletepw">>,  PD, ""),

  orddict:from_list([ {thread_id,  ThreadId}
                    , {author,     Author}
                    , {email,      Email}
                    , {subject,    Subject}
                    , {message,    Message}
                    , {attach,     Attach}
                    , {attach_key, state_get_var(attach_key, State)}
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
  redirect_to_thread(BoardId, ThreadId, Req3, State).

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
  render_page("thread", Req, State).

%% @private
%% @doc get thread info if thread exists
chain_get_thread_info({Req0, State0}) ->
  {ThreadId, Req} = cowboy_req:binding(mcb_thread, Req0),
  {BoardId, Req}  = cowboy_req:binding(mcb_board,  Req0),
  lager:debug("http GET thread ~s", [ThreadId]),
  case macaba_board_cli:get_thread(BoardId, ThreadId) of
    {error, not_found} ->
      {error, render_page(404, "thread_404", Req, State0)};
    ThreadInfo ->
      State = state_set_var(thread_info, ThreadInfo, State0),
      {ok, {Req, State}}
  end.

%% @private
%% @doc get all posts in thread
chain_get_thread_posts({Req0, State0}) ->
  {ThreadId, Req} = cowboy_req:binding(mcb_thread, Req0),
  {BoardId, Req}  = cowboy_req:binding(mcb_board,  Req0),
  Posts = macaba_board_cli:get_thread_preview(BoardId, ThreadId, all),
  State1 = state_set_var(posts, Posts, State0),
  case Posts of
    [] ->
      {error, render_page(404, "thread_404", Req0, State1)};
    _ ->
      State2 = state_set_var(first_post, hd(Posts), State1),
      {ok, {Req, State2}}
  end.


%%%-----------------------------------------------------------------------------
%% @doc Do GET attach/att_id
%%%-----------------------------------------------------------------------------
macaba_handle_attach(<<"GET">>, {Req0, State0}) ->
  State1 = state_set_var(thumbnail, false, State0),
  {_, {Req, State}} = macaba_web:chain_run(
                         [ fun chain_get_attach/1
                         , fun chain_attach_send/1
                         ], {Req0, State1}),
  {Req, State}.

%%%-----------------------------------------------------------------------------
%% @doc Do GET attach/att_id/thumb
%%%-----------------------------------------------------------------------------
macaba_handle_attach_thumb(<<"GET">>, {Req0, State0}) ->
  State1 = state_set_var(thumbnail, true, State0),
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
  State1 = state_set_var(body_id, AttachId, State0),

  AttachMod = macaba_plugins:mod(attachments),
  case AttachMod:read_header(AttachId) of
    {error, not_found} ->
      {Req1, State} = render_page(404, "attach_404", Req, State1),
      {error, {Req1, State}};
    Att ->
      %% for thumbnail attachid.thumbnail_hash=attach_body_id
      State2 = case state_get_var(thumbnail, State1) of
                 false ->
                   lager:debug("http GET attach ~s",
                               [bin_to_hex:bin_to_hex(AttachId)]),
                   State1;
                 true ->
                   lager:debug("http GET thumb ~s",
                               [bin_to_hex:bin_to_hex(AttachId)]),
                   state_set_var(
                     body_id, Att#mcb_attachment.thumbnail_hash,
                     State1)
               end,
      State = state_set_var(attach, Att, State2),
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
      Att       = state_get_var(attach, State0),
      AttachId  = state_get_var(body_id, State0),
      Headers   = [ {<<"Content-Type">>, Att#mcb_attachment.content_type} ],
      AttachMod = macaba_plugins:mod(attachments),
      AttBody   = AttachMod:read_body(AttachId),
      {ok, Req} = cowboy_req:reply(
                    200, Headers, AttBody#mcb_attachment_body.data, Req0),
      State = State0#mcb_html_state{already_rendered=true},
      {ok, {Req, State}}
  end.

%%%-----------------------------------------------------------------------------
%%% HELPER FUNCTIONS
%%%-----------------------------------------------------------------------------

%%%-----------------------------------------------------------------------------
%% @private
render_page(TemplateName, Req0, State0) ->
  render_page(200, TemplateName, Req0, State0).

%% @private
render_page(HttpStatus, TemplateName, Req0,
            State=#mcb_html_state{
              page_vars=PageVars,
              already_rendered=false
             }) ->
  %% lager:debug("Before render: vars=~p", [PageVars]),
  Body = macaba_web:render(TemplateName, PageVars),
  Headers = [{<<"Content-Type">>, <<"text/html">>}],
  {ok, Req} = cowboy_req:reply(HttpStatus, Headers, Body, Req0),
  {Req, State#mcb_html_state{already_rendered=true}};

render_page(_, _, Req, State=#mcb_html_state{already_rendered=true}) ->
  {Req, State}.

%%%-----------------------------------------------------------------------------
%% @private
response_text(HttpStatus, Body, Req0, State=#mcb_html_state{}) ->
  Headers = [{<<"Content-Type">>, <<"text/plain">>}],
  {ok, Req} = cowboy_req:reply(HttpStatus, Headers, Body, Req0),
  {Req, State}.

%%%-----------------------------------------------------------------------------
%% @private
redirect(URL, Req0, State) ->
  {ok, Req} = cowboy_req:reply(
                301, [{<<"Location">>, macaba:as_binary(URL)}],
                <<>>, Req0),
  {Req, State}.

%%%-----------------------------------------------------------------------------
%% @private
redirect_to_thread(BoardId, ThreadId, Req, State) ->
   redirect("/board/" ++ macaba:as_string(BoardId) ++ "/thread/"
                ++ macaba:as_string(ThreadId), Req, State).

%%%-----------------------------------------------------------------------------
%% @private
redirect_to_thread_and_post(BoardId, ThreadId, PostId, Req, State) ->
   redirect("/board/" ++ macaba:as_string(BoardId) ++ "/thread/"
                ++ macaba:as_string(ThreadId) ++ "#i"
                ++ macaba:as_string(PostId), Req, State).

%%%-----------------------------------------------------------------------------
%% @private
render_error(Msg, Req0, State0) ->
  State1 = state_set_var(error, Msg, State0),
  {Req1, State2} = render_page(400, "error", Req0, State1),
  {error, {Req1, State2}}.

%%%-----------------------------------------------------------------------------
%% @private
state_set_var(K, V, State = #mcb_html_state{ page_vars=P0 }) ->
  P = orddict:store(K, V, P0),
  State#mcb_html_state{ page_vars = P }.

%% @private
state_get_var(K, #mcb_html_state{ page_vars=PV }) ->
  orddict:fetch(K, PV).

%%%-----------------------------------------------------------------------------
%% @private
%% @doc Returns pair of boolean() for POST method and multipart/* content-type
is_POST_and_multipart(Req0) ->
  {Method, Req1} = cowboy_req:method(Req0),
  {CT, _Req2} = cowboy_req:header(<<"content-type">>, Req1),
  Post = case Method of
           <<"POST">>  -> true;
           _ -> false
         end,
  Multipart = case CT of
                <<"multipart/", _/binary>> -> true;
                _ -> false
              end,
  {Post, Multipart}.

%%%-----------------------------------------------------------------------------
%% @private
%% @doc Retrieves POST body with urlencoded form data and saves it to
%% state.post_data
parse_body_qs(Req0, State0) ->
  {ok, PD0, Req1} = cowboy_req:body_qs(Req0),
  %%PD = [{K, V} || {K, V} <- PD0],
  {Req1, State0#mcb_html_state{ post_data = orddict:from_list(PD0) }}.

%%%-----------------------------------------------------------------------------
%% @private
%% @doc Retrieves POST body with multipart form data, parses fields and array
%% fields and saves it to state.post_data
parse_multipart_form_data(Req0, State0) ->
  {MPD, Req1} = acc_multipart(Req0),
  %% lager:debug("{{post}} multipart data ~p", [MPD]),
  {Req1, parse_multipart_form_data_1(MPD, State0)}.

%% @private
parse_multipart_form_data_1([], State) -> State;
parse_multipart_form_data_1([{Headers, Value} | Rest],
                            State=#mcb_html_state{ post_data=PD0 }) ->
  FieldName = get_multipart_field_name(Headers),
  PD1 = set_multipart_value(FieldName, Value, PD0),

  ContentType = macaba:propget(<<"content-type">>, Headers, undefined),
  PD = orddict:store({content_type, FieldName}, ContentType, PD1),

  %% lager:debug("parse field ~p value ~p", [FieldName, Value]),
  State1 = State#mcb_html_state{ post_data=PD },
  parse_multipart_form_data_1(Rest, State1).

%% @private
get_multipart_field_name([{<<"content-disposition">>, Bin} | _]) ->
  [<<"form-data">>|Parts] = binary:split(Bin, <<";">>, [global, trim]),
  [Ret] = [begin
             $" = binary:last(Name),
             binary:part(Name, 0, byte_size(Name) - 1)
           end || <<" name=\"", Name/binary>> <- Parts],
  Ret.

%%%-----------------------------------------------------------------------------
%% @private
%% @doc Sets field value parsed from POST multipart form, if value name starts
%% with "array_" sets it as list instead, accumulating multiple values
set_multipart_value(<<"array_", _/binary>>=Name, Value, []) ->
  orddict:store(Name, [Value], []);
set_multipart_value(<<"array_", _/binary>>=Name, Value, Dict) ->
  try
    X = orddict:fetch(Name, Dict),
    orddict:store(Name, [Value | X], Dict)
  catch _E ->
      orddict:store(Name, [Value], Dict)
  end;
set_multipart_value(Name, Value, Dict) ->
  orddict:store(Name, Value, Dict).

%%%-----------------------------------------------------------------------------
%% @private
acc_multipart(Req) ->
  acc_multipart(cowboy_req:multipart_data(Req), []).
acc_multipart({headers, Headers, Req}, Acc) ->
  acc_multipart(cowboy_req:multipart_data(Req), [{Headers, []}|Acc]);
acc_multipart({body, Data, Req}, [{Headers, BodyAcc}|Acc]) ->
  acc_multipart(cowboy_req:multipart_data(Req),
                [{Headers, [Data|BodyAcc]}|Acc]);
acc_multipart({end_of_part, Req}, [{Headers, BodyAcc}|Acc]) ->
  acc_multipart(cowboy_req:multipart_data(Req),
                [{Headers, list_to_binary(lists:reverse(BodyAcc))}|Acc]);
acc_multipart({eof, Req}, Acc) ->
  {lists:reverse(Acc), Req}.

%%%-----------------------------------------------------------------------------
%% @private
-spec get_user(Req :: cowboy_req:req(), State :: #mcb_html_state{}) ->
                  {cowboy_req:req(), #mcb_html_state{}}.
get_user(Req0, State0) ->
  {SesId, Req} = cowboy_req:cookie(ses_cookie_name(), Req0),
  User = macaba_web:get_session(SesId),
  State = state_set_var(user, macaba:record_to_proplist(User), State0),
  {Req, State#mcb_html_state{user=User}}.

%% @private
%% @doc Creates session process, sets response cookie, and sets user field
%% in state
create_session_for(U=#mcb_user{}, Req0, State0) ->
  {RemoteAddr, _} = cowboy_req:peer(Req0),
  Opts = [ {remote_addr, RemoteAddr}
         , {user, U}
         ],
  {SesId, _SesPid} = macaba_web:new_session(Opts),
  Req = cowboy_req:set_resp_cookie(ses_cookie_name(), SesId, [], Req0),
  State = State0#mcb_html_state{ user=U },
  {Req, State}.

%% @doc Gets ses cookie name from config
ses_cookie_name() ->
  {ok, CookieName} = macaba_conf:get([<<"board">>, <<"session_cookie_name">>]),
  CookieName.

%%% Local Variables:
%%% erlang-indent-level: 2
%%% End:
