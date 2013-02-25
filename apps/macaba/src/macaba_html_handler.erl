%%%------------------------------------------------------------------------
%%% @doc This module has few predefined handlers (init, handle and terminate)
%%% which are called by cowboy on incoming HTTP request.
%%% Serves HTML templates, and provides basic HTTP access to the board.
%%% Created: 2013-02-16 Dmytro Lytovchenko <kvakvs@yandex.ru>
%%%------------------------------------------------------------------------
-module(macaba_html_handler).

-export([ init/3
        , handle/2
        , terminate/3]).
-export([ macaba_handle_index/2
        , macaba_handle_board/2
        , macaba_handle_thread_new/2
        , macaba_handle_thread/2
        , macaba_handle_post_new/2
        , macaba_handle_attach/2
        ]).
-export([ chain_get_boards/1
        , chain_get_board_info/1
        , chain_get_threads/1
        , chain_get_thread_posts/1
        , chain_get_thread_info/1
        , chain_check_post_attach/1
        , chain_thread_new/1
        , chain_get_attach/1
        ]).

-include_lib("macaba/include/macaba_types.hrl").

-type board_cmd_t() :: index | board | thread | thread_new.
-record(mcb_html_state, {
          mode           :: board_cmd_t(),
          page_vars = [] :: orddict:orddict(),
          already_rendered = false :: boolean(),
          post_data = [] :: orddict:orddict()
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
    lager:debug("http ~p: ~p", [Method, Mode]),

    %% parse request body as multipart, this will not work for POST urlencoded
    {Req2, State1} = case Method of
                       <<"POST">> -> parse_multipart_form_data(Req1, State0);
                       _ -> {Req1, State0}
                     end,
    FnName = macaba:as_atom("macaba_handle_" ++ macaba:as_string(Mode)),
    {Req3, State2} = apply(?MODULE, FnName, [Method, {Req2, State1}]),
    {ok, Req3, State2}
  catch
    E -> T = lists:flatten(io_lib:format(
                             "~p ~p", [E, erlang:get_stacktrace()])),
         lager:error(E),
         {ReqE, StateE} = response_text(500, T, Req0, State0),
         {ok, ReqE, StateE}
  end.

%%%-----------------------------------------------------------------------------
terminate(_Reason, _Req, _State) ->
  ok.

%%%-----------------------------------------------------------------------------
%% @private
%% -spec macaba_handle_index(Method :: binary(), Req :: tuple(),
%%                           State :: #mcb_html_state{}) ->
%%                              {Req2 :: tuple(), State2 :: #mcb_html_state{}}.
%% @doc GET /
macaba_handle_index(<<"GET">>, {Req0, State0}) ->
  Boards = macaba_board_cli:get_boards(),
  State1 = state_set_var(boards, Boards, State0),
  render_page("index", Req0, State1).

%%%---------------------------------------------------
%% @doc Do GET board/id/
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
  {ok, BPageSize} = application:get_env(macaba, board_page_size),
  {ok, PreviewSize} = application:get_env(macaba, thread_preview_last_posts),
  Threads = macaba_board_cli:get_threads(BoardId, {Page, BPageSize},
                                         PreviewSize),
  State = state_set_var(threads, Threads, State0),
  {ok, {Req, State}}.

%%%-----------------------------------------------------------------------------
%% @doc Create thread POST on board/id/new
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
  case macaba_board:detect_content_type(Attach) of
    no_idea ->
      render_error(<<"bad_attach_format">>, Req0, State0);
    empty ->
      %% do nothing for empty attach
      State1 = state_set_var(attach_key, <<>>, State0),
      {ok, {Req0, State1}};
    _ ->
      AttachKey = crypto:sha(Attach),
      case macaba_board:attachment_exists(AttachKey) of
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
  PostOpt = get_post_create_options(Req, State0),
  ThreadOpt = orddict:from_list([]),
  macaba_board:new_thread(BoardId, ThreadOpt, PostOpt),
  {ok, {Req, State0}}.

%%%---------------------------------------------------
%% @doc Create post in thread on board/b_id/thread/t_id/post/new
macaba_handle_post_new(<<"POST">>, {Req0, State}) ->
  {BoardId, Req} = cowboy_req:binding(mcb_board, Req0),
  %% {ThreadId, Req2} = cowboy_req:binding(mcb_thread, Req1),
  PostOpt = get_post_create_options(Req, State),

  Post = macaba_board:new_post(BoardId, PostOpt),
  %%Post = #mcb_post{},

  ThreadId = Post#mcb_post.thread_id,
  PostId   = Post#mcb_post.post_id,
  redirect("/board/" ++ macaba:as_string(BoardId) ++ "/thread/"
           ++ macaba:as_string(ThreadId) ++ "#i"
           ++ macaba:as_string(PostId), Req, State).

%%%---------------------------------------------------
%% @private
get_post_create_options(_Req0, State=#mcb_html_state{post_data=PD}) ->
  %% {ok, PostVals, Req1} = cowboy_req:body_qs(Req0),
  ThreadId = macaba:propget(<<"thread_id">>, PD, ""),
  Author   = macaba:propget(<<"author">>,    PD, ""),
  Subject  = macaba:propget(<<"subject">>,   PD, ""),
  Message  = macaba:propget(<<"message">>,   PD, ""),
  Attach   = macaba:propget(<<"attach">>,    PD, ""),
  %%Captcha  = macaba:propget(<<"captcha">>,   PD, ""),

  orddict:from_list([ {thread_id, ThreadId}
                    , {author, Author}
                    , {subject, Subject}
                    , {message, Message}
                    , {attach, Attach}
                    , {attach_key, state_get_var(attach_key, State)}
                    ]).

%%%---------------------------------------------------
%% @doc Do GET board/b_id/thread/t_id
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
  case macaba_board_cli:get_thread(ThreadId) of
    {error, not_found} ->
      {Req1, State1} = render_page(404, "thread_404", Req, State0),
      {error, {Req1, State1}};
    ThreadInfo ->
      State = state_set_var(thread_info, ThreadInfo, State0),
      {ok, {Req, State}}
  end.

%% @private
%% @doc get all posts in thread
chain_get_thread_posts({Req0, State0}) ->
  {ThreadId, Req} = cowboy_req:binding(mcb_thread, Req0),
  Posts = macaba_board_cli:get_thread_preview(ThreadId, all),
  State1 = state_set_var(posts, Posts, State0),
  State = state_set_var(first_post, hd(Posts), State1),
  {ok, {Req, State}}.


%%%---------------------------------------------------
%% @doc Do GET attach/att_id
macaba_handle_attach(<<"GET">>, {Req0, State0}) ->
  {_, {Req1, State}} = macaba_web:chain_run(
                         [ fun chain_get_attach/1
                         ], {Req0, State0}),
  %% TODO: Etag/if modified since support
  case State#mcb_html_state.already_rendered of
    true ->
      {Req1, State};

    false ->
      Att       = state_get_var(attach, State),
      AttachId  = state_get_var(attach_id, State),
      Headers   = [ {<<"Content-Type">>, Att#mcb_attachment.content_type}
                  ],
      AttBody   = macaba_db_riak:read(mcb_attachment_body, AttachId),
      {ok, Req} = cowboy_req:reply(
                    200, Headers, AttBody#mcb_attachment_body.data, Req1),
      {Req, State}
  end.

%% @private
%% @doc get thread info if thread exists
chain_get_attach({Req0, State0}) ->
  {AttachId0, Req} = cowboy_req:binding(mcb_attach, Req0),
  AttachId = macaba:hexstr_to_bin(binary_to_list(AttachId0)),
  State1 = state_set_var(attach_id, AttachId, State0),

  case macaba_db_riak:read(mcb_attachment, AttachId) of
    {error, not_found} ->
      {Req1, State} = render_page(404, "attach_404", Req, State1),
      {error, {Req1, State}};
    Att ->
      State = state_set_var(attach, Att, State1),
      %% assume if header exists, then body exists too
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
parse_multipart_form_data(Req0, State0) ->
  {MPD, Req1} = acc_multipart(Req0),
  %% lager:debug("{{post}} multipart data ~p", [MPD]),
  {Req1, parse_multipart_form_data_1(MPD, State0)}.

parse_multipart_form_data_1([], State) -> State;
parse_multipart_form_data_1([{Headers, Value} | Rest],
                            State=#mcb_html_state{ post_data=PD0 }) ->
  FieldName = get_multipart_field_name(Headers),
  PD1 = orddict:store(FieldName, Value, PD0),

  ContentType = macaba:propget(<<"content-type">>, Headers, undefined),
  PD = orddict:store({content_type, FieldName}, ContentType, PD1),

  %% lager:debug("parse field ~p value ~p", [FieldName, Value]),
  State1 = State#mcb_html_state{ post_data=PD },
  parse_multipart_form_data_1(Rest, State1).

get_multipart_field_name([{<<"content-disposition">>, Bin} | _]) ->
  [<<"form-data">>|Parts] = binary:split(Bin, <<";">>, [global, trim]),
  [Ret] = [begin
             $" = binary:last(Name),
             binary:part(Name, 0, byte_size(Name) - 1)
           end || <<" name=\"", Name/binary>> <- Parts],
  Ret.

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

%%% Local Variables:
%%% erlang-indent-level: 2
%%% End:
