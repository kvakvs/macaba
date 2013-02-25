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
        ]).
-export([ chain_get_boards/1
        , chain_get_board_info/1
        , chain_get_threads/1
        , chain_get_thread_posts/1
        , chain_get_thread_info/1
        ]).

-include_lib("macaba/include/macaba_types.hrl").

-type board_cmd_t() :: index | board | thread | thread_new.
-record(mcb_html_state, {
          mode           :: board_cmd_t(),
          page_vars = [] :: orddict:orddict()
         }).

%%%-----------------------------------------------------------------------------
init(_Transport, Req, [Mode]) ->
  {ok, Req, #mcb_html_state{
         mode = Mode
        }}.

%%%-----------------------------------------------------------------------------
handle(Req0, State0 = #mcb_html_state{ mode=Mode }) ->
  try
    {Method, Req1} = cowboy_req:method(Req0),
    lager:debug("http ~p: ~p", [Method, Mode]),
    FnName = macaba:as_atom("macaba_handle_" ++ macaba:as_string(Mode)),
    {Req2, State2} = apply(?MODULE, FnName, [Method, {Req1, State0}]),
    {ok, Req2, State2}
  catch
    E -> T = lists:flatten(io_lib:format(
                             "~p ~p", [E, erlang:get_stacktrace()])),
         lager:error(E),
         {Req3, State3} = response_text(500, T, Req0, State0),
         {ok, Req3, State3}
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
      {Req1, State1} = render_page(404, "error_404", Req, State0),
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
  {BoardId, Req} = cowboy_req:binding(mcb_board, Req0),

  PostOpt = get_post_create_options(Req),
  ThreadOpt = orddict:from_list([]),
  _Thread = macaba_board:new_thread(BoardId, ThreadOpt, PostOpt),
  redirect("/board/" ++ macaba:as_string(BoardId), Req, State0).

%%%---------------------------------------------------
%% @doc Create post in thread on board/b_id/thread/t_id/post/new
macaba_handle_post_new(<<"POST">>, {Req0, State}) ->
  {BoardId, Req} = cowboy_req:binding(mcb_board, Req0),
  %% {ThreadId, Req2} = cowboy_req:binding(mcb_thread, Req1),
  PostOpt = get_post_create_options(Req),
  Post = macaba_board:new_post(BoardId, PostOpt),

  ThreadId = Post#mcb_post.thread_id,
  PostId   = Post#mcb_post.post_id,
  redirect("/board/" ++ macaba:as_string(BoardId) ++ "/thread/"
           ++ macaba:as_string(ThreadId) ++ "#i"
           ++ macaba:as_string(PostId), Req, State).

%%%---------------------------------------------------
%% @private
get_post_create_options(Req0) ->
  {ok, PostVals, Req1} = cowboy_req:body_qs(Req0),
  ThreadId = macaba:propget(<<"thread_id">>, PostVals, undefined),
  Author   = macaba:propget(<<"author">>,    PostVals, undefined),
  Subject  = macaba:propget(<<"subject">>,   PostVals, undefined),
  Message  = macaba:propget(<<"message">>,   PostVals, undefined),
  Attach   = macaba:propget(<<"attach">>,    PostVals, undefined),
  Captcha  = macaba:propget(<<"captcha">>,   PostVals, undefined),

  %% {ok, RBody, Req2} = cowboy_req:body(Req1),
  %% lager:debug("body ~p", [RBody]),
  %% {MP, _Req2} = handle_multipart(cowboy_req:multipart_data(Req1), []),
  %% lager:debug("multipart ~p", [MP]),
  {File, _Req2} = acc_multipart(Req1),
  lager:debug("multipart data ~p", [File]),

  orddict:from_list(
    [ {thread_id, ThreadId}
    , {author, Author}
    , {subject, Subject}
    , {message, Message}
    , {attach, Attach}
    , {captcha, Captcha} % TODO: captcha support
    ]).

acc_multipart(Req) ->
  acc_multipart(cowboy_req:multipart_data(Req), []).
acc_multipart({headers, Headers, Req}, Acc) ->
  acc_multipart(cowboy_req:multipart_data(Req), []);
acc_multipart({body, Data, Req}, _) ->
  acc_multipart(cowboy_req:multipart_data(Req), Data);
acc_multipart({end_of_part, Req}, Data) ->
  acc_multipart(cowboy_req:multipart_data(Req), Data);
acc_multipart({eof, Req}, Data) ->
  {Data, Req}.

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
      {Req1, State1} = render_page(404, "error_404", Req, State0),
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

%%%-----------------------------------------------------------------------------
%%% HELPER FUNCTIONS
%%%-----------------------------------------------------------------------------

%%%-----------------------------------------------------------------------------
%% @private
render_page(TemplateName, Req0, State0) ->
  render_page(200, TemplateName, Req0, State0).

%% @private
render_page(HttpStatus, TemplateName, Req0,
            State=#mcb_html_state{ page_vars = PageVars }) ->
  %% lager:debug("Before render: vars=~p", [PageVars]),
  Body = macaba_web:render(TemplateName, PageVars),
  Headers = [{<<"Content-Type">>, <<"text/html">>}],
  {ok, Req} = cowboy_req:reply(HttpStatus, Headers, Body, Req0),
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
state_set_var(K, V, State = #mcb_html_state{ page_vars=P0 }) ->
  P = orddict:store(K, V, P0),
  State#mcb_html_state{ page_vars = P }.


%%% Local Variables:
%%% erlang-indent-level: 2
%%% End:
