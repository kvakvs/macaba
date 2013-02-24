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
        , chain_get_boards/1
        , chain_get_board_info/1
        , chain_get_threads/1
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
  {ok, {Req, State}} = macaba_web:chain_run(
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
  BoardInfo = macaba_board_cli:get_board(BoardId),
  State = state_set_var(board_info, BoardInfo, State0),
  {ok, {Req, State}}.

%% @private
%% @doc get visible threads
chain_get_threads({Req0, State0}) ->
  {BoardId, Req1} = cowboy_req:binding(mcb_board, Req0),
  {Page, Req} = cowboy_req:binding(mcb_page, Req1),
  {ok, BPageSize} = application:get_env(macaba, board_page_size),
  Threads = macaba_board_cli:get_threads(BoardId, {Page, BPageSize}),
  State1 = state_set_var(threads, Threads, State0),

  {ok, LastCount} = application:get_env(macaba, thread_preview_last_posts),
  ThreadIdList = [T || #mcb_thread{thread_id=T} <- Threads],
  Previews = macaba_board_cli:get_thread_previews(ThreadIdList, LastCount),
  State = state_set_var(previews, Previews, State1),
  {ok, {Req, State}}.

%%%-----------------------------------------------------------------------------
%% @doc Create thread POST on board/id/new
macaba_handle_thread_new(<<"POST">>, {Req0, State0}) ->
  {BoardId, Req1} = cowboy_req:binding(mcb_board, Req0),

  {ok, PostVals, Req2} = cowboy_req:body_qs(Req1),
  ThreadId = macaba:propget(<<"thread_id">>, PostVals, undefined),
  Author   = macaba:propget(<<"author">>,    PostVals, undefined),
  Subject  = macaba:propget(<<"subject">>,   PostVals, undefined),
  Message  = macaba:propget(<<"message">>,   PostVals, undefined),
  Attach   = macaba:propget(<<"attach">>,    PostVals, undefined),
  Captcha  = macaba:propget(<<"captcha">>,   PostVals, undefined),

  PostOpt = orddict:from_list(
              [ {thread_id, macaba:as_integer(ThreadId)}
              , {author, Author}
              , {subject, Subject}
              , {message, Message}
              , {attach, Attach}
              , {captcha, Captcha} % TODO: captcha support
              ]),
  ThreadOpt = orddict:from_list(
                [
                ]),
  _Thread = macaba_board:new_thread(BoardId, ThreadOpt, PostOpt),
  redirect("/board/" ++ macaba:as_string(BoardId), Req2, State0).

%%%-----------------------------------------------------------------------------
%%% HELPER FUNCTIONS
%%%-----------------------------------------------------------------------------

%%%-----------------------------------------------------------------------------
%% @private
render_page(TemplateName, Req0,
            State=#mcb_html_state{ page_vars = PageVars }) ->
  Body = macaba_web:render(TemplateName, PageVars),
  Headers = [{<<"Content-Type">>, <<"text/html">>}],
  {ok, Req} = cowboy_req:reply(200, Headers, Body, Req0),
  {Req, State}.

%%%-----------------------------------------------------------------------------
%% @private
response_text(Status, Body, Req0, State=#mcb_html_state{}) ->
  Headers = [{<<"Content-Type">>, <<"text/plain">>}],
  {ok, Req} = cowboy_req:reply(Status, Headers, Body, Req0),
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
