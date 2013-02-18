%%%------------------------------------------------------------------------
%%% @doc This module has few predefined handlers (init, handle and terminate)
%%% which are called by cowboy on incoming HTTP request.
%%%
%%% Serves HTML templates, and provides basic HTTP access to the board.
%%%
%%% Created: 2013-02-16 Dmytro Lytovchenko <kvakvs@yandex.ru>
%%%------------------------------------------------------------------------
-module(macaba_html_handler).

-export([init/3]).
-export([handle/2]).
-export([terminate/3]).

%%-include_lib("macaba/include/macaba_types.hrl").
-type board_cmd_t() :: index | board | board_new | thread.
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
  {Method, Req1} = cowboy_req:method(Req0),
  {Req, State} = macaba_handle(Mode, Method, Req1, State0),
  {ok, Req, State}.

%%%-----------------------------------------------------------------------------
terminate(_Reason, _Req, _State) ->
  ok.

%%%-----------------------------------------------------------------------------
%% @private
-spec macaba_handle(Mode :: atom(), Method :: binary(), Req :: tuple(),
                    State :: #mcb_html_state{}) ->
                       {Req2 :: tuple(), State2 :: #mcb_html_state{}}.

macaba_handle(index, <<"GET">>, Req0, State0) ->
  State1 = state_set_var(boards, macaba_board:get_boards(), State0),
  {Req, State} = render_page("index", Req0, State1),
  {Req, State};

%%%---------------------------------------------------
%% @doc Do GET action on specified board
macaba_handle(board, <<"GET">>, Req0, State0) ->
  State1 = state_set_var(boards, macaba_board:get_boards(), State0),

  {BoardId, Req1} = cowboy_req:binding(mcb_board, Req0),
  BoardInfo = macaba_board:get_board(BoardId),
  State2 = state_set_var(board_info, BoardInfo, State1),

  State3 = state_set_var(threads, macaba_board:get_threads(BoardId), State2),

  {Req, State} = render_page("board", Req1, State3),
  {Req, State};

%%%---------------------------------------------------
%% @doc Do some POST action on specified board
macaba_handle(board_new, <<"POST">>, Req0, State0) ->
  {BoardId, Req1} = cowboy_req:binding(mcb_board, Req0),

  {ok, PostVals, Req2} = cowboy_req:body_qs(Req1),
  ThreadId = macaba:propget(<<"thread_id">>, PostVals, undefined),
  Name     = macaba:propget(<<"name">>,      PostVals, undefined),
  Subject  = macaba:propget(<<"subject">>,   PostVals, undefined),
  Message  = macaba:propget(<<"message">>,   PostVals, undefined),
  Attach   = macaba:propget(<<"attach">>,    PostVals, undefined),
  Captcha  = macaba:propget(<<"captcha">>,   PostVals, undefined),

  PostOpt = [ {thread_id, macaba:as_integer(ThreadId)}
            , {name, Name}
            , {subject, Subject}
            , {message, Message}
            , {attach, Attach}
            , {captcha, Captcha} % TODO: check captcha before creating
            ],
  ThreadOpt = [
              ],
  _Thread = macaba_board:new_thread(ThreadOpt, PostOpt),

  lager:info("detected POST ~p", [Req0]),
  {Req, State} = redirect("/board/" ++ macaba:as_string(BoardId),
                          Req2, State0),
  {Req, State};

%%%---------------------------------------------------
macaba_handle(_, _, Req, _State) ->
  cowboy_req:reply(404, Req). % Method not allowed.

%%%------------------------------------------------------------------------
%% @private
render_page(TemplateName, Req0, 
            State=#mcb_html_state{ page_vars = PageVars }) ->
  Body = macaba_web:render(TemplateName, PageVars),
  Headers = [{<<"Content-Type">>, <<"text/html">>}],
  {ok, Req} = cowboy_req:reply(200, Headers, Body, Req0),
  {Req, State}.

%%%------------------------------------------------------------------------
%% @private
redirect(URL, Req0, State) ->
  {ok, Req} = cowboy_req:reply(
                301, [{<<"Location">>, macaba:as_binary(URL)}],
                <<>>, Req0),
  {Req, State}.

%%%------------------------------------------------------------------------
%% @private
state_set_var(K, V, State = #mcb_html_state{ page_vars=P0 }) ->
  P = orddict:store(K, V, P0),
  State#mcb_html_state{ page_vars = P }.


%%% Local Variables:
%%% erlang-indent-level: 2
%%% End:
