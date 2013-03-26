%%%-----------------------------------------------------------------------------
%%% @doc This module has few predefined handlers (init, handle and terminate)
%%% which are called by cowboy on incoming HTTP request.
%%% Serves HTML templates, and provides basic HTTP access to the board.
%%% Created: 2013-02-16 Dmytro Lytovchenko <kvakvs@yandex.ru>
%%%-----------------------------------------------------------------------------
-module(mcweb_rest_thread).

%% REST Callbacks
-export([ init/3
        , rest_init/2
        , allowed_methods/2
        , content_types_provided/2
        , content_types_accepted/2
        , resource_exists/2
        , post_is_create/2
        , create_path/2
        ]).
-include_lib("macaba/include/macaba_types.hrl").
-include_lib("mcweb/include/mcweb.hrl").

init(_Transport, _Req, []) ->
  {upgrade, protocol, cowboy_rest}.

rest_init(Req, HandlerOpts) ->
  mcweb_rest:rest_init_helper(Req, HandlerOpts).

content_types_provided(Req, State) ->
  {[
    {{<<"application">>, <<"json">>, []}, thread_get}
   ], Req, State}.

content_types_accepted(Req, State) ->
  {[
    {{<<"application">>, <<"x-www-form-urlencoded">>, []}, thread_post}
   ], Req, State}.

allowed_methods(Req, State) ->
  {[<<"GET">>, <<"POST">>], Req, State}.

resource_exists(Req0, State0) ->
  {BoardId, Req1} = cowboy_req:binding(mcb_board, Req0),
  {ThreadId, Req2} = cowboy_req:binding(mcb_thread, Req1),
  case macaba_thread:get_dynamic(BoardId, ThreadId) of
    {error, _} -> {false, Req2, State0};
    {ok, _TD} ->
      {ok, _T} = macaba_thread:get(BoardId, ThreadId),
      {false, Req2, State0}
  end.

post_is_create(Req, State) ->
  {true, Req, State}.

create_path(_Req, _State) ->
  error.
%%  {<<$/, (new_paste_id())/binary>>, Req, State}.


%%%-----------------------------------------------------------------------------
%% init({_Transport, http}, Req, [Mode]) ->
%%   {ok, Req, #mcb_html_state{
%%          mode = Mode
%%         }}.

%% -spec handle(cowboy_req:req(), mcweb:html_state()) ->
%%                 {ok, cowboy_req:req(), mcweb:html_state()}.

%% handle(Req0, State0) ->
%%   mcweb:handle_helper(?MODULE, Req0, State0).

%% terminate(_Reason, _Req, _State) ->
%%   ok.

%%%-----------------------------------------------------------------------------
%% @doc /rest - entry point for REST calls
%%%-----------------------------------------------------------------------------
%% -spec macaba_handle_rest(Method :: binary(),
%%                          Req :: cowboy_req:req(),
%%                          State :: mcweb:html_state()) ->
%%                             mcweb:handler_return().

%% macaba_handle_rest(_Method, Req0, State0) ->
%%   mcweb:response_json(200, "{\"result\":\"ok\"}", Req0, State0).

%%%-----------------------------------------------------------------------------
%%% Utility: Preview markup
%%%-----------------------------------------------------------------------------
%% -spec macaba_handle_util_preview(Method :: binary(),
%%                                  Req :: cowboy_req:req(),
%%                                  State :: mcweb:html_state()) ->
%%                                     mcweb:handler_return().

%% macaba_handle_util_preview(<<"POST">>, Req0, State0) ->
%%   lager:debug("http POST util/preview"),
%%   PD = State0#mcb_html_state.post_data,
%%   Message = macaba:propget(<<"markup">>, PD, <<>>),
%%   MessageProcessed = macaba_plugins:call(markup, [Message]),
%%   ReplyJson = [{html, iolist_to_binary(MessageProcessed)}],
%%   mcweb:response_json(200, ReplyJson, Req0, State0).

%%%-----------------------------------------------------------------------------
%%% Thread manage
%%%-----------------------------------------------------------------------------
%% -spec macaba_handle_thread_manage(Method :: binary(),
%%                                   Req :: cowboy_req:req(),
%%                                   State :: mcweb:html_state()) ->
%%                                      mcweb:handler_return().

%% macaba_handle_thread_manage(<<"POST">>, Req0, State0) ->
%%   lager:debug("http POST /rest/thread/manage"),
%%   {_, Req, State} = mcweb:chain_run(
%%                         [ fun mcweb:chain_fail_if_below_admin/2
%%                         , fun chain_thread_manage_do/2
%%                         ], Req0, State0),
%%   {Req, State}.

%% %%%------------------------------------------------------------------%
%% chain_thread_manage_do(Req0, State0) ->
%%   {_BoardId, Req1} = cowboy_req:binding(mcb_board, Req0),
%%   {_ThreadId, Req2} = cowboy_req:binding(mcb_thread, Req1),
%%   %Ban = macaba:propget(<<"ban">>, 

%%   ReplyJson = [{result, "ok"}],
%%   mcweb:response_json(200, ReplyJson, Req2, State0).

%%%-----------------------------------------------------------------------------
%%% HELPER FUNCTIONS
%%%-----------------------------------------------------------------------------


%%% Local Variables:
%%% erlang-indent-level: 2
%%% End:
