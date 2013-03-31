%%%-----------------------------------------------------------------------------
%%% @doc REST resource for post and post-related REST tool calls
%%% Serves HTML templates, and provides basic HTTP access to the board.
%%% Created: 2013-02-16 Dmytro Lytovchenko <kvakvs@yandex.ru>
%%%-----------------------------------------------------------------------------
-module(mcweb_rest_post).

%% REST Callbacks
-export([ init/3
        , rest_init/2
        , allowed_methods/2
        , content_types_provided/2
        %% , content_types_accepted/2
        , resource_exists/2
        , post_is_create/2
        , create_path/2
        , process_post/2
        , provide_json/2
        ]).

-include_lib("macaba/include/macaba_types.hrl").
-include_lib("mcweb/include/mcweb.hrl").

init(_Transport, _Req, []) ->
  {upgrade, protocol, cowboy_rest}.

rest_init(Req, HandlerOpts) ->
  %% lager:debug("req ~p~n", [Req]),
  mcweb_rest:rest_init_helper(Req, HandlerOpts).

content_types_provided(Req, State) ->
  {[
    {{<<"application">>, <<"json">>, []}, provide_json}
   ], Req, State}.

%% content_types_accepted(Req, State) ->
%%   {[
%%     {{<<"application">>, <<"json">>, []}, accept_post_json}
%%    ], Req, State}.

allowed_methods(Req, State) ->
  {[<<"GET">>, <<"POST">>], Req, State}.

resource_exists(Req0, State0) ->
  {BoardId, Req1} = cowboy_req:binding(mcb_board, Req0),
  {PostId, Req2} = cowboy_req:binding(mcb_post, Req1),
  case {BoardId, PostId} of
    {undefined, undefined} -> {true, Req2, State0};
    {_, _} ->
      case macaba_post:get(BoardId, PostId) of
        {error, _} -> {false, Req2, State0};
        {ok, _TD} -> {true, Req2, State0}
      end
  end.

post_is_create(Req0, State0) ->
  {P, _} = cowboy_req:path(Req0),
  case P of
    <<"/rest/post/preview">> -> {false, Req0, State0};
    _ -> {true, Req0, State0}
  end.

create_path(Req, State) ->
  {<<"/">>, Req, State}.

%% @doc Return GET result as JSON
provide_json(Req0, State0) ->
  lager:debug("provide_json"),
  Reply = jsx:encode([{hello, "world"}]),
  {Reply, Req0, State0}.

%% @doc Return POST result as JSON
process_post(Req0, State0) ->
  {P, _} = cowboy_req:path(Req0),
  handle_POST_as_json(P, Req0, State0).

%% @private
%% @doc Return post markup preview
handle_POST_as_json(<<"/rest/post/preview">>, Req0, State0) ->
  %% lager:debug("handle_post_as_json ~p", [Path]),
  PD = State0#mcb_html_state.rest_body_json,
  Message = macaba:propget(<<"markup">>, PD, <<>>),
  MessageProcessed = macaba_plugins:call(markup, [Message]),
  ReplyJson = [{html, iolist_to_binary(MessageProcessed)}],
  Reply = jsx:encode(ReplyJson),
  Req = cowboy_req:set_resp_body(Reply, Req0),
  {true, Req, State0}.


%%%-----------------------------------------------------------------------------
%%% HELPER FUNCTIONS
%%%-----------------------------------------------------------------------------


%%% Local Variables:
%%% erlang-indent-level: 2
%%% End:
