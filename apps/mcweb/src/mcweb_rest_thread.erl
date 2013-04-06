%%%-----------------------------------------------------------------------------
%%% @doc REST resource for thread
%%% @version 2013-02-16
%%% @author Dmytro Lytovchenko <kvakvs@yandex.ru>
%%%-----------------------------------------------------------------------------
-module(mcweb_rest_thread).

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
        , is_authorized/2
        , render_json_response/2
        ]).

-include_lib("mcb/include/macaba_types.hrl").
-include_lib("mcweb/include/mcweb.hrl").

init(_Transport, _Req, []) ->
  {upgrade, protocol, cowboy_rest}.

rest_init(Req, HandlerOpts) ->
  mcweb_rest:rest_init_helper(Req, HandlerOpts).

content_types_provided(Req, State) ->
  {[
    {{<<"application">>, <<"json">>, []}, render_json_response}
   ], Req, State}.

%% content_types_accepted(Req, State) ->
%%   {[
%%     {{<<"application">>, <<"json">>, []}, accept_POST_json}
%%    ], Req, State}.

allowed_methods(Req, State) ->
  {[<<"GET">>, <<"POST">>], Req, State}.

resource_exists(Req0, State0) ->
  {BoardId, Req1} = cowboy_req:binding(mcb_board, Req0),
  {ThreadId, Req2} = cowboy_req:binding(mcb_thread, Req1),
  case mcb_thread:get_dynamic(BoardId, ThreadId) of
    {error, _} -> {false, Req2, State0};
    {ok, TD} ->
      case mcb_thread:get(BoardId, ThreadId) of
        {ok, T} ->
          State1 = mcweb:state_set_var(thread_dynamic, TD, State0),
          State2 = mcweb:state_set_var(thread, T, State1),
          {true, Req2, State2};
        {error, _} ->
          {false, Req2, State0}
      end
  end.

%%%-----------------------------------------------------------------------------
is_authorized(Req0, State0) ->
  {Do, Req} = cowboy_req:qs_val(<<"do">>, Req0),
  case Do of
    <<"manage">> -> {is_authorized_mod(State0), Req, State0};
    <<"delete">> -> {is_authorized_mod(State0), Req, State0};
               _ -> {true, Req, State0}
  end.

%%%---------------------------------------------------------
is_authorized_mod(State) ->
  U = State#mcb_html_state.user,
  case U#mcb_user.level >= ?USERLEVEL_MOD of
    true  -> true;
    false -> {false, <<"Basic Realm=\"", ?MCB_BASICAUTHREALM, "\"">>}
  end.

%%%-----------------------------------------------------------------------------
post_is_create(Req0, State0) ->
  {Do, Req} = cowboy_req:qs_val(<<"do">>, Req0),
  case Do of
    <<"manage">> -> {false, Req, State0};
    <<"delete">> -> {false, Req, State0};
               _ -> {true, Req, State0}
  end.

%%%-----------------------------------------------------------------------------
create_path(Req0, State0) ->
  {BoardId, Req1} = cowboy_req:binding(mcb_board, Req0),
  {ThreadId, Req2} = cowboy_req:binding(mcb_thread, Req1),
  {<<"/board/", BoardId/binary, "/thread/", ThreadId/binary>>
     , Req2, State0}.

%%%-----------------------------------------------------------------------------
%% @doc Return POST result as JSON
process_post(Req0, State0) ->
  {Do, Req} = cowboy_req:qs_val(<<"do">>, Req0),
  handle_POST_as_json(Do, Req, State0).

%%%-----------------------------------------------------------------------------
%% @private
handle_POST_as_json(Do, Req0, State0)
  when Do =:= <<"manage">> orelse Do =:= <<"delete">> ->
  {_BoardId, Req1} = cowboy_req:binding(mcb_board, Req0),
  {_ThreadId, Req2} = cowboy_req:binding(mcb_thread, Req1),
  PD = State0#mcb_html_state.rest_body_json,
  Update = mcb:propget(<<"update">>, PD, []),

  T = mcweb:state_get_var(thread, State0),
  Locked = mcb:propget(<<"locked">>, Update, T#mcb_thread.read_only),
  Pinned = mcb:propget(<<"pinned">>, Update, T#mcb_thread.pinned),
  Hidden = mcb:propget(<<"hidden">>, Update, T#mcb_thread.hidden),
  T2 = T#mcb_thread{
         read_only = Locked,
         pinned = Pinned,
         hidden = Hidden
        },
  mcb_thread:update(T2),

  ReplyJson = [{result, <<"ok">>}],
  Reply = jsx:encode(ReplyJson),
  Req = cowboy_req:set_resp_body(Reply, Req2),

  {true, Req, State0};

%%%---------------------------------------------------------
%% @doc Error if do =/= manage or delete
handle_POST_as_json(_Do, Req0, State0) ->
  {false, Req0, State0}.

%% @doc Does this ever get called?
render_json_response(Req0, State0) ->
  ReplyJ = mcweb:state_get_var(rest_result, State0),
  {jsx:encode(ReplyJ), Req0, State0}.


%%%-----------------------------------------------------------------------------
%%% HELPER FUNCTIONS
%%%-----------------------------------------------------------------------------


%%% Local Variables:
%%% erlang-indent-level: 2
%%% End:
