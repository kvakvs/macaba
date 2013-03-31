%%%-----------------------------------------------------------------------------
%%% @doc Library for REST resources
%%% Created: 2013-03-24 Dmytro Lytovchenko <kvakvs@yandex.ru>
%%%-----------------------------------------------------------------------------
-module(mcweb_rest).

-export([ rest_init_helper/2
        ]).

-include_lib("macaba/include/macaba_types.hrl").
-include_lib("mcweb/include/mcweb.hrl").

-spec rest_init_helper(Req :: cowboy_req:req(),
                       HandlerOpts :: any()) ->
                              {ok, cowboy_req:req(), #mcb_html_state{}}.

rest_init_helper(Req0, _HandlerOpts) ->
  {ok, Body, Req1} = cowboy_req:body(Req0),

  State0 = #mcb_html_state{
    rest_body = Body,
    rest_body_json = case Body of
                       <<>> -> undefined;
                       _ ->
                         %% lager:debug("init body=~p", [Body]),
                         jsx:decode(Body)
                     end
    },
  %% try if there is authorization header
  {WWWAuth, Req2} = cowboy_req:header(<<"authorization">>, Req1),
  {Req, State} = handle_basic_auth(WWWAuth, Req2, State0),

  %% {User, Req} = mcweb:get_user(Req1),
  %% State = State1#mcb_html_state{
  %%           user = User
  %%          },
  {ok, Req, State}.

%%%-----------------------------------------------------------------------------
%% @private
%% @doc Receives www-authenticate cookie, extracts realm, login and pass, and
%% checks authentication. Sets ses cookie and user field in state on success.
handle_basic_auth(<<"Basic ", B64/binary>>, Req0, State0) ->
  LoginPass = base64:decode(B64),
  [Login, Pass] = binary:split(LoginPass, [<<":">>]),
  lager:debug("handle basic auth L=~s P=~s", [Login, Pass]),
  case mcweb:check_admin_login_password(Login, Pass) of
    true ->
      Admin = #mcb_user{level=?USERLEVEL_ADMIN},
      %% mcweb:create_session_for(Admin, Req0, State0);
      {Req0, State0#mcb_html_state{ user=Admin }};
    false ->
      {Req0, State0}
  end;
handle_basic_auth(undefined, Req, State) -> {Req, State};
handle_basic_auth(_Whatever, Req, State) -> {Req, State}.

%%% Local Variables:
%%% erlang-indent-level: 2
%%% End:
