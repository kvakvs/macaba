%%%-----------------------------------------------------------------------------
%%% @doc This module has few predefined handlers (init, handle and terminate)
%%% which are called by cowboy on incoming HTTP request.
%%% Serves HTML templates, and provides basic HTTP access to the board.
%%% Created: 2013-02-16 Dmytro Lytovchenko <kvakvs@yandex.ru>
%%%-----------------------------------------------------------------------------
-module(macaba_html_admin).

-export([ init/3
        , handle/2
        , terminate/3]).
-export([ macaba_handle_admin/2
        , macaba_handle_admin_site/2
        %% , macaba_handle_admin_site_boards/2
        , macaba_handle_admin_site_offline/2
        , macaba_handle_admin_login/2
        , macaba_handle_admin_logout/2
        ]).
-export([ chain_check_admin_login/1
        , chain_check_mod_login/1
        ]).

-include_lib("macaba/include/macaba_types.hrl").

%%%-----------------------------------------------------------------------------
init({_Transport, http}, Req, [Mode]) ->
  {ok, Req, #mcb_html_state{
         mode = Mode
        }}.

handle(Req0, State0) ->
  macaba_web:handle_helper(?MODULE, Req0, State0).

terminate(_Reason, _Req, _State) ->
  ok.

%%%-----------------------------------------------------------------------------
%% @doc GET: /admin/login - login form
%%%-----------------------------------------------------------------------------
macaba_handle_admin_login(<<"GET">>, {Req0, State0}) ->
  lager:debug("http GET admin/login"),
  {_, {Req, State}} = macaba_web:chain_run(
                        [ fun macaba_html_handler:chain_get_boards/1
                        ], {Req0, State0}),
  macaba_web:render_page("admin_login", Req, State);

%%%-----------------------------------------------------------------------------
%% POST: /admin - login
macaba_handle_admin_login(<<"POST">>, {Req0, State0}) ->
  lager:debug("http POST admin/login"),
  {_, {Req, State}} = macaba_web:chain_run(
                        [ fun chain_check_admin_login/1
                        , fun chain_check_mod_login/1
                        ], {Req0, State0}),
  macaba_web:redirect("/", Req, State).


%%%-----------------------------------------------------------------------------
%% @doc GET: /admin - landing page
%%%-----------------------------------------------------------------------------
macaba_handle_admin(<<"GET">>, {Req0, State0}) ->
  lager:debug("http GET admin"),
  {_, {Req, State}} = macaba_web:chain_run(
                        [ fun macaba_html_handler:chain_get_boards/1
                        , fun(X) -> chain_fail_if_user(X, anon) end
                        ], {Req0, State0}),
  macaba_web:render_page("admin", Req, State).

%% @private
%% @doc Checks admin login and password from macaba.config
chain_check_admin_login({Req0, State0=#mcb_html_state{ post_data=PD }}) ->
  {ok, ALogin} = macaba_conf:get([<<"board">>, <<"admin_login">>]),
  {ok, APassword} = macaba_conf:get([<<"board">>, <<"admin_password">>]),
  Login = macaba:propget(<<"login">>, PD),
  Password = macaba:propget(<<"password">>, PD),
  %% lager:debug("L=~s:P=~s AL=~s:AP=~s", [Login, Password, ALogin, APassword]),
  case {ALogin =:= Login, APassword =:= Password} of
    {true, true} ->
      {Req, State} = macaba_web:create_session_for(#mcb_user{type=admin},
                                                   Req0, State0),
      %% stop checking passwords right here
      {error, {Req, State}};
    _ ->
      {ok, {Req0, State0}}
  end.

%% @private
%% @doc Checks mod login and password from database
chain_check_mod_login({Req0, State0}) ->
  {ok, {Req0, State0}}.

%% @private
%% @doc Gets user from ses cookie, fails if user type is Role
chain_fail_if_user({Req0, State0}, FailIfRole) ->
  User = State0#mcb_html_state.user,
  #mcb_user{type=Type} = User,
  case Type of
    FailIfRole ->
      {error, macaba_web:render_error(<<"Not authenticated">>, Req0, State0)};
    _ ->
      {ok, {Req0, State0}}
  end.

%% @private
%% @doc Gets user from ses cookie, fails if user type is NOT Role
chain_fail_if_user_not({Req0, State0}, FailIfNotRole) ->
  User = State0#mcb_html_state.user,
  #mcb_user{type=Type} = User,
  case Type of
    X when X =/= FailIfNotRole ->
      NotRole = atom_to_binary(FailIfNotRole, latin1),
      {error, macaba_web:render_error(<<"User role is not ", NotRole/binary>>,
                                      Req0, State0)};
    _ -> {ok, {Req0, State0}}
  end.

%%%-----------------------------------------------------------------------------
%% @doc GET: /admin/logout - delete admin cookie
%%%-----------------------------------------------------------------------------
macaba_handle_admin_logout(Method, {Req0, State0}) ->
  lager:debug("http ~s admin/logout", Method),
  %% {_, {Req, State}} = macaba_web:chain_run(
  %%                       [], {Req0, State0}),
  Coo = macaba_web:ses_cookie_name(),
  Req = cowboy_req:set_resp_cookie(Coo, <<>>, [{path, <<"/">>}], Req0),
  macaba_web:redirect("/", Req, State0).

%%%-----------------------------------------------------------------------------
%% @doc GET: /admin/site - site config page
%%%-----------------------------------------------------------------------------
macaba_handle_admin_site(<<"GET">>, {Req0, State0}) ->
  lager:debug("http GET admin/site"),
  {_, {Req, State1}} = macaba_web:chain_run(
                        [ fun macaba_html_handler:chain_get_boards/1
                        , fun(X) -> chain_fail_if_user(X, anon) end
                        ], {Req0, State0}),
  #mcb_site_config{
      boards = Boards0,
      offline = Offline,
      offline_message = OfflineMsg
    } = macaba_board:get_site_config(),
  Boards1 = lists:map(fun macaba:record_to_proplist/1, Boards0),
  Boards = jsx:encode(Boards1, [{indent, 2}]),
  State2 = macaba_web:state_set_var(site_boards, Boards, State1),
  State3 = macaba_web:state_set_var(site_offline, Offline, State2),
  State  = macaba_web:state_set_var(site_offline_message, OfflineMsg, State3),
  macaba_web:render_page("admin_site", Req, State).

%%%-----------------------------------------------------------------------------
%% @doc GET: /admin/logout - delete admin cookie
%%%-----------------------------------------------------------------------------
macaba_handle_admin_site_offline(<<"POST">>, {Req0, State0}) ->
  lager:debug("http POST admin/site/offline"),
  {_, {Req, State}} = macaba_web:chain_run(
                        [ fun(X) -> chain_fail_if_user_not(X, admin) end
                        , fun chain_edit_site_offline/1
                        ], {Req0, State0}),
  macaba_web:redirect("/", Req, State).

chain_edit_site_offline({Req0, State0=#mcb_html_state{post_data=PD}}) ->
  Site0 = macaba_board:get_site_config(),
  Offline = macaba:propget(<<"offline">>, PD, false),
  OfflineMsg = macaba:propget(<<"offline_message">>, PD,
                              Site0#mcb_site_config.offline_message),
  Site = Site0#mcb_site_config{
           offline = macaba:as_bool(Offline),
           offline_message = macaba:as_string(OfflineMsg)
          },
  macaba_board:set_site_config(Site),
  {ok, {Req0, State0}}.

%%%-----------------------------------------------------------------------------
%%% HELPER FUNCTIONS
%%%-----------------------------------------------------------------------------


%%% Local Variables:
%%% erlang-indent-level: 2
%%% End:
