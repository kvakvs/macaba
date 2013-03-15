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
-export([ macaba_handle_offline/3
        , macaba_handle_admin/3
        , macaba_handle_admin_site/3
        , macaba_handle_admin_site_boards/3
        , macaba_handle_admin_site_offline/3
        , macaba_handle_admin_login/3
        , macaba_handle_admin_logout/3
        ]).
-export([ chain_check_admin_login/2
        , chain_check_mod_login/2
        ]).

-include_lib("macaba/include/macaba_types.hrl").

%%%-----------------------------------------------------------------------------
init({_Transport, http}, Req, [Mode]) ->
  {ok, Req, #mcb_html_state{
         mode = Mode
        }}.

-spec handle(cowboy_req:req(), macaba_web:html_state()) ->
                {ok, cowboy_req:req(), macaba_web:html_state()}.

handle(Req0, State0) ->
  macaba_web:handle_helper(?MODULE, Req0, State0).

terminate(_Reason, _Req, _State) ->
  ok.

%%%-----------------------------------------------------------------------------
%% @doc GET: /offline - board shut down by admin
%%%-----------------------------------------------------------------------------
-spec macaba_handle_offline(Method :: binary(),
                            Req :: cowboy_req:req(),
                            State :: macaba_web:html_state()) ->
                               macaba_web:handler_return().

macaba_handle_offline(_, Req0, State0) ->
  Site = macaba_board:get_site_config(),
  M = Site#mcb_site_config.offline_message,
  State = macaba_web:state_set_var(offline_message, M, State0),
  macaba_web:render_page("offline", Req0, State).

%%%-----------------------------------------------------------------------------
%% @doc GET: /admin/login - login form
%%%-----------------------------------------------------------------------------
-spec macaba_handle_admin_login(Method :: binary(),
                                Req :: cowboy_req:req(),
                                State :: macaba_web:html_state()) ->
                                   macaba_web:handler_return().

macaba_handle_admin_login(<<"GET">>, Req0, State0) ->
  lager:debug("http GET admin/login"),
  {_, Req, State} = macaba_web:chain_run(
                        [ fun macaba_html_handler:chain_get_boards/2
                        ], Req0, State0),
  macaba_web:render_page("admin_login", Req, State);

%% POST: /admin/login - login
macaba_handle_admin_login(<<"POST">>, Req0, State0) ->
  lager:debug("http POST admin/login"),
  {_, Req, State} = macaba_web:chain_run(
                        [ fun chain_check_admin_login/2
                        , fun chain_check_mod_login/2
                        ], Req0, State0),
  macaba_web:redirect("/", Req, State).


%%%-----------------------------------------------------------------------------
%% @doc GET: /admin - landing page
%%%-----------------------------------------------------------------------------
-spec macaba_handle_admin(Method :: binary(),
                          Req :: cowboy_req:req(),
                          State :: macaba_web:html_state()) ->
                             macaba_web:handler_return().

macaba_handle_admin(<<"GET">>, Req0, State0) ->
  lager:debug("http GET admin"),
  {_, Req, State} = macaba_web:chain_run(
                      [ fun macaba_html_handler:chain_get_boards/2
                      , fun(R,S) -> chain_fail_if_user(R, S, anon) end
                      ], Req0, State0),
  macaba_web:render_page("admin", Req, State).

%% @private
%% @doc Checks admin login and password from macaba.config
chain_check_admin_login(Req0, State0=#mcb_html_state{ post_data=PD }) ->
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
      macaba_web:chain_fail(Req, State);
    _ ->
      macaba_web:chain_success(Req0, State0)
  end.

%% @private
%% @doc Checks mod login and password from database
chain_check_mod_login(Req0, State0) ->
  {ok, Req0, State0}.

%% @private
%% @doc Gets user from ses cookie, fails if user type is Role
chain_fail_if_user(Req0, State0, FailIfRole) ->
  User = State0#mcb_html_state.user,
  #mcb_user{type=Type} = User,
  case Type of
    FailIfRole ->
      macaba_web:chain_fail(
        macaba_web:render_error(<<"Not authenticated">>, Req0, State0));
    _ ->
      macaba_web:chain_success(Req0, State0)
  end.

%% @private
%% @doc Gets user from ses cookie, fails if user type is NOT Role
chain_fail_if_user_not(Req0, State0, FailIfNotRole) ->
  User = State0#mcb_html_state.user,
  #mcb_user{type=Type} = User,
  case Type of
    X when X =/= FailIfNotRole ->
      NotRole = atom_to_binary(FailIfNotRole, latin1),
      macaba_web:chain_fail(
        macaba_web:render_error(<<"User role is not ", NotRole/binary>>,
                                Req0, State0));
    _ ->
      macaba_web:chain_success(Req0, State0)
  end.

%%%-----------------------------------------------------------------------------
%% @doc GET: /admin/logout - delete admin cookie
%%%-----------------------------------------------------------------------------
-spec macaba_handle_admin_logout(Method :: binary(),
                                 Req :: cowboy_req:req(),
                                 State :: macaba_web:html_state()) ->
                                    macaba_web:handler_return().

macaba_handle_admin_logout(Method, Req0, State0) ->
  lager:debug("http ~s admin/logout", [Method]),
  Req = macaba_web:clear_session_cookie(Req0),
  macaba_web:redirect("/", Req, State0).

%%%-----------------------------------------------------------------------------
%% @doc GET: /admin/site - site config page
%%%-----------------------------------------------------------------------------
-spec macaba_handle_admin_site(Method :: binary(),
                               Req :: cowboy_req:req(),
                               State :: macaba_web:html_state()) ->
                                  macaba_web:handler_return().

macaba_handle_admin_site(<<"GET">>, Req0, State0) ->
  lager:debug("http GET admin/site"),
  {_, Req, State} = macaba_web:chain_run(
                        [ fun macaba_html_handler:chain_get_boards/2
                        , fun(R, S) -> chain_fail_if_user(R, S, anon) end
                        , fun chain_show_admin_site/2
                        ], Req0, State0),
  {Req, State}.

%% @private
chain_show_admin_site(Req0, State0) ->
  #mcb_site_config{
      boards = Boards0,
      offline = Offline,
      offline_message = OfflineMsg
    } = macaba_board:get_site_config(),
  Boards1 = lists:map(fun macaba:record_to_proplist/1, Boards0),
  Boards = jsx:encode(Boards1, [{indent, 2}]),
  State1 = macaba_web:state_set_var(site_boards, Boards, State0),
  State2 = macaba_web:state_set_var(site_offline, Offline, State1),
  State3  = macaba_web:state_set_var(site_offline_message, OfflineMsg, State2),
  macaba_web:chain_success(macaba_web:render_page("admin_site", Req0, State3)).

%%%-----------------------------------------------------------------------------
%% @doc GET: /admin/site/offline - edit board offline settings
%%%-----------------------------------------------------------------------------
-spec macaba_handle_admin_site_offline(Method :: binary(),
                                       Req :: cowboy_req:req(),
                                       State :: macaba_web:html_state()) ->
                                          macaba_web:handler_return().

macaba_handle_admin_site_offline(<<"POST">>, Req0, State0) ->
  lager:debug("http POST admin/site/offline"),
  {_, Req, State} = macaba_web:chain_run(
                      [ fun(R, S) -> chain_fail_if_user_not(R, S, admin) end
                      , fun chain_edit_site_offline/2
                      ], Req0, State0),
  {Req, State}.

chain_edit_site_offline(Req0, State0=#mcb_html_state{post_data=PD}) ->
  Site0 = macaba_board:get_site_config(),
  Offline = macaba:as_bool(macaba:propget(<<"offline">>, PD, false)),
  OfflineMsg = macaba:propget(<<"offline_message">>, PD,
                              Site0#mcb_site_config.offline_message),
  Site = Site0#mcb_site_config{
           offline = Offline,
           offline_message = macaba:as_binary(OfflineMsg)
          },
  macaba_board:set_site_config(Site),
  lager:info("board offline mode set to: ~p", [Offline]),
  macaba_app:change_offline_mode(Offline),
  macaba_web:chain_success(macaba_web:redirect("/admin", Req0, State0)).

%%%-----------------------------------------------------------------------------
%% @doc GET: /admin/site/boards - edit boards list
%%%-----------------------------------------------------------------------------
-spec macaba_handle_admin_site_boards(Method :: binary(),
                                      Req :: cowboy_req:req(),
                                      State :: macaba_web:html_state()) ->
                                         macaba_web:handler_return().

macaba_handle_admin_site_boards(<<"POST">>, Req0, State0) ->
  lager:debug("http POST admin/site/boards"),
  {_, Req1, State1} = macaba_web:chain_run(
                        [ fun(R, S) -> chain_fail_if_user_not(R, S, admin) end
                        , fun chain_edit_site_boards/2
                        ], Req0, State0),
  macaba_web:redirect("/admin", Req1, State1).

chain_edit_site_boards(Req0, State0=#mcb_html_state{post_data=PD}) ->
  Site0 = macaba_board:get_site_config(),
  BoardsJson = macaba:propget(<<"boards">>, PD),
  %% lager:debug("Boards: ~p", [BoardsJson]),
  Boards = lists:map(fun(ErlJson) ->
                         macaba_json:from_json(mcb_board, ErlJson)
                     end, jsx:decode(BoardsJson)),
  Site = Site0#mcb_site_config{
           boards = Boards
          },
  macaba_board:set_site_config(Site),
  macaba_web:chain_success(Req0, State0).

%%%-----------------------------------------------------------------------------
%%% HELPER FUNCTIONS
%%%-----------------------------------------------------------------------------


%%% Local Variables:
%%% erlang-indent-level: 2
%%% End:
