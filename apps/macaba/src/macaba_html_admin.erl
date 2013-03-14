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

%%%-----------------------------------------------------------------------------
%% TODO: merge this with macaba_html_handler
handle(Req0, State0 = #mcb_html_state{ mode=Mode }) ->
  try
    {Method, Req1} = cowboy_req:method(Req0),

    %% parse request body as multipart, this will not work for POST urlencoded
    {Req2, State1} = case macaba_web:is_POST_and_multipart(Req1) of
                       {true, true}  ->
                         macaba_web:parse_multipart_form_data(Req1, State0);
                       {true, false}  ->
                         macaba_web:parse_body_qs(Req1, State0);
                       {false, _} ->
                         {Req1, State0}
                     end,
    {Req3, State2} = macaba_web:get_user(Req2, State1),
    FnName = macaba:as_atom("macaba_handle_" ++ macaba:as_string(Mode)),
    {Req4, State3} = apply(?MODULE, FnName, [Method, {Req3, State2}]),
    {ok, Req4, State3}
  catch
    E -> T = lists:flatten(io_lib:format("handle error: ~p ~p",
                                         [E, erlang:get_stacktrace()])),
         lager:error(E),
         {ReqE, StateE} = macaba_web:response_text(500, T, Req0, State0),
         {ok, ReqE, StateE}
  end.

%%%-----------------------------------------------------------------------------
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
%% @doc Gets user from ses cookie, checks if its type is Role, changes to login
%% page if user.type=Role
chain_fail_if_user({Req0, State0}, FailIfRole) ->
  %% #mcb_user{type=Type} = macaba_ses:get_user(Req0, State0),
  User = State0#mcb_html_state.user,
  #mcb_user{type=Type} = User,
  case Type of
    FailIfRole ->
      %% Login required if accessing this as anonymous
      %% TODO: remember old URL
      %% lager:debug("fail if user=~p -- ~p", [FailIfRole, User]),
      %%{error, macaba_web:redirect("/admin/login", Req0, State0)};
      {error, macaba_web:render_error(<<"Not authenticated">>, Req0, State0)};
    _ ->
      %% lager:debug("user role=~p -- ~p", [FailIfRole, User]),
      {ok, {Req0, State0}}
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
  Site = macaba_board:get_site_config(),
  Boards0 = lists:map(fun macaba:record_to_proplist/1,
                      Site#mcb_site_config.boards),
  Boards = jsx:encode(Boards0),
  State = macaba_web:state_set_var(siteconfig, [ {boards, Boards} ], State1),
  macaba_web:render_page("admin_site", Req, State).

%%%-----------------------------------------------------------------------------
%%% HELPER FUNCTIONS
%%%-----------------------------------------------------------------------------


%%% Local Variables:
%%% erlang-indent-level: 2
%%% End:
