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
%%        , macaba_handle_admin_logout/2
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
%% @doc GET/POST: /admin
%%%-----------------------------------------------------------------------------
macaba_handle_admin(<<"GET">>, {Req0, State0}) ->
  lager:debug("http GET admin"),
  %% Boards = macaba_board_cli:get_boards(),
  %% State1 = state_set_var(boards, Boards, State0),
  {_, {Req, State}} = macaba_web:chain_run(
                        [ fun macaba_html_handler:chain_get_boards/1
                        , fun(X) -> chain_fail_if_user(X, anon) end
                        ], {Req0, State0}),
  macaba_web:render_page("admin", Req, State);

macaba_handle_admin(<<"POST">>, {Req0, State0}) ->
  lager:debug("http POST admin"),
  {_, {Req, State}} = macaba_web:chain_run(
                        [ fun chain_check_admin_login/1
                        , fun chain_check_mod_login/1
                        ], {Req0, State0}),
  macaba_web:redirect("/admin/", Req, State).

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
chain_fail_if_user({Req0, State0}, Role) ->
  %% {Req, State} = get_user(Req0, State0),
  #mcb_user{type=Type} = State0#mcb_html_state.user,
  case Type of
    Role ->
      %% Login required if accessing this as anonymous
      {error, macaba_web:render_page("admin_login", Req0, State0)};
    _ ->
      {ok, {Req0, State0}}
  end.

%%%-----------------------------------------------------------------------------
%%% HELPER FUNCTIONS
%%%-----------------------------------------------------------------------------


%%% Local Variables:
%%% erlang-indent-level: 2
%%% End:
