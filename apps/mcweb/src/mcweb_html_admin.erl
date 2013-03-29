%%%-----------------------------------------------------------------------------
%%% @doc This module has few predefined handlers (init, handle and terminate)
%%% which are called by cowboy on incoming HTTP request.
%%% Serves HTML templates, and provides basic HTTP access to the board.
%%% Created: 2013-02-16 Dmytro Lytovchenko <kvakvs@yandex.ru>
%%%-----------------------------------------------------------------------------
-module(mcweb_html_admin).

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
-include_lib("mcweb/include/mcweb.hrl").

%%%-----------------------------------------------------------------------------
init({_Transport, http}, Req, [Mode]) ->
  {ok, Req, #mcb_html_state{
         mode = Mode
        }}.

-spec handle(cowboy_req:req(), mcweb:html_state()) ->
                {ok, cowboy_req:req(), mcweb:html_state()}.

handle(Req0, State0) ->
  mcweb:handle_helper(?MODULE, Req0, State0).

terminate(_Reason, _Req, _State) ->
  ok.

%%%-----------------------------------------------------------------------------
%% @doc GET: /offline - board shut down by admin
%%%-----------------------------------------------------------------------------
-spec macaba_handle_offline(Method :: binary(),
                            Req :: cowboy_req:req(),
                            State :: mcweb:html_state()) ->
                               mcweb:handler_return().

macaba_handle_offline(_, Req0, State0) ->
  Site = macaba_board:get_site_config(),
  M = Site#mcb_site_config.offline_message,
  State = mcweb:state_set_var(offline_message, M, State0),
  mcweb:render_page("offline", Req0, State).

%%%-----------------------------------------------------------------------------
%% @doc GET: /admin/login - login form
%%%-----------------------------------------------------------------------------
-spec macaba_handle_admin_login(Method :: binary(),
                                Req :: cowboy_req:req(),
                                State :: mcweb:html_state()) ->
                                   mcweb:handler_return().

macaba_handle_admin_login(<<"GET">>, Req0, State0) ->
  {_, Req, State} = mcweb:chain_run(
                        [ fun mcweb_html_public:chain_get_boards/2
                        ], Req0, State0),
  mcweb:render_page("admin_login", Req, State);

%% POST: /admin/login - login
macaba_handle_admin_login(<<"POST">>, Req0, State0) ->
  {_, Req, State} = mcweb:chain_run(
                        [ fun chain_check_admin_login/2
                        , fun chain_check_mod_login/2
                        ], Req0, State0),
  mcweb:redirect("/", Req, State).


%%%-----------------------------------------------------------------------------
%% @doc GET: /admin - landing page
%%%-----------------------------------------------------------------------------
-spec macaba_handle_admin(Method :: binary(),
                          Req :: cowboy_req:req(),
                          State :: mcweb:html_state()) ->
                             mcweb:handler_return().

macaba_handle_admin(<<"GET">>, Req0, State0) ->
  {_, Req, State} = mcweb:chain_run(
                      [ fun mcweb_html_public:chain_get_boards/2
                      , fun mcweb:chain_fail_if_below_mod/2
                      ], Req0, State0),
  mcweb:render_page("admin", Req, State).

%% @private
%% @doc Checks admin login and password from macaba.config
chain_check_admin_login(Req0, State0=#mcb_html_state{ post_data=PD }) ->
  {ok, ALogin} = macaba_conf:get([<<"board">>, <<"admin_login">>]),
  {ok, APassword} = macaba_conf:get([<<"board">>, <<"admin_password">>]),
  Login = macaba:propget(<<"login">>, PD),
  Password = macaba:propget(<<"password">>, PD),
  case {ALogin =:= Login, APassword =:= Password} of
    {true, true} ->
      {Req, State} = mcweb:create_session_for(
                       #mcb_user{level=?USERLEVEL_ADMIN},
                       Req0, State0),
      %% stop checking passwords right here
      mcweb:chain_fail(Req, State);
    _ ->
      mcweb:chain_success(Req0, State0)
  end.

%% @private
%% @doc Checks mod login and password from database
chain_check_mod_login(Req0, State0) ->
  {ok, Req0, State0}.

%%%-----------------------------------------------------------------------------
%% @doc GET: /admin/logout - delete admin cookie
%%%-----------------------------------------------------------------------------
-spec macaba_handle_admin_logout(Method :: binary(),
                                 Req :: cowboy_req:req(),
                                 State :: mcweb:html_state()) ->
                                    mcweb:handler_return().

macaba_handle_admin_logout(Method, Req0, State0) ->
  Req = mcweb:clear_session_cookie(Req0),
  mcweb:redirect("/", Req, State0).

%%%-----------------------------------------------------------------------------
%% @doc GET: /admin/site - site config page
%%%-----------------------------------------------------------------------------
-spec macaba_handle_admin_site(Method :: binary(),
                               Req :: cowboy_req:req(),
                               State :: mcweb:html_state()) ->
                                  mcweb:handler_return().

macaba_handle_admin_site(<<"GET">>, Req0, State0) ->
  {_, Req, State} = mcweb:chain_run(
                        [ fun mcweb_html_public:chain_get_boards/2
                        , fun mcweb:chain_fail_if_below_mod/2
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
  State1 = mcweb:state_set_var(site_boards, Boards, State0),
  State2 = mcweb:state_set_var(site_offline, Offline, State1),
  State3  = mcweb:state_set_var(site_offline_message, OfflineMsg, State2),
  mcweb:chain_success(mcweb:render_page("admin_site", Req0, State3)).

%%%-----------------------------------------------------------------------------
%% @doc GET: /admin/site/offline - edit board offline settings
%%%-----------------------------------------------------------------------------
-spec macaba_handle_admin_site_offline(Method :: binary(),
                                       Req :: cowboy_req:req(),
                                       State :: mcweb:html_state()) ->
                                          mcweb:handler_return().

macaba_handle_admin_site_offline(<<"POST">>, Req0, State0) ->
  {_, Req, State} = mcweb:chain_run(
                      [ fun mcweb:chain_fail_if_below_admin/2
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
  mcweb_app:change_offline_mode(Offline),
  mcweb:chain_success(mcweb:redirect("/admin", Req0, State0)).

%%%-----------------------------------------------------------------------------
%% @doc GET: /admin/site/boards - edit boards list
%%%-----------------------------------------------------------------------------
-spec macaba_handle_admin_site_boards(Method :: binary(),
                                      Req :: cowboy_req:req(),
                                      State :: mcweb:html_state()) ->
                                         mcweb:handler_return().

macaba_handle_admin_site_boards(<<"POST">>, Req0, State0) ->
  {_, Req1, State1} = mcweb:chain_run(
                        [ fun mcweb:chain_fail_if_below_admin/2
                        , fun chain_edit_site_boards/2
                        ], Req0, State0),
  mcweb:redirect("/admin", Req1, State1).

chain_edit_site_boards(Req0, State0=#mcb_html_state{post_data=PD}) ->
  Site0 = macaba_board:get_site_config(),
  BoardsJson = macaba:propget(<<"boards">>, PD),
  Boards = lists:map(fun(ErlJson) ->
                         macaba_json:from_json(mcb_board, ErlJson)
                     end, jsx:decode(BoardsJson)),
  Site = Site0#mcb_site_config{
           boards = Boards
          },
  macaba_board:set_site_config(Site),
  mcweb:chain_success(Req0, State0).

%%%-----------------------------------------------------------------------------
%%% HELPER FUNCTIONS
%%%-----------------------------------------------------------------------------


%%% Local Variables:
%%% erlang-indent-level: 2
%%% End:
