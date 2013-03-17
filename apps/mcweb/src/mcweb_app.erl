%%%------------------------------------------------------------------------
%%% @doc Defines start point for application, also starts dependencies if
%%% started from the console, and, sort of, prepared to be started as a
%%% release, but its not tested and not sure why you would want that?
%%% Created: 2013-02-16 Dmytro Lytovchenko <kvakvs@yandex.ru>
%%%------------------------------------------------------------------------
-module(mcweb_app).

-behaviour(application).

%% Application callbacks
-export([ start/0
        , start/2
        , stop/1
        , start_web/0
        , change_offline_mode/1
        ]).

-include_lib("macaba/include/macaba_types.hrl").
-include_lib("mcweb/include/mcweb.hrl").
-define(MACABA_LISTENER, macaba_http_listener).

%% ===================================================================
%% Application callbacks
%% ===================================================================
start() ->
  macaba:ensure_started(sasl),
  macaba:ensure_started(gproc),
  lager:start(),
  application:start(mcweb).

start(_StartType, _StartArgs) ->
  start_web(),
  mcweb_sup:start_link().

stop(_State) ->
  ok.

%% @doc TODO: move this out of macaba_app
start_web() ->
  ok = macaba:ensure_started(crypto),
  ok = macaba:ensure_started(ranch),
  ok = macaba:ensure_started(cowboy),

  Site = macaba_board:get_site_config(),
  Disp = cowboy_compile_dispatch(Site#mcb_site_config.offline),
  cowboy_start_listener(Disp).

cowboy_start_listener(Disp) ->
  {ok, HttpPort} = macaba_conf:get_or_fatal([<<"html">>, <<"listen_port">>]),
  {ok, Listeners} = macaba_conf:get_or_fatal([<<"html">>, <<"listeners">>]),
  cowboy:start_http(?MACABA_LISTENER, Listeners,
                    [{port, HttpPort}],
                    [{env, [{dispatch, Disp}]}]
                   ).

change_offline_mode(Offline) ->
  Disp = cowboy_compile_dispatch(Offline),
  %% cowboy:stop_listener(?MACABA_LISTENER),
  %% cowboy_start_listener(Disp).
  cowboy:set_env(?MACABA_LISTENER, dispatch, Disp).

cowboy_compile_dispatch(Offline) ->
  CurrentDir = filename:absname(""),
  CSSPath = filename:join([CurrentDir, "priv", "css"]),
  JSPath  = filename:join([CurrentDir, "priv", "js"]),
  ImgPath = filename:join([CurrentDir, "priv", "img"]),

  Mime = {mimetypes, [ {<<".css">>, [<<"text/css">>]}
                     , {<<".js">>,  [<<"application/javascript">>]}
                     , {<<".jpg">>,  [<<"image/jpeg">>]}
                     , {<<".png">>,  [<<"image/png">>]}
                     ]},
  SMod = cowboy_static,
  HMod = mcweb_html_public,
  AMod = mcweb_html_admin,

  St1 = {"/css/[...]", SMod, [{directory, CSSPath}, Mime]},
  St2 = {"/js/[...]",  SMod, [{directory, JSPath},  Mime]},
  St3 = {"/img/[...]", SMod, [{directory, ImgPath}, Mime]},

  %%--- anonymous/public resources ---
  Index    = {"/", HMod, [index]},
  TNew     = {"/board/:mcb_board/thread/new", HMod, [thread_new]},
  TManage  = {"/board/:mcb_board/thread/:mcb_thread/manage", HMod,
              [thread_manage]},
  TShow    = {"/board/:mcb_board/thread/:mcb_thread", HMod, [thread]},
  TRepl    = {"/board/:mcb_board/post/new", HMod, [post_new]},
  BShow1   = {"/board/:mcb_board/:mcb_page", HMod, [board]},
  BShow2   = {"/board/:mcb_board", HMod, [board]},
  AttThumb = {"/attach/:mcb_attach/thumb", HMod, [attach_thumb]},
  Attach   = {"/attach/:mcb_attach", HMod, [attach]},
  UPvw     = {"/util/preview", HMod, [util_preview]},

  %%--- admin resources ---
  ASiteB  = {"/admin/site/boards", AMod, [admin_site_boards]},
  ASiteO  = {"/admin/site/offline", AMod, [admin_site_offline]},
  ASite   = {"/admin/site", AMod, [admin_site]},
  ALogin  = {"/admin/login", AMod, [admin_login]},
  ALogout = {"/admin/logout", AMod, [admin_logout]},
  ALanding= {"/admin", AMod, [admin]},

  %% if board is offline, attaches, boards, threads and preview is shutting down
  %% TODO: make a good 404 page
  case Offline of
    false ->
      BoardResources = [ AttThumb, Attach
                       , TNew, TManage, TShow, TRepl
                       , BShow1, BShow2
                       , UPvw, Index
                       ],
      CatchAll = [];
    true ->
      BoardResources = [],
      CatchAll = [{'_', AMod, [offline]}]
  end,
  cowboy_router:compile(
    [ {'_',
       [ St1, St2, St3 ]
       ++ BoardResources
       ++ [ ASiteB, ASiteO, ASite, ALogin, ALogout, ALanding ]
       ++ CatchAll
      } ]).

%%% Local Variables:
%%% erlang-indent-level: 2
%%% End:
