%%%------------------------------------------------------------------------
%%% @doc Defines start point for application, also starts dependencies if
%%% started from the console, and, sort of, prepared to be started as a
%%% release, but its not tested and not sure why you would want that?
%%% Created: 2013-02-16 Dmytro Lytovchenko <kvakvs@yandex.ru>
%%%------------------------------------------------------------------------
-module(macaba_app).

-behaviour(application).

%% Application callbacks
-export([ start/0
        , start/2
        , stop/1
        , start_web/0]).

%% ===================================================================
%% Application callbacks
%% ===================================================================
start() ->
  application:start(sasl),
  macaba:ensure_started(gproc),
  lager:start(),
  application:start(macaba).

start(_StartType, _StartArgs) ->
  macaba_sup:start_link().

stop(_State) ->
  ok.

%% @doc TODO: move this out of macaba_app
start_web() ->
  ok = macaba:ensure_started(crypto),
  ok = macaba:ensure_started(ranch),
  ok = macaba:ensure_started(cowboy),

  CurrentDir = filename:absname(""),
  CSSPath = filename:join([CurrentDir, "priv", "css"]),
  JSPath  = filename:join([CurrentDir, "priv", "js"]),
  ImgPath = filename:join([CurrentDir, "priv", "img"]),

  Mime = {mimetypes, [ {<<".css">>, [<<"text/css">>]}
                     , {<<".js">>,  [<<"application/javascript">>]}
                     , {<<".jpg">>,  [<<"image/jpeg">>]}
                     , {<<".png">>,  [<<"image/png">>]}
                     ]},
  S = cowboy_static,
  H = macaba_html_handler,

  St1 = {"/css/[...]", S, [{directory, CSSPath}, Mime]},
  St2 = {"/js/[...]",  S, [{directory, JSPath},  Mime]},
  St3 = {"/img/[...]", S, [{directory, ImgPath}, Mime]},

  TNew     = {"/board/:mcb_board/thread/new", H, [thread_new]},
  TManage = {"/board/:mcb_board/thread/:mcb_thread/manage", H, [thread_manage]},
  TShow    = {"/board/:mcb_board/thread/:mcb_thread", H, [thread]},
  TRepl    = {"/board/:mcb_board/post/new", H, [post_new]},
  BShow1   = {"/board/:mcb_board/:mcb_page", H, [board]},
  BShow2   = {"/board/:mcb_board", H, [board]},
  AttThumb = {"/attach/:mcb_attach/thumb", H, [attach_thumb]},
  Attach   = {"/attach/:mcb_attach", H, [attach]},
  ALogin   = {"/admin", H, [admin]},

  Disp = cowboy_router:compile(
           [ {'_', [ St1, St2, St3
                   , AttThumb, Attach
                   , TNew, TManage, TShow, TRepl
                   , BShow1, BShow2
                   , ALogin
                   , {"/", H, [index]}
                   ]}
           ]),
  {ok, HttpPort} = macaba_conf:get_or_fatal([<<"html">>, <<"listen_port">>]),
  {ok, Listeners} = macaba_conf:get_or_fatal([<<"html">>, <<"listeners">>]),
  cowboy:start_http(macaba_http_listener, Listeners,
                    [{port, HttpPort}],
                    [{env, [{dispatch, Disp}]}]
                   ).

%%% Local Variables:
%%% erlang-indent-level: 2
%%% End:
