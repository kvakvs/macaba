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
        , stop/1 ]).

%% ===================================================================
%% Application callbacks
%% ===================================================================
start() ->
  application:start(sasl),
  macaba:ensure_started(gproc),
  lager:start(),
  application:start(macaba).

start(_StartType, _StartArgs) ->
  %% TODO: reorder start calls to db and board and leader (spawned under sup)
  macaba_db_mnesia:start(),
  macaba_db_riak:start(),
  macaba_board:start(),
  start_web(),
  macaba_sup:start_link().

stop(_State) ->
  ok.

start_web() ->
  ok = application:start(crypto),
  ok = application:start(ranch),
  ok = application:start(cowboy),

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
  TShow    = {"/board/:mcb_board/thread/:mcb_thread", H, [thread]},
  TRepl    = {"/board/:mcb_board/post/new", H, [post_new]},
  BShow1   = {"/board/:mcb_board/:mcb_page", H, [board]},
  BShow2   = {"/board/:mcb_board", H, [board]},
%%  AttSmall = {"/attach/:mcb_attach/thumb", H, [attach_thumb]},
  Attach   = {"/attach/:mcb_attach", H, [attach]},

  Disp = cowboy_router:compile(
           [ {'_', [ St1, St2, St3
                   , TNew, TShow, TRepl
                   , BShow1, BShow2
                   , Attach %%, AttSmall
                   , {"/", H, [index]}
                   ]}
           ]),
  {ok, HttpPort} = application:get_env(macaba, http_port),
  {ok, Listeners} = application:get_env(macaba, http_listeners),
  cowboy:start_http(macaba_http_listener, Listeners,
                    [{port, HttpPort}],
                    [{env, [{dispatch, Disp}]}]
                   ).

%%% Local Variables:
%%% erlang-indent-level: 2
%%% End:
