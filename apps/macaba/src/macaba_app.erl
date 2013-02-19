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
  CSSPath = filename:join([CurrentDir, "priv", "web", "css"]),
  JSPath  = filename:join([CurrentDir, "priv", "web", "js"]),
  ImgPath = filename:join([CurrentDir, "priv", "web", "img"]),

  Mime = {mimetypes, [ {<<".css">>, [<<"text/css">>]}
                     , {<<".js">>,  [<<"application/javascript">>]}
                     ]},
  %% a module to handle HTML interface to the board, render templates, board
  %% and thread lists
  Html = macaba_html_handler,
  Disp = cowboy_router:compile(
           [ {'_', [
                     {"/css/[...]", cowboy_static, [{directory, CSSPath}, Mime]}
                   , {"/js/[...]",  cowboy_static, [{directory, JSPath},  Mime]}
                   , {"/img/[...]", cowboy_static, [{directory, ImgPath}, Mime]}
                   , {"/board/:mcb_board/new", Html, [board_new]}
                   , {"/board/:mcb_board/[...]", Html, [board]}
                , {"/board/:mcb_board/thread/:mcb_thread/[...]", Html, [thread]}
                   , {"/", Html, [index]}
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
