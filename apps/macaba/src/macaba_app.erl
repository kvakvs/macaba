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
    JSPath = filename:join([CurrentDir, "priv", "web", "js"]),
    ImgPath = filename:join([CurrentDir, "priv", "web", "img"]),

    MT = {mimetypes, [ {<<".css">>, [<<"text/css">>]}
                     , {<<".js">>, [<<"application/javascript">>]}
                     ]},
    Dispatch = [ %% {URIHost, list({URIPath, Handler, Opts})}
                 {'_', [ {[<<"css">>, '...'], cowboy_static, [{directory, CSSPath}, MT]}
                       , {[<<"js">>,  '...'], cowboy_static, [{directory, JSPath},  MT]}
                       , {[<<"img">>, '...'], cowboy_static, [{directory, ImgPath}, MT]}
%%                       , {[<<"ws">>,  '...'], macaba_ws_handler, []}
                       , {[<<"html">>,'...'], macaba_html_handler, []}
                       ]}
        ],
    {ok, HttpPort} = application:get_env(macaba, macaba_http_port),
    {ok, Listeners} = application:get_env(macaba, macaba_http_listeners),
    cowboy:start_http(macaba_http_listener, Listeners,
                      [{port, HttpPort}],
                      [{dispatch, Dispatch}]
                     ).
