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
        ]).

-include_lib("macaba/include/macaba_types.hrl").
%% -define(MACABA_LISTENER, macaba_http_listener).

%% ===================================================================
%% Application callbacks
%% ===================================================================
start() ->
  macaba:ensure_started(sasl),
  macaba:ensure_started(gproc),
  lager:start(),
  application:start(macaba).

start(_StartType, _StartArgs) ->
  macaba_sup:start_link().

stop(_State) ->
  ok.


%%% Local Variables:
%%% erlang-indent-level: 2
%%% End:
