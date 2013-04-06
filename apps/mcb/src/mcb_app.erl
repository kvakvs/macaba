%%%------------------------------------------------------------------------
%%% @doc Defines start point for application, also starts dependencies if
%%% started from the console, and, sort of, prepared to be started as a
%%% release, but its not tested and not sure why you would want that?
%%% @version 2013-02-16
%%% @author Dmytro Lytovchenko <kvakvs@yandex.ru>
%%%------------------------------------------------------------------------
-module(mcb_app).

-behaviour(application).

%% Application callbacks
-export([ start/0
        , start/2
        , stop/1
        ]).

-include_lib("mcb/include/macaba_types.hrl").
%% -define(MACABA_LISTENER, macaba_http_listener).

%% ===================================================================
%% Application callbacks
%% ===================================================================
start() ->
  mcb:ensure_started(sasl),
  mcb:ensure_started(gproc),
  lager:start(),
  application:start(mcb).

start(_StartType, _StartArgs) ->
  mcb_sup:start_link().

stop(_State) ->
  ok.


%%% Local Variables:
%%% erlang-indent-level: 2
%%% End:
