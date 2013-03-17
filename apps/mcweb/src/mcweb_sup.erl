-module(mcweb_sup).

-behaviour(supervisor).

%% API
-export([start_link/0]).

%% Supervisor callbacks
-export([init/1]).

%% Helper macro for declaring children of supervisor
child(I, Type) ->
  child(I, Type, permanent, []).
child(I, Type, Persist) ->
  child(I, Type, Persist, []).
child(I, Type, Persist, StartLinkParams) ->
  {I, {I, start_link, StartLinkParams}, Persist, 5000, Type, [I]}.

%% ===================================================================
%% API functions
%% ===================================================================

start_link() ->
  supervisor:start_link({local, ?MODULE}, ?MODULE, []).

%% ===================================================================
%% Supervisor callbacks
%% ===================================================================

init([]) ->
  {ok, { {one_for_one, 15, 60},
         [
          child(mcweb_ses_sup, supervisor)
         ]} }.


%%% Local Variables:
%%% erlang-indent-level: 2
%%% End:
