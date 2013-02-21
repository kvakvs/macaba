-module(macaba_sup).

-behaviour(supervisor).

%% API
-export([start_link/0]).

%% Supervisor callbacks
-export([init/1]).

%% Helper macro for declaring children of supervisor
child(I, Type) -> {I, {I, start_link, []}, permanent, 5000, Type, [I]}.

%% ===================================================================
%% API functions
%% ===================================================================

start_link() ->
  supervisor:start_link({local, ?MODULE}, ?MODULE, []).

%% ===================================================================
%% Supervisor callbacks
%% ===================================================================

init([]) ->
  {ok, { {one_for_one, 5, 10}, [ child(macaba_ses_sup, supervisor)
                               , child(macaba_masternode, worker)
                               ]} }.

%%% Local Variables:
%%% erlang-indent-level: 2
%%% End:
