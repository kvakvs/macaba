%%%-------------------------------------------------------------------
%%% @doc Supervisor
%%% @version 2013-02-21
%%% @author Dmytro Lytovchenko <kvakvs@yandex.ru>
%%%-------------------------------------------------------------------
-module(macaba_sup).

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
  %% Nodes = case application:get_env(macaba, cluster) of
  %%           {ok, Value} -> Value;
  %%           undefined -> [node()]
  %%         end,
  {ok, { {one_for_one, 15, 60},
         [ child(macaba_conf, worker)
         , child(macaba_hooks, worker) % run this after conf, before plugins
         , child(macaba_plugins, worker) % run this after conf
         , child(macaba_masternode, worker)
         %% , child(macaba_ses_sup, supervisor)
         , child(macaba_startup, worker, transient)
         , child(macaba_board_worker, worker) % run this when board started
         ]} }.


%%% Local Variables:
%%% erlang-indent-level: 2
%%% End:
