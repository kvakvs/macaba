%%%-------------------------------------------------------------------
%%% @doc Supervisor
%%% @version 2013-02-21
%%% @author Dmytro Lytovchenko <kvakvs@yandex.ru>
%%%-------------------------------------------------------------------
-module(mcb_sup).

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
  %% Nodes = case application:get_env(mcb, cluster) of
  %%           {ok, Value} -> Value;
  %%           undefined -> [node()]
  %%         end,
  {ok, { {one_for_one, 15, 60},
         [ child(mcb_conf, worker)
         , child(mcb_hooks, worker) % run this after conf, before plugins
         , child(mcb_plugins, worker) % run this after conf
         , child(mcb_masternode, worker)
         %% , child(mcb_ses_sup, supervisor)
         , child(mcb_startup, worker, transient)
         , child(mcb_board_worker, worker) % run this when board started
         ]} }.


%%% Local Variables:
%%% erlang-indent-level: 2
%%% End:
