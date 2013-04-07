-module(mcbd_app).

-behaviour(application).

%% Application callbacks
-export([start/2, stop/1]).

%% ===================================================================
%% Application callbacks
%% ===================================================================

start(_StartType, _StartArgs) ->
  mcb:ensure_started(mcweb),
  case mcbd_sup:start_link() of
    {ok, Pid} ->
      ok = riak_core:register_vnode_module(mcbd_vnode),
      ok = riak_core_node_watcher:service_up(mcbd, self()),

      %% ok = riak_core:register_vnode_module(rts_entry_vnode),
      %% ok = riak_core_node_watcher:service_up(rts_entry, self()),

      %% ok = riak_core:register_vnode_module(rts_stat_vnode),
      %% ok = riak_core_node_watcher:service_up(rts_stat, self()),

      %%EntryRoute = {["mcbd", "entry", client], rts_wm_entry, []},
      %%webmachine_router:add_route(EntryRoute),
      {ok, Pid};

    {error, Reason} ->
      {error, Reason}
  end.
  %% case mcbd_sup:start_link() of
  %%   {ok, Pid} ->
  %%     ok = riak_core:register([{vnode_module, mcbd_vnode}]),
  %%     ok = riak_core_ring_events:add_guarded_handler(mcbd_ring_event_handler, []),
  %%     ok = riak_core_node_watcher_events:add_guarded_handler(mcbd_node_event_handler, []),
  %%     ok = riak_core_node_watcher:service_up(mcbd, self()),
  %%     {ok, Pid};
  %%   {error, Reason} ->
  %%     {error, Reason}
  %% end.

stop(_State) ->
  ok.

%%% Local Variables:
%%% erlang-indent-level: 2
%%% End:
