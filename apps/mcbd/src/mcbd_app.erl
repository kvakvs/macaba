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
            ok = riak_core:register([{vnode_module, mcbd_vnode}]),
            ok = riak_core_ring_events:add_guarded_handler(mcbd_ring_event_handler, []),
            ok = riak_core_node_watcher_events:add_guarded_handler(mcbd_node_event_handler, []),
            ok = riak_core_node_watcher:service_up(mcbd, self()),
            {ok, Pid};
        {error, Reason} ->
            {error, Reason}
    end.

stop(_State) ->
    ok.
