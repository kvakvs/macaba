-module(mcbd).

-export([ ping/0
        , get_site_config/1
        ]).

-include("mcbd_internal.hrl").
-include_lib("riak_core/include/riak_core_vnode.hrl").

%% Public API

%% @doc Pings a random vnode to make sure communication is functional
ping() ->
  DocIdx = riak_core_util:chash_key({<<"ping">>, term_to_binary(now())}),
  PrefList = riak_core_apl:get_primary_apl(DocIdx, 1, mcbd),
  [{IndexNode, _Type}] = PrefList,
  riak_core_vnode_master:sync_spawn_command(IndexNode, ping, mcbd_vnode_master).

%% @doc Get site configuration
read(Op, Args) ->
  {ok, ReqID} = mcbd_read_fsm:get(Op, Args),
  wait_for_reqid(ReqID, ?TIMEOUT).

%%% Local Variables:
%%% erlang-indent-level: 2
%%% End:
