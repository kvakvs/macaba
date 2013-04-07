%% @doc Supervise the distributed Macaba reader FSM.
-module(mcbd_read_fsm_sup).
-behavior(supervisor).

-export([start_read_fsm/1,
         start_link/0]).
-export([init/1]).

start_read_fsm(Args) ->
  supervisor:start_child(?MODULE, Args).

start_link() ->
  supervisor:start_link({local, ?MODULE}, ?MODULE, []).

init([]) ->
  GetFsm = {undefined,
            {rts_get_fsm, start_link, []},
            temporary, 5000, worker, [rts_get_fsm]},
  {ok, {{simple_one_for_one, 10, 10}, [GetFsm]}}.

%%% Local Variables:
%%% erlang-indent-level: 2
%%% End:
