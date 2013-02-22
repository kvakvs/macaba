%%%------------------------------------------------------------------------
%%% @doc Master node for Macaba cluster, elected by gen_leader. Master node
%%% does synchronization jobs, updates dynamics from Mnesia to RIAK, and
%%% reloads Mnesia dynamic data on startup from RIAK.
%%% Created: 2013-02-20 Dmytro Lytovchenko <kvakvs@yandex.ru>
%%%------------------------------------------------------------------------
-module(macaba_masternode).

-behaviour(gen_leader).

%% API
-export([ start_link/1
        , start_link/2
        ]).

%% gen_leader callbacks
-export([init/1
        , handle_cast/3
        , handle_call/4
        , handle_info/3
        , handle_leader_call/4
        , handle_leader_cast/3
        , handle_DOWN/3
        , elected/3
        , surrendered/3
        , from_leader/3
        , code_change/4
        , terminate/2
        ]).


-define(SERVER, ?MODULE).

-record(leader_state, { }).

%%%===================================================================
%%% API
%%%===================================================================

%%--------------------------------------------------------------------
%% @doc Starts the server
-spec start_link([node()]) -> {ok, pid()} | ignore | {error, Error :: any()}.
start_link(Nodes) ->
  start_link(Nodes, []).

-spec start_link([node()], node() | list()) ->
                    {ok, pid()} | ignore | {error, Error :: any()}.
start_link(Nodes, Seed) when is_list(Nodes), is_atom(Seed) ->
  start_link(Nodes, {seed_node, Seed});

start_link(Nodes, Opts) ->
  gen_leader:start_link(?SERVER, Nodes, Opts, ?MODULE, [], []).

%%%===================================================================
%%% gen_leader callbacks
%%%===================================================================

%%--------------------------------------------------------------------
%% @private
%% @doc Initializes the server
-spec init(Args :: list()) -> {ok, #leader_state{}} | ignore |
                              {stop, Reason :: any()}.
init([]) ->
  {ok, #leader_state{}}.

%%--------------------------------------------------------------------
%% @private
%% @doc Called only in the leader process when it is elected. The Synch
%% term will be broadcasted to all the nodes in the cluster.
-spec elected(State :: #leader_state{},
              Election :: gen_leader:election(),
              Node :: node() | undefined) ->
                 {ok, Synch :: any(), State :: #leader_state{}}.
elected(State, _Election, undefined) ->
  Synch = [],
  {ok, Synch, State};

%% Called only in the leader process when a new candidate joins the
%% cluster. The Synch term will be sent to Node.
elected(State, _Election, _Node) ->
  {reply, [], State}.

%%--------------------------------------------------------------------
%% @private
%% @doc Called in all members of the cluster except the leader. Synch is a
%% term returned by the leader in the elected/3 callback.
-spec surrendered(#leader_state{}, Synch :: any(),
                  Election :: gen_leader:election()) -> {ok, #leader_state{}}.
surrendered(State, _Synch, _Eelection) ->
  {ok, State}.

%%--------------------------------------------------------------------
%% @private
%% @doc Handling call messages. Called in the leader.
-spec handle_leader_call(Request :: any(), From :: pid(),
                         State :: #leader_state{},
                         Election :: gen_leader:election()) ->
                            {reply, Reply :: any(), Broadcast :: any(),
                             #leader_state{}} |
                            {reply, Reply :: any(), #leader_state{}} |
                            {noreply, #leader_state{}} |
                            {stop, Reason :: any(), Reply :: any(),
                             #leader_state{}} |
                            {stop, Reason :: any(), #leader_state{}}.
handle_leader_call(_Request, _From, State, _Election) ->
  {reply, ok, State}.

%%--------------------------------------------------------------------
%% @private
%% @doc Handling cast messages. Called in the leader.
-spec handle_leader_cast(Request :: any(), #leader_state{},
                         Election :: gen_leader:election()) ->
                                {ok, Broadcast :: any(), #leader_state{}} |
                                {noreply, #leader_state{}} |
                                {stop, Reason :: any(), #leader_state{}}.
handle_leader_cast(_Request, State, _Election) ->
  {noreply, State}.

%%--------------------------------------------------------------------
%% @private
%% @doc Handling messages from leader.
-spec from_leader(Request :: any(), #leader_state{},
                  Election :: gen_leader:election()) ->
                     {ok, #leader_state{}} | {noreply, #leader_state{}} |
                     {stop, Reason :: any(), #leader_state{}}.
from_leader(_Synch, State, _Election) ->
  {ok, State}.

%%--------------------------------------------------------------------
%% @private
%% @doc Handling nodes going down. Called in the leader only.
-spec handle_DOWN(Node :: node(), #leader_state{},
                  Election :: gen_leader:election()) ->
                     {ok, #leader_state{}} |
                     {ok, Broadcast :: any(), #leader_state{}}.
handle_DOWN(_Node, State, _Election) ->
  {ok, State}.

%%--------------------------------------------------------------------
%% @private
%% @doc Handling call messages
-spec handle_call(Request :: any(), From :: pid(), #leader_state{},
                  Election :: gen_leader:election()) ->
                     {reply, Reply :: any(), #leader_state{}} |
                     {noreply, #leader_state{}} |
                     {stop, Reason :: any(), Reply :: any(), #leader_state{}} |
                     {stop, Reason :: any(), #leader_state{}}.
handle_call(get_leader, _, S, E) ->
  {reply, gen_leader:leader_node(E), S};

handle_call(_Request, _From, State, _Election) ->
  Reply = ok,
  {reply, Reply, State}.

%%--------------------------------------------------------------------
%% @private
%% @doc Handling cast messages
-spec handle_cast(Msg :: any(), #leader_state{},
                  Election :: gen_leader:election()) ->
                     {noreply, #leader_state{}} |
                     {stop, Reason :: any(), #leader_state{}}.
handle_cast(_Msg, State, _Election) ->
  {noreply, State}.

%%--------------------------------------------------------------------
%% @private
%% @doc Handling all non call/cast messages
-spec handle_info(Info :: any(), #leader_state{},
                  Election :: gen_leader:election()) ->
                     {noreply, #leader_state{}} |
                     {stop, Reason :: any(), #leader_state{}}.
handle_info(_Info, State, _Election) ->
  {noreply, State}.

%%--------------------------------------------------------------------
%% @private
%% @doc This function is called by a gen_leader when it is about to
%% terminate. It should be the opposite of Module:init/1 and do any
%% necessary cleaning up. When it returns, the gen_leader terminates
%% with Reason. The return value is ignored.
-spec terminate(Reason :: any(), #leader_state{}) -> ok.
terminate(_Reason, _State) ->
  ok.

%%--------------------------------------------------------------------
%% @private
%% @doc Convert process state when code is changed
-spec code_change(OldVsn :: any(), #leader_state{},
                  Election :: gen_leader:election(), Extra :: any()) ->
                     {ok, #leader_state{}} |
                     {ok, #leader_state{},
                      NewElection :: gen_leader:election()}.
code_change(_OldVsn, State, _Election, _Extra) ->
  {ok, State}.


%%%===================================================================
%%% Internal functions
%%%===================================================================

%%% Local Variables:
%%% erlang-indent-level: 2
%%% End:
