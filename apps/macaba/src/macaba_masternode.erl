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

-record(leader_state, {
          riak_sync_tab,
          sync = false :: boolean(),
          is_leader = false :: boolean()
         }).
%% @doc Represents an value in updated Mnesia memory table. Current cluster
%% leader fetches records from ETS and sends them to RIAK periodically.
%% TODO: Stop syncing if leader changes, transfer update lists to new leader?
-record(mcb_riak_sync, {
            id     :: {atom(), binary()}
         }).
-define(RESYNC_SLEEP_MSEC, 1000).

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
  T = ets:new(mcb_riak_sync, [ protected
                             , {keypos, #mcb_riak_sync.id}
                             ]),
  lager:debug("masternode: init"),
  {ok, #leader_state{
    riak_sync_tab = T
    }}.

%%--------------------------------------------------------------------
%% @private
%% @doc Called only in the leader process when it is elected. The Synch
%% term will be broadcasted to all the nodes in the cluster.
-spec elected(State :: #leader_state{},
              Election :: gen_leader:election(),
              Node :: node() | undefined) ->
                 {ok, Synch :: any(), State :: #leader_state{}}.
elected(State, _Election, undefined) ->
  lager:debug("masternode: elected(node=undef)"),
  Synch = [],
  {ok, Synch, State#leader_state{is_leader=true}};

%% Called only in the leader process when a new candidate joins the
%% cluster. The Synch term will be sent to Node.
elected(State, _Election, Node) ->
  lager:debug("masternode: elected(node=~p)", [Node]),
  {reply, [], State#leader_state{is_leader=true}}.

%%--------------------------------------------------------------------
%% @private
%% @doc Called in all members of the cluster except the leader. Synch is a
%% term returned by the leader in the elected/3 callback.
-spec surrendered(#leader_state{}, Synch :: any(),
                  Election :: gen_leader:election()) -> {ok, #leader_state{}}.
surrendered(State, _Synch, _Eelection) ->
  lager:debug("masternode: surrendered"),
  %% TODO: actions when leader is lost by this node
  {ok, State#leader_state{is_leader=false}}.

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

%% @doc Command from mnesia db that a record was updated. Handled only if sync
%% flag in state set to true
handle_leader_call({updated_in_mnesia, Type, Key}, _From,
                   State=#leader_state{sync=true}, _Election) ->
  %%lager:debug("masternode: resync mark ~p:~p", [Type, Key]),
  SyncValue = #mcb_riak_sync{id={Type, Key}},
  ets:insert(State#leader_state.riak_sync_tab, SyncValue),
  {reply, ok, State};

%% @doc Starts sync mode for Mnesia data
handle_leader_call(start_resync, _From, State=#leader_state{sync=false}, _E) ->
  lager:info("masternode: resync to RIAK enabled"),
  erlang:send_after(?RESYNC_SLEEP_MSEC, self(), do_resync),
  {reply, ok, State#leader_state{sync=true}};

handle_leader_call(start_resync, _From, State=#leader_state{sync=true}, _E) ->
  {reply, already_started, State};

handle_leader_call(get_leader, _, S, Election) ->
  {reply, gen_leader:leader_node(Election), S};

handle_leader_call(Request, _From, State, _Election) ->
  lager:error("masternode: unk leader_call ~p", [Request]),
  {reply, {unknown_leader_call, Request}, State}.

%%--------------------------------------------------------------------
%% @private
%% @doc Handling cast messages. Called in the leader.
-spec handle_leader_cast(Request :: any(), #leader_state{},
                         Election :: gen_leader:election()) ->
                                {ok, Broadcast :: any(), #leader_state{}} |
                                {noreply, #leader_state{}} |
                                {stop, Reason :: any(), #leader_state{}}.
handle_leader_cast(Request, State, _Election) ->
  lager:error("masternode: unk leader_cast ~p", [Request]),
  {noreply, State}.

%%--------------------------------------------------------------------
%% @private
%% @doc Handling messages from leader.
-spec from_leader(Request :: any(), #leader_state{},
                  Election :: gen_leader:election()) ->
                     {ok, #leader_state{}} | {noreply, #leader_state{}} |
                     {stop, Reason :: any(), #leader_state{}}.
from_leader(_Synch, State, _Election) ->
  lager:debug("masternode: from_leader", []),
  {ok, State}.

%%--------------------------------------------------------------------
%% @private
%% @doc Handling nodes going down. Called in the leader only.
-spec handle_DOWN(Node :: node(), #leader_state{},
                  Election :: gen_leader:election()) ->
                     {ok, #leader_state{}} |
                     {ok, Broadcast :: any(), #leader_state{}}.
handle_DOWN(_Node, State, _Election) ->
  lager:debug("masternode: DOWN"),
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
handle_call(get_leader, _, S, Election) ->
  {reply, gen_leader:leader_node(Election), S};

handle_call(Request, _From, State, _Election) ->
  lager:error("masternode: unk local call ~p", [Request]),
  {reply, {unknown_local_call, Request}, State}.

%%--------------------------------------------------------------------
%% @private
%% @doc Handling cast messages
-spec handle_cast(Msg :: any(), #leader_state{},
                  Election :: gen_leader:election()) ->
                     {noreply, #leader_state{}} |
                     {stop, Reason :: any(), #leader_state{}}.
handle_cast(Msg, State, _Election) ->
  lager:error("masternode: unk cast ~p", [Msg]),
  {noreply, State}.

%%--------------------------------------------------------------------
%% @private
%% @doc Handling all non call/cast messages
-spec handle_info(Info :: any(), #leader_state{},
                  Election :: gen_leader:election()) ->
                     {noreply, #leader_state{}} |
                     {stop, Reason :: any(), #leader_state{}}.

%% @doc Iterates over keys in mcb_riak_sync and writes updated records to Riak
handle_info(do_resync, State=#leader_state{sync=true}, _E) ->
  sync_mnesia_to_riak(State#leader_state.riak_sync_tab),
  erlang:send_after(?RESYNC_SLEEP_MSEC, self(), do_resync),
  {noreply, State};

handle_info(do_resync, State=#leader_state{sync=false}, _E) ->
  lager:error("masternode: bad incoming 'do_resync' when sync=false", []),
  {noreply, State};

handle_info(Info, State, _Election) ->
  lager:error("masternode: unk info ~p", [Info]),
  {noreply, State}.

%%--------------------------------------------------------------------
%% @private
%% @doc This function is called by a gen_leader when it is about to
%% terminate. It should be the opposite of Module:init/1 and do any
%% necessary cleaning up. When it returns, the gen_leader terminates
%% with Reason. The return value is ignored.
-spec terminate(Reason :: any(), #leader_state{}) -> ok.
terminate(_Reason, _State) ->
  lager:debug("masternode: terminated"),
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

%% @private
%% @doc Iterates over ETS mcb_riak_sync tab, each key represents record type
%% and key to read from Mnesia and save to RIAK.
sync_mnesia_to_riak(Tab) ->
  sync_mnesia_to_riak_2(Tab, ets:first(Tab)),
  %% this is called synchronously from masternode process, so we guarantee
  %% that there will be no writes to ETS while sync is in progress
  ets:delete_all_objects(Tab).

%% @private
sync_mnesia_to_riak_2(_Tab, '$end_of_table') -> ok;
sync_mnesia_to_riak_2(Tab, K) ->
  [#mcb_riak_sync{id={Type, Key}}] = ets:lookup(Tab, K),
  case macaba_db_mnesia:read(Type, Key) of
    {error, not_found} ->
      lager:debug("masternode sync: delete ~p key=~p", [Type, Key]),
      macaba_db_riak:delete(Type, Key);
    Value ->
      lager:debug("masternode sync: write ~p key=~p", [Type, Key]),
      macaba_db_riak:write(Type, Value)
  end,
  sync_mnesia_to_riak_2(Tab, ets:next(Tab, K)).


%%% Local Variables:
%%% erlang-indent-level: 2
%%% End:
