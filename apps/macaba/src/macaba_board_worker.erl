%%%-------------------------------------------------------------------
%%% @doc Board worker is a permanently running gen_server doing various
%%% board tasks, such as background deletion of objects
%%% Created: 2013-03-02
%%% @author Dmytro Lytovchenko <kvakvs@yandex.ru>
%%%-------------------------------------------------------------------
-module(macaba_board_worker).

-behaviour(gen_server).

%% API
-export([ delete_thread/2
        ]).
-export([ start_link/0
        ]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).

-record(bw_state, {}).
-define(SERVER, ?MODULE).

%%====================================================================
%% API
%%====================================================================

delete_thread(BoardId, ThreadId) ->
  ?SERVER ! {delete_thread, {BoardId, ThreadId}}.

%%--------------------------------------------------------------------
%% @doc Starts the server
-spec start_link() -> {ok, pid()} | ignore | {error, Error :: any()}.
start_link() ->
  gen_server:start_link({local, ?SERVER}, ?MODULE, [], []).

%%====================================================================
%% gen_server callbacks
%%====================================================================
%%--------------------------------------------------------------------
%% @private
%% @doc Initializes the server
-spec init(Args :: list()) -> {ok, #bw_state{}} | ignore
                                  | {stop, Reason :: any()}.
init([]) ->
  {ok, #bw_state{}}.

%%--------------------------------------------------------------------
%% @doc Handling call messages
-spec handle_call(Request :: any(), From :: pid, #bw_state{}) ->
                         {reply, Reply :: any(), #bw_state{}} |
                         {reply, Reply :: any(), #bw_state{},
                          Timeout :: integer()} | {noreply, #bw_state{}} |
                         {noreply, #bw_state{}, Timeout :: integer()} |
                         {stop, Reason :: any(), Reply :: any(), #bw_state{}} |
                         {stop, Reason :: any(), #bw_state{}}.
handle_call(Request, _From, State) ->
  Reply = ok,
  lager:error("board_worker: unk call ~p", [Request]),
  {reply, Reply, State}.

%%--------------------------------------------------------------------
%% @doc Handling cast messages
-spec handle_cast(Msg :: any(), #bw_state{}) ->
                         {noreply, #bw_state{}} |
                         {noreply, #bw_state{}, Timeout :: integer()} |
                         {stop, Reason :: any(), #bw_state{}}.
handle_cast(Msg, State) ->
  lager:error("board_worker: unk case ~p", [Msg]),
  {noreply, State}.

%%--------------------------------------------------------------------
%% @doc Handling all non call/cast messages
-spec handle_info(Info :: any(), #bw_state{}) ->
                         {noreply, #bw_state{}} |
                         {noreply, #bw_state{}, Timeout :: integer()} |
                         {stop, Reason :: any(), #bw_state{}}.
handle_info({delete_thread, {BoardId, ThreadId}}, State) ->
  macaba_board:delete_thread(BoardId, ThreadId),
  {noreply, State};

handle_info(Info, State) ->
  lager:error("board_worker: unk info ~p", [Info]),
  {noreply, State}.

%%--------------------------------------------------------------------
%% @doc This function is called by a gen_server when it is about to
%% terminate. It should be the opposite of Module:init/1 and do any necessary
%% cleaning up. When it returns, the gen_server terminates with Reason.
%% The return value is ignored.
-spec terminate(Reason :: any(), #bw_state{}) -> any().
terminate(_Reason, _State) ->
  ok.

%%--------------------------------------------------------------------
%% @doc Convert process state when code is changed
-spec code_change(OldVsn :: any(), #bw_state{}, Extra :: any()) ->
                         {ok, #bw_state{}}.
code_change(_OldVsn, State, _Extra) ->
  {ok, State}.

%%--------------------------------------------------------------------
%%% Internal functions
%%--------------------------------------------------------------------


%%% Local Variables:
%%% erlang-indent-level: 2
%%% End:
