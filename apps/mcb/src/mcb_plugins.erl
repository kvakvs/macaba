%%%-------------------------------------------------------------------
%%% @doc Startup service ensuring that other services start in the correct
%%% order, and then shutting down itself.
%%% @version 2013-02-21
%%% @author Dmytro Lytovchenko <kvakvs@yandex.ru>
%%%-------------------------------------------------------------------
-module(mcb_plugins).

-behaviour(gen_server).

%% API
-export([ call/2,
          mod/1
        ]).
-export([ start_link/0
        ]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).

-record(state, {}).
-define(SERVER, ?MODULE).

%%====================================================================
%% API
%%====================================================================
%% @private
%% @doc Gets config parameter board.markup_plugin, converts it to M,F,A and
%% calls to transform user input
call(markup, [Txt]) ->
  {ok, [MarkupMod0, MarkupFun0]} = mcb_conf:get(
                                     [<<"board">>, <<"markup_plugin">>],
                                     [<<"mcb_markup">>, <<"process">>]),
  MarkupMod = erlang:binary_to_atom(MarkupMod0, latin1),
  MarkupFun = erlang:binary_to_atom(MarkupFun0, latin1),
  U0 = erlang:apply(MarkupMod, MarkupFun, [Txt]),
  U = string:join(U0, "<br/>\n"),
  unicode:characters_to_binary(lists:flatten(U), utf8).

mod(T) ->
  {ok, Mod0} = mcb_conf:get([<<"plugins">>, mcb:as_binary(T)],
                               <<"undefined">>),
  mcb:as_atom(Mod0).

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
-spec init(Args :: list()) -> {ok, #state{}} | ignore
                                | {stop, Reason :: any()}.
init([]) ->
  ets:new(mcb_plugins, [named_table]),
  {ok, #state{}}.

%%--------------------------------------------------------------------
%% @doc Handling call messages
-spec handle_call(Request :: any(), From :: {pid(), any()}, #state{}) ->
                         {reply, Reply :: any(), #state{}} |
                         {reply, Reply :: any(), #state{},
                          Timeout :: non_neg_integer()} | {noreply, #state{}} |
                         {noreply, #state{}, Timeout :: non_neg_integer()} |
                         {stop, Reason :: any(), Reply :: any(), #state{}} |
                         {stop, Reason :: any(), #state{}}.
handle_call(_Request, _From, State) ->
  Reply = ok,
  {reply, Reply, State}.

%%--------------------------------------------------------------------
%% @doc Handling cast messages
-spec handle_cast(Msg :: any(), #state{}) ->
                         {noreply, #state{}} |
                         {noreply, #state{}, Timeout :: non_neg_integer()} |
                         {stop, Reason :: any(), #state{}}.
handle_cast(_Msg, State) ->
  {noreply, State}.

%%--------------------------------------------------------------------
%% @doc Handling all non call/cast messages
-spec handle_info(Info :: any(), #state{}) ->
                         {noreply, #state{}} |
                         {noreply, #state{}, Timeout :: non_neg_integer()} |
                         {stop, Reason :: any(), #state{}}.
handle_info(_Info, State) ->
  {noreply, State}.

%%--------------------------------------------------------------------
%% @doc This function is called by a gen_server when it is about to
%% terminate. It should be the opposite of Module:init/1 and do any necessary
%% cleaning up. When it returns, the gen_server terminates with Reason.
%% The return value is ignored.
-spec terminate(Reason :: any(), #state{}) -> any().
terminate(_Reason, _State) ->
  ok.

%%--------------------------------------------------------------------
%% @doc Convert process state when code is changed
-spec code_change(OldVsn :: any(), #state{}, Extra :: any()) ->
                     {ok, #state{}}.
code_change(_OldVsn, State, _Extra) ->
  {ok, State}.

%%--------------------------------------------------------------------
%%% Internal functions
%%--------------------------------------------------------------------


%%% Local Variables:
%%% erlang-indent-level: 2
%%% End:
