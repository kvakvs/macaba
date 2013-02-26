%%%-------------------------------------------------------------------
%%% @doc Configuration loader
%%% Created: 2013-02-21
%%% @author Dmytro Lytovchenko <kvakvs@yandex.ru>
%%%-------------------------------------------------------------------
-module(macaba_conf).

-behaviour(gen_server).

%% API
-export([ get/1
        , get/2
        , get_or_fatal/1
        ]).
-export([ start_link/0
        ]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).

-record(conf_state, {
          conf = []
         }).
-define(SERVER, ?MODULE).

%%====================================================================
%% API
%%====================================================================

get(Key) ->
  gen_server:call(?SERVER, {get, Key}).

get(Key, Default) ->
  case gen_server:call(?SERVER, {get, Key}) of
    {ok, V} -> {ok, V};
    {error, not_found} -> {ok, Default};
    {error, What} -> {error, What}
  end.

get_or_fatal(Key) ->
  case gen_server:call(?SERVER, {get, Key}) of
    {ok, V} -> {ok, V};
    {error, not_found} ->
      macaba:fatal("Config parameter not found", Key);
    {error, What} ->
      macaba:fatal("Config access error", {{parameter, Key}, {error, What}})
  end.

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
-spec init(Args :: list()) -> {ok, #conf_state{}} | ignore |
                              {stop, Reason :: any()}.
init([]) ->
  ConfData = case file:read_file("macaba.config") of
               {ok, C1} ->
                 binary_to_list(C1);
               {error, Err1} ->
                 macaba:fatal("Loading macaba.config", Err1)
             end,
  Conf = case etoml:parse(ConfData) of
           {ok, C2} ->
             C2;
           {error, Err2} ->
             macaba:fatal("Parsing macaba.config", Err2)
         end,
  {ok, #conf_state{ conf=Conf }}.

%%--------------------------------------------------------------------
%% @doc Handling call messages
-spec handle_call(Request :: any(), From :: pid, #conf_state{}) ->
                         {reply, Reply :: any(), #conf_state{}} |
                         {reply, Reply :: any(), #conf_state{},
                          Timeout :: integer()} | {noreply, #conf_state{}} |
                         {noreply, #conf_state{}, Timeout :: integer()} |
                         {stop, Reason :: any(), Reply :: any(),
                          #conf_state{}} |
                         {stop, Reason :: any(), #conf_state{}}.
handle_call({get, Key}, _From, State=#conf_state{conf=Conf}) ->
  case traverse(Key, Conf) of
    {ok, V} ->
      {reply, {ok, V}, State};
    {error, What} ->
      {reply, {error, What}, State}
  end;

handle_call(_Request, _From, State) ->
  Reply = ok,
  {reply, Reply, State}.

%%--------------------------------------------------------------------
%% @doc Handling cast messages
-spec handle_cast(Msg :: any(), #conf_state{}) ->
                         {noreply, #conf_state{}} |
                         {noreply, #conf_state{}, Timeout :: integer()} |
                         {stop, Reason :: any(), #conf_state{}}.
handle_cast(_Msg, State) ->
  {noreply, State}.

%%--------------------------------------------------------------------
%% @doc Handling all non call/cast messages
-spec handle_info(Info :: any(), #conf_state{}) ->
                         {noreply, #conf_state{}} |
                         {noreply, #conf_state{}, Timeout :: integer()} |
                         {stop, Reason :: any(), #conf_state{}}.
handle_info(_Info, State) ->
  {noreply, State}.

%%--------------------------------------------------------------------
%% @doc This function is called by a gen_server when it is about to
%% terminate. It should be the opposite of Module:init/1 and do any necessary
%% cleaning up. When it returns, the gen_server terminates with Reason.
%% The return value is ignored.
-spec terminate(Reason :: any(), #conf_state{}) -> any().
terminate(_Reason, _State) ->
  ok.

%%--------------------------------------------------------------------
%% @doc Convert process state when code is changed
-spec code_change(OldVsn :: any(), #conf_state{}, Extra :: any()) ->
                         {ok, #conf_state{}}.
code_change(_OldVsn, State, _Extra) ->
  {ok, State}.

%%--------------------------------------------------------------------
%%% Internal functions
%%--------------------------------------------------------------------
traverse([], Tree) -> Tree;
traverse([K|K2], Tree) ->
  Subtree = macaba:propget(K, Tree),
  traverse(K2, Subtree).

%%% Local Variables:
%%% erlang-indent-level: 2
%%% End:
