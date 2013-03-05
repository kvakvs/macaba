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

-include_lib("macaba/include/macaba_types.hrl").

-record(conf_state, {
            conf = [] :: proplist_of(binary()|proplist_t())
          , load_time :: calendar:datetime()
         }).
-define(SERVER, ?MODULE).
-define(CONF_FILENAME,    "macaba.config").
-define(CONF_RELOAD_MSEC, 30000).

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
  case load_config(#conf_state{}) of
    State=#conf_state{} ->
      erlang:send_after(?CONF_RELOAD_MSEC, self(), conf_reload),
      {ok, State};
    {error, Err1} ->
      macaba:fatal("Loading config " ?CONF_FILENAME, Err1)
  end.

%%--------------------------------------------------------------------
%% @doc Handling call messages
-spec handle_call(Request :: any(), From :: {pid(), any()}, #conf_state{}) ->
                         {reply, Reply :: any(), #conf_state{}} |
                         {reply, Reply :: any(), #conf_state{},
                          Timeout :: non_neg_integer()}
                       | {noreply, #conf_state{}} |
                         {noreply, #conf_state{}, Timeout :: non_neg_integer()}
                       | {stop, Reason :: any(), Reply :: any(),
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
                         {noreply, #conf_state{}}
                       | {noreply, #conf_state{}, Timeout :: non_neg_integer()}
                       | {stop, Reason :: any(), #conf_state{}}.
handle_cast(_Msg, State) ->
  {noreply, State}.

%%--------------------------------------------------------------------
%% @doc Handling all non call/cast messages
-spec handle_info(Info :: any(), #conf_state{}) ->
                         {noreply, #conf_state{}}
                       | {noreply, #conf_state{}, Timeout :: non_neg_integer()}
                       | {stop, Reason :: any(), #conf_state{}}.
handle_info(conf_reload, State) ->
  erlang:send_after(?CONF_RELOAD_MSEC, self(), conf_reload),
  FTime = calendar:datetime_to_gregorian_seconds(
            filelib:last_modified(?CONF_FILENAME)),
  OldTime = calendar:datetime_to_gregorian_seconds(State#conf_state.load_time),
  case FTime > OldTime of
    false ->
      {noreply, State};
    true ->
      case load_config(State) of
        S2=#conf_state{} ->
          lager:info("Config reloaded"),
          {noreply, S2};
        {error, E} ->
          lager:error("Config reload error: ~p", [E]),
          {noreply, State}
      end
  end;

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

%% @private
%% @doc Traverses the tree following list of keys, returns subtree or value
traverse([], Tree) -> {ok, Tree};
traverse([K|K2], Tree) ->
  case macaba:propget(K, Tree, undefined) of
    undefined -> {error, not_found};
    Subtree -> traverse(K2, Subtree)
  end.

%% @private
%% @doc Reloads config and updates the state with config and time
-spec load_config(#conf_state{}) -> #conf_state{} | {error, tuple()}.
load_config(S) ->
  case file:read_file(?CONF_FILENAME) of
    {ok, C1} ->
      ConfData = binary_to_list(C1),
      Conf = case etoml:parse(ConfData) of
               {ok, C2} -> C2;
               {error, Err2} -> {error, {syntax, Err2}}
             end,
      S#conf_state{
          conf      = Conf
        , load_time = filelib:last_modified(?CONF_FILENAME)
       };
    {error, Err1} ->
      {error, {not_found, Err1}}
  end.


%%% Local Variables:
%%% erlang-indent-level: 2
%%% End:
