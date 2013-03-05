%%%------------------------------------------------------------------------
%%% @doc An anonymous or registered user session, is spawned when sesid is
%%% generated, and is deleted automatically, when session is not accessed
%%% for defined ses timeout time
%%% Created: 2013-02-16 Dmytro Lytovchenko <kvakvs@yandex.ru>
%%%------------------------------------------------------------------------
-module(macaba_ses).
-behaviour(gen_server).

%% API
-export([start_link/1]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).

-include_lib("macaba/include/macaba_types.hrl").

-record(mcb_session, {
          %% this like, allows ipv6 too, but will we ever support that?
          remote_addr = {0,0,0,0} :: ipaddr_t(),
          user :: #mcb_user{}
         }).
-define(SERVER, ?MODULE).

%%====================================================================
%% API
%%====================================================================
%% @doc Starts the server
-spec start_link([{atom(), any()}]) ->
                    {ok, pid()} | ignore | {error, Error :: any()}.
start_link(Params) ->
  gen_server:start_link( %%{local, ?SERVER},
    ?MODULE, Params, []).

%%====================================================================
%% gen_server callbacks
%%====================================================================
%% @private
%% @doc Initializes the server
-spec init(Args :: list()) -> {ok, #mcb_session{}} | ignore
                                  | {stop, Reason :: any()}.
init(Params) ->
  RemoteAddr = macaba:propget(remote_addr, Params, {0,0,0,0}),
  User = macaba:propget(user, Params, #mcb_user{}),
  {ok, #mcb_session{
     user = User,
     remote_addr = RemoteAddr
    }}.

%%--------------------------------------------------------------------
%% @doc Handling call messages
-spec handle_call(Request :: any(), From :: {pid(), any()}, #mcb_session{}) ->
                     {reply, Reply :: any(), #mcb_session{}} |
                     {reply, Reply :: any(), #mcb_session{},
                      Timeout :: non_neg_integer()}
                       | {noreply, #mcb_session{}} |
                     {noreply, #mcb_session{}, Timeout :: non_neg_integer()} |
                     {stop, Reason :: any(), Reply :: any(),
                      #mcb_session{}} | {stop, Reason :: any(), #mcb_session{}}.
handle_call(get_user, _From, State) ->
  {reply, State#mcb_session.user, State};

handle_call(_Request, _From, State) ->
  Reply = ok,
  {reply, Reply, State}.

%%--------------------------------------------------------------------
%% @doc Handling cast messages
-spec handle_cast(Msg :: any(), #mcb_session{}) ->
                     {noreply, #mcb_session{}} |
                     {noreply, #mcb_session{}, Timeout :: non_neg_integer()} |
                     {stop, Reason :: any(), #mcb_session{}}.
handle_cast(_Msg, State) ->
  {noreply, State}.

%%--------------------------------------------------------------------
%% @doc Handling all non call/cast messages
-spec handle_info(Info :: any(), #mcb_session{}) ->
                     {noreply, #mcb_session{}} |
                     {noreply, #mcb_session{}, Timeout :: non_neg_integer()} |
                     {stop, Reason :: any(), #mcb_session{}}.
handle_info(_Info, State) ->
  {noreply, State}.

%%--------------------------------------------------------------------
%% @doc This function is called by a gen_server when it is about to
%% terminate. It should be the opposite of Module:init/1 and do any necessary
%% cleaning up. When it returns, the gen_server terminates with Reason.
%% The return value is ignored.
-spec terminate(Reason :: any(), #mcb_session{}) -> any().
terminate(_Reason, _State) ->
  ok.

%%--------------------------------------------------------------------
%% @doc Convert process state when code is changed
-spec code_change(OldVsn :: any(), #mcb_session{}, Extra :: any()) ->
                                 {ok, #mcb_session{}}.
code_change(_OldVsn, State, _Extra) ->
  {ok, State}.

%%--------------------------------------------------------------------
%%% Internal functions
%%--------------------------------------------------------------------


%%% Local Variables:
%%% erlang-indent-level: 2
%%% End:
