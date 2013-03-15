%%%------------------------------------------------------------------------
%%% @doc An anonymous or registered user session, is spawned when sesid is
%%% generated, and is deleted automatically, when session is not accessed
%%% for defined ses timeout time
%%% Created: 2013-02-16 Dmytro Lytovchenko <kvakvs@yandex.ru>
%%%------------------------------------------------------------------------
-module(macaba_ses).
-behaviour(gen_server).

%% API
-export([ start_link/1
        , get/1
        , new/1
        ]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).

-include_lib("macaba/include/macaba_types.hrl").

-record(mcb_session, {
          %% this like, allows ipv6 too, but will we ever support that?
          sesid = <<>> :: binary(),
          remote_addr = {0,0,0,0} :: ipaddr_t(),
          user :: #mcb_user{}
         }).
-define(SERVER, ?MODULE).
-define(SESTIMEOUT_MSEC, 60 * 60 * 1000). %% 60 min session timeout

%%====================================================================
%% API
%%====================================================================

%%%------------------------------------------------------------------------
%% @doc Examines request for session cookie, and gets current user
-spec get(SesId :: undefined | binary()) -> {ok, pid()} | {error, not_found}.
get(<<>>) -> {error, not_found};
get(SesId) when is_binary(SesId) ->
  case catch gproc:lookup_local_name({macaba_session, SesId}) of
    X when is_pid(X) -> {ok, X};
    _Error -> {error, not_found}
  end;
get(_) -> {error, not_found}.

%%%------------------------------------------------------------------------
%% @doc Creates session process with given 'Params' returns SesId and Pid
%% Params[remote_addr] - erlang tuple with IPv4 or IPv6, Params[user] -
%% #mcb_user{} structure
-spec new(Params :: orddict:orddict()) -> {binary(), pid()}.
new(Params0) ->
  SesId = make_random_sesid(32),
  %%Pid = gen_server:start_link(macaba_ses, [Params], []),
  Params1 = [{sesid, SesId} | Params0],
  lager:debug("ses:new id=~s", [SesId]),
  {ok, Pid} = supervisor:start_child(macaba_ses_sup, [Params1]),
  {SesId, Pid}.

%%%------------------------------------------------------------------------
%% @doc Starts the server
-spec start_link([{atom(), any()}]) ->
                    {ok, pid()} | ignore | {error, Error :: any()}.
start_link(Params) ->
  gen_server:start_link(%%{local, ?SERVER}, 
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
  User       = macaba:propget(user, Params, #mcb_user{}),
  SesId      = macaba:propget(sesid, Params),
  gproc:add_local_name({macaba_session, SesId}),
  {ok, #mcb_session{
     sesid       = SesId,
     user        = User,
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
  {reply, State#mcb_session.user, State, ?SESTIMEOUT_MSEC};

handle_call(_Request, _From, State) ->
  Reply = ok,
  {reply, Reply, State, ?SESTIMEOUT_MSEC}.

%%--------------------------------------------------------------------
%% @doc Handling cast messages
-spec handle_cast(Msg :: any(), #mcb_session{}) ->
                     {noreply, #mcb_session{}} |
                     {noreply, #mcb_session{}, Timeout :: non_neg_integer()} |
                     {stop, Reason :: any(), #mcb_session{}}.
handle_cast(_Msg, State) ->
  {noreply, State, ?SESTIMEOUT_MSEC}.

%%--------------------------------------------------------------------
%% @doc Handling all non call/cast messages
-spec handle_info(Info :: any(), #mcb_session{}) ->
                     {noreply, #mcb_session{}} |
                     {noreply, #mcb_session{}, Timeout :: non_neg_integer()} |
                     {stop, Reason :: any(), #mcb_session{}}.
handle_info(_Info, State) ->
  {noreply, State, ?SESTIMEOUT_MSEC}.

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

%% @private
%% @doc Creates random sesid, repeats until it doesn't exist
make_random_sesid(Length) ->
  SesId = make_random_sesid_internal(crypto:rand_bytes(Length*4), []),
  case ?MODULE:get(SesId) of
    {error, not_found} -> SesId;
    {ok, _} -> make_random_sesid(Length) % loop here until ses is uniq
  end.

%% @private
make_random_sesid_internal(<<>>, A) -> list_to_binary(A);
make_random_sesid_internal(<< X:32, Rest/binary >>, A) ->
  Ch = case X rem 62 of
         C when C < 10 -> $0 + C;
         C when C < 36 -> $A + C - 10;
         C -> $a + C - 36
       end,
  make_random_sesid_internal(Rest, [Ch|A]).

%%% Local Variables:
%%% erlang-indent-level: 2
%%% End:
