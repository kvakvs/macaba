%%%------------------------------------------------------------------------
%%% @doc Utility functions for web server and templates
%%% Created: 2013-02-17 Dmytro Lytovchenko <kvakvs@yandex.ru>
%%%------------------------------------------------------------------------
-module(macaba_web).

-export([ compile/1
        , render/2
        , chain_run/2
        , get_session/1
        , new_session/1
        ]).

-include_lib("macaba/include/macaba_types.hrl").

%%%------------------------------------------------------------------------
-spec compile(string()) -> atom().
compile(TplName) ->
  TplModule = list_to_atom(TplName ++ "_dtl"),
  %% recompile-debug
  erlydtl:compile("templates/" ++ TplName ++ ".dtl", TplModule,
                  [ verbose
                  , {out_dir, "templates/ebin"}
                  , {doc_root, "templates/"}
                  , {custom_tags_dir, "templates/custom_tags"}
                  ]),
  TplModule.

%%%------------------------------------------------------------------------
-spec render(string(), [{atom(), any()}]) -> iolist().
render(TplName, TplOptions) ->
  TplModule = compile(TplName),
  case erlang:function_exported(TplModule, render, 1) of
    true ->
      {ok, Content} = TplModule:render(TplOptions),
      Content;
    false ->
      erlang:error({error, not_exported, {TplModule, render, 1}})
  end.

%%%------------------------------------------------------------------------
%% @doc Runs list of functions passing opaque state through them and stopping
%% if any of functions returns error.
-spec chain_run([fun()], State :: any()) ->
                   {ok, any()} | {error, any(), any()}.
chain_run([], State) -> {ok, State};
chain_run([F | Tail], State) ->
  case F(State) of
    {ok, State2} ->
      chain_run(Tail, State2);
    {error, State2} ->
      {error, State2}
  end.

%%%------------------------------------------------------------------------
%% @doc Examines request for session cookie, and gets current user
-spec get_session(SesId :: undefined | binary()) -> #mcb_user{}.
get_session(undefined) -> #mcb_user{};
get_session(SesId) ->
  case gproc:lookup_local_name({macaba_session, SesId}) of
    undefined -> #mcb_user{};
    Pid -> gen_server:call(Pid, get_user)
  end.

%%%------------------------------------------------------------------------
%% @doc Creates session process with given 'Params' returns SesId and Pid
%% Params[remote_addr] - erlang tuple with IPv4 or IPv6, Params[user] -
%% #mcb_user{} structure
-spec new_session(Params :: orddict:orddict()) -> {binary(), pid()}.
new_session(Params) ->
  SesId = make_random_sesid(32, []),
  %%Pid = gen_server:start_link(macaba_ses, [Params], []),
  {ok, Pid} = supervisor:start_link(macaba_ses, [Params]),
  {SesId, Pid}.

%%%------------------------------------------------------------------------
%% @private
make_random_sesid(0, A) -> list_to_binary(A);
make_random_sesid(X, A) ->
  Ch = case random:uniform(62)-1 of
         C when C < 10 -> $0 + C;
         C when C < 10+26 -> $A + C - 26;
         C -> $a + C - 52
       end,
  make_random_sesid(X-1, [Ch|A]).

%%% Local Variables:
%%% erlang-indent-level: 2
%%% End:
