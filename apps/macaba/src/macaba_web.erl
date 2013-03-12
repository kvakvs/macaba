%%%------------------------------------------------------------------------
%%% @doc Utility functions for web server and templates
%%% Created: 2013-02-17 Dmytro Lytovchenko <kvakvs@yandex.ru>
%%%------------------------------------------------------------------------
-module(macaba_web).

-export([ compile/1
        , render/2
        , chain_run/2
        , get_poster_id/1
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
%% @doc Using user IP and user-agent
get_poster_id(Req0) ->
  {{{IP1, IP2, IP3, _IP4}, _Port}, Req1} = cowboy_req:peer(Req0),
  {UA, _Req2} = cowboy_req:header(<<"user-agent">>, Req1),
  Id0 = erlang:term_to_binary({IP1, IP2, IP3, UA}),
  %% split 160 bits of sha evenly and bxor together
  <<Id1:53, Id2:53, Id3:53, _:1>> = crypto:sha(Id0),
  Id = Id1 bxor Id2 bxor Id3,
  get_poster_id_encode(Id, []).

%% @doc Encode a long integer in base62
get_poster_id_encode(0, A) -> iolist_to_binary(A);
get_poster_id_encode(X, A) ->
  Ch = case X rem 62 of
        C when C < 10 -> $0 + C;
        C when C < 36 -> $A + C - 10;
        C -> $a + C - 36
       end,
  get_poster_id_encode(X div 62, [Ch | A]).

%%% Local Variables:
%%% erlang-indent-level: 2
%%% End:
