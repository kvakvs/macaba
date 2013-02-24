%%%------------------------------------------------------------------------
%%% @doc Utility functions for web server and templates
%%% Created: 2013-02-17 Dmytro Lytovchenko <kvakvs@yandex.ru>
%%%------------------------------------------------------------------------
-module(macaba_web).

-export([ compile/1
        , render/2
        , chain_run/2
        ]).

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

%%% Local Variables:
%%% erlang-indent-level: 2
%%% End:
