%%%------------------------------------------------------------------------
%%% @doc Processes a raw unicode text entered by user, to produce HTML
%%% according to Wakaba formatting rules.
%%% Created: 2013-02-28 Dmytro Lytovchenko <kvakvs@yandex.ru>
%%%------------------------------------------------------------------------
-module(macaba_markup).

-export([ process/1
        ]).

process(T) when is_binary(T) ->
  process(unicode:characters_to_list(T, utf8));

process(T0) when is_list(T0) ->
  %% html encode and add end of line to simplify regex searching
  T1 = [$\n | xmerl_lib:export_text(T0) ++ "\n"],
  Rules = [% **bold**
            {"(\\s)\\*\\*(.+?)\\*\\*(\\s)", "\\1<strong>\\2</strong>\\3"}
          %% __bold__
          , {"(\\s)__(.+?)__(\\s)", "\\1<strong>\\2</strong>\\3"}
          %% *italic*
          , {"(\\s)\\*(.+?)\\*(\\s)", "\\1<em>\\2</em>\\3"}
          %% _italic_
          , {"(\\s)_(.+?)_(\\s)", "\\1<em>\\2</em>\\3"}
          ],
  T2 = lists:foldl(fun({Re, Replace}, Acc) ->
                       re:replace(Acc, Re, Replace, [multiline, {return, list}])
                   end, T1, Rules),
  %% io:format(standard_error, "T2=~p~n", [T2]),

  %% detect line-constructs like blockquote or code
  U1 = lists:map(fun process_line/1, string:tokens(T2, [ $\n ])),

  %% merge similar tagged constructs
  %% io:format(standard_error, "U1=~p~n", [U1]),
  U2 = lists:foldl(fun({Tag, L}, []) -> [{Tag, L}];
                      ({Tag, L}, [{Tag, M} | Tail]) ->
                                         [{Tag, lines([L, M])} | Tail];
                      (L, Acc) -> [L | Acc]
                  end, [], lists:reverse(U1)),
  %% io:format(standard_error, "U2=~p~n", [U2]),
  U = lists:map(fun({blockquote, L}) -> "<blockquote>" ++ L ++ "</blockquote>";
                   ({code, L})       -> "<pre>" ++ L ++ "</pre>";
                   (L)               -> L
                end, U2),
  %% io:format(standard_error, "U=~p~n", [U]),

  %% remove leading and trailing empty lines
  %%string:strip(string:join(U, "<br />\n"), both, $\n).
  U.

%% @private
%% @doc Wraps whole-line constructs into {something, Line}, then later we can
%% merge them
process_line(L) ->
  Trim1 = string:strip(L, left, $ ),
  Trim = string:strip(Trim1, left, $\t),
  case Trim of
    %% quote block
    [$&, $g, $t, $; | _]  -> {blockquote, L};
    _ ->
      case L of
        [$ , $ , $ , $  | L1] -> {code, L1};
        [$\t | L1]            -> {code, L1};
        _                     -> L
      end
  end.

lines(List) ->
  string:join(List, "<br />\n").

%%% Local Variables:
%%% erlang-indent-level: 2
%%% End:
