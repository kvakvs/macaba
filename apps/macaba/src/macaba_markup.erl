%%%-----------------------------------------------------------------------------
%%% @doc Processes a raw unicode text entered by user, to produce HTML
%%% according to Wakaba formatting rules.
%%% Created: 2013-02-28 Dmytro Lytovchenko <kvakvs@yandex.ru>
%%%-----------------------------------------------------------------------------
-module(macaba_markup).

-export([ wakabamark/1
        , erlmarkdown/1
        ]).

erlmarkdown(T) ->
  markdown:conv_utf8(T).

wakabamark(T) when is_binary(T) ->
  wakabamark(unicode:characters_to_list(T, utf8));

wakabamark(T0) when is_list(T0) ->
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
          %% inline code
          , {"([^`])`([^`].+?[^`])`([^`])",
             "\\1<code>\\2</code>\\3"}
          , {"``", "`"}
          %% [spoiler][/spoiler]
          , {"\\[spoiler\\](.+?)\\[/spoiler\\]",
             "<span class=\"spoiler\">\\1</span>"}
          %% URL
          %% , {"((https?|ftp)://[-\\.a-zA-Z0-9]+?/?\\?"
          %%    "[-_.=/a-zA-z0-9;&\"]*?)",
          , {"(https?://[^\\s<>\"]*?)((?:\\s|<|>|\"|\\.|\\)|\\]|!"
             "|\\?|,|&#44;|&quot;)*(?:[\\s<>\"]|$))"
             , "<a href=\"\\1\">\\1</a>"}
          %% line breaks for ordered/unordered list items
          %% , {"- .*?\\R\s+.*?\\R", ""}
          ],
  T2 = lists:foldl(fun({Re0, Replace}, Acc) ->
                       %% {ok, Re} = re:compile(Re0, [unicode]),
                       re:replace(Acc, Re0, Replace,
                                  [global, unicode, multiline, {return, list}])
                   end, T1, Rules),

  %% detect line-constructs like blockquote or code
  U1 = lists:map(fun process_line/1, string:tokens(T2, [ $\n ])),

  %% merge similar tagged constructs
  U2 = lists:foldl(fun({Tag, L}, []) -> [{Tag, L}];
                      ({Tag, L}, [{Tag, M} | Tail]) ->
                                         [{Tag, lines(Tag, [L, M])} | Tail];
                      (L, Acc) -> [L | Acc]
                  end, [], lists:reverse(U1)),
  U = lists:map(fun({blockquote, L}) ->
                    "<blockquote>" ++ L ++ "</blockquote>";
                   ({code, L}) ->
                    "<pre>" ++ L ++ "</pre>";
                   ({ordered_list, L}) ->
                    "<ol><li>" ++ L ++ "</li></ol>";
                   ({unordered_list, L}) ->
                    "<ul><li>" ++ L ++ "</li></ul>";
                   (L)               -> L
                end, U2),

  %% remove leading and trailing empty lines
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
    [X, $. | L1] when X >= $0 andalso X  =< $9 -> {ordered_list, L1};
    [$*, $  | L1] -> {unordered_list, L1};
    [$-, $  | L1] -> {unordered_list, L1};
    _ ->
      case L of
        [$ , $ , $ , $  | L1] -> {code, L1};
        [$\t | L1]            -> {code, L1};
        _ -> L
      end
  end.

lines(blockquote, List) -> string:join(List, "<br />\n");
lines(code, List) -> string:join(List, "\n");
lines(ordered_list, List) -> string:join(List, "</li>\n<li>");
lines(unordered_list, List) -> string:join(List, "</li>\n<li>").

%%% Local Variables:
%%% erlang-indent-level: 2
%%% End:
