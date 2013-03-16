%%%------------------------------------------------------------------------
%%% @doc Tests for Macaba utility library
%%% Created: 2013-02-23 Dmytro Lytovchenko <kvakvs@yandex.ru>
%%%------------------------------------------------------------------------
-module(macaba_markup_tests).

-include_lib("eunit/include/eunit.hrl").

%%%------------------------------------------------------------------------
%%% Test list
%%%------------------------------------------------------------------------

html_handler_test_() ->
  {setup, fun setup/0, fun teardown/1,
   {foreach, fun foreach_setup/0, fun foreach_teardown/1,
    [ {"Pagination",        fun pagination/0}
    , {"Conversions",       fun conversions/0}
    , {"Markup Emphasis-1", fun markup_emphasis1/0}
    , {"Markup Emphasis-2", fun markup_emphasis2/0}
    , {"Markup Strong",     fun markup_strong/0}
    , {"Markup Blockquote", fun markup_blockquote/0}
    , {"Markup Code",       fun markup_code/0}
    , {"Markup Code Inline",fun markup_code_inl/0}
    , {"Markup Href",       fun markup_href/0}
    ]
   }}.

%%%------------------------------------------------------------------------
%%% Fixtures
%%%------------------------------------------------------------------------
setup() ->
  %% ok = meck:new(macaba_conf, [unstick]),
  %% ok = meck:expect(
  %%        macaba_conf, get,
  %%        fun([<<"plugins">>, <<"wakabamark">>, <<"enable_markdown">>]) ->
  %%            {ok, true};
  %%           ([<<"plugins">>, <<"wakabamark">>, <<"enable_bbcode">>]) ->
  %%            {ok, true}
  %%        end),
  ok.

teardown(ok) ->
  meck:unload(),
  ok.

foreach_setup() ->
  ok.

foreach_teardown(ok) ->
  ok.

%%%------------------------------------------------------------------------
%%% Tests
%%%------------------------------------------------------------------------
pagination() ->
  L = [1,2,3,4,5,6,7,8,9,10],
  ?assertEqual([], macaba:pagination(L, 0, 4)),
  ?assertEqual([1,2,3,4], macaba:pagination(L, 1, 4)),
  ?assertEqual([5,6,7,8], macaba:pagination(L, 2, 4)),
  ?assertEqual([9,10], macaba:pagination(L, 3, 4)),
  ?assertEqual([], macaba:pagination(L, 4, 4)).

conversions() ->
  ?assertEqual(12345, macaba:as_integer(12345)),
  ?assertEqual(12345, macaba:as_integer("12345")),
  ?assertEqual(12345, macaba:as_integer(<<"12345">>)),

  ?assertEqual({1,2,3,4}, macaba:as_ipv4("1.2.3.4")),
  ?assertEqual({1,2,3,4}, macaba:as_ipv4(<<"1.2.3.4">>)),

  ?assertEqual(<<"12345">>, macaba:as_binary('12345')),
  ?assertEqual(<<"12345">>, macaba:as_binary(<<"12345">>)),
  ?assertEqual(<<"12345">>, macaba:as_binary("12345")),
  ?assertEqual(<<"12345">>, macaba:as_binary(12345)),

  ?assertEqual(true, macaba:as_bool("1")),
  ?assertEqual(false, macaba:as_bool("0")),
  ?assertEqual(true, macaba:as_bool("true")),
  ?assertEqual(false, macaba:as_bool("false")),
  ?assertEqual(true, macaba:as_bool(1)),
  ?assertEqual(false, macaba:as_bool(0)),

  ?assertEqual("12345", macaba:as_string(12345)),
  ?assertEqual("12345", macaba:as_string("12345")),
  ?assertEqual("12345", macaba:as_string(<<"12345">>)),
  ?assertEqual("12345", macaba:as_string('12345')).

call_wakabamark(T) ->
  macaba_markup:wakabamark_process(T, [{bbcode, true}, {markdown, true}]).

markup_emphasis1() ->
  P = fun call_wakabamark/1,
  %% ?assertEqual(["\t<em>hello</em>"], P("\t*hello*")),
  ?assertEqual(["<em>hello</em>\t"], P("*hello*\t")),
  ?assertEqual([" <em>hello</em>"], P(" *hello*")),
  ?assertEqual(["<em>hello</em> "], P("*hello* ")),

  ?assertEqual([" <em>hello</em> "], P(" *hello* ")),
  %% ?assertEqual(["\t<em>hello</em> "], P("\t*hello* ")),
  ?assertEqual([" <em>hello</em>\t"], P(" *hello*\t")),

  ?assertEqual(["fgs <em>hello</em> "], P("fgs *hello* ")),
  %% ?assertEqual(["fgs", "\t<em>hello</em> "], P("fgs\n\t*hello* ")),
  ?assertEqual(["fgs", "<em>hello</em>\t"], P("fgs\n*hello*\t")).

markup_emphasis2() ->
  P = fun call_wakabamark/1,
  %% ?assertEqual(["\t<em>hello</em>"], P("\t*hello*")),
  ?assertEqual(["<em>hello</em>\t"], P("*hello*\t")),
  ?assertEqual([" <em>hello</em>"], P(" *hello*")),
  ?assertEqual(["<em>hello</em> "], P("*hello* ")),

  ?assertEqual([" <em>hello</em> "], P(" *hello* ")),
  %% ?assertEqual(["\t<em>hello</em> "], P("\t*hello* ")),
  ?assertEqual([" <em>hello</em>\t"], P(" *hello*\t")),

  ?assertEqual(["*hello\t"], P("*hello\t")),

  ?assertEqual(["fgs <em>hello</em> "], P("fgs *hello* ")),
  %% ?assertEqual(["fgs", "\t<em>hello</em> "], P("fgs\n\t*hello* ")),
  ?assertEqual(["fgs", "<em>hello</em>\t"], P("fgs\n*hello*\t")).

markup_strong() ->
  P = fun call_wakabamark/1,
  ?assertEqual(["   <strong>hello</strong>"], P("   **hello**")),
  ?assertEqual(["<strong>hello</strong>\t"], P("**hello**\t")),
  ?assertEqual([" <strong>hello</strong>"], P(" **hello**")),
  ?assertEqual(["<strong>hello</strong> "], P("**hello** ")),

  ?assertEqual([" <strong>hello</strong> "], P(" **hello** ")),
  ?assertEqual(["   <strong>hello</strong> "], P("   **hello** ")),
  ?assertEqual([" <strong>hello</strong>\t"], P(" **hello**\t")),

  ?assertEqual(["**hello\t"], P("**hello\t")),
  ?assertEqual(["hello\t**"], P("hello\t**")),
  ?assertEqual(["hello**\t"], P("hello**\t")),

  ?assertEqual(["fgs <strong>hello</strong> "], P("fgs **hello** ")),
  %% ?assertEqual(["fgs", "\t<strong>hello</strong> "], P("fgs\n\t**hello** ")),
  ?assertEqual(["fgs", "<strong>hello</strong>\tfds"],
               P("fgs\n**hello**\tfds")).

markup_blockquote() ->
  P = fun call_wakabamark/1,
  ?assertEqual(["<blockquote>&gt;hello</blockquote>"], P(">hello")),
  ?assertEqual(["<blockquote>&gt; hello</blockquote>"], P("> hello")),
  ?assertEqual(["<blockquote>&gt;hello</blockquote>", "fgs"], P(">hello\nfgs")),
  ?assertEqual(["<blockquote>&gt;foo<br />\n&gt;baz</blockquote>", "fgs"],
               P(">foo\n>baz\nfgs")).

markup_code() ->
  P = fun call_wakabamark/1,
  ?assertEqual(["<pre>hello</pre>"], P("    hello")),
  ?assertEqual(["<pre>hello</pre>"], P("\thello")),
  ?assertEqual(["<pre>foo</pre>", "fgs"], P("    foo\nfgs")),
  ?assertEqual(["<pre>foo\nbar</pre>", "fgs"], P("    foo\n    bar\nfgs")).

markup_code_inl() ->
  P = fun call_wakabamark/1,
  ?assertEqual(["<code>foo</code>\t"], P("`foo`\t")),
  ?assertEqual([" <code>foo</code>"], P(" `foo`")),
  ?assertEqual(["  <code>foo</code> "], P("  `foo` ")),
  ?assertEqual(["`foo`\t"], P("``foo``\t")).

markup_href() ->
  P = fun call_wakabamark/1,
  QUrl = "https://abc000.macaba.git-hub.c0m/?q=a&amp;b=%20d&amp;",
  ?assertEqual(["<a href=\""++QUrl++"\">"++QUrl++"</a>"],
               P("https://abc000.macaba.git-hub.c0m/?q=a&b=%20d&")).


%%% Local Variables:
%%% erlang-indent-level: 2
%%% End:
