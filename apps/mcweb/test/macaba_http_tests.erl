%%%------------------------------------------------------------------------
%%% @doc HTTP tests ensuring that different parts of Macaba are functional
%%% Created: 2013-03-01 Dmytro Lytovchenko <kvakvs@yandex.ru>
%%%------------------------------------------------------------------------
-module(macaba_http_tests).

-include_lib("eunit/include/eunit.hrl").

%%%------------------------------------------------------------------------
%%% Test list
%%%------------------------------------------------------------------------

html_handler_test_() ->
  {setup, fun setup/0, fun teardown/1,
   {foreach, fun foreach_setup/0, fun foreach_teardown/1,
    [ %%{"Site Root",fun check_site_root/0}
    ]
   }}.

%%%------------------------------------------------------------------------
%%% Fixtures
%%%------------------------------------------------------------------------
setup() ->
  %% file:copy("../../../macaba.config", "macaba.config"),
  %% macaba_app:start().
  ok.

teardown(ok) ->
  %%application:stop(macaba).
  ok.

foreach_setup() ->
  ok.

foreach_teardown(ok) ->
  ok.

%%%------------------------------------------------------------------------
%%% Tests
%%%------------------------------------------------------------------------

check_site_root() ->
  %%macaba_test_lib:request("http://localhost:12000/", "GET").
  ok.


%%% Local Variables:
%%% erlang-indent-level: 2
%%% End:
