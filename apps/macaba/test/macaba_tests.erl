%%%------------------------------------------------------------------------
%%% @doc Tests for Macaba utility library
%%% Created: 2013-02-23 Dmytro Lytovchenko <kvakvs@yandex.ru>
%%%------------------------------------------------------------------------
-module(macaba_tests).

-include_lib("eunit/include/eunit.hrl").

%%%------------------------------------------------------------------------
%%% Test list
%%%------------------------------------------------------------------------

html_handler_test_() ->
  {setup, fun setup/0, fun teardown/1,
   {foreach, fun foreach_setup/0, fun foreach_teardown/1,
    [ {"Pagination", fun pagination/0}
    , {"Conversions", fun conversions/0}
    ]
   }}.
    
%%%------------------------------------------------------------------------
%%% Fixtures
%%%------------------------------------------------------------------------
setup() ->
    ok.

teardown(ok) ->
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

    %% Bkt1 = make_test_bucket(2),
    %% M1 = call_group_mod(add, 1, all, [Bkt1]),
    %% ?assertEqual(ok, M1),
    %% ?assertEqual(true, group_exists(1)),

    %% %% Duplicate group should fail
    %% M1Err = call_group_mod(add, 1, all, []),
    %% ?assertMatch({error, #ofp_error_msg{}}, M1Err),

    %% linc_us3_groups:update_reference_count(?SWITCH_ID, 1, 333),

    %% %% Inserting duplicate group should fail
    %% MDup = call_group_mod(add, 1, all, []),
    %% ?assertMatch({error, #ofp_error_msg{}}, MDup),

    %% %% check that counters are zero
    %% G1Stats = linc_us3_groups:get_stats(?SWITCH_ID,
    %%                                     #ofp_group_stats_request{ group_id = 1 }),
    %% ?assertEqual(333, stats_get(G1Stats, 1, reference_count)),
    %% ?assertEqual(0, stats_get(G1Stats, 1, packet_count)),
    %% ?assertEqual(0, stats_get(G1Stats, 1, byte_count)),

    %% %% send a random package
    %% Pkt2 = test_packet_vlan(),
    %% linc_us3_groups:apply(1, Pkt2),

    %% %% check that counters changed
    %% G2Stats = linc_us3_groups:get_stats(?SWITCH_ID,
    %%                                     #ofp_group_stats_request{ group_id = 1 }),
    %% ?assertEqual(1, stats_get(G2Stats, 1, packet_count)),
    %% ?assertEqual(Pkt2#linc_pkt.size, stats_get(G2Stats, 1, byte_count)).


%%% Local Variables:
%%% erlang-indent-level: 2
%%% End:
