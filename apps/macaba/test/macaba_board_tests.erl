%%%------------------------------------------------------------------------
%%% @doc Tests for Macaba board, thread and post libraries
%%% Created: 2013-02-23 Dmytro Lytovchenko <kvakvs@yandex.ru>
%%%------------------------------------------------------------------------
-module(macaba_board_tests).

-include_lib("eunit/include/eunit.hrl").
-include_lib("macaba/include/macaba_types.hrl").

%%%------------------------------------------------------------------------
%%% Test list
%%%------------------------------------------------------------------------

board_engine_test_() ->
  {setup, fun setup/0, fun teardown/1,
   {foreach, fun foreach_setup/0, fun foreach_teardown/1,
    [ {"Basic Mnesia: Thread dynamic", fun try_mnesia_thread_dynamic/0}
    , {"Basic Mnesia: Board dynamic", fun try_mnesia_board_dynamic/0}
    , {"Basic Riak: Thread", fun try_riak_thread/0}
    , {"Post to a new thread", fun try_new_thread/0}
    , {"Post with sage", fun try_post_with_sage/0}
    , {"Post with attach", fun try_post_with_attach/0}
    , {"Board tests", fun try_board/0}
    ]
   }}.

%%%------------------------------------------------------------------------
%%% Fixtures
%%%------------------------------------------------------------------------
setup() ->
  file:copy("../../../macaba.config", "macaba.config"),
  application:start(sasl),
  net_kernel:start(['macaba@localhost', longnames]),
  macaba:ensure_started(gproc),
  lager:start(),
  RPClusters = [ {macaba_cluster
            , [ {ping_request_timeout, 1500} % keep alive
                , {rec_timer, [ {value, 200}, {factor, 2}
                                , {incr, 0}, {max_value, 15000} ]}
              ] , [ {macaba_pool_srv1, [ {size, 10}, {max_overflow, 10} ],
                     [ {host, "127.0.0.1"}, {port, 8087} ]} ]} ],
  application:set_env(riak_pool, clusters, RPClusters),
  application:set_env(riak_pool, default_cluster, macaba_cluster),
  application:start(macaba),
  ok.

teardown(ok) ->
  application:stop(macaba),
  ok.

foreach_setup() ->
  %% timer:sleep(500),
  macaba_db:reset_all_data(),
  ok.

foreach_teardown(ok) ->
  %% timer:sleep(500),
  macaba_db:reset_all_data(),
  ok.

%%%------------------------------------------------------------------------
%%% Tests
%%%------------------------------------------------------------------------

try_riak_thread() ->
  BoardId = <<"testboard0">>,
  ThreadId = <<"12345">>,
  T = #mcb_thread{ thread_id = ThreadId, board_id = BoardId },
  macaba_db_riak:write(mcb_thread, T),
  ?assertMatch({ok, T}, macaba_thread:get(BoardId, ThreadId)).

%% @doc Basic check that Mnesia is reading and writing fine
try_mnesia_board_dynamic() ->
  %% io:format(standard_error, "+++ try_mnesia_board_dynamic~n", []),
  BoardId = <<"testboard0">>,
  BD1 = #mcb_board_dynamic{ board_id = BoardId },
  {atomic, _} = macaba_db_mnesia:write(mcb_board_dynamic, BD1),
  {ok, BD2} = macaba_db_mnesia:read(mcb_board_dynamic, BoardId),
  ?assertEqual(BD1, BD2).

%%%------------------------------------------------------------------------
try_mnesia_thread_dynamic() ->
  %% io:format(standard_error, "+++ try_mnesia_thread_dynamic~n", []),
  BoardId = <<"testboard0">>,
  ThreadId1 = <<"012345">>,
  TD1Key = macaba_db:key_for(mcb_thread_dynamic, {BoardId, ThreadId1}),
  TD1 = #mcb_thread_dynamic{ board_id = BoardId,
                             thread_id = ThreadId1,
                             internal_mnesia_key = TD1Key },
  {atomic, _} = macaba_db_mnesia:write(mcb_thread_dynamic, TD1),
  {ok, TD2} = macaba_thread:get_dynamic(BoardId, ThreadId1),
  ?assertEqual(TD1, TD2).

%%%------------------------------------------------------------------------
try_new_thread() ->
  %% io:format(standard_error, "+++ try_new_thread-1~n", []),
  BoardId = <<"unconfigured">>,
  %%----------------------
  %% post a new thread
  %%----------------------
  PostOpt1 = make_post_opts([{thread_id, <<"new">>}]),
  {ok, Thread1, Post1} = macaba_thread:new(BoardId, [], PostOpt1),
  ?assertMatch(#mcb_post{}, Post1),
  ?assertMatch(#mcb_thread{}, Thread1),
  ThreadId1 = Thread1#mcb_thread.thread_id,

  %% new thread has same id as first post
  {ok, TestP1} = macaba_post:get(BoardId, ThreadId1),
  ?assertMatch(#mcb_post{}, TestP1),
  {ok, TestT1} = macaba_thread:get(BoardId, ThreadId1),
  ?assertMatch(#mcb_thread{}, TestT1),

  %%----------------------
  %% post a reply
  %%----------------------
  PostOpt2 = make_post_opts([{thread_id, ThreadId1}]),
  {ok, Post2} = macaba_post:new(BoardId, PostOpt2),
  ?assertMatch(#mcb_post{}, Post2),

  %%----------------------
  %% enum threads
  %%----------------------
  io:format(standard_error, "+++ try_new_thread-4~n", []),
  {ok, [], Threads3} = macaba_board:get_threads(BoardId),
  ?assert(lists:any(fun(X) -> X =:= Thread1 end, Threads3)),

  %% io:format(standard_error, "+++ try_new_thread-end~n", []),
  ok.

%%%------------------------------------------------------------------------
try_post_with_sage() ->
  %% io:format(standard_error, "+++ try_post_with_sage~n", []),
  BoardId = <<"unconfigured">>,
  PostOpt1 = make_post_opts([{thread_id, <<"new">>}]),
  {ok, Thread1, _Post1} = macaba_thread:new(BoardId, [], PostOpt1),
  ThreadId1 = Thread1#mcb_thread.thread_id,

  PostOpt2 = make_post_opts([{thread_id, ThreadId1}, {email, <<"sage">>}]),
  {ok, Post2} = macaba_post:new(BoardId, PostOpt2),
  ?assertMatch(#mcb_post{}, Post2),

  {ok, [], Threads3} = macaba_board:get_threads(BoardId),
  %% io:format(standard_error, "sage-threads3 ~p~n", [Threads3]),
  ?assert(lists:any(fun(X) -> X =:= Thread1 end, Threads3)),
  ok.

%%%------------------------------------------------------------------------
try_post_with_attach() ->
  BoardId = <<"unconfigured">>,
  Gif1 = base64:decode(
           "R0lGODlhAQABAIABAP///wAAACH5BAEKAAEALAAAAAABAAEAAAICTAEAOw=="),
  PostOpt1 = make_post_opts([{thread_id, <<"new">>}, {attach, [Gif1]}]),
  io:format(standard_error, "+++ try_post_with_attach-1~n", []),
  {Thread1, _Post1} = macaba_thread:new(BoardId, [], PostOpt1),
  io:format(standard_error, "+++ try_post_with_attach-1.1~n", []),
  ThreadId1 = Thread1#mcb_thread.thread_id,

  io:format(standard_error, "+++ try_post_with_attach-2~n", []),
  Gif2 = <<Gif1/binary, "@">>,
  PostOpt2 = make_post_opts([{thread_id, ThreadId1}, {attach, [Gif2]}]),
  {ok, Post2} = macaba_post:new(BoardId, PostOpt2),
  ?assertMatch(#mcb_post{}, Post2),

  io:format(standard_error, "+++ try_post_with_attach-3~n", []),
  {ok, [], Threads3} = macaba_board:get_threads(BoardId),
  %% io:format(standard_error, "attach-threads3 ~p~n", [Threads3]),
  ?assert(lists:any(fun(X) -> X =:= Thread1 end, Threads3)),

  %% io:format(standard_error, "+++ try_post_with_attach-end~n", []),
  ok.

%%%------------------------------------------------------------------------
try_board() ->
  ?assertMatch({error, not_found}, macaba_board:get(<<"random123456">>)),
  %% BoardId = <<"unconfigured">>,
  ok.

%%%------------------------------------------------------------------------
%%% Helpers
%%%------------------------------------------------------------------------

%% @doc Makes default post opts, merges with fields and values in ModifyFields
make_post_opts(ModifyFields) ->
  F = orddict:from_list([ {thread_id,  <<>>}
                        , {author,     <<>>}
                        , {email,      <<>>}
                        , {subject,    <<>>}
                        , {message,    <<"fgsfds">>}
                        , {attach,     []}
                        , {attach_key, <<>>}
                        , {deletepw,   <<"fgsfds">>}
                        ]),
  lists:foldl(fun({ModField, ModValue}, Accum) ->
                  orddict:store(ModField, ModValue, Accum)
              end, F, ModifyFields).

%%% Local Variables:
%%% erlang-indent-level: 2
%%% End:
