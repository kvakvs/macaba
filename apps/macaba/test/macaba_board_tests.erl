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

html_handler_test_() ->
  {setup, fun setup/0, fun teardown/1,
   {foreach, fun foreach_setup/0, fun foreach_teardown/1,
    [ {"Post to a new thread", fun try_new_thread/0}
    , {"Post with sage", fun try_post_with_sage/0}
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
  RP_C = [ {macaba_cluster
            , [ {ping_request_timeout, 1500} % keep alive
                , {rec_timer, [ {value, 200}
                                , {factor, 2}
                                , {incr, 0}
                                , {max_value, 15000}
                              ]}
              ] , [ {macaba_pool_srv1, [ {size, 10}
                                         , {max_overflow, 10}
                                       ],
                     [ {host, "127.0.0.1"}
                       , {port, 8087}
                     ]} ]} ],
  application:set_env(riak_pool, clusters, RP_C),
  application:set_env(riak_pool, default_cluster, macaba_cluster),
  %% macaba_db_riak:start(),
  %% macaba_db_mnesia:start(),
  %% application:start(macaba),
  macaba_sup:start_link(),
  ok.

teardown(ok) ->
  application:stop(gproc),
  application:stop(sasl),
  %% application:stop(macaba),
  ok.

foreach_setup() ->
  ok.

foreach_teardown(ok) ->
  ok.

%%%------------------------------------------------------------------------
%%% Tests
%%%------------------------------------------------------------------------

try_new_thread() ->
  BoardId = <<"unconfigured">>,
  %%----------------------
  %% post a new thread
  %%----------------------
  PostOpt1 = make_post_opts([{thread_id, <<"new">>}]),
  {Thread1, Post1} = macaba_thread:new(BoardId, [], PostOpt1),
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
  {ok, Threads3} = macaba_board:get_threads(BoardId),
  io:format(standard_error, "normal: threads3 ~p~n", [Threads3]),
  ?assert(lists:any(fun(X) -> X =:= Thread1 end, Threads3)),
  ok.

try_post_with_sage() ->
  BoardId = <<"unconfigured">>,
  PostOpt1 = make_post_opts([{thread_id, <<"new">>}]),
  {Thread1, Post1} = macaba_thread:new(BoardId, [], PostOpt1),
  ThreadId1 = Thread1#mcb_thread.thread_id,

  PostOpt2 = make_post_opts([{thread_id, ThreadId1}, {email, <<"sage">>}]),
  {ok, Post2} = macaba_post:new(BoardId, PostOpt2),
  ?assertMatch(#mcb_post{}, Post2),

  {ok, Threads3} = macaba_board:get_threads(BoardId),
  io:format(standard_error, "sage: threads3 ~p~n", [Threads3]),
  ?assert(lists:any(fun(X) -> X =:= Thread1 end, Threads3)),
  ok.

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
                        , {attach,     <<>>}
                        , {attach_key, <<>>}
                        , {deletepw,   <<"fgsfds">>}
                        ]),
  lists:foldl(fun({ModField, ModValue}, Accum) ->
                  orddict:store(ModField, ModValue, Accum)
              end, F, ModifyFields).

%%% Local Variables:
%%% erlang-indent-level: 2
%%% End:
