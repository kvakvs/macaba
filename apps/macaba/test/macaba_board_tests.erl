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
  PostOpt = orddict:from_list([ {thread_id,  <<"new">>}
                              , {author,     <<>>}
                              , {email,      <<>>}
                              , {subject,    <<>>}
                              , {message,    <<"fgsfds">>}
                              , {attach,     <<>>}
                              , {attach_key, <<>>}
                              , {deletepw,   <<"fgsfds">>}
                              ]),
  BoardId = <<"undefined">>,
  {ok, Post} = macaba_post:new(BoardId, PostOpt),
  ?assertMatch(#mcb_post{}, Post),
  %% %% new thread has same id as first post
  %% T = macaba_thread:get(BoardId, Post#mcb_post.post_id),
  %% ?assertMatch(#mcb_thread{}, T),
  ok.

%%% Local Variables:
%%% erlang-indent-level: 2
%%% End:
