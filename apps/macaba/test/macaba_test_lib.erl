%%%------------------------------------------------------------------------
%%% @doc Testing library + functions for testing HTTP
%%% Created: 2013-02-26 Dmytro Lytovchenko <kvakvs@yandex.ru>
%%%------------------------------------------------------------------------
-module(macaba_test_lib).

-export([ request/3
        , mock_riak/0
        , unmock_riak/0
        ]).

-define(HTTP_TIMEOUT, 5000).

%% @doc Execute a Method request to the URL, Opts: body: body of the request
%% http_options: http options for lhttpc, request_headers: headers for lhttpc
request(Url, Method, Opts) ->
  Body    = macaba:propget(request_body,    Opts, []),
  Options = macaba:propget(http_options,    Opts, []),
  Headers = macaba:propget(request_headers, Opts, []),
  lhttpc_request(Url, Method, Headers, Body, ?HTTP_TIMEOUT, Options).

mock_riak() ->
  meck:new(riak_pool_auto, [passthrough]),
  PutFn = fun(B, K, V) -> erlang:put({riakmock, B, K}, V) end,
  meck:expect(riak_pool_auto, put, PutFn),
  GetFn = fun(B, K) -> erlang:get({riakmock, B, K}) end,
  meck:expect(riak_pool_auto, get, GetFn),
  DeleteFn = fun(B, K) -> erlang:erase({riakmock, B, K}) end,
  meck:expect(riak_pool_auto, delete, GetFn).

unmock_riak() ->
  meck:unload().
