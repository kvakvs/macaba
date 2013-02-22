%%%------------------------------------------------------------------------
%%% @doc Database layer for using Mnesia as distributed storage
%%% We only use mnesia for operations which require transactions and locking,
%%% that's thread counters. Other data is stored in RIAK (see macaba_db_riak)
%%% Created: 2013-02-19 Dmytro Lytovchenko <kvakvs@yandex.ru>
%%%------------------------------------------------------------------------
-module(macaba_db_mnesia).

-export([ start/0
        , read/2
        , update/3
        , write/2
        ]).

-include_lib("macaba/include/macaba_types.hrl").

%%--------------------------------------------------------------------
%% @doc Prepare database for use
start() ->
  lager:info("macaba_db_mnesia: starting"),
  {ok, Nodes} = application:get_env(macaba, cluster),
  CS = mnesia:create_schema(Nodes),
  lager:debug("create schema: ~p", [CS]),
  macaba:ensure_started(mnesia),
  BD = mnesia:create_table(
         mcb_board_dynamic,
         [ {ram_copies, [node()]}
         , {attributes, record_info(fields, mcb_board_dynamic)}
         %% , {index, [#mcb_board_dynamic.board_id]}
         , {type, set}
         ]),
  lager:debug("creating board dynamics table: ~p", [BD]),
  TD = mnesia:create_table(
         mcb_thread_dynamic,
         [ {ram_copies, [node()]}
         , {attributes, record_info(fields, mcb_thread_dynamic)}
         %% , {index, [#mcb_thread_dynamic.thread_id]}
         , {type, set}
         ]),
  lager:debug("creating thread dynamics table: ~p", [TD]).

%%--------------------------------------------------------------------
%% @!private
%% @!doc Create Mnesia table in memory
%% create_mem_table(Record, RecInfo, IndexPositions) ->
%%   {atomic, ok} = mnesia:create_table(Record,
%%                                      [ {ram_copies, [node()]}
%%                                      %% , {index, IndexPositions}
%%                                      , {attributes, RecInfo}
%%                                      ]).
  %%mnesia:add_table_index(mcb_board, field1),
  %%{atomic, ok} = mnesia:add_table_copy(Record, node(), ram_copies).

%%--------------------------------------------------------------------
-spec read(Type :: macaba_mnesia_object(),
           Key  :: any()) -> orddict:orddict() | tuple() | {error, not_found}.
read(Tab, Key) ->
  RFun = fun() -> mnesia:read({Tab, Key}) end,
  case mnesia:transaction(RFun) of
    {atomic, [Row]} -> Row;
    _ -> {error, not_found}
  end.

%%--------------------------------------------------------------------
-spec write(Type :: macaba_mnesia_object(),
            Value :: any()) -> {atomic, any()} | {error, any()}.
write(Tab, Value) ->
  RFun = fun() -> mnesia:write(Value) end,
  mnesia:transaction(RFun).

%%--------------------------------------------------------------------
%% @doc Start transaction, read, do Fun(Object), write, return new value
-spec update(Tab :: atom(), Key :: any(), Fun :: fun()) ->
                {atomic, any()} | {error, any()}.
update(Tab, Key, Fun) ->
  UF = fun() ->
           [Object1] = mnesia:read(Tab, Key, read),
           Object = Fun(Object1),
           mnesia:write(Object),
           Object
       end,
  mnesia:transaction(UF).

%%% Local Variables:
%%% erlang-indent-level: 2
%%% End:
