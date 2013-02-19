%%%------------------------------------------------------------------------
%%% @doc Database layer for using Mnesia as distributed storage
%%% We only use mnesia for operations which require transactions and locking,
%%% that's thread counters. Other data is stored in RIAK (see macaba_db_riak)
%%% Created: 2013-02-19 Dmytro Lytovchenko <kvakvs@yandex.ru>
%%%------------------------------------------------------------------------
-module(macaba_db_mnesia).

-export([ start/0
        , update/3
        ]).

-include_lib("macaba/include/macaba_types.hrl").

%%--------------------------------------------------------------------
%% @doc Prepare database for use
start() ->
  macaba:ensure_started(mnesia),
  create_mem_table(mcb_board_dynamic, record_info(fields, mcb_board_dynamic)).

%%--------------------------------------------------------------------
%% @private
%% @doc Create Mnesia table in memory
create_mem_table(Record, RecInfo) ->
  mnesia:create_table(Record,
                      [ {ram_copies, [node()]}
                      , {index, [2]} % indexing 1st record field
                      , {attributes, RecInfo}
                      ]),
  %%mnesia:add_table_index(mcb_board, field1),
  mnesia:add_table_copy(Record, node(), ram_copies).

%%--------------------------------------------------------------------
-spec read(Type :: macaba_mnesia_object(),
           Key  :: any()) -> orddict:orddict() | tuple() | {error, not_found}.
read(Tab = mcb_board_dynamic, Key) ->
  RFun = fun() -> mnesia:read({Tab, Key}) end,
  {atomic, [Row]} = mnesia:transaction(RFun),
  Row.

%%--------------------------------------------------------------------
%% @doc Start transaction, read, do Fun(Object), write, return new value
update(Tab, Key, Fun) ->
  UF = fun() ->
           [Object] = mnesia:read({Tab, Key}),
           NewObject = Fun(Object),
           mnesia:write(NewObject),
           NewObject
       end,
  mnesia:transaction(UF).

%%% Local Variables:
%%% erlang-indent-level: 2
%%% End:
