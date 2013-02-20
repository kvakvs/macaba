%%%------------------------------------------------------------------------
%%% @doc Database layer for using Mnesia as distributed storage
%%% We only use mnesia for operations which require transactions and locking,
%%% that's thread counters. Other data is stored in RIAK (see macaba_db_riak)
%%% Created: 2013-02-19 Dmytro Lytovchenko <kvakvs@yandex.ru>
%%%------------------------------------------------------------------------
-module(macaba_db_riak).

-export([ start/0
        , read/2
        , read_riakobj/2
        , write/2
        ]).

-include_lib("macaba/include/macaba_types.hrl").
-include_lib("riakc/include/riakc.hrl").

%%--------------------------------------------------------------------
%% @doc Prepare database for use
start() ->
  macaba:ensure_started(riak_pool).

%%--------------------------------------------------------------------
%% @private
-spec bucket_for(macaba_riak_object()) -> binary().

bucket_for(mcb_site_config)    -> <<"board-conf">>;
bucket_for(mcb_board_dynamic)   -> <<"board-count">>;
bucket_for(mcb_thread)          -> <<"thread-head">>;
bucket_for(mcb_post)            -> <<"post">>;
bucket_for(mcb_attachment)      -> <<"attach-head">>;
bucket_for(mcb_attachment_body) -> <<"attach-body">>.

%%--------------------------------------------------------------------
%% @doc Reads a single {bucket,key} from RIAK database. No conflict resolution
%% or merging is performed, no vclock preserved
-spec read(Type :: macaba_riak_object(),
           Key  :: any()) ->
              tuple() | binary() | {error, not_found}.

read(Type, Key) when is_binary(Key) ->
    read_internal(Type, bucket_for(Type), Key).

%%--------------------------------------------------------------------
%% @private
%% @doc Read object from database, apply versioning to see if it needs upgrade
%% binary file contents return without versioning as is. No conflict resolution
%% or merging is performed here.
-spec read_internal(Type :: macaba_riak_object(),
                    B :: binary(),
                    K :: binary()) -> tuple() | binary().

read_internal(mcb_attachment_body, B, K) ->
  %% attachment body - no versioning, return as binary
  case riak_pool_auto:get(B, K) of
    {error, notfound} ->
      {error, not_found};
    {ok, O} ->
      riakc_obj:get_value(O)
  end;

read_internal(Type, B, K) ->
  %% versioning on read for all other objects
  case riak_pool_auto:get(B, K) of
    {error, notfound} ->
      {error, not_found};
    {ok, O} ->
      X = riakc_obj:get_value(O),
      {Version, Value} = binary_to_term(X, [safe]),
      macaba_db:upgrade(Type, Version, Value)
  end.
%%--------------------------------------------------------------------
%% @doc Reads a single {bucket,key} from RIAK database as Riak Object.
%% Performs the inplace data upgrade and returns whole Riak object with metadata
%% and vector clocks. Reader is suggested to perform merge on conflict
-spec read_riakobj(Type :: macaba_riak_object(),
                   Key  :: any()) ->
                      riakc_obj() | {error, not_found}.

read_riakobj(Type, Key) when is_binary(Key) ->
  read_riakobj_internal(Type, bucket_for(Type), Key).

%%--------------------------------------------------------------------
%% @private
-spec read_riakobj_internal(Type :: macaba_riak_object(),
                            B :: binary(),
                            K :: binary()) -> riakc_obj().

read_riakobj_internal(Type, B, K) ->
  %% versioning on read for all other objects
  case riak_pool_auto:get(B, K) of
    {error, notfound} ->
      {error, not_found};
    {ok, RObject} ->
      X = riakc_obj:get_value(RObject),
      {Version, Value} = binary_to_term(X, [safe]),
      NewValue = macaba_db:upgrade(Type, Version, Value),
      riakc_obj:update_value(RObject, NewValue)
  end.

%%--------------------------------------------------------------------
get_key_for_object(      #mcb_post{ post_id   = Id }) -> Id;
get_key_for_object(    #mcb_thread{ thread_id = Id }) -> Id;
get_key_for_object(#mcb_attachment{ attach_id = Id }) -> Id.

%%--------------------------------------------------------------------
-spec write(Type :: macaba_riak_object(), Value :: any()) ->
               ok | {error, any()}.

write(Type, Value) ->
  Key = get_key_for_object(Value),
  write_internal(Type, bucket_for(Type), Key, Value).

%%--------------------------------------------------------------------
write_internal(Type, B, K, Value) ->
  %%------------------------------------
  %% TODO: vector clocks and shit
  %%------------------------------------
  Bin = macaba_db:encode(Type, Value),
  riak_pool_auto:put(B, K, Bin).

%%riak_pool_auto::put_raw(Pid, riakc_obj:new_obj(Bucket, Key, Vclock, Contents))

%%% Local Variables:
%%% erlang-indent-level: 2
%%% End:
