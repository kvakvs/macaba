%%%------------------------------------------------------------------------
%%% @doc Database layer for using Mnesia as distributed storage
%%% We only use mnesia for operations which require transactions and locking,
%%% that's thread counters. Other data is stored in RIAK (see macaba_db_riak)
%%% @version 2013-02-19
%%% @author Dmytro Lytovchenko <kvakvs@yandex.ru>
%%%------------------------------------------------------------------------
-module(macaba_db_riak).

-export([ bucket_for/1
        , start/0
        , read/2
        , read_riakobj/2
        , write/2
        , delete/2
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

bucket_for(mcb_site_config)     -> <<"site-conf">>;
bucket_for(mcb_board_dynamic)   -> <<"board-d">>;  % also in _db_mnesia
bucket_for(mcb_thread)          -> <<"thrd">>;
bucket_for(mcb_thread_dynamic)  -> <<"thrd-d">>; % also in _db_mnesia
bucket_for(mcb_post)            -> <<"post">>;
bucket_for(mcb_attachment)      -> <<"att">>;
bucket_for(mcb_attachment_body) -> <<"att-body">>.

%%--------------------------------------------------------------------
%% @doc Reads a single {bucket,key} from RIAK database. No conflict resolution
%% or merging is performed, no vclock preserved
-spec read(Type :: macaba_riak_object(),
           Key  :: binary()) ->
              {ok, macaba_riak_record()} | {error, not_found}.

read(Type, Key) when is_binary(Key) ->
    read_internal(Type, bucket_for(Type), Key).

%%--------------------------------------------------------------------
%% @private
%% @doc Read object from database, apply versioning to see if it needs upgrade
%% binary file contents return without versioning as is. No conflict resolution
%% or merging is performed here.
-spec read_internal(Type :: macaba_riak_object(),
                    B :: binary(), K :: binary()) ->
                       {ok, macaba_riak_record()} | {error, not_found}.
read_internal(Type, B, K) ->
  %% versioning on read for all other objects
  case riak_pool_auto:get(B, K) of
    {error, notfound} ->
      %%lager:error("riak read_i not found ~p key=~p", [Type, K]),
      {error, not_found};
    {ok, O} ->
      X1 = riakc_obj:get_value(O),
      %%lager:debug("riak read_i found ~p key=~p", [Type, K]),
      {Version, Value} = binary_to_term(X1, [safe]),
      X2 = macaba_db:upgrade(Type, Version, Value),
      {ok, X2}
  end.
%%--------------------------------------------------------------------
%% @doc Reads a single {bucket,key} from RIAK database as Riak Object.
%% Performs the inplace data upgrade and returns whole Riak object with metadata
%% and vector clocks. Reader is suggested to perform merge on conflict
-spec read_riakobj(Type :: macaba_riak_object(),
                   Key  :: binary()) -> riakc_obj() | {error, not_found}.

read_riakobj(Type, Key) when is_binary(Key) ->
  read_riakobj_internal(Type, bucket_for(Type), Key).

%%--------------------------------------------------------------------
%% @private
-spec read_riakobj_internal(Type :: macaba_riak_object(),
                            B :: binary(),
                            K :: binary()) ->
                               {ok, riakc_obj()} | {error, not_found}.

read_riakobj_internal(Type, B, K) ->
  %% versioning on read for all other objects
  case riak_pool_auto:get(B, K) of
    {error, notfound} ->
      {error, not_found};
    {ok, RObject} ->
      VList0 = riakc_obj:get_values(RObject),
      VList = lists:map(fun(X) -> binary_to_term(X, [safe]) end, VList0),
      NewVList = [macaba_db:upgrade(Type, Version, Value)
                  || {Version, Value} <- VList],
      %% Now if we got multiple values, try to resolve conflict and merge
      {ok, resolve_conflict(NewVList)}
  end.

%%--------------------------------------------------------------------
%% @doc Take list of objects and try to merge conflicting changes
resolve_conflict([First|_]) -> First.

%%--------------------------------------------------------------------
-spec write(Type :: macaba_riak_object(),
            Value :: macaba_riak_record()) -> ok | {error, any()}.

write(Type, Value) ->
  Key = macaba_db:get_key_for_object(Value),
  write_internal(Type, bucket_for(Type), Key, Value).
  %% R = write_internal(Type, bucket_for(Type), Key, Value),

%%--------------------------------------------------------------------
%% @private
write_internal(Type, B, K, Value) when is_binary(B), is_binary(K) ->
  %%------------------------------------
  %% TODO: vector clocks and shit
  %%------------------------------------
  Bin = macaba_db:encode(Type, Value),
  Obj = riakc_obj:new(B, K, Bin),
  R = riak_pool_auto:put(Obj),
  %%lager:debug("riak write_i ~p key=~p result=~p value ~p", [Type, K, R, Value]).
  R.

%%--------------------------------------------------------------------
delete(Type, Value) when is_tuple(Value) ->
  riak_pool_auto:delete(bucket_for(Type),
                        macaba_db:get_key_for_object(Value));
delete(Type, K) when is_binary(K) ->
  riak_pool_auto:delete(bucket_for(Type), K).

%%riak_pool_auto::put_raw(Pid, riakc_obj:new_obj(Bucket, Key, Vclock, Contents))

%%% Local Variables:
%%% erlang-indent-level: 2
%%% End:
