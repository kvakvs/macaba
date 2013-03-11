%%%------------------------------------------------------------------------
%%% @doc Database tools for Macaba board, provides code for encoding, decoding
%%% and versioning (upgrading) data pieces
%%% Created: 2013-02-19 Dmytro Lytovchenko <kvakvs@yandex.ru>
%%%------------------------------------------------------------------------
-module(macaba_db).

-export([ upgrade/3
        , decode/2
        , encode/2
        , current_version_for/1
        , get_key_for_object/1
        , key_for/2
        , reset_all_data/0
        ]).

-include_lib("macaba/include/macaba_types.hrl").

%% @doc You would not want to call this in production, no really
reset_all_data() ->
  mnesia:clear_table(mcb_board_dynamic),
  mnesia:clear_table(mcb_thread_dynamic),
  OTypes = [ mcb_site_config, mcb_board_dynamic, mcb_thread_dynamic
           , mcb_thread, mcb_post, mcb_attachment, mcb_attachment_body],
  lists:foreach(fun(T) -> reset_all_data_riak(T) end, OTypes),
  %% create board dynamic for default board
  lists:foreach(fun(Board) ->
                  BD = #mcb_board_dynamic{board_id = Board#mcb_board.board_id},
                  macaba_db_mnesia:write(mcb_board_dynamic, BD)
                end, macaba_board:get_boards()).

%% @private
%% @doc Deletes all RIAK records for given object type
reset_all_data_riak(ObjType) ->
  Bucket = macaba_db_riak:bucket_for(ObjType),
  {ok, Keys} = riak_pool_auto:list_keys(Bucket),
  lists:foreach(fun(K) ->
                    ok = riak_pool_auto:delete(Bucket, K)
                end, Keys).

%%%-----------------------------------------------------------------------------
%% @doc Stub for data upgrade function, to support multiple versions
%% of the same data. On successful upgrade data is written back too!
-spec upgrade(Type :: macaba_riak_object(), Ver :: integer(), any()) -> any().

upgrade(mcb_site_config,     ?MCB_SITE_CONFIG_VER,     X) -> X;
upgrade(mcb_board_dynamic,   ?MCB_BOARD_DYNAMIC_VER,   X) -> X;
upgrade(mcb_thread,          ?MCB_THREAD_VER,          X) -> X;
upgrade(mcb_thread_dynamic,  ?MCB_THREAD_DYNAMIC_VER,  X) -> X;
upgrade(mcb_post,            ?MCB_POST_VER,            X) -> X;
upgrade(mcb_attachment,      ?MCB_ATTACHMENT_VER,      X) -> X;
upgrade(mcb_attachment_body, ?MCB_ATTACHMENT_BODY_VER, X) -> X.

%%%-----------------------------------------------------------------------------
%% @doc Decodes database object, as a tuple of {version, binaryencoded}
%% if version is too low, the object is filtered through upgrade/3
-spec decode(T :: macaba_riak_object(), Bin :: binary()) -> tuple().

decode(T, Bin) ->
  {Version, Value} = binary_to_term(Bin, [safe]),
  upgrade(T, Version, Value).

%%%-----------------------------------------------------------------------------
%% @doc Encodes database object with current version. ON READ if version is too
%% low, its gets filtered through upgrade/3
-spec encode(T :: macaba_riak_object(), P :: any()) -> binary().

encode(T, P) -> term_to_binary( {current_version_for(T), P} ).

%%%-----------------------------------------------------------------------------
%% @doc Version for newly created riak object
-spec current_version_for(macaba_riak_object()) -> integer().

current_version_for(mcb_site_config)     -> ?MCB_SITE_CONFIG_VER;
current_version_for(mcb_board_dynamic)   -> ?MCB_BOARD_DYNAMIC_VER;
current_version_for(mcb_thread)          -> ?MCB_THREAD_VER;
current_version_for(mcb_thread_dynamic)  -> ?MCB_THREAD_DYNAMIC_VER;
current_version_for(mcb_post)            -> ?MCB_POST_VER;
current_version_for(mcb_attachment)      -> ?MCB_ATTACHMENT_VER;
current_version_for(mcb_attachment_body) -> ?MCB_ATTACHMENT_BODY_VER.

%%--------------------------------------------------------------------
%% @doc Extracts key from object
get_key_for_object(#mcb_thread{ thread_id=TId, board_id=BId }) ->
  key_for(mcb_thread, {BId, TId});
get_key_for_object(#mcb_post{ post_id=PId, board_id=BId }) ->
  key_for(mcb_post, {BId, PId});
get_key_for_object(#mcb_thread_dynamic{ board_id=BId, thread_id=TId }) ->
  key_for(mcb_thread_dynamic, {BId, TId});
get_key_for_object(#mcb_board_dynamic{   board_id  = Id }) -> Id;
get_key_for_object(#mcb_attachment{      hash      = Id }) -> Id;
get_key_for_object(#mcb_attachment_body{ key       = Id }) -> Id.

%%%-----------------------------------------------------------------------------
%% @doc Creates complex key
key_for(mcb_thread, {BId, TId}) ->
  << "B=", BId/binary, ":T=", TId/binary >>;
key_for(mcb_post,   {BId, PId}) ->
  << "B=", BId/binary, ":P=", PId/binary >>;
key_for(mcb_thread_dynamic, {BId, TId}) ->
  << "B=", BId/binary, ":T=", TId/binary >>;
key_for(T, K) ->
  lager:error("key_for T=~p K=~p unknown type, ~p",
              [T, K, erlang:get_stacktrace()]),
  erlang:error({error, badarg}).

%%%-----------------------------------------------------------------------------

%%% Local Variables:
%%% erlang-indent-level: 2
%%% End:
