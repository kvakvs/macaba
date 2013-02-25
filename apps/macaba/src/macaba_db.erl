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
        ]).

-include_lib("macaba/include/macaba_types.hrl").

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
get_key_for_object(#mcb_board_dynamic{   board_id  = Id }) -> Id;
get_key_for_object(#mcb_thread{          thread_id = Id }) -> Id;
get_key_for_object(#mcb_thread_dynamic{  thread_id = Id }) -> Id;
get_key_for_object(#mcb_post{            post_id   = Id }) -> Id;
get_key_for_object(#mcb_attachment{      hash      = Id }) -> Id;
get_key_for_object(#mcb_attachment_body{ key       = Id }) -> Id.

%%%-----------------------------------------------------------------------------

