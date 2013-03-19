%%%-------------------------------------------------------------------
%%% @doc Attachment backend storing files as RIAK blobs, use this backend
%%% for very large installations with multiple RIAK nodes, it is considered
%%% to be too slow for single-node installs.
%%%
%%% To enable this backend set value of plugins.attachments in macaba.config
%%% to "macaba_attach_riak"
%%%
%%% Created: 2013-03-03
%%% @author Dmytro Lytovchenko <kvakvs@yandex.ru>
%%%-------------------------------------------------------------------
-module(macaba_attach_riak).
-behaviour(gen_macaba_storage).

-export([ start_storage/0
        , stop_storage/1
        , exists/1
        , delete/1
        , read_header/1
        , read_body/1
        , write_header/1
        , write_body/1
        ]).

-include_lib("macaba/include/macaba_types.hrl").
-record(attach_riak_storage_state, {}).

%%%-----------------------------------------------------------------------------
%% @doc Starts storage connection
start_storage() ->
  %% RIAK is already started as main database, so do nothing here
  #attach_riak_storage_state{}.

%%%-----------------------------------------------------------------------------
%% @doc Stops storage connection
stop_storage(#attach_riak_storage_state{}) ->
  ok.

%%%-----------------------------------------------------------------------------
write_header(A=#mcb_attachment{}) ->
  macaba_db_riak:write(mcb_attachment, A).

%%%-----------------------------------------------------------------------------
write_body(B=#mcb_attachment_body{}) ->
  macaba_db_riak:write(mcb_attachment_body, B).

%%%-----------------------------------------------------------------------------
%% @doc Checks if attachment exists in storage
-spec exists(Key :: binary()) -> boolean().
exists(<<>>) -> false;
exists(Digest) ->
  case macaba_db_riak:read(mcb_attachment, Digest) of
    {ok, #mcb_attachment{}} -> true;
    {error, not_found} -> false
  end.


%%%-----------------------------------------------------------------------------
%% @doc Deletes attachment by Id, does not update post which contained it!
-spec delete(A :: binary()) -> ok | {error, not_found}.
delete(AttId) ->
  AttIdHex = bin_to_hex:bin_to_hex(AttId),
  case ?MODULE:read_header(AttId) of
    {error, not_found} ->
      lager:error("board: delete_attachment ~s not found", [AttIdHex]),
      {error, not_found};
    {ok, A = #mcb_attachment{}} ->
      macaba_db_riak:delete(mcb_attachment_body,
                            A#mcb_attachment.thumbnail_hash),
      macaba_db_riak:delete(mcb_attachment_body, AttId),
      macaba_db_riak:delete(mcb_attachment, AttId),
      lager:info("board: delete_attachment ~s", [AttIdHex]),
      ok
  end.

-spec read_header(AttachId :: binary()) ->
                     {ok, #mcb_attachment{}} | {error, not_found}.
read_header(AttachId) ->
  macaba_db_riak:read(mcb_attachment, AttachId).

-spec read_body(AttachId :: binary()) ->
                   {ok, #mcb_attachment{}} | {error, not_found}.
read_body(AttachId) ->
  macaba_db_riak:read(mcb_attachment_body, AttachId).

%%% Local Variables:
%%% erlang-indent-level: 2
%%% End:
