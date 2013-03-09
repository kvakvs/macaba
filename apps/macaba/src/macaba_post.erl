%%%------------------------------------------------------------------------
%%% @doc Post data model, represents thread posts and actions with posts
%%% Created: 2013-03-09 Dmytro Lytovchenko <kvakvs@yandex.ru>
%%%------------------------------------------------------------------------
-module(macaba_post).

-export([ construct/2
        , write_attach_set_ids/2

        , get/2
        , new/2
        , delete/2
        , delete_attach/2
        , delete_dirty/2
        ]).

-include_lib("macaba/include/macaba_types.hrl").

%%%-----------------------------------------------------------------------------
%% @doc Creates structure for a new post, returns it. Does not write.
-spec construct(BoardId :: binary(), Opts :: orddict:orddict()) ->
                   {ok, #mcb_post{}} | {error, any()}.
construct(BoardId, Opts) when is_binary(BoardId) ->
  ThreadId  = macaba:propget(thread_id, Opts),
  Author    = macaba:propget(author,    Opts),
  Email     = macaba:propget(email,     Opts),
  Subject   = macaba:propget(subject,   Opts),
  Message   = macaba:propget(message,   Opts),
  DeletePw  = macaba:propget(deletepw,  Opts),

  case string:strip(binary_to_list(Message), both) of
    [] -> {error, body_empty};
    _ ->
      %% if this crashes, don't create anything and fail here
      MessageProcessed = macaba_plugins:call(markup, [Message]),

      PostId = macaba:as_binary(macaba_board:next_post_id(BoardId)),
      P = #mcb_post{
        thread_id   = macaba:as_binary(ThreadId)
        , post_id     = PostId
        , board_id    = BoardId
        , subject     = Subject
        , author      = Author
        , email       = Email
        , message_raw = Message
        , message     = MessageProcessed
        , created     = get_now_utc()
        , delete_pass = DeletePw
       },
      {ok, P}
  end.

%%%-----------------------------------------------------------------------------
%% @doc Writes attachments from Opts to database and updates post rec
write_attach_set_ids(P, Opts) ->
  %% Write attach and set attach_id in post
  %% TODO: Multiple attachments
  Attach    = macaba:propget(attach,     Opts),
  AttachKey = macaba:propget(attach_key, Opts),
  case Attach of
    <<>> ->
      P;
    _ ->
      case macaba_attach:write(AttachKey, Attach) of
        {error, _}=Err ->
          lager:error("board: write attach: ~p", [Err]),
          P;
        {ok, AttachId} ->
          P#mcb_post{ attach_ids = [AttachId] }
      end
  end.

%%%-----------------------------------------------------------------------------
%% @doc Loads post info
-spec get(BoardId :: binary(), PostId :: binary()) ->
             [#mcb_post{}] | {error, not_found}.
get(BoardId, PostId) when is_binary(BoardId), is_binary(PostId) ->
  macaba_db_riak:read(mcb_post, macaba_db:key_for(mcb_post, {BoardId, PostId})).


%%%-----------------------------------------------------------------------------
%% @doc Creates new post, writes to database
-spec new(BoardId :: binary(), Opts :: orddict:orddict()) ->
             {ok, #mcb_post{}} | {error, any()}.
new(BoardId, Opts) when is_binary(BoardId) ->
  case construct(BoardId, Opts) of
    {ok, Post0} ->
      Post  = macaba_post:write_attach_set_ids(Post0, Opts),
      ThreadId = macaba:propget(thread_id, Opts),
      macaba_thread:add_post(BoardId, ThreadId, Post0),
      {ok, Post};
    {error, E} ->
      {error, E}
  end.

%%%-----------------------------------------------------------------------------
-spec delete_attach(BoardId :: binary(),
                         PostId :: binary()) -> ok | {error, any()}.
delete_attach(BoardId, PostId) ->
  P = macaba_post:get(BoardId, PostId),
  AttachMod = macaba_plugins:mod(attachments),
  lists:foreach(fun(AttId) -> AttachMod:delete(AttId) end,
                P#mcb_post.attach_ids),
  P2 = P#mcb_post{ attach_ids=[], attach_deleted=true },
  macaba_db_riak:write(mcb_post, P2).

%%%-----------------------------------------------------------------------------
-spec delete(BoardId :: binary(),
             PostId :: binary()) -> ok | {error, any()}.
delete(BoardId, PostId) ->
  P = ?MODULE:get(BoardId, PostId),
  Upd = fun(TD = #mcb_thread_dynamic{ post_ids=L }) ->
            L2 = lists:delete(PostId, L),
            %% if thread empty, send message to worker to delete thread
            case L2 of
              [] -> macaba_board_worker:thread_delete(
                      BoardId, P#mcb_post.thread_id);
              _ -> ok
            end,
            TD#mcb_thread_dynamic{ post_ids=L2 }
        end,
  TDKey = macaba_db:key_for(
            mcb_thread_dynamic, {BoardId, P#mcb_post.thread_id}),
  case macaba_db_mnesia:update(mcb_thread_dynamic, TDKey, Upd) of
    {atomic, _} ->
      ?MODULE:delete_dirty(BoardId, PostId);
    E ->
      lager:error("post: delete: dynamic update error ~p", [E]),
      {error, E}
  end.

%%%-----------------------------------------------------------------------------
%% @doc Deletes post and attached files without updating thread_dynamic
-spec delete_dirty(BoardId :: binary(),
                   PostId :: binary()) -> ok | {error, not_found}.
delete_dirty(BoardId, PostId) ->
  PKey = macaba_db:key_for(mcb_post, {BoardId, PostId}),
  case ?MODULE:get(BoardId, PostId) of
    {error, not_found} ->
      lager:error("post: delete B=~s P=~s not found", [BoardId, PostId]),
      {error, not_found};
    P = #mcb_post{} ->
      AttachMod = macaba_plugins:mod(attachments),
      lists:foreach(fun(AttId) -> AttachMod:delete(AttId) end,
                    P#mcb_post.attach_ids),
      macaba_db_riak:delete(mcb_post, PKey),
      lager:info("post: delete B=~s P=~s", [BoardId, PostId]),
      ok
  end.

%%%-----------------------------------------------------------------------------
%% @private
%% @doc Gets unix time for now
-define(SECONDS_1970, 1970*365*24*3600).
get_now_utc() ->
  {UTCD, UTCT} = calendar:universal_time(),
  calendar:datetime_to_gregorian_seconds({UTCD, UTCT}) - ?SECONDS_1970.

%%% Local Variables:
%%% erlang-indent-level: 2
%%% End:
