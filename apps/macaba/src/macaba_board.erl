%%%------------------------------------------------------------------------
%%% @doc Board data model, is used by board client (macaba_board_cli) and then
%%% formatted to HTML or JSON etc
%%% Created: 2013-02-17 Dmytro Lytovchenko <kvakvs@yandex.ru>
%%%------------------------------------------------------------------------
-module(macaba_board).

-export([ construct_post/2
        , get_board/1
        , get_boards/0
        , get_thread/2
        , get_threads/1
        , get_thread_contents/3
        , get_thread_dynamic/2
        , get_post/2
        , new_post/2
        , new_thread/3
        , start/0
        , load_board_dynamics/0
        , detect_content_type/1
        , delete_thread/2
        , delete_post/2
        , delete_post_attach/2
        , delete_post_dirty/2
        ]).

-include_lib("macaba/include/macaba_types.hrl").

%%%-----------------------------------------------------------------------------
start() -> ok.

%%%-----------------------------------------------------------------------------
%% @doc May be SLOW! Enumerates RIAK keys in board bucket, and calculates thread
%% lists for boards. Do this only on one node of the macaba cluster.
%% This is called from macaba_masternode:handle_leader_call after startup been
%% finished, call initiated by macaba_startup temporary module
load_board_dynamics() ->
  lager:info("[load_board_dynamics] enumerating threads and caching..."),
  %% TODO: if record in Mnesia exists, we have this job on >1 node, fatal!
  Boards = get_boards(),
  update_dynamics_for_board(Boards).

%% @private
%% @doc Reloads saved dynamics for each board from RIAK on startup
update_dynamics_for_board([]) -> ok;
update_dynamics_for_board([B = #mcb_board{} | Boards]) ->
  BoardId = B#mcb_board.board_id,
  BD = case macaba_db_riak:read(mcb_board_dynamic, BoardId) of
         {error, not_found} -> #mcb_board_dynamic{board_id = BoardId};
         Value -> Value
       end,
  lager:debug("{{dbinit}} upd_dyn_b bd=~p", [BD]),
  %%T = fun() -> mnesia:write(mcb_board_dynamic, BD, write) end,
  %%{atomic, _} = mnesia:transaction(T),
  macaba_db_mnesia:write(mcb_board_dynamic, BD),
  update_dynamics_for_threads(BoardId, BD#mcb_board_dynamic.threads),
  update_dynamics_for_board(Boards).

%% @private
update_dynamics_for_threads(_BoardId, []) -> ok;
update_dynamics_for_threads(BoardId, [ThreadId | Threads])
  when is_binary(ThreadId) ->
  TD = get_thread_dynamic(BoardId, ThreadId),
  lager:debug("{{dbinit}} upd_dyn_t td=~p", [TD]),
  %% T = fun() -> mnesia:write(mcb_thread_dynamic, TD, write) end,
  %% {atomic, _} = mnesia:transaction(T),
  macaba_db_mnesia:write(mcb_thread_dynamic, TD),
  update_dynamics_for_threads(BoardId, Threads).

%%%-----------------------------------------------------------------------------
delete_thread(BoardId, ThreadId) ->
  lager:info("board: delete_thread B=~s T=~s", [BoardId, ThreadId]),
  BUpd = fun(BD = #mcb_board_dynamic{ threads=T }) ->
            T2 = lists:delete(ThreadId, T),
            BD#mcb_board_dynamic{ threads=T2 }
        end,
  {atomic, _} = macaba_db_mnesia:update(mcb_board_dynamic, BoardId, BUpd),

  TDKey = macaba_db:key_for(mcb_thread_dynamic, {BoardId, ThreadId}),
  TD = macaba_db_mnesia:read(mcb_thread_dynamic, TDKey),
  lists:foreach(fun(P) -> delete_post_dirty(BoardId, P) end,
                TD#mcb_thread_dynamic.post_ids),

  TKey = macaba_db:key_for(mcb_thread, {BoardId, ThreadId}),
  macaba_db_riak:delete(mcb_thread, TKey),

  %% mnesia delete will also delete in riak but 1 sec later, delete now
  macaba_db_riak:delete(mcb_thread_dynamic, TDKey),
  macaba_db_mnesia:delete(mcb_thread_dynamic, TDKey).

%%%-----------------------------------------------------------------------------
-spec delete_post_attach(BoardId :: binary(),
                         PostId :: binary()) -> ok | {error, any()}.
delete_post_attach(BoardId, PostId) ->
  P = get_post(BoardId, PostId),
  AttachMod = macaba_plugins:mod(attachments),
  lists:foreach(fun(AttId) -> AttachMod:delete(AttId) end,
                P#mcb_post.attach_ids),
  P2 = P#mcb_post{ attach_ids=[], attach_deleted=true },
  macaba_db_riak:write(mcb_post, P2).

%%%-----------------------------------------------------------------------------
-spec delete_post(BoardId :: binary(),
                  PostId :: binary()) -> ok | {error, any()}.
delete_post(BoardId, PostId) ->
  P = get_post(BoardId, PostId),
  Upd = fun(TD = #mcb_thread_dynamic{ post_ids=L }) ->
            L2 = lists:delete(PostId, L),
            %% if thread empty, send message to worker to delete thread
            case L2 of
              [] -> macaba_board_worker:delete_thread(
                      BoardId, P#mcb_post.thread_id);
              _ -> ok
            end,
            TD#mcb_thread_dynamic{ post_ids=L2 }
        end,
  TDKey = macaba_db:key_for(mcb_thread_dynamic,
                            {BoardId, P#mcb_post.thread_id}),
  case macaba_db_mnesia:update(mcb_thread_dynamic, TDKey, Upd) of
    {atomic, _} ->
      delete_post_dirty(BoardId, PostId);
    E ->
      lager:error("board: delete_post dynamic update error ~p", [E]),
      {error, E}
  end.

%%%-----------------------------------------------------------------------------
%% @doc Deletes post and attached files without updating thread_dynamic
-spec delete_post_dirty(BoardId :: binary(),
                        PostId :: binary()) -> ok | {error, not_found}.
delete_post_dirty(BoardId, PostId) ->
  PKey = macaba_db:key_for(mcb_post, {BoardId, PostId}),
  case get_post(BoardId, PostId) of
    {error, not_found} ->
      lager:error("board: delete_post B=~s P=~s not found", [BoardId, PostId]),
      {error, not_found};
    P = #mcb_post{} ->
      AttachMod = macaba_plugins:mod(attachments),
      lists:foreach(fun(AttId) -> AttachMod:delete(AttId) end,
                    P#mcb_post.attach_ids),
      macaba_db_riak:delete(mcb_post, PKey),
      lager:info("board: delete_post B=~s P=~s", [BoardId, PostId]),
      ok
  end.

%%%-----------------------------------------------------------------------------
get_thread_dynamic(BoardId, ThreadId)
  when is_binary(BoardId), is_binary(ThreadId) ->

  case macaba_db_riak:read(
         mcb_thread_dynamic,
         macaba_db:key_for(mcb_thread_dynamic, {BoardId, ThreadId})) of
    {error, not_found} ->
      #mcb_thread_dynamic{
        internal_mnesia_key = macaba_db:key_for(
                                mcb_thread_dynamic, {BoardId, ThreadId}),
        board_id = BoardId,
        thread_id = ThreadId
      };
    Value -> Value
  end.

%%%-----------------------------------------------------------------------------
%% @doc Returns list of configured boards
-spec get_boards() -> [#mcb_board{}].
get_boards() ->
  case macaba_db_riak:read(mcb_site_config, <<"default">>) of
    #mcb_site_config{ boards=B } -> B;
    {error, not_found} -> fake_default_boards()
  end.

%%%-----------------------------------------------------------------------------
%% @doc Returns list of configured boards (TODO: cache in memory?)
-spec get_board(BoardId :: binary()) -> #mcb_board{} | {error, not_found}.
get_board(BoardId) ->
  case lists:keysearch(BoardId, #mcb_board.board_id, get_boards()) of
    {value, X} -> X;
    false -> {error, not_found}
  end.

%%%-----------------------------------------------------------------------------
%% @private
fake_default_boards() ->
  {ok, DefaultAnon} = macaba_conf:get([<<"board">>,
                                       <<"default_anonymous_name">>]),
  [#mcb_board{
        board_id       = <<"unconfigured">>
      , short_name     = <<"default_board">>
      , category       = <<"no_category">>
      , title          = "Board not configured"
      , anonymous_name = DefaultAnon
      , max_threads    = 20 * 10
     }].

%%%-----------------------------------------------------------------------------
%% @doc Returns list of threads in board (only info headers, no contents!), also
%% a proplist with board contents (first post and X last posts - configurable)
-spec get_threads(BoardId :: binary()) ->
                     {[#mcb_thread{}], [{binary(), [#mcb_post{}]}]}.
get_threads(BoardId) when is_binary(BoardId) ->
  Ids = case macaba_db_mnesia:read(mcb_board_dynamic, BoardId) of
          {error, not_found} -> [];
          #mcb_board_dynamic{threads=T} -> T
        end,
  Threads0 = [begin
                TKey = macaba_db:key_for(mcb_thread, {BoardId, T}),
                case macaba_db_riak:read(mcb_thread, TKey) of
                  #mcb_thread{}=Value -> Value;
                  {error, _} -> []
                end
              end || T <- Ids],
  Threads = lists:flatten(Threads0),
  Threads.

%%%-----------------------------------------------------------------------------
%% @doc Thread is identified by board name and number
get_thread(BoardId, ThreadId)
  when is_binary(BoardId), is_binary(ThreadId) ->

  K = macaba_db:key_for(mcb_thread, {BoardId, ThreadId}),
  case macaba_db_riak:read(mcb_thread, K) of
    #mcb_thread{}=Value -> Value;
    {error, _} -> {error, not_found}
  end.

%%%-----------------------------------------------------------------------------
%% @doc By thread id reads thread dynamic, to get ids of posts, loads first and
%% configured amount of last posts into a proplist. Give 'all' for LastCount to
%% load whole thread
-spec get_thread_contents(BoardId :: binary(),
                          ThreadId :: binary(),
                          LastCount0 :: integer() | 'all') -> [#mcb_post{}].

get_thread_contents(BoardId, ThreadId, LastCount0)
  when is_binary(BoardId), is_binary(ThreadId) ->

  TD = case macaba_db_mnesia:read(
              mcb_thread_dynamic,
              macaba_db:key_for(mcb_thread_dynamic, {BoardId, ThreadId})) of
         {error, not_found} -> #mcb_thread_dynamic{};
         D -> D
       end,
  PostIds = TD#mcb_thread_dynamic.post_ids,

  %% if lastcount was set to 'all' - change it to thread length
  LastCount1 = case LastCount0 of
                 all -> length(PostIds);
                 _ -> LastCount0
               end,
  LastCount = min(LastCount1, length(PostIds)),

  %% get first and cut last
  First = case PostIds of [] -> []; [F|_] -> get_post(BoardId, F) end,
  %% FIXME: this may run slow on large threads >1000 posts?
  LastIds = case PostIds of
               [] -> [];
               _ ->
                 PostIds2 = tl(PostIds),
                T = min(length(PostIds2), max(0, length(PostIds2) - LastCount)),
                lists:nthtail(T, PostIds2)
            end,
  Last = lists:map(fun(Id) -> get_post(BoardId, Id) end, LastIds),
  lists:flatten([First | Last]).

%%%-----------------------------------------------------------------------------
%% @doc Loads post info
-spec get_post(BoardId :: binary(), PostId :: binary()) ->
                  [#mcb_post{}] | {error, not_found}.
get_post(BoardId, PostId) when is_binary(BoardId), is_binary(PostId) ->
  macaba_db_riak:read(mcb_post, macaba_db:key_for(mcb_post, {BoardId, PostId})).

%%%-----------------------------------------------------------------------------
%% @doc Creates a new thread with a single post, thread_id is set to the first
%% post id. Writes both thread and post to database.
new_thread(BoardId, ThreadOpts, PostOpts) when is_binary(BoardId) ->
  Post0    = construct_post(BoardId, PostOpts),
  PostId   = Post0#mcb_post.post_id,

  Hidden   = macaba:propget(hidden,    ThreadOpts, false),
  Pinned   = macaba:propget(pinned,    ThreadOpts, false),
  ReadOnly = macaba:propget(read_only, ThreadOpts, false),
  ThreadId = PostId,

  Thread = #mcb_thread{
      thread_id = ThreadId
    , board_id  = BoardId
    , hidden    = Hidden
    , pinned    = Pinned
    , read_only = ReadOnly
   },
  ThreadDyn = #mcb_thread_dynamic{
      internal_mnesia_key = macaba_db:key_for(
                              mcb_thread_dynamic, {BoardId, ThreadId})
    , thread_id = ThreadId
    , board_id  = BoardId
    , post_ids  = [PostId]
   },
  %% TDKey = macaba_db:key_for(mcb_thread_dynamic, {BoardId, PostId}),
  macaba_db_mnesia:write(mcb_thread_dynamic, ThreadDyn),
  Post1 = post_write_attach_set_ids(Post0, PostOpts),

  %% link post to thread
  Post = Post1#mcb_post{ thread_id = PostId },
  macaba_db_riak:write(mcb_post, Post),
  macaba_db_riak:write(mcb_thread, Thread),

  %% add thread to board
  F = fun(BD = #mcb_board_dynamic{ threads=T }) ->
          T2 = [ThreadId | T],
          BD#mcb_board_dynamic{ threads = T2}
      end,
  {atomic, _NewD} = macaba_db_mnesia:update(mcb_board_dynamic, BoardId, F),
  check_board_threads_limit(BoardId),
  {Thread, Post}.

%%%-----------------------------------------------------------------------------
%% @private
%% @doc Reads board info and cuts extra threads in the end according to board
%% settings.
check_board_threads_limit(BoardId) ->
  Board = get_board(BoardId),
  F = fun(BD = #mcb_board_dynamic{ threads=T }) ->
          Cut = min(Board#mcb_board.max_threads, length(T)),
          {T2, Delete} = lists:split(Cut, T),
          %% send messages to delete sunken threads
          [macaba_board_worker:delete_thread(BoardId, ThreadId)
           || ThreadId <- Delete],
          BD#mcb_board_dynamic{ threads = T2 }
      end,
  case macaba_db_mnesia:update(mcb_board_dynamic, BoardId, F) of
    {atomic, _} ->
      ok;
    Err ->
      lager:error("board: thread limits check error ~p", [Err])
  end.

%%%-----------------------------------------------------------------------------
%% @private
post_write_attach_set_ids(P, Opts) ->
  %% Write attach and set attach_id in post
  %% TODO: Multiple attachments
  Attach    = macaba:propget(attach,     Opts),
  AttachKey = macaba:propget(attach_key, Opts),
  case Attach of
    <<>> -> P;
    Data when byte_size(Data) > 4 ->
      AttachId = write_attachment(AttachKey, Attach),
      P#mcb_post{ attach_ids = [AttachId] }
  end.

%%%-----------------------------------------------------------------------------
%% @doc Creates new post, writes to database
-spec new_post(BoardId :: binary(), Opts :: orddict:orddict()) ->
                  {ok, #mcb_post{}} | {error, any()}.
new_post(BoardId, Opts) when is_binary(BoardId) ->
  case construct_post(BoardId, Opts) of
    {ok, Post0} ->
      Post  = post_write_attach_set_ids(Post0, Opts),
      macaba_db_riak:write(mcb_post, Post),

      ThreadId = macaba:propget(thread_id, Opts),

      %% update thread post list
      ReplyF = fun(TD = #mcb_thread_dynamic{ post_ids=L }) ->
                   TD#mcb_thread_dynamic{ post_ids=L++[Post#mcb_post.post_id] }
               end,
      TDKey = macaba_db:key_for(mcb_thread_dynamic, {BoardId, ThreadId}),
      {atomic, _} = macaba_db_mnesia:update(mcb_thread_dynamic, TDKey, ReplyF),

      %% update board thread list (bump thread)
      bump_if_no_sage(BoardId, ThreadId, Post),
      {ok, Post};
    {error, E} ->
      {error, E}
  end.

%% @private
%% @doc Checks email field of the new post, if it contains no <<"sage">> -
%% bumps thread to become first on board
-spec bump_if_no_sage(BoardId :: binary(), ThreadId :: binary(),
                      Post :: #mcb_post{}) -> boolean().
bump_if_no_sage(_BoardId, _ThreadId, #mcb_post{email = <<"sage">>}) -> false;
bump_if_no_sage(BoardId, ThreadId, _Post) ->
  Board = get_board(BoardId),
  TD = get_thread_dynamic(BoardId, ThreadId),
  SoftPostLimit = Board#mcb_board.max_thread_posts,
  case length(TD#mcb_thread_dynamic.post_ids) > SoftPostLimit of
    true ->
      false; % over soft limit, no bumping
    false ->
      BumpF = fun(BD = #mcb_board_dynamic{ threads=T }) ->
                  BD#mcb_board_dynamic{
                    threads = [ThreadId | lists:delete(ThreadId, T)]
                   }
              end,
      {atomic, _} = macaba_db_mnesia:update(mcb_board_dynamic, BoardId, BumpF),
      true
  end.

%%%-----------------------------------------------------------------------------
%% @doc Creates structure for a new post, returns it. Does not write.
-spec construct_post(BoardId :: binary(), Opts :: orddict:orddict()) ->
                        {ok, #mcb_post{}} | {error, any()}.
construct_post(BoardId, Opts) when is_binary(BoardId) ->
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

      PostId = macaba:as_binary(next_board_post_id(BoardId)),
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
%% @private
%% @doc Writes to database, no unique checks or existence check
write_attachment(_, <<>>) -> <<>>;
write_attachment(Digest, Data) ->
  ContentType = detect_content_type(Data),
  {ThumbKey, ThumbSize} = write_thumbnail(ContentType, Data),
  A = #mcb_attachment{
    size           = byte_size(Data),
    hash           = Digest,
    content_type   = ContentType,
    thumbnail_hash = ThumbKey,
    thumbnail_size = ThumbSize
   },
  AttachMod = macaba_plugins:mod(attachments),
  AttachMod:write_header(A),
  B = #mcb_attachment_body{
    key  = Digest,
    data = Data
   },
  AttachMod:write_body(B),
  Digest.

%%%-----------------------------------------------------------------------------
%% @private
-spec write_thumbnail(ContentType :: atom()|binary(), Data :: binary()) ->
                         {RiakKey :: binary(), Sz :: integer()}.
write_thumbnail(empty, _)   -> {<<>>, 0};
write_thumbnail(no_idea, _) -> {<<>>, 0};
write_thumbnail(<<"image/gif">>,  Data) -> write_thumbnail_1(gif, Data);
write_thumbnail(<<"image/png">>,  Data) -> write_thumbnail_1(png, Data);
write_thumbnail(<<"image/jpeg">>, Data) -> write_thumbnail_1(jpg, Data).

%% @private
write_thumbnail_1(TypeAtom, Data) ->
  {ok, Image} = eim:load(Data),
  {ok, FitH} = macaba_conf:get([<<"board">>, <<"thread">>,
                                <<"thumbnail_height">>]),
  {ok, FitW} = macaba_conf:get([<<"board">>, <<"thread">>,
                               <<"thumbnail_width">>]),
  TData = eim:derive(Image, TypeAtom, {fit, FitW, FitH}),
  TDigest = crypto:sha(TData),
  TBody = #mcb_attachment_body{
    key  = TDigest,
    data = TData
   },
  AttachMod = macaba_plugins:mod(attachments),
  AttachMod:write_body(TBody),
  {TDigest, byte_size(TData)}.

%%%-----------------------------------------------------------------------------
%% @private
-spec detect_content_type(binary()) -> empty | no_idea | binary().
detect_content_type(<<>>) ->
  empty;
detect_content_type(<<"GIF87a", _/binary>>) ->
  <<"image/gif">>;
detect_content_type(<<"GIF89a", _/binary>>) ->
  <<"image/gif">>;
detect_content_type(<<16#ff, 16#d8, 16#ff, 16#e0, _:16, "JFIF", 0,
                      _/binary>>) ->
  <<"image/jpeg">>; % jpeg without EXIF
detect_content_type(<<16#ff, 16#d8, 16#ff, 16#e1, _:16, "Exif", 0,
                      _/binary>>) ->
  <<"image/jpeg">>; % jpeg with EXIF
detect_content_type(<<16#ff, 16#d8, 16#ff, 16#e9, _:16, "SPIFF", 0,
                      _/binary>>) ->
  <<"image/jpeg">>; % jpeg
detect_content_type(<<137, 80, 78, 71, 13, 10, 26, 10, _/binary>>) ->
  <<"image/png">>;
detect_content_type(_) ->
  no_idea.

%%%-----------------------------------------------------------------------------
%% @private
%% @doc Gets unix time for now
-define(SECONDS_1970, 1970*365*24*3600).
get_now_utc() ->
  {UTCD, UTCT} = calendar:universal_time(),
  calendar:datetime_to_gregorian_seconds({UTCD, UTCT}) - ?SECONDS_1970.

%%%-----------------------------------------------------------------------------
%% @doc Generates new post_id for creating thread on the board
next_board_post_id(BoardId) when is_binary(BoardId) ->
  lager:debug("next_board_post_id board=~p", [BoardId]),
  F = fun(BD = #mcb_board_dynamic{ last_post_id=L }) ->
          BD#mcb_board_dynamic{ last_post_id = L+1 }
      end,
  {atomic, NewD} = macaba_db_mnesia:update(mcb_board_dynamic, BoardId, F),
  macaba:as_binary(NewD#mcb_board_dynamic.last_post_id).

%%% Local Variables:
%%% erlang-indent-level: 2
%%% End:
