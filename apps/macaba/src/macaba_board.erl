%%%------------------------------------------------------------------------
%%% @doc Board data model, is used by board client (macaba_board_cli) and then
%%% formatted to HTML or JSON etc
%%% Created: 2013-02-17 Dmytro Lytovchenko <kvakvs@yandex.ru>
%%%------------------------------------------------------------------------
-module(macaba_board).

-export([ construct_post/2
        , get_board/1
        , get_boards/0
        , get_thread/1
        , get_threads/1
        , get_thread_contents/2
        , get_thread_dynamic/1
        , new_post/2
        , new_thread/3
        , start/0
        , load_board_dynamics/0
        , detect_content_type/1
        , attachment_exists/1
        ]).

-include_lib("macaba/include/macaba_types.hrl").

%%%-----------------------------------------------------------------------------
start() ->
  ok.

%%%-----------------------------------------------------------------------------
%% @private
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
  lager:debug("upd_dyn_b bd=~p", [BD]),
  %%T = fun() -> mnesia:write(mcb_board_dynamic, BD, write) end,
  %%{atomic, _} = mnesia:transaction(T),
  macaba_db_mnesia:write(mcb_board_dynamic, BD),
  update_dynamics_for_threads(BD#mcb_board_dynamic.threads),
  update_dynamics_for_board(Boards).

get_thread_dynamic(ThreadId) ->
  case macaba_db_riak:read(mcb_thread_dynamic, ThreadId) of
    {error, not_found} -> #mcb_thread_dynamic{thread_id = ThreadId};
    Value -> Value
  end.

%% @private
update_dynamics_for_threads([]) -> ok;
update_dynamics_for_threads([ThreadId | Threads]) when is_binary(ThreadId) ->
  TD = get_thread_dynamic(ThreadId),
  lager:debug("upd_dyn_t td=~p", [TD]),
  %% T = fun() -> mnesia:write(mcb_thread_dynamic, TD, write) end,
  %% {atomic, _} = mnesia:transaction(T),
  macaba_db_mnesia:write(mcb_thread_dynamic, TD),
  update_dynamics_for_threads(Threads).

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
  [#mcb_board{
      board_id= <<"default">>,
      category="default",
      title="Default board"
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
  Threads0 = [case macaba_db_riak:read(mcb_thread, T) of
                #mcb_thread{}=Value -> Value;
                {error, _} -> []
              end || T <- Ids],
  Threads = lists:flatten(Threads0),
  Threads.

%%%-----------------------------------------------------------------------------
get_thread(ThreadId) ->
  case macaba_db_riak:read(mcb_thread, ThreadId) of
    #mcb_thread{}=Value -> Value;
    {error, _} -> {error, not_found}
  end.

%%%-----------------------------------------------------------------------------
%% @doc By thread id reads thread dynamic, to get ids of posts, loads first and
%% configured amount of last posts into a proplist. Give 'all' for LastCount to
%% load whole thread
-spec get_thread_contents(ThreadId :: binary(),
                          LastCount0 :: integer() | 'all') -> [#mcb_post{}].

get_thread_contents(ThreadId, LastCount0) when is_binary(ThreadId) ->
  TD = case macaba_db_mnesia:read(mcb_thread_dynamic, ThreadId) of
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
  First = case PostIds of [] -> []; [F|_] -> get_post(F) end,
  %% FIXME: this may run slow on large threads >1000 posts?
  PostIds2 = tl(PostIds),
  T = min(length(PostIds2), max(0, length(PostIds2) - LastCount)),
  LastIds = lists:nthtail(T, PostIds2),
  Last = lists:map(fun get_post/1, LastIds),
  lists:flatten([First | Last]).

%%%-----------------------------------------------------------------------------
%% @doc Loads post info
-spec get_post(PostId :: binary()) -> [#mcb_post{}] | {error, not_found}.
get_post(PostId) when is_binary(PostId) ->
  macaba_db_riak:read(mcb_post, PostId).

%%%-----------------------------------------------------------------------------
%% @doc Creates a new thread with a single post, thread_id is set to the first
%% post id. Writes both thread and post to database.
new_thread(BoardId, ThreadOpts, PostOpts) when is_binary(BoardId) ->
  Post0    = construct_post(BoardId, PostOpts),
  PostId   = Post0#mcb_post.post_id,

  Hidden   = macaba:propget(hidden,    ThreadOpts, false),
  Pinned   = macaba:propget(pinned,    ThreadOpts, false),
  ReadOnly = macaba:propget(read_only, ThreadOpts, false),

  Thread = #mcb_thread{
      thread_id = PostId
    , hidden    = Hidden
    , pinned    = Pinned
    , read_only = ReadOnly
   },
  ThreadDyn = #mcb_thread_dynamic{
    thread_id = PostId,
    post_ids  = [PostId]
   },
  macaba_db_mnesia:write(mcb_thread_dynamic, ThreadDyn),
  Post1 = post_write_attach_set_ids(Post0, PostOpts),

  %% link post to thread
  Post = Post1#mcb_post{ thread_id = PostId },
  macaba_db_riak:write(mcb_post, Post),
  macaba_db_riak:write(mcb_thread, Thread),

  %% add thread to board
  F = fun(BD = #mcb_board_dynamic{ threads=T }) ->
          BD#mcb_board_dynamic{ threads = [PostId | T]}
      end,
  {atomic, _NewD} = macaba_db_mnesia:update(mcb_board_dynamic, BoardId, F),
  {Thread, Post}.

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
new_post(BoardId, Opts) when is_binary(BoardId) ->
  Post0 = construct_post(BoardId, Opts),
  Post  = post_write_attach_set_ids(Post0, Opts),
  macaba_db_riak:write(mcb_post, Post),

  ThreadId = macaba:propget(thread_id, Opts),

  %% update thread post list
  ReplyF = fun(TD = #mcb_thread_dynamic{ post_ids=L }) ->
               TD#mcb_thread_dynamic{ post_ids=L++[Post#mcb_post.post_id] }
           end,
  {atomic, _} = macaba_db_mnesia:update(mcb_thread_dynamic, ThreadId, ReplyF),

  %% update board thread list (bump thread)
  bump_if_no_sage(BoardId, ThreadId, Post),
  Post.

%% @private
%% @doc Checks email field of the new post, if it contains no <<"sage">> -
%% bumps thread to become first on board
bump_if_no_sage(_BoardId, _ThreadId, #mcb_post{email = <<"sage">>}) -> ok;
bump_if_no_sage(BoardId, ThreadId, Post) ->
  BumpF = fun(BD = #mcb_board_dynamic{ threads=T }) ->
              BD#mcb_board_dynamic{
                threads = [ThreadId | lists:delete(ThreadId, T)]
               }
          end,
  {atomic, _} = macaba_db_mnesia:update(mcb_board_dynamic, BoardId, BumpF).

%%%-----------------------------------------------------------------------------
%% @doc Creates structure for a new post, returns it. Does not write.
%% BUG: Creating post with attachment actually writes attachment to database!
construct_post(BoardId, Opts) when is_binary(BoardId) ->
  ThreadId  = macaba:propget(thread_id, Opts),
  Author    = macaba:propget(author,    Opts),
  Email     = macaba:propget(email,     Opts),
  Subject   = macaba:propget(subject,   Opts),
  Message   = macaba:propget(message,   Opts),

  PostId = macaba:as_binary(next_board_post_id(BoardId)),
  #mcb_post{
    thread_id   = macaba:as_binary(ThreadId),
    post_id     = PostId,
    subject     = Subject,
    author      = Author,
    email       = Email,
    message_raw = Message,
    message     = macaba_markup:process(Message),
    created     = get_now_utc(),
    %% attach_ids = [macaba:as_binary(AttachId)],
    sage        = false
   }.

%%%-----------------------------------------------------------------------------
%% @private
attachment_exists(<<>>) -> false;
attachment_exists(Digest) ->
  case macaba_db_riak:read(mcb_attachment, Digest) of
    #mcb_attachment{} -> true;
    {error, not_found} -> false
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
  macaba_db_riak:write(mcb_attachment, A),
  B = #mcb_attachment_body{
    key  = Digest,
    data = Data
   },
  macaba_db_riak:write(mcb_attachment_body, B),
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
  macaba_db_riak:write(mcb_attachment_body, TBody),
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
  F = fun(BD = #mcb_board_dynamic{ last_post_id=L }) ->
          BD#mcb_board_dynamic{ last_post_id = L+1 }
      end,
  {atomic, NewD} = macaba_db_mnesia:update(mcb_board_dynamic, BoardId, F),
  macaba:as_binary(NewD#mcb_board_dynamic.last_post_id).

%%% Local Variables:
%%% erlang-indent-level: 2
%%% End:
