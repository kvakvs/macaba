%%%------------------------------------------------------------------------
%%% @doc Thread data model, represents threads and actions with threads
%%% Created: 2013-03-09 Dmytro Lytovchenko <kvakvs@yandex.ru>
%%%------------------------------------------------------------------------
-module(macaba_thread).

-export([ get/2
        , get_contents/3
        , get_dynamic/2
        , add_post/3
        , new/3
        , delete/2
        , set_read_only/3
        ]).

-include_lib("macaba/include/macaba_types.hrl").

%%%-----------------------------------------------------------------------------
%% @doc Creates a new thread with a single post, thread_id is set to the first
%% post id. Writes both thread and post to database.
new(BoardId, ThreadOpts, PostOpts) when is_binary(BoardId) ->
  {ok, Post0} = macaba_post:construct(BoardId, PostOpts),
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
  TDKey = macaba_db:key_for(mcb_thread_dynamic, {BoardId, ThreadId}),
  ThreadDyn = #mcb_thread_dynamic{
      internal_mnesia_key = TDKey
    , thread_id = ThreadId
    , board_id  = BoardId
    , post_ids  = [PostId]
   },
  %% TDKey = macaba_db:key_for(mcb_thread_dynamic, {BoardId, PostId}),
  macaba_db_mnesia:write(mcb_thread_dynamic, ThreadDyn),
  Post1 = macaba_post:write_attach_set_ids(Post0, PostOpts),

  %% link post to thread
  Post = Post1#mcb_post{ thread_id = PostId },
  macaba_db_riak:write(mcb_post, Post),
  macaba_db_riak:write(mcb_thread, Thread),

  macaba_board_worker:board_add_thread(BoardId, ThreadId),
  {Thread, Post}.

%%%-----------------------------------------------------------------------------
delete(BoardId, ThreadId) ->
  lager:info("thread: delete B=~s T=~s", [BoardId, ThreadId]),
  BUpd = fun(BD = #mcb_board_dynamic{ threads=T }) ->
            T2 = lists:delete(ThreadId, T),
            BD#mcb_board_dynamic{ threads=T2 }
        end,
  {atomic, _} = macaba_db_mnesia:update(mcb_board_dynamic, BoardId, BUpd),

  TDKey = macaba_db:key_for(mcb_thread_dynamic, {BoardId, ThreadId}),
  {ok, TD} = macaba_db_mnesia:read(mcb_thread_dynamic, TDKey),
  lists:foreach(fun(P) -> macaba_post:delete_dirty(BoardId, P) end,
                TD#mcb_thread_dynamic.post_ids),

  TKey = macaba_db:key_for(mcb_thread, {BoardId, ThreadId}),
  macaba_db_riak:delete(mcb_thread, TKey),

  %% mnesia delete will also delete in riak but 1 sec later, delete now
  macaba_db_riak:delete(mcb_thread_dynamic, TDKey),
  macaba_db_mnesia:delete(mcb_thread_dynamic, TDKey).

%%%-----------------------------------------------------------------------------
%% @doc Thread is identified by board name and number
-spec get(BoardId :: binary(),
          ThreadId :: binary()) -> {ok, #mcb_thread{}} | {error, not_found}.

get(BoardId, ThreadId) when is_binary(BoardId), is_binary(ThreadId) ->
  K = macaba_db:key_for(mcb_thread, {BoardId, ThreadId}),
  case macaba_db_riak:read(mcb_thread, K) of
    {ok, #mcb_thread{}=Value} -> {ok, Value};
    {error, _} -> {error, not_found}
  end.

%%%-----------------------------------------------------------------------------
-spec get_dynamic(BoardId :: binary(),
                  ThreadId :: binary()) ->
                     {ok, #mcb_thread_dynamic{}} | {error, not_found}.

get_dynamic(BoardId, ThreadId) when is_binary(BoardId), is_binary(ThreadId) ->
  TDKey = macaba_db:key_for(mcb_thread_dynamic, {BoardId, ThreadId}),
  case macaba_db_riak:read(mcb_thread_dynamic, TDKey) of
    {error, not_found} ->
      lager:debug("thread: get_dynamic not found ~p - make default", [TDKey]),
      #mcb_thread_dynamic{
          internal_mnesia_key = TDKey
        , board_id = BoardId
        , thread_id = ThreadId
      };
    {ok, Value} -> {ok, Value}
  end.

%%%-----------------------------------------------------------------------------
%% @doc Sets thread read-only, ignores possible chance of conflicting writes
-spec set_read_only(BoardId :: binary(),
                    ThreadId :: binary(),
                    RO :: boolean()) ->
                           ok | {error, not_found}.
set_read_only(BoardId, ThreadId, RO) ->
  case ?MODULE:get(BoardId, ThreadId) of
    {ok, #mcb_thread{ read_only = RO }} ->
      ok; % do nothing if already set
    {ok, T = #mcb_thread{}} ->
      %% do not bother with concurrent writes conflicting, i will regret it
      lager:info("thread: set_read_only B=~s T=~s -> ~p",
                 [BoardId, ThreadId, RO]),
      T2 = T#mcb_thread{ read_only = RO },
      macaba_db_riak:write(mcb_thread, T2);
    {error, not_found} ->
      {error, not_found}
  end.

%%%-----------------------------------------------------------------------------
%% @doc Reads thread dynamic to get ids of posts, loads first post and
%% configured amount of last posts into a proplist. Give atom 'all' for
%% LastCount to load whole thread
-spec get_contents(BoardId :: binary(),
                          ThreadId :: binary(),
                          LastCount0 :: integer() | 'all') -> [#mcb_post{}].

get_contents(BoardId, ThreadId, LastCount0)
  when is_binary(BoardId), is_binary(ThreadId) ->
  TDKey = macaba_db:key_for(mcb_thread_dynamic, {BoardId, ThreadId}),
  case macaba_db_mnesia:read(mcb_thread_dynamic, TDKey) of
    {error, not_found} -> TD = #mcb_thread_dynamic{};
    {ok, TD} -> TD
  end,
  PostIds = TD#mcb_thread_dynamic.post_ids,

  %% if lastcount was set to 'all' - change it to thread length
  LastCount1 = case LastCount0 of
                 all -> length(PostIds);
                 _ -> LastCount0
               end,
  LastCount = min(LastCount1, length(PostIds)),

  %% get first and cut last
  First = case PostIds of
            [] -> [];
            [F|_] ->
              {ok, PFirst} = macaba_post:get(BoardId, F),
              PFirst
          end,
  %% FIXME: this may run slow on large threads >1000 posts?
  LastIds = case PostIds of
               [] -> [];
               _ ->
                 PostIds2 = tl(PostIds),
                T = min(length(PostIds2), max(0, length(PostIds2) - LastCount)),
                lists:nthtail(T, PostIds2)
            end,
  Last = lists:map(fun(Id) ->
                       {ok, P} = macaba_post:get(BoardId, Id),
                       P
                   end, LastIds),
  lists:flatten([First | Last]).


%%%-----------------------------------------------------------------------------
%% @private
%% @doc Writes post to database and adds post to thread, bumps thread if soft
%% post limit not reached, also locks thread if hard post limit reached. Post
%% must already have its attachments saved
add_post(BoardId, ThreadId, Post = #mcb_post{}) ->
  lager:debug("thread: add_post"),
  macaba_db_riak:write(mcb_post, Post),

  {ok, Board} = macaba_board:get(BoardId),
  ThreadHardLimit = Board#mcb_board.max_thread_post_lock,
  ThreadSoftLimit = Board#mcb_board.max_thread_posts,

  %% update thread post list
  ReplyF = fun(TD = #mcb_thread_dynamic{ post_ids=L }) ->
               L2 = L ++ [Post#mcb_post.post_id],
               case length(L2) of
                 ThreadHardLimit ->
                   %% lock once when reaching hard limit, allow mods to
                   %% unlock if needed
                   macaba_board_worker:thread_set_read_only(
                     BoardId, ThreadId, true);
                 _ -> ok
               end,
               TD#mcb_thread_dynamic{ post_ids=L2 }
           end,
  TDKey = macaba_db:key_for(mcb_thread_dynamic, {BoardId, ThreadId}),
  {atomic, _} = macaba_db_mnesia:update(mcb_thread_dynamic, TDKey, ReplyF),

  %% update board thread list (bump thread)
  macaba_board:thread_bump_if_no_sage(BoardId, ThreadId, ThreadSoftLimit, Post),
  ok.

%%% Local Variables:
%%% erlang-indent-level: 2
%%% End:

