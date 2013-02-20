%%%------------------------------------------------------------------------
%%% @doc Board and board list handling
%%% Created: 2013-02-17 Dmytro Lytovchenko <kvakvs@yandex.ru>
%%%------------------------------------------------------------------------
-module(macaba_board).

-export([ construct_post/1
        , get_board/1
        , get_boards/0
        , get_threads/1
        , new_post/1
        , new_thread/3
        , start/0
        ]).

-include_lib("macaba/include/macaba_types.hrl").

%%%-----------------------------------------------------------------------------
start() ->
  ok.

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
  case lists:keysearch(BoardId, #mcb_board.board_id, db_get_boards()) of
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
%% @doc Returns list of threads in board (only info headers, no contents!)
get_threads(BoardId) when is_binary(BoardId) ->
  D = macaba_db_mnesia:read(mcb_board_dynamic, BoardId),
  D#mcb_board_dynamic.threads.

%%%-----------------------------------------------------------------------------
%% @doc Creates a new thread with a single post, thread_id is set to the first
%% post id. Writes both thread and post to database.
new_thread(BoardId, ThreadOpts, PostOpts) when is_binary(BoardId) ->
  Post0    = construct_post(PostOpts),
  PostId   = Post0#mcb_post.post_id,

  Hidden   = macaba:propget(hidden,    ThreadOpts, false),
  Pinned   = macaba:propget(pinned,    ThreadOpts, false),
  ReadOnly = macaba:propget(read_only, ThreadOpts, false),

  Thread = #mcb_thread{
    thread_id = PostId,
    post_ids  = [PostId],
    hidden    = Hidden,
    pinned    = Pinned,
    read_only = ReadOnly
   },
  %% link post to thread
  Post = Post0#mcb_post{ thread_id = PostId },
  macaba_db_riak:write(mcb_post, Post),
  macaba_db_riak:write(mcb_thread, Thread),
  {Thread, Post}.

%%%-----------------------------------------------------------------------------
%% @doc Creates new post, writes to database
new_post(Opts) ->
  Post = construct_post(Opts),
  macaba_db_riak:write(mcb_post, Post),

  %%-----------------------------------------------
  %% TODO vector clocks and conflict resolution
  %%-----------------------------------------------
  ThreadId = macaba:propget(thread_id, Opts),

  %% update thread post list
  Thread0  = macaba_db_riak:read(mcb_thread, ThreadId),
  NewIds = Thread0#mcb_thread.post_ids ++ [Post#mcb_post.post_id],
  Thread = Thread0#mcb_thread{ post_ids=NewIds },
  macaba_db_riak:write(mcb_thread, Thread),

  Post.

%%%-----------------------------------------------------------------------------
%% @doc Creates structure for a new post, returns it. Does not write.
construct_post(Opts) ->
  BoardId   = macaba:propget(board_id,  Opts),
  ThreadId  = macaba:propget(thread_id, Opts),
  Author    = macaba:propget(author,    Opts),
  Subject   = macaba:propget(subject,   Opts),
  Message   = macaba:propget(message,   Opts),
  Attach    = macaba:propget(attach,    Opts),
  AttachId  = write_attachment(Attach),

  PostId = next_board_post_id(BoardId),
  #mcb_post{
    thread_id = ThreadId,
    post_id   = PostId,
    subject   = Subject,
    author    = Author,
    message   = Message,
    created   = get_now_utc(),
    attach_id = AttachId,
    sage      = false
   }.

%%%-----------------------------------------------------------------------------
%% @private
%% @doc Gets unix time for now
-define(SECONDS_1970, 1970*365*24*3600).
get_now_utc() ->
  {UTCD, UTCT} = calendar:universal_time(),
  calendar:datetime_to_gregorian_seconds({UTCD, UTCT}) - ?SECONDS_1970.

%%%-----------------------------------------------------------------------------
next_board_post_id(BoardId) when is_binary(BoardId) ->
  F = fun(BD = #mcb_board_dynamic{ last_post_id=L }) ->
          BD#mcb_board_dynamic{ last_post_id = L+1 }
      end,
  {atomic, NewD} = macaba_db_mnesia:update(mcb_board_dynamic, BoardId, F),
  NewD#mcb_board_dynamic.last_post_id.

%%%-----------------------------------------------------------------------------
%% @!private
%% @!doc Searches for existing thread if the parameter is a number.
%% If parameter is <<"new">> creates a new thread
%% find_or_create_thread(undefined, _, _Opts) -> {error, thread_id_not_found};
%% find_or_create_thread(_, undefined, _Opts) -> {error, board_not_found};
%% find_or_create_thread(<<"new">>, BoardId, ThreadOpts) ->
%%   T = new_thread(BoardId, ThreadOpts),
%%   {ok, T}.

%%%-----------------------------------------------------------------------------
%% @private
%% @doc Searches database for existing file, if so - returns {error, exists}
write_attachment(undefined) -> undefined;
write_attachment(_A) -> undefined.

%%% Local Variables:
%%% erlang-indent-level: 2
%%% End:
