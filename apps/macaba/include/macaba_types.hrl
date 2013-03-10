-ifndef(MACABA_TYPES_HRL).
-define(MACABA_TYPES_HRL, true).

%% moved to macaba.config[board]
%% -define(MACABA_COOKIE, <<"macaba">>).

-type proplist_t() :: [ {K :: atom()|binary()|string(), V :: any()} ].
-type proplist_of(T) :: [ {K :: atom()|binary()|string(), V :: T} ].

-type ipv4_t() :: {
              non_neg_integer(),non_neg_integer(),
              non_neg_integer(),non_neg_integer()
             }.
-type ipv6_t() :: {
              non_neg_integer(),non_neg_integer(),
              non_neg_integer(),non_neg_integer(),
              non_neg_integer(),non_neg_integer(),
              non_neg_integer(),non_neg_integer()
             }.
-type ipaddr_t() :: ipv4_t() | ipv6_t().

%% @doc Static rarely changed info about a board
-record(mcb_board, {
          %% short unique id (like "b", or "1", or "board0") used as db key
          %% and as part of URL, can NOT be changed without deleting board!
            board_id :: binary()
          %% used for sorting/simple grouping in templates, else not used
          , category :: binary() % trusted HTML
          %% displayed long board title like "Random" or "Requests" etc
          , title    :: binary() % trusted HTML
          %% displayed short board name like "/b/", can be changed later
          , short_name :: binary() % trusted HTML
          , anonymous_name :: binary() % trusted HTML
          %% will delete last threads over this limit
          , max_threads = 20 :: integer()
          %% soft limit: post count before thread stops bumping
          , max_thread_posts = 500 :: integer()
          %% hard limit: thread is locked at this point (mods can unlock)
          , max_thread_post_lock = 2500 :: integer()

        %% , post_mod_only   :: boolean() % only mods can post and make threads
        %% , thread_mod_only :: boolean() % only mods can make threads
        %% , thread_requires_attach :: boolean() % upload pic for new thread
         }).

-define(MCB_SITE_CONFIG_VER, 1).
%% @doc Website configuration, and boards list
-record(mcb_site_config, {
          boards = []     :: [#mcb_board{}],
          offline = false :: boolean()
         }).

-define(MCB_BOARD_DYNAMIC_VER, 1).
%% @doc Concurrently changed part of board stored in memory
-record(mcb_board_dynamic, {
            board_id         :: binary()
          , last_post_id = 1 :: integer()
          , threads = []     :: [binary()]
         }).

-define(MCB_THREAD_VER, 1).
-record(mcb_thread, {
            %% first post_id equals to thread_id but we never display thread_id
            thread_id         :: binary()
          , board_id          :: binary()
          , hidden = false    :: boolean() % invisible
          , pinned = false    :: boolean() % doesn't sink
          , read_only = false :: boolean() % admins only can post
          %%, author            :: string()  % copy of first post' author
          %%, subject           :: string()  % copy of first post' subject
          %%, created = 0       :: integer() % unix time
         }).

-define(MCB_THREAD_DYNAMIC_VER, 1).
%% @doc Concurrently changed part of thread stored in memory
-record(mcb_thread_dynamic, {
            internal_mnesia_key :: binary()
          , thread_id     :: binary()
          , board_id      :: binary()
          , post_ids = [] :: [binary()]
         }).

-define(MCB_POST_VER, 1).
-record(mcb_post, {
            thread_id          :: binary()
          , board_id           :: binary()
          , post_id            :: binary()
          , subject = <<>>     :: binary()  % trusted HTML
          , author  = <<>>     :: binary()  % trusted HTML
          , email   = <<>>     :: binary()  % trusted HTML
          , message = <<>>     :: binary()  % trusted HTML
          , message_raw = <<>> :: binary()  % trusted HTML
          , created            :: integer() % unix time
          , attach_ids = []    :: [binary()]
          , attach_deleted = false :: boolean()
          , delete_pass = <<>> :: binary()
         }).

-define(MCB_ATTACHMENT_VER, 1).
-record(mcb_attachment, {
            hash            :: binary() % file checksum, also key to file body
          , size = 0        :: integer()
          , thumbnail_hash  :: binary() % thumbnail checksum and key in db
          , thumbnail_size  :: integer()
          , content_type    :: binary()
          , references = [] :: [binary()] % list of posts referring to file
         }).

-define(MCB_ATTACHMENT_BODY_VER, 1).
-record(mcb_attachment_body, {
          key :: binary(),
          data :: binary()
         }).

%% @doc Mnesia objects are stored in memory only and built on node start
-type macaba_mnesia_object() :: mcb_board_dynamic | mcb_thread_dynamic.
-type macaba_mnesia_record() :: #mcb_board_dynamic{} | #mcb_thread_dynamic{}.
%% @doc RIAK objects are written once and changed rarely, and persist on disk
%% RIAK also stores all Mnesia objects, they are updated regularly by leader
%% node, and reloaded on cluster1 restart
-type macaba_riak_object()   :: mcb_site_config | mcb_thread | mcb_post
                              | mcb_attachment | mcb_attachment_body
                              | macaba_mnesia_object().
-type macaba_riak_record()   :: #mcb_site_config{} | #mcb_thread{} | #mcb_post{}
                              | #mcb_attachment{} | #mcb_attachment_body{}
                              | macaba_mnesia_record().
-type macaba_db_object()     :: macaba_riak_object().

%% @doc A user login/password credentials structure used for login
-define(MCB_USER_CRED_VER, 1).
-record(mcb_user_cred, {
            login = <<>>     :: binary()
          , password = <<>>  :: binary()
         }).

%% @doc A user info structure, you can get this by calling macaba_web:get_user
-record(mcb_user, {
            type = 'anon' :: 'anon' | 'mod' | 'admin'
          , session_key :: binary()
          , session_pid :: pid()
          , name = <<>> :: binary()
         }).

-endif. % MACABA_TYPES_HRL
