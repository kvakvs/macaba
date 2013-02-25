-ifndef(MACABA_TYPES_HRL).
-define(MACABA_TYPES_HRL, true).

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
            board_id :: binary()
          , category :: string() % simple grouping, trusted HTML
          , title    :: string() % trusted HTML
        %% , post_mod_only   :: boolean() % only mods can post and make threads
        %% , thread_mod_only :: boolean() % only mods can make threads
          , thread_requires_attach :: boolean() % upload pic for new thread
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
            thread_id     :: binary()
          , post_ids = [] :: [binary()]
         }).

-define(MCB_POST_VER, 1).
-record(mcb_post, {
            thread_id       :: binary()
          , post_id         :: binary()
          , subject = ""    :: string()  % trusted HTML
          , author  = ""    :: string()  % trusted HTML
          , message = ""    :: string()  % trusted HTML
          , created         :: integer() % unix time
          , attach_ids = [] :: [binary()]
          , sage = false    :: boolean()
         }).

-define(MCB_ATTACHMENT_VER, 1).
-record(mcb_attachment, {
          %% attach_id = ""  :: binary()
          %% , filename = ""   :: string() % unique random filename
            size = 0        :: integer()
          , content_type    :: binary()
          , references = [] :: [binary()] % list of posts referring to file
          , hash            :: binary() % file checksum, also key to file body
         }).

-define(MCB_ATTACHMENT_BODY_VER, 1).
-record(mcb_attachment_body, {
          key :: binary(),
          data :: binary()
         }).

%% @doc Mnesia objects are stored in memory only and built on node start
-type macaba_mnesia_object() :: mcb_board_dynamic | mcb_thread_dynamic.
%% @doc RIAK objects are written once and changed rarely, and persist on disk
%% RIAK also stores all Mnesia objects, they are updated regularly by leader
%% node, and reloaded on cluster1 restart
-type macaba_riak_object()   :: mcb_site_config | mcb_thread | mcb_post
                              | mcb_attachment | macaba_mnesia_object().
-type macaba_db_object()     :: macaba_riak_object().

%% @doc A user info structure, you can get this by calling macaba_web:get_user
-record(mcb_user, {
          type = 'anon' :: 'anon' | 'mod' | 'admin',
          session_key :: binary(),
          session_pid :: pid()
         }).

-endif. % MACABA_TYPES_HRL
