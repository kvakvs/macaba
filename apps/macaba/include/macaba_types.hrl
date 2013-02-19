-ifndef(MACABA_TYPES_HRL).
-define(MACABA_TYPES_HRL, true).

-type proplist_t() :: [ {K :: term(), V :: term()} ].

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
         }).

-define(MCB_SITE_CONFIG_VER, 1).
%% @doc Website configuration, and boards list
-record(mcb_site_config, {
          boards = []     :: [#mcb_board{}],
          offline = false :: boolean()
         }).

%%-define(MCB_BOARD_DYNAMIC_VER, 1).
%% @doc Dynamic data for board stored in Mnesia and written using transactions
-record(mcb_board_dynamic, {
            board_id         :: binary()
          , last_post_id = 1 :: integer()
          , threads          :: [binary()]
         }).

-define(MCB_THREAD_VER, 1).
-record(mcb_thread, {
          %% first post id equals to thread id but we never display thread id
            thread_id         :: binary()
          , post_ids = []     :: [binary()]
          , hidden = false    :: boolean() % invisible
          , pinned = false    :: boolean() % doesn't sink
          , read_only = false :: boolean() % admins only can post
         }).

-define(MCB_POST_VER, 1).
-record(mcb_post, {
            thread_id      :: binary()
          , post_id        :: binary()
          , subject = ""   :: string()  % trusted HTML
          , author  = ""   :: string()  % trusted HTML
          , message = ""   :: string()  % trusted HTML
          , created = ""   :: integer() % unix time
          , attach_id = "" :: binary()
          , sage = false   :: boolean()
         }).

-define(MCB_ATTACHMENT_VER, 1).
-record(mcb_attachment, {
            attach_id = ""  :: binary()
          , filename = ""   :: string() % unique random filename
          , size = 0        :: integer()
          , references = [] :: [binary()] % list of posts referring to file
          , hash            :: binary() % file checksum, also key to file body
         }).

-type macaba_mnesia_object() :: mcb_board_dynamic.
-type macaba_riak_object()   :: mcb_site_config | mcb_thread | mcb_post
                              | mcb_attachment.
-type macaba_db_object()     :: macaba_mnesia_object() | macaba_riak_object().

-endif. % MACABA_TYPES_HRL
