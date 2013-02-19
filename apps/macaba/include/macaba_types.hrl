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

-record(mcb_board, {
            board_id :: binary()
          , category :: string() % simple grouping, trusted HTML
          , title    :: string() % trusted HTML
          , threads  :: [integer()]
         }).

-define(MCB_BOARD_DYNAMIC_VER, 1).
%% @doc Dynamic data for board stored in RIAK
-record(mcb_board_dynamic, {
            board_id :: binary()
          , post_id  :: integer()
         }).

-define(MCB_THREAD_VER, 1).
-record(mcb_thread, {
          %% first post id equals to thread id but we never display thread id
            thread_id  :: integer()
          , post_ids   :: [integer()]
          , hidden     :: boolean() % invisible
          , pinned     :: boolean() % doesn't sink
          , read_only  :: boolean() % admins only can post
         }).

-define(MCB_POST_VER, 1).
-record(mcb_post, {
            thread_id  :: integer()
          , post_id    :: integer()
          , subject    :: string()  % trusted HTML
          , author     :: string()  % trusted HTML
          , message    :: string()  % trusted HTML
          , created    :: integer() % unix time
          , attach_id  :: binary()
          , sage       :: boolean()
         }).

-type macaba_dbobject() :: mcb_board_dynamic | mcb_thread | mcb_post.
