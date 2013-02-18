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
            board_id :: atom()
          , category :: string() % simple grouping, trusted HTML
          , title    :: string() % trusted HTML
          , post_id  :: integer() % dynamic data stored in database and memory
          , threads  :: [integer()]
         }).

-record(mcb_thread, {
          %% first post id equals to thread id but we never display thread id
            thread_id  :: integer()
          , post_ids   :: [integer()]
          , hidden     :: boolean() % invisible
          , pinned     :: boolean() % doesn't sink
          , read_only  :: boolean() % admins only can post
         }).

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

-type macaba_dbobject() :: mcb_board | mcb_thread | mcb_post.
