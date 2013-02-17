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
          id    :: atom(),
          title :: string()
         }).

-record(mcb_thread, {
          id         :: integer(),
          subject    :: string(),
          author     :: string(),
          created    :: integer(),
          flags = [] :: orddict:orddict()
         }).

-type macaba_dbobject() :: mcb_board | mcb_thread | mcb_thread_list.
