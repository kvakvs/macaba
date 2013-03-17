-ifndef(MCWEB_HRL).
-define(MCWEB_HRL, true).

-record(mcb_html_state, {
            mode                     :: atom()
          , page_vars = []           :: orddict:orddict()
          , already_rendered = false :: boolean()
          , post_data = []           :: orddict:orddict()
          , user                     :: #mcb_user{}
          , site_offline = false     :: boolean()
         }).

-endif. % MCWEB_HRL
