-ifndef(MCWEB_HRL).
-define(MCWEB_HRL, true).

 -compile([{parse_transform, lager_transform}]).

-record(mcb_html_state, {
            mode                     :: atom()
          , page_vars = []           :: orddict:orddict()
          , already_rendered = false :: boolean()
          , post_data = []           :: orddict:orddict()
          , user                     :: #mcb_user{}
          , site_offline = false     :: boolean()
          %% fields for REST
          , rest_body = <<>>         :: binary()
          , rest_body_json           :: jsx:json_term()
         }).

-endif. % MCWEB_HRL
