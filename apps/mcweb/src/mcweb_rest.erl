%%%-----------------------------------------------------------------------------
%%% @doc Library for REST resources
%%% Created: 2013-03-24 Dmytro Lytovchenko <kvakvs@yandex.ru>
%%%-----------------------------------------------------------------------------
-module(mcweb_rest).

-export([ rest_init_helper/2
        ]).

-include_lib("macaba/include/macaba_types.hrl").
-include_lib("mcweb/include/mcweb.hrl").

-spec rest_init_helper(Req :: cowboy_req:req(),
                       HandlerOpts :: any()) ->
                              {ok, cowboy_req:req(), #mcb_html_state{}}.

rest_init_helper(Req0, _HandlerOpts) ->
  {ok, Body, Req1} = cowboy_req:body(Req0),
  {User, Req} = mcweb:get_user(Req1),
  State = #mcb_html_state{
    rest_body = Body,
    rest_body_json = case Body of
                       <<>> -> undefined;
                       _ ->
                         %% lager:debug("init body=~p", [Body]),
                         jsx:decode(Body)
                     end,
    user = User
   },
  {ok, Req, State}.

%%% Local Variables:
%%% erlang-indent-level: 2
%%% End:
