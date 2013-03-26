%%%------------------------------------------------------------------------
%%% @doc Utility functions for web server and templates
%%% Created: 2013-02-17 Dmytro Lytovchenko <kvakvs@yandex.ru>
%%%------------------------------------------------------------------------
-module(mcweb).

-export([ handle_helper/3
        , compile/1
        , render/2
        , chain_run/3
        , chain_success/1, chain_success/2, chain_fail/1, chain_fail/2
        , render_page/3, render_page/4
        , response_text/4
        , response_json/4
        , redirect/3
        , redirect_to_thread/4
        , redirect_to_thread_and_post/5
        , render_error/3
        , state_set_var/3
        , state_get_var/2
        , is_POST_and_multipart/1
        , parse_body_qs/2
        , parse_multipart_form_data/2
        , get_user/1
        , get_user_save_to_state/2
        , create_session_for/3
        , ses_cookie_name/0
        , clear_session_cookie/1
        , get_poster_id/1
        , get_user_identification/1
        , chain_fail_if_level_below/3
        , chain_fail_if_below_admin/2
        , chain_fail_if_below_mod/2
        , safe/2, safe_length/2
        ]).

-include_lib("macaba/include/macaba_types.hrl").
-include_lib("mcweb/include/mcweb.hrl").

-type html_state() :: #mcb_html_state{}.
-type handler_return() :: {cowboy_req:req(), macaba_web:html_state()}.
-type chain_return() :: {chain_ok|chain_error
                         , cowboy_req:req(), macaba_web:html_state()}.

-export_type([ handler_return/0
             , html_state/0
             , chain_return/0
             ]).

%%%------------------------------------------------------------------------
-spec handle_helper(Module :: atom(),
                    Req :: cowboy_req:req(),
                    State :: macaba_web:html_state()) ->
                       {ok, cowboy_req:req(), macaba_web:html_state()}.

%% @doc A handler entry point for all web resources. Checks and parses POST
%% fields, checks user cookie and extracts user from session storage, checks
%% offline flag, calculates and calls handler for the resource being processed,
%% catches errors
handle_helper(Module, Req0, State0 = #mcb_html_state{ mode=Mode }) ->
  try
    {Method, Req1} = cowboy_req:method(Req0),

    %% parse request body as multipart, this will not work for POST urlencoded
    {Req2, State1} = case ?MODULE:is_POST_and_multipart(Req1) of
                       {true, true}  ->
                         ?MODULE:parse_multipart_form_data(Req1, State0);
                       {true, false}  ->
                         ?MODULE:parse_body_qs(Req1, State0);
                       {false, _} ->
                         {Req1, State0}
                     end,
    {Req3, State2} = ?MODULE:get_user_save_to_state(Req2, State1),

    %% site offline flag
    %% TODO: cache site config in memory or in state
    #mcb_site_config{ offline=SiteOffline } = macaba_board:get_site_config(),
    State3 = State2#mcb_html_state{ site_offline = SiteOffline },
    State4 = ?MODULE:state_set_var(site_offline, SiteOffline, State3),

    FnName = macaba:as_atom("macaba_handle_" ++ macaba:as_string(Mode)),
    {Req4, State5} = apply(Module, FnName, [Method, Req3, State4]),
    {ok, Req4, State5}
  catch
    E ->
      T = lists:flatten(io_lib:format("handle error: ~p ~p",
                                      [E, erlang:get_stacktrace()])),
      lager:error(E),
      {ReqE, StateE} = ?MODULE:response_text(500, T, Req0, State0),
      {ok, ReqE, StateE}
  end.

%%%------------------------------------------------------------------------
-spec compile(string()) -> atom().
compile(TplName) ->
  TplModule = list_to_atom(TplName ++ "_dtl"),
  %% recompile-debug
  Priv = code:priv_dir(mcweb),
  erlydtl:compile(
    Priv ++ "/tpl/" ++ TplName ++ ".dtl", TplModule,
    [ verbose
    , {out_dir, filename:join([Priv, "tpl", "ebin"])}
    , {doc_root, filename:join([Priv, "tpl"])}
    , {custom_tags_dir, filename:join([Priv, "tpl", "custom_tags"])}
    ]),
  TplModule.

%%%------------------------------------------------------------------------
%% @doc Attempts to reset session cookie to empty value
-spec clear_session_cookie(Req :: cowboy_req:req()) -> cowboy_req:req().
clear_session_cookie(Req0) ->
  Coo = ?MODULE:ses_cookie_name(),
  cowboy_req:set_resp_cookie(Coo, <<>>, [{path, <<"/">>}, {max_age, 0}], Req0).

%%%------------------------------------------------------------------------
-spec render(string(), [{atom(), any()}]) -> iolist().
render(TplName, TplOptions) ->
  TplModule = compile(TplName),
  case erlang:function_exported(TplModule, render, 1) of
    true ->
      {ok, Content} = TplModule:render(TplOptions),
      Content;
    false ->
      erlang:error({error, not_exported, {TplModule, render, 1}})
  end.

%%%------------------------------------------------------------------------
%% @doc Runs list of functions passing opaque state through them and stopping
%% if any of functions returns error.
-type handler_fun_t() :: fun((cowboy_req:req(), macaba_web:html_state()) ->
                                macaba_web:chain_return()).
-spec chain_run(FunList :: [handler_fun_t()],
                Req :: cowboy_req:req(),
                State :: macaba_web:html_state()) ->
                   {ok, cowboy_req:req(), macaba_web:html_state()} |
                   {error, cowboy_req:req(), macaba_web:html_state()}.

chain_run([], Req, State) -> {ok, Req, State};
chain_run([F | Tail], Req, State) ->
  case F(Req, State) of
    {chain_ok, Req2, State2}   -> chain_run(Tail, Req2, State2);
    {chain_fail, Req3, State3} -> {error, Req3, State3}
  end.

chain_success(Req, State = #mcb_html_state{}) -> {chain_ok, Req, State}.
chain_success({Req, State = #mcb_html_state{}}) -> {chain_ok, Req, State}.

chain_fail(Req, State = #mcb_html_state{}) -> {chain_fail, Req, State}.
chain_fail({Req, State = #mcb_html_state{}}) -> {chain_fail, Req, State}.

%%%-----------------------------------------------------------------------------
%% @doc Renders HTML page for response
-spec render_page(TemplateName :: string(),
                  Req0 :: cowboy_req:req(),
                  State :: macaba_web:html_state()) ->
                     macaba_web:handler_return().

render_page(TemplateName, Req0, State0) ->
  render_page(200, TemplateName, Req0, State0).

%% @doc Renders HTML page for response
-spec render_page(HttpStatus :: integer(),
                  TemplateName :: string(),
                  Req0 :: cowboy_req:req(),
                  State :: macaba_web:html_state()) ->
                     macaba_web:handler_return().

render_page(HttpStatus, TemplateName, Req0,
            State=#mcb_html_state{
              page_vars=PageVars,
              already_rendered=false
             }) ->
  %% lager:debug("Before render: vars=~p", [PageVars]),
  Body = ?MODULE:render(TemplateName, PageVars),
  Headers = [ {<<"Content-Type">>, <<"text/html">>}
            , {<<"Expires">>, <<"0">>}
            ],
  {ok, Req} = cowboy_req:reply(HttpStatus, Headers, Body, Req0),
  {Req, State#mcb_html_state{already_rendered=true}};

render_page(_, _, Req, State=#mcb_html_state{already_rendered=true}) ->
  {Req, State}.

%%%-----------------------------------------------------------------------------
%% @doc Does text/plain response
-spec response_text(HttpStatus :: integer(),
                    Body :: iolist() | binary(),
                    Req0 :: cowboy_req:req(),
                    State :: macaba_web:html_state()) ->
                       macaba_web:handler_return().

response_text(HttpStatus, Body, Req0, State=#mcb_html_state{}) ->
  Headers = [ {<<"Content-Type">>, <<"text/plain">>}
            , {<<"Expires">>, <<"0">>}
            ],
  {ok, Req} = cowboy_req:reply(HttpStatus, Headers, Body, Req0),
  {Req, State}.

%%%-----------------------------------------------------------------------------
%% @doc Does application/json response
-spec response_json(HttpStatus :: integer(),
                    J :: jsx:json_term(),
                    Req0 :: cowboy_req:req(),
                    State :: macaba_web:html_state()) ->
                       macaba_web:handler_return().

response_json(HttpStatus, J, Req0, State=#mcb_html_state{}) ->
  Headers = [ {<<"Content-Type">>, <<"application/json">>}
            , {<<"Expires">>, <<"0">>}
            ],
  Body = jsx:encode(J),
  {ok, Req} = cowboy_req:reply(HttpStatus, Headers, Body, Req0),
  {Req, State}.

%%%-----------------------------------------------------------------------------
%% @doc Redirects user
-spec redirect(URL :: binary()|string(),
               Req0 :: cowboy_req:req(),
               State :: macaba_web:html_state()) ->
                  macaba_web:handler_return().

redirect(URL, Req0, State=#mcb_html_state{}) ->
  {ok, Req} = cowboy_req:reply(
                301, [ {<<"Location">>, macaba:as_binary(URL)}
                     , {<<"Expires">>, <<"0">>}
                     ],
                <<>>, Req0),
  {Req, State}.

%%%-----------------------------------------------------------------------------
%% @doc Redirects user to given thread
-spec redirect_to_thread(BoardId :: binary(),
                         ThreadId :: binary(),
                         Req0 :: cowboy_req:req(),
                         State :: macaba_web:html_state()) ->
                            macaba_web:handler_return().

redirect_to_thread(BoardId, ThreadId, Req, State) ->
   redirect("/board/" ++ macaba:as_string(BoardId) ++ "/thread/"
                ++ macaba:as_string(ThreadId), Req, State).

%%%-----------------------------------------------------------------------------
%% @doc Redirects user to thread and post in it
-spec redirect_to_thread_and_post(
        BoardId :: binary(),
        ThreadId :: binary(),
        PostId :: binary(),
        Req0 :: cowboy_req:req(),
        State :: macaba_web:html_state()) -> macaba_web:handler_return().

redirect_to_thread_and_post(BoardId, ThreadId, PostId, Req, State) ->
   redirect("/board/" ++ macaba:as_string(BoardId) ++ "/thread/"
                ++ macaba:as_string(ThreadId) ++ "#i"
                ++ macaba:as_string(PostId), Req, State).

%%%-----------------------------------------------------------------------------
%% @doc Renders error page with custom message
-spec render_error(Msg0 :: binary()|string(),
                   Req0 :: cowboy_req:req(),
                   State :: macaba_web:html_state()) ->
                      macaba_web:handler_return().

render_error(Msg, Req0, State0) ->
  State1 = state_set_var(error, macaba:as_binary(Msg), State0),
  {Req1, State2} = render_page(400, "error", Req0, State1),
  {Req1, State2}.

%%%-----------------------------------------------------------------------------
%% @doc Sets page_vars for rendering template
-spec state_set_var(K :: atom(), V :: any(),
                    State :: macaba_web:html_state()) ->
                       macaba_web:html_state().

state_set_var(K, V, State = #mcb_html_state{ page_vars=P0 }) ->
  P = orddict:store(K, V, P0),
  State#mcb_html_state{ page_vars = P }.

%%%-----------------------------------------------------------------------------
%% @doc Retrieves value of some page_vars element
-spec state_get_var(K :: atom(), State :: macaba_web:html_state()) -> any().

state_get_var(K, #mcb_html_state{ page_vars=PV }) ->
  orddict:fetch(K, PV).

%%%-----------------------------------------------------------------------------
%% @doc Returns pair of boolean() for POST method and multipart/* content-type
-spec is_POST_and_multipart(Req0 :: cowboy_req:req()) ->
                               {boolean(), boolean()}.

is_POST_and_multipart(Req0) ->
  {Method, Req1} = cowboy_req:method(Req0),
  {CT, _Req2} = cowboy_req:header(<<"content-type">>, Req1),
  Post = case Method of
           <<"POST">>  -> true;
           _ -> false
         end,
  Multipart = case CT of
                <<"multipart/", _/binary>> -> true;
                _ -> false
              end,
  {Post, Multipart}.

%%%-----------------------------------------------------------------------------
%% @doc Retrieves POST body with urlencoded form data and saves it to
%% state.post_data
parse_body_qs(Req0, State0) ->
  {ok, PD0, Req1} = cowboy_req:body_qs(Req0),
  %%PD = [{K, V} || {K, V} <- PD0],
  {Req1, State0#mcb_html_state{ post_data = orddict:from_list(PD0) }}.

%%%-----------------------------------------------------------------------------
%% @doc Retrieves POST body with multipart form data, parses fields and array
%% fields and saves it to state.post_data
parse_multipart_form_data(Req0, State0) ->
  {MPD, Req1} = acc_multipart(Req0),
  %% lager:debug("{{post}} multipart data ~p", [MPD]),
  {Req1, parse_multipart_form_data_1(MPD, State0)}.

%% @private
parse_multipart_form_data_1([], State) -> State;
parse_multipart_form_data_1([{Headers, Value} | Rest],
                            State=#mcb_html_state{ post_data=PD0 }) ->
  FieldName = get_multipart_field_name(Headers),
  PD1 = set_multipart_value(FieldName, Value, PD0),

  ContentType = macaba:propget(<<"content-type">>, Headers, undefined),
  PD = orddict:store({content_type, FieldName}, ContentType, PD1),

  %% lager:debug("parse field ~p value ~p", [FieldName, Value]),
  State1 = State#mcb_html_state{ post_data=PD },
  parse_multipart_form_data_1(Rest, State1).

%% @private
get_multipart_field_name([{<<"content-disposition">>, Bin} | _]) ->
  [<<"form-data">>|Parts] = binary:split(Bin, <<";">>, [global, trim]),
  [Ret] = [begin
             $" = binary:last(Name),
             binary:part(Name, 0, byte_size(Name) - 1)
           end || <<" name=\"", Name/binary>> <- Parts],
  Ret.

%%%-----------------------------------------------------------------------------
%% @doc Sets field value parsed from POST multipart form, if value name starts
%% with "array_" sets it as list instead, accumulating multiple values
set_multipart_value(<<"array_", _/binary>>=Name, Value, []) ->
  orddict:store(Name, [Value], []);
set_multipart_value(<<"array_", _/binary>>=Name, Value, Dict) ->
  try
    X = orddict:fetch(Name, Dict),
    orddict:store(Name, [Value | X], Dict)
  catch _E ->
      orddict:store(Name, [Value], Dict)
  end;
set_multipart_value(Name, Value, Dict) ->
  orddict:store(Name, Value, Dict).

%%%-----------------------------------------------------------------------------
%% @private
acc_multipart(Req) ->
  acc_multipart(cowboy_req:multipart_data(Req), []).
acc_multipart({headers, Headers, Req}, Acc) ->
  acc_multipart(cowboy_req:multipart_data(Req), [{Headers, []}|Acc]);
acc_multipart({body, Data, Req}, [{Headers, BodyAcc}|Acc]) ->
  acc_multipart(cowboy_req:multipart_data(Req),
                [{Headers, [Data|BodyAcc]}|Acc]);
acc_multipart({end_of_part, Req}, [{Headers, BodyAcc}|Acc]) ->
  acc_multipart(cowboy_req:multipart_data(Req),
                [{Headers, list_to_binary(lists:reverse(BodyAcc))}|Acc]);
acc_multipart({eof, Req}, Acc) ->
  {lists:reverse(Acc), Req}.

%%%-----------------------------------------------------------------------------
-spec get_user_save_to_state(Req :: cowboy_req:req(),
                             State :: macaba_web:html_state()) ->
                                macaba_web:handler_return().
%% @doc Attempts to extract cookie and find session with that cookie, else
%% returns anonymous user. Saves result state or clears cookie in request
get_user_save_to_state(Req0, State0) ->
  {User, Req} = get_user(Req0),
  State = state_set_var(user, macaba:record_to_proplist(User), State0),
  %% lager:debug("get_user: coo=~s user=~p", [SesId, User]),
  {Req, State#mcb_html_state{user=User}}.

%%%-----------------------------------------------------------------------------
-spec get_user(Req :: cowboy_req:req()) -> {#mcb_user{}, cowboy_req:req()}.
%% @doc Extracts cookie from request, find user and return user and slightly
%% modified request
get_user(Req0) ->
  {SesId, Req1} = cowboy_req:cookie(ses_cookie_name(), Req0),
  case mcweb_ses:get(SesId) of
    {error, not_found} ->
      %% lager:debug("web:get_user ses '~s' not found", [SesId]),
      Req = clear_session_cookie(Req1),
      {#mcb_user{}, Req};
    {ok, Pid} ->
      U = gen_server:call(Pid, get_user),
      {U, Req1}
  end.

%% @doc Creates session process, sets response cookie, and sets user field
%% in state
create_session_for(U=#mcb_user{}, Req0, State0) ->
  {RemoteAddr, _} = cowboy_req:peer(Req0),
  Opts = [ {remote_addr, RemoteAddr}
         , {user, U}
         ],
  {SesId, _SesPid} = mcweb_ses:new(Opts),
  %% lager:debug("set resp cookie ~p=~p", [ses_cookie_name(), SesId]),
  Req = cowboy_req:set_resp_cookie(
          ses_cookie_name(), SesId, [{path, <<"/">>}], Req0),
  State = State0#mcb_html_state{ user=U },
  {Req, State}.

%% @doc Gets ses cookie name from config
ses_cookie_name() ->
  {ok, CookieName} = macaba_conf:get([<<"board">>, <<"session_cookie_name">>]),
  CookieName.

%%%------------------------------------------------------------------------
%% @doc Extract user identification from Request, such as IP address, Accept
%% and User-Agent fields
get_user_identification(Req0) ->
  {{{IP1, IP2, IP3, IP4}, _Port}, Req1} = cowboy_req:peer(Req0),
  IP = iolist_to_binary([integer_to_list(IP1), $., integer_to_list(IP2), $.,
                         integer_to_list(IP3), $., integer_to_list(IP4)]),
  {XForw, Req2} = cowboy_req:header(<<"x-forwarded-for">>, Req1),
  {UA, Req3} = cowboy_req:header(<<"user-agent">>, Req2),
  {Accept, _Req4} = cowboy_req:header(<<"accept">>, Req3),
  #mcb_userid{
                 ip           = IP
               , ip_num       = ip_to_integer({IP1, IP2, IP3, IP4})
               , proxy_ip     = XForw
               , proxy_ip_num = ip_to_integer(XForw)
               , user_agent   = UA
               , accept       = Accept
               , tor_detected = is_tor_node(IP)
             }.

ip_to_integer(X) when is_binary(X) ->
  ip_to_integer(binary_to_list(X));
ip_to_integer(X) when is_list(X) ->
  X1 = lists:takewhile(fun(C) -> C =/= $, end, X),
  case inet_parse:address(X1) of
    {ok, X2} -> ip_to_integer(X2);
    _ -> 0
  end;
ip_to_integer({A,B,C,D}) ->
  A*16777216+B*65536+C*256+D;
ip_to_integer(_) -> 0.

%%%------------------------------------------------------------------------
%% @doc TODO: Detect if the IP belongs to one of TOR exit nodes
%% https://www.dan.me.uk/torlist/
is_tor_node(_IP) -> false.

%%%------------------------------------------------------------------------
%% @doc Using user IP and user-agent
get_poster_id(#mcb_userid{ ip=IP, proxy_ip=ProxyIP,
                           user_agent=UA, accept=Accept }) ->
  %% NOTE: To reduce sensitivity of poster_id, remove some info from the
  %% tuple below
  Id0 = erlang:term_to_binary({IP, ProxyIP, UA, Accept}),
  %% split 160 bits of sha evenly and bxor together
  <<Id1:53, Id2:53, Id3:53, _:1>> = crypto:sha(Id0),
  Id = Id1 bxor Id2 bxor Id3,
  get_poster_id_encode(Id, []).

%% @doc Encode a long integer in base62
get_poster_id_encode(0, A) -> iolist_to_binary(A);
get_poster_id_encode(X, A) ->
  Ch = case X rem 62 of
        C when C < 10 -> $0 + C;
        C when C < 36 -> $A + C - 10;
        C -> $a + C - 36
       end,
  get_poster_id_encode(X div 62, [Ch | A]).


%%%------------------------------------------------------------------------
%% @doc Gets user from ses cookie, fails if user is below FailIfLevelBelow. To
%% use in chain handlers for authenticated users
%% To add in chain (example):
%% {_, Req, State} = mcweb:chain_run(
%% [fun(R, S) -> mcweb:chain_fail_if_level_below(R, S, ?USERLEVEL_ADMIN) end]
%% );
chain_fail_if_level_below(Req0, State0, FailIfLevelBelow) ->
  User = State0#mcb_html_state.user,
  #mcb_user{level=Level} = User,
  case Level < FailIfLevelBelow of
    true ->
      mcweb:chain_fail(
        mcweb:render_error(<<"User level is too low">>, Req0, State0));
    _ ->
      mcweb:chain_success(Req0, State0)
  end.

chain_fail_if_below_admin(Req0, State0) ->
  mcweb:chain_fail_if_level_below(Req0, State0, ?USERLEVEL_ADMIN).

chain_fail_if_below_mod(Req0, State0) ->
  mcweb:chain_fail_if_level_below(Req0, State0, ?USERLEVEL_MOD).

%%%------------------------------------------------------------------------
%% @doc Ensures safe contents for HTML and max length
safe(T, Size) ->
  safe_length(safe_htmlencode(T, []), Size).

%% @doc Ensures max length, cuts extra
safe_length(T, Size) when is_binary(T), byte_size(T) < Size -> T;
safe_length(T, Size) when is_binary(T) -> binary:part(T, {0, Size}).

safe_htmlencode(<<>>, Accum) -> iolist_to_binary(lists:reverse(Accum));
safe_htmlencode(<<$<, Tail/binary>>, Accum) ->
  safe_htmlencode(Tail, ["&lt;" | Accum]);
safe_htmlencode(<<$&, Tail/binary>>, Accum) ->
  safe_htmlencode(Tail, ["&amt;" | Accum]);
safe_htmlencode(<<$", Tail/binary>>, Accum) ->
  safe_htmlencode(Tail, ["&quot;" | Accum]);
safe_htmlencode(<<$', Tail/binary>>, Accum) ->
  safe_htmlencode(Tail, ["&apos;" | Accum]);
safe_htmlencode(<<$>, Tail/binary>>, Accum) ->
  safe_htmlencode(Tail, ["&gt;" | Accum]);
safe_htmlencode(<<X:8, Tail/binary>>, Accum) ->
  safe_htmlencode(Tail, [X | Accum]).

%%% Local Variables:
%%% erlang-indent-level: 2
%%% End:
