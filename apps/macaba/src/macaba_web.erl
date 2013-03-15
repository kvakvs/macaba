%%%------------------------------------------------------------------------
%%% @doc Utility functions for web server and templates
%%% Created: 2013-02-17 Dmytro Lytovchenko <kvakvs@yandex.ru>
%%%------------------------------------------------------------------------
-module(macaba_web).

-export([ handle_helper/3
        , compile/1
        , render/2
        , chain_run/2
        , get_poster_id/1
        , render_page/3, render_page/4
        , response_text/4
        , redirect/3
        , redirect_to_thread/4
        , redirect_to_thread_and_post/5
        , render_error/3
        , state_set_var/3
        , state_get_var/2
        , is_POST_and_multipart/1
        , parse_body_qs/2
        , parse_multipart_form_data/2
        , get_user/2
        , create_session_for/3
        , ses_cookie_name/0
        ]).

-include_lib("macaba/include/macaba_types.hrl").

%%%------------------------------------------------------------------------
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
    {Req3, State2} = macaba_web:get_user(Req2, State1),

    %% site offline flag
    %% TODO: cache site config in memory or in state
    #mcb_site_config{ offline=SiteOffline } = macaba_board:get_site_config(),
    State3 = State2#mcb_html_state{ site_offline = SiteOffline },
    State4 = ?MODULE:state_set_var(site_offline, SiteOffline, State3),

    FnName = macaba:as_atom("macaba_handle_" ++ macaba:as_string(Mode)),
    {Req4, State5} = apply(Module, FnName, [Method, {Req3, State4}]),
    {ok, Req4, State5}
  catch
    E -> T = lists:flatten(io_lib:format("handle error: ~p ~p",
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
  erlydtl:compile("templates/" ++ TplName ++ ".dtl", TplModule,
                  [ verbose
                  , {out_dir, "templates/ebin"}
                  , {doc_root, "templates/"}
                  , {custom_tags_dir, "templates/custom_tags"}
                  ]),
  TplModule.

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
-spec chain_run([fun()], State :: any()) ->
                   {ok, any()} | {error, any(), any()}.
chain_run([], State) -> {ok, State};
chain_run([F | Tail], State) ->
  case F(State) of
    {ok, State2} ->
      chain_run(Tail, State2);
    {error, State2} ->
      {error, State2}
  end.

%%%------------------------------------------------------------------------
%% @doc Using user IP and user-agent
get_poster_id(Req0) ->
  {{{IP1, IP2, IP3, _IP4}, _Port}, Req1} = cowboy_req:peer(Req0),
  {UA, _Req2} = cowboy_req:header(<<"user-agent">>, Req1),
  Id0 = erlang:term_to_binary({IP1, IP2, IP3, UA}),
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

%%%-----------------------------------------------------------------------------
%% @doc Renders HTML page for response
render_page(TemplateName, Req0, State0) ->
  render_page(200, TemplateName, Req0, State0).

%% @doc Renders HTML page for response
render_page(HttpStatus, TemplateName, Req0,
            State=#mcb_html_state{
              page_vars=PageVars,
              already_rendered=false
             }) ->
  %% lager:debug("Before render: vars=~p", [PageVars]),
  Body = macaba_web:render(TemplateName, PageVars),
  Headers = [ {<<"Content-Type">>, <<"text/html">>}
            , {<<"Expires">>, <<"0">>}
            ],
  {ok, Req} = cowboy_req:reply(HttpStatus, Headers, Body, Req0),
  {Req, State#mcb_html_state{already_rendered=true}};

render_page(_, _, Req, State=#mcb_html_state{already_rendered=true}) ->
  {Req, State}.

%%%-----------------------------------------------------------------------------
%% @doc Does text/plain response
response_text(HttpStatus, Body, Req0, State=#mcb_html_state{}) ->
  Headers = [ {<<"Content-Type">>, <<"text/plain">>}
            , {<<"Expires">>, <<"0">>}
            ],
  {ok, Req} = cowboy_req:reply(HttpStatus, Headers, Body, Req0),
  {Req, State}.

%%%-----------------------------------------------------------------------------
%% @doc Redirects user
redirect(URL, Req0, State=#mcb_html_state{}) when is_tuple(Req0) ->
  {ok, Req} = cowboy_req:reply(
                301, [ {<<"Location">>, macaba:as_binary(URL)}
                     , {<<"Expires">>, <<"0">>}
                     ],
                <<>>, Req0),
  {Req, State}.

%%%-----------------------------------------------------------------------------
%% @doc Redirects user to given thread
redirect_to_thread(BoardId, ThreadId, Req, State) ->
   redirect("/board/" ++ macaba:as_string(BoardId) ++ "/thread/"
                ++ macaba:as_string(ThreadId), Req, State).

%%%-----------------------------------------------------------------------------
%% @doc Redirects user to thread and post in it
redirect_to_thread_and_post(BoardId, ThreadId, PostId, Req, State) ->
   redirect("/board/" ++ macaba:as_string(BoardId) ++ "/thread/"
                ++ macaba:as_string(ThreadId) ++ "#i"
                ++ macaba:as_string(PostId), Req, State).

%%%-----------------------------------------------------------------------------
%% @doc Renders error page with custom message
render_error(Msg, Req0, State0) ->
  State1 = state_set_var(error, Msg, State0),
  {Req1, State2} = render_page(400, "error", Req0, State1),
  {error, {Req1, State2}}.

%%%-----------------------------------------------------------------------------
%% @doc Sets page_vars for rendering template
state_set_var(K, V, State = #mcb_html_state{ page_vars=P0 }) ->
  P = orddict:store(K, V, P0),
  State#mcb_html_state{ page_vars = P }.

%% @doc Retrieves value of some page_vars element
state_get_var(K, #mcb_html_state{ page_vars=PV }) ->
  orddict:fetch(K, PV).

%%%-----------------------------------------------------------------------------
%% @doc Returns pair of boolean() for POST method and multipart/* content-type
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
%% @doc Attempts to extract cookie and find session with that cookie, else
%% returns anonymous user
-spec get_user(Req :: cowboy_req:req(), State :: #mcb_html_state{}) ->
                  {cowboy_req:req(), #mcb_html_state{}}.
get_user(Req0, State0) ->
  {SesId, Req} = cowboy_req:cookie(ses_cookie_name(), Req0),
  User = case macaba_ses:get(SesId) of
           {error, not_found} ->
             lager:debug("web:get_user ses '~s' not found", [SesId]),
             #mcb_user{};
           {ok, Pid} ->
             gen_server:call(Pid, get_user)
         end,
  State = state_set_var(user, macaba:record_to_proplist(User), State0),
  %% lager:debug("get_user: coo=~s user=~p", [SesId, User]),
  {Req, State#mcb_html_state{user=User}}.

%% @doc Creates session process, sets response cookie, and sets user field
%% in state
create_session_for(U=#mcb_user{}, Req0, State0) ->
  {RemoteAddr, _} = cowboy_req:peer(Req0),
  Opts = [ {remote_addr, RemoteAddr}
         , {user, U}
         ],
  {SesId, _SesPid} = macaba_ses:new(Opts),
  %% lager:debug("set resp cookie ~p=~p", [ses_cookie_name(), SesId]),
  Req = cowboy_req:set_resp_cookie(
          ses_cookie_name(), SesId, [{path, <<"/">>}], Req0),
  State = State0#mcb_html_state{ user=U },
  {Req, State}.

%% @doc Gets ses cookie name from config
ses_cookie_name() ->
  {ok, CookieName} = macaba_conf:get([<<"board">>, <<"session_cookie_name">>]),
  CookieName.

%%% Local Variables:
%%% erlang-indent-level: 2
%%% End:
