%%%------------------------------------------------------------------------
%%% @doc Contains a few utility functions for which I've found no better place
%%% Created: 2013-02-16 Dmytro Lytovchenko <kvakvs@yandex.ru>
%%%------------------------------------------------------------------------
-module(macaba).

-export([ ensure_started/1
        , as_atom/1
        , as_string/1
        , as_bool/1
        , as_binary/1
        , as_integer/1
        , as_ipv4/1
        , as_existing_atom/1
        , pagination/3
        , propget/2
        , propget/3
        %% , propget_binary/2
        %% , propget_binary/3
        , record_to_proplist/1
        , hexstr_to_bin/1
        , fatal/2
        %% ANSI ESC codes for color and bold in console
        , color/1, endfont/0, font/1
        , unix_now/0
        ]).

-include_lib("macaba/include/macaba_types.hrl").

%%-----------------------------------------------------------------------------
record_to_proplist(#mcb_board{}=Rec) ->
  lists:zip(record_info(fields, mcb_board), tl(tuple_to_list(Rec)));

record_to_proplist(#mcb_thread{}=Rec) ->
  lists:zip(record_info(fields, mcb_thread), tl(tuple_to_list(Rec)));

record_to_proplist(#mcb_post{}=Rec) ->
  UTC = calendar:gregorian_seconds_to_datetime(Rec#mcb_post.created
                                               + 1970*365*86400),
  Local = calendar:universal_time_to_local_time(UTC),
  lists:zip(record_info(fields, mcb_post), tl(tuple_to_list(Rec)))
    ++ [{created_local, Local}, {created_utc, UTC}];

record_to_proplist(#mcb_attachment{}=Rec) ->
  HashHex = bin_to_hex:bin_to_hex(Rec#mcb_attachment.hash),
  THashHex = bin_to_hex:bin_to_hex(Rec#mcb_attachment.thumbnail_hash),
  lists:zip(record_info(fields, mcb_attachment), tl(tuple_to_list(Rec)))
    ++ [{hash_hex, HashHex}, {thumbnail_hash_hex, THashHex}];

record_to_proplist(#mcb_attachment_body{}=Rec) ->
  lists:zip(record_info(fields, mcb_attachment_body), tl(tuple_to_list(Rec)));

record_to_proplist(#mcb_user{}=Rec) ->
  lists:zip(record_info(fields, mcb_user), tl(tuple_to_list(Rec)));

record_to_proplist(X) -> error({badarg, X}).

%% unix_time_to_str(U) ->
%%   DT = calendar:gregorian_seconds_to_datetime(U + 1970*365*86400),
%%   {{Year,Month,Day},{Hour,Min,Sec}} = DT,
%%   F = io_lib:format("~4.10.0B-~2.10.0B-~2.10.0B ~2.10.0B:~2.10.0B:~2.10.0B",
%%         [Year, Month, Day, Hour, Min, Sec]),
%%   lists:flatten(F).

%% -spec propget_binary(K :: any(), Prop :: proplist_t()) -> binary().
%% propget_binary(K, Prop) -> as_binary(propget(K, Prop)).
%% -spec propget_binary(K :: any(), Prop :: proplist_t(),
%%                      Default :: binary()) -> binary().
%% propget_binary(K, Prop, Default) -> as_binary(propget(K, Prop, Default)).

%%-----------------------------------------------------------------------------
%% @doc Faster replacement for proplists:get_value
propget(K, Proplist) ->
  case lists:keyfind(K, 1, Proplist) of
    false ->
      undefined;
    {K, V} ->
      V
  end.

%%-----------------------------------------------------------------------------
%% @doc Faster replacement for proplists:get_value with default value
propget(K, Proplist, Default) ->
  case lists:keyfind(K, 1, Proplist) of
    false ->
      Default;
    {K, V} ->
      V
  end.

%%-----------------------------------------------------------------------------
ensure_started(App) ->
  ensure_started_1(App, 25).

ensure_started_1(_, 0) -> erlang:error({macaba, ensure_started, retries_count});
ensure_started_1(App, Retries) ->
  case application:start(App) of
    ok ->
      ok;
    {error, {already_started, App}} ->
      ok;
    {error, {not_started, Dependency}} ->
      ensure_started_1(Dependency, Retries-1),
      ensure_started_1(App, Retries-1)
  end.

%%-----------------------------------------------------------------------------
as_atom(A) when is_atom(A) -> A;
as_atom(A) when is_list(A) -> list_to_existing_atom(A);
as_atom(A) when is_binary(A) -> binary_to_existing_atom(A, latin1).

%%-----------------------------------------------------------------------------
as_string(X) when is_atom(X) -> atom_to_list(X);
as_string(X) when is_list(X) -> X;
as_string(X) when is_integer(X) -> integer_to_list(X);
as_string(X) when is_binary(X) -> binary_to_list(X);
as_string(X) when is_boolean(X) ->
    case X of true -> "true"; false -> "false" end.

%%-----------------------------------------------------------------------------
%% @doc Converts string, integer() binary() to bool 'true'|'false'
-spec as_bool(X :: boolean() | binary() | list() | integer()) -> boolean().
as_bool(X) when is_boolean(X) -> X;
as_bool(0) -> false;
as_bool(X) when is_integer(X) -> true;
as_bool(X) when is_binary(X) -> as_bool(binary_to_list(X));
as_bool("1") -> true;
as_bool("0") -> false;
as_bool("true") -> true;
as_bool("false") -> false.

%%--------------------------------------------------------------------
-spec as_binary(X :: atom() | integer() | binary() | list()) -> binary().
as_binary(X) when is_atom(X) -> atom_to_binary(X, latin1);
as_binary(X) when is_integer(X) -> list_to_binary(integer_to_list(X));
as_binary(X) when is_binary(X) -> X;
as_binary(X) when is_list(X) -> list_to_binary(X);
as_binary(X) -> as_binary(io_lib:format("~p", [X])).

%%-----------------------------------------------------------------------------
%% @doc Converts string or binary() to ipv4 {X,X,X,X}
-spec as_ipv4(Bin :: binary() | list()) -> ipv4_t().

as_ipv4(A) when is_atom(A) ->
  Str = atom_to_list(A),
  {ok, Ip} = inet_parse:ipv4_address(Str),
  Ip;

as_ipv4(Bin) when is_binary(Bin) ->
  Str = binary_to_list(Bin),
  {ok, Ip} = inet_parse:ipv4_address(Str),
  Ip;

as_ipv4(Str) when is_list(Str) ->
  {ok, Ip} = inet_parse:ipv4_address(Str),
  Ip.

%%-----------------------------------------------------------------------------
%% @doc Converts integer, list or binary to integer
-spec as_integer(Value :: binary() | string() | integer()) -> integer().
as_integer(Value)
  when is_integer(Value) ->
  Value;
as_integer(Value)
  when is_binary(Value) ->
  {Num,_} = string:to_integer(binary_to_list(Value)),
  Num;
as_integer(Value)
  when is_list(Value) ->
  {Num,_} = string:to_integer(Value),
  Num.

%%-----------------------------------------------------------------------------
%% @doc Converts string or binary() to existing atom, or undefined is returned
-spec as_existing_atom(A :: binary() | list() | atom()) -> atom().
as_existing_atom(A) when is_atom(A) -> A;
as_existing_atom(A) when is_binary(A) ->
  Str = binary_to_list(A),
  try list_to_existing_atom(Str)
  catch error:badarg -> undefined end;
as_existing_atom(Str) when is_list(Str) ->
  try list_to_existing_atom(Str)
  catch error:badarg -> undefined end.

%%--------------------------------------------------------------------
%% @doc Pagination helper, cuts piece of list according to Page id
%% TODO: move out of this module
-spec pagination(L :: list(), Page :: integer(),
                 PageSize :: integer()) -> list().
pagination(_L, Page, _PageSize) when Page =< 0 -> [];
pagination(L, Page, PageSize) when (Page-1) * PageSize > length(L) -> [];
pagination(L, 1, PageSize0) ->
  PageSize1 = min(PageSize0, length(L)),
  {H, _} = lists:split(PageSize1, L),
  H;
pagination(L, Page, PageSize0) ->
  PageSize1 = min(PageSize0, length(L)),
  {_, T} = lists:split(PageSize1 * (Page-1), L),
  PageSize2 = min(PageSize0, length(T)),
  {H, _} = lists:split(PageSize2, T),
  H.

%% hex(N) when N < 10 ->
%%   $0+N;
%% hex(N) when N >= 10, N < 16 ->
%%   $a+(N-10).

int(C) when $0 =< C, C =< $9 ->
  C - $0;
int(C) when $A =< C, C =< $F ->
  C - $A + 10;
int(C) when $a =< C, C =< $f ->
  C - $a + 10.

%% to_hex(N) when N < 256 ->
%%   [hex(N div 16), hex(N rem 16)].

%% list_to_hexstr([]) -> 
%%   [];
%% list_to_hexstr([H|T]) ->
%%   to_hex(H) ++ list_to_hexstr(T).

%% bin_to_hexstr(Bin) ->
%%   list_to_hexstr(binary_to_list(Bin)).

hexstr_to_bin(S) ->
  list_to_binary(hexstr_to_list(S)).

hexstr_to_list([X,Y|T]) ->
  [int(X)*16 + int(Y) | hexstr_to_list(T)];
hexstr_to_list([]) ->
  [].


%% @doc ANSI ESCape color codes: reset font color/weight
endfont() -> [27 | "[0m"].
%% @doc ANSI ESCape color codes: font weight
font(bold) -> [27 | "[1;00m"];
font(thin) -> [27 | "[1;01m"].
%% @doc ANSI ESCape color codes: font color
color(black) -> [27 | "[1;30m"];
color(red) -> [27 | "[1;31m"];
color(green) -> [27 | "[1;32m"];
color(yellow) -> [27 | "[1;33m"];
color(blue) -> [27 | "[1;34m"];
color(magenta) -> [27 | "[1;35m"];
color(cyan) -> [27 | "[1;36m"];
color(white) -> [27 | "[1;37m"].

%% @doc Some error has happened, further execution has no sense - suicide
fatal(Msg, Err) ->
  io:format(standard_error, "~s===================================~n"
            "~s~s: ~p~s~n"
            "===================================~s~n",
            [color(white), color(red), Msg, Err, color(white), endfont()]),
  init:stop(),
  error(config_parse_error).

unix_now() ->
  {Mega, S, _} = os:timestamp(),
  Mega * 1000000 + S.

%%% Local Variables:
%%% erlang-indent-level: 2
%%% End:
