-module(macaba).

-export([ ensure_started/1
        , propget/2
        , propget/3
        , as_string/1
        , as_bool/1
        , as_binary/1
        , as_ipv4/1
        , as_existing_atom/1
        ]).

-include_lib("macaba/include/macaba_types.hrl").

%%--------------------------------------------------------------------
propget(K, Proplist) ->
	case lists:keyfind(K, 1, Proplist) of
		false ->
			undefined;
		{K, V} ->
			V
	end.

%%--------------------------------------------------------------------
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
as_bool(X) when is_list(X) ->
    case catch list_to_existing_atom(X) of
        Y when is_boolean(Y) -> Y;
        _                    -> false
    end.            

%%--------------------------------------------------------------------
-spec as_binary(X :: any()) -> binary().
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
    Str = binary_to_list(A)
    , try list_to_existing_atom(Str)
      catch error:badarg -> undefined
      end;
as_existing_atom(Str) when is_list(Str) ->
    try list_to_existing_atom(Str)
    catch error:badarg -> undefined
    end.

%%--------------------------------------------------------------------
