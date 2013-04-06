%%%-----------------------------------------------------------------------------
%%% @doc Attach storage behaviour
%%% @version 2013-03-19
%%% @author Dmytro Lytovchenko <kvakvs@yandex.ru>
%%%-----------------------------------------------------------------------------
-module(gen_macaba_storage).
-export([behaviour_info/1]).

behaviour_info(callbacks) ->
  [ {start_storage, 0}
  , {stop_storage, 1}
  , {exists, 1}
  , {delete, 1}
  , {read_header, 1}
  , {read_body, 1}
  , {write_header, 1}
  , {write_body, 1}
  ];

behaviour_info(_) ->
  undefined.


%%% Local Variables:
%%% erlang-indent-level: 2
%%% End:
