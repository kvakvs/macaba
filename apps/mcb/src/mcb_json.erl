%%%-----------------------------------------------------------------------------
%%% @doc Converts things to JSON and back.
%%% @version 2013-03-14
%%% @author Dmytro Lytovchenko <kvakvs@yandex.ru>
%%%-----------------------------------------------------------------------------
-module(mcb_json).

-export([ to_json/1
        , from_json/2
        ]).

-include_lib("mcb/include/macaba_types.hrl").

%%%-----------------------------------------------------------------------------
to_json(_) -> erlang:throw({error, badarg}).

%%%-----------------------------------------------------------------------------
from_json(mcb_board, J) when is_list(J) ->
  %% TODO: Get board by board_id and update it if some fields were missing?
  %% or any other sort of validation, json schema?
  BI = mcb:as_binary(mcb:propget(<<"board_id">>, J)),
  Ca = mcb:as_binary(mcb:propget(<<"category">>, J)),
  T = mcb:as_binary(mcb:propget(<<"title">>, J)),
  SN = mcb:as_binary(mcb:propget(<<"short_name">>, J)),
  AN = mcb:as_binary(mcb:propget(<<"anonymous_name">>, J)),
  MT = mcb:as_integer(mcb:propget(<<"max_threads">>, J)),
  MTP = mcb:as_integer(mcb:propget(<<"max_thread_posts">>, J)),
  MTPL = mcb:as_integer(mcb:propget(<<"max_thread_post_lock">>, J)),
  PI = mcb:as_binary(mcb:propget(<<"poster_id">>, J)),
  PIST = mcb:as_binary(mcb:propget(<<"poster_id_sage_text">>, J)),
  #mcb_board{ board_id = BI,
              category = Ca,
              title = T,
              short_name = SN,
              anonymous_name = AN,
              max_threads = MT,
              max_thread_posts = MTP,
              max_thread_post_lock = MTPL,
              poster_id = PI,
              poster_id_sage_text = PIST
            };

from_json(_, _) -> erlang:throw({error, badarg}).

%%% Local Variables:
%%% erlang-indent-level: 2
%%% End:
