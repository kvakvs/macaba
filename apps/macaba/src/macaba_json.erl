%%%-----------------------------------------------------------------------------
%%% @doc Converts things to JSON and back.
%%% @version 2013-03-14
%%% @author Dmytro Lytovchenko <kvakvs@yandex.ru>
%%%-----------------------------------------------------------------------------
-module(macaba_json).

-export([ to_json/1
        , from_json/2
        ]).

-include_lib("macaba/include/macaba_types.hrl").

%%%-----------------------------------------------------------------------------
to_json(_) -> erlang:throw({error, badarg}).

%%%-----------------------------------------------------------------------------
from_json(mcb_board, J) when is_list(J) ->
  %% TODO: Get board by board_id and update it if some fields were missing?
  %% or any other sort of validation, json schema?
  BI = macaba:as_binary(macaba:propget(<<"board_id">>, J)),
  Ca = macaba:as_binary(macaba:propget(<<"category">>, J)),
  T = macaba:as_binary(macaba:propget(<<"title">>, J)),
  SN = macaba:as_binary(macaba:propget(<<"short_name">>, J)),
  AN = macaba:as_binary(macaba:propget(<<"anonymous_name">>, J)),
  MT = macaba:as_integer(macaba:propget(<<"max_threads">>, J)),
  MTP = macaba:as_integer(macaba:propget(<<"max_thread_posts">>, J)),
  MTPL = macaba:as_integer(macaba:propget(<<"max_thread_post_lock">>, J)),
  PI = macaba:as_binary(macaba:propget(<<"poster_id">>, J)),
  PIST = macaba:as_binary(macaba:propget(<<"poster_id_sage_text">>, J)),
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
