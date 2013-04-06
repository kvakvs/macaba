%%%------------------------------------------------------------------------
%%% @doc Attachment data model, represents attachments and actions
%%% @version 2013-03-09
%%% @author Dmytro Lytovchenko <kvakvs@yandex.ru>
%%%------------------------------------------------------------------------
-module(macaba_attach).

-export([ write/1
        , write_thumbnail/2
        , detect_content_type/1
        ]).

-include_lib("macaba/include/macaba_types.hrl").

%%%-----------------------------------------------------------------------------
%% @private
%% @doc Writes to database, no unique checks or existence check
-spec write(Data :: binary()) ->
               {ok, Key::binary()} | {error, any()}.

write(<<>>) -> {error, no_data};
write(Data) when is_binary(Data) ->
  case ?MODULE:detect_content_type(Data) of
    {error, ContentTypeError} ->
      {error, {content_type, ContentTypeError}};

    {ok, ContentType} ->
      ThumbnailFun = get_thumbnail_fun(),
      %%case ThumbnailFun(ContentType, Data) of
        %% {ok, {ThumbKey, ThumbSize}} ->
      {ok, {ThumbKey, ThumbSize}} = ThumbnailFun(ContentType, Data),
      Key = crypto:sha(Data),
      CTime = calendar:local_time(),
      Created = erlang:localtime_to_universaltime(CTime),
      ETag = mcweb:create_and_format_etag({Key, CTime, byte_size(Data)}),
      A = #mcb_attachment{
          size           = byte_size(Data)
        , hash           = Key
        , content_type   = ContentType
        , thumbnail_hash = ThumbKey
        , thumbnail_size = ThumbSize
        , created        = Created
        , etag           = ETag
       },
      AttachMod = macaba_plugins:mod(attachments),
      AttachMod:write_header(A),
      B = #mcb_attachment_body{
        key  = Key,
        data = Data
       },
      AttachMod:write_body(B),
      {ok, Key}

      %%   {error, Err} -> {error, Err}
      %% end
  end.

%%%-----------------------------------------------------------------------------
%% @private
%% @doc Queries config for if thumbnailer was enabled or disabled, and returns
%% wrapper fun for generating a real or fake thumbnail
-type thumbnail_fun_t() :: fun((binary(), binary()) ->
                                  {ok, {_, _}} | {error, _}).
-spec get_thumbnail_fun() -> thumbnail_fun_t().
get_thumbnail_fun() ->
  {ok, ThumbnailerEnabled} = macaba_conf:get(
                               [<<"board">>, <<"thumbnailer">>], true),
  case ThumbnailerEnabled of
    true ->
      fun(CT, D) -> write_thumbnail(CT, D) end;
    false ->
      fun(_, _) -> {ok, {<<>>, 0}} end
  end.


%%%-----------------------------------------------------------------------------
%% @doc Detects content type by data first bytes, creates thumbnail with
%% imagemagick, writes thumbnail to attachment storage
-spec write_thumbnail(ContentType :: atom()|binary(),
                      Data :: binary()) ->
                         {ok, {RiakKey :: binary(), Sz :: integer()}}.
%% write_thumbnail(empty, _)   -> {error, no_data};
%% write_thumbnail(no_idea, _) -> {error, unknown_content_type};
write_thumbnail(<<"image/gif">>,  Data) -> write_thumbnail_1(gif, Data);
write_thumbnail(<<"image/png">>,  Data) -> write_thumbnail_1(png, Data);
write_thumbnail(<<"image/jpeg">>, Data) -> write_thumbnail_1(jpg, Data).

%% @private
-spec write_thumbnail_1(TypeAtom :: atom(), Data :: binary()) ->
                           {ok, {RiakKey :: binary(), Sz :: integer()}}.
write_thumbnail_1(TypeAtom, Data) ->
  {ok, Image} = eim:load(Data),
  %% Image = case TypeAtom of gif -> convert_to_jpeg(Image0); _ -> Image0 end,
  {ok, FitH} = macaba_conf:get([<<"board">>, <<"thread">>,
                                <<"thumbnail_height">>]),
  {ok, FitW} = macaba_conf:get([<<"board">>, <<"thread">>,
                               <<"thumbnail_width">>]),
  TData = eim:derive(Image, TypeAtom, {fit, FitW, FitH}),
  TDigest = crypto:sha(TData),
  TBody = #mcb_attachment_body{
    key  = TDigest,
    data = TData
   },
  AttachMod = macaba_plugins:mod(attachments),
  AttachMod:write_body(TBody),
  {ok, {TDigest, byte_size(TData)}}.

%%%-----------------------------------------------------------------------------
%% @doc Attempts to figure out file type by first bytes of data
-spec detect_content_type(binary()) -> {error, empty | no_idea}
                                         | {ok, binary()}.

detect_content_type(<<>>) ->
  {error, empty};
detect_content_type(<<"GIF87a", _/binary>>) ->
  {ok, <<"image/gif">>};
detect_content_type(<<"GIF89a", _/binary>>) ->
  {ok, <<"image/gif">>};
detect_content_type(<<16#ff, 16#d8, 16#ff, 16#e0, _:16, "JFIF", 0,
                      _/binary>>) ->
  {ok, <<"image/jpeg">>}; % jpeg without EXIF
detect_content_type(<<16#ff, 16#d8, 16#ff, 16#e1, _:16, "Exif", 0,
                      _/binary>>) ->
  {ok, <<"image/jpeg">>}; % jpeg with EXIF
detect_content_type(<<16#ff, 16#d8, 16#ff, 16#e9, _:16, "SPIFF", 0,
                      _/binary>>) ->
  {ok, <<"image/jpeg">>}; % jpeg
detect_content_type(<<137, 80, 78, 71, 13, 10, 26, 10, _/binary>>) ->
  {ok, <<"image/png">>};
detect_content_type(_) ->
  {error, no_idea}.

%%%-----------------------------------------------------------------------------

%%% Local Variables:
%%% erlang-indent-level: 2
%%% End:
