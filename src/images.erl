-module(images).
-export([
          magic/1,

          frame_to_web/1,
          frame_to_web/2,

          get_frame_size/2
        ]).

-include("eknife.hrl").

% image type

-define(M_MARK, 16#FF).
-define(M_SOF0, 16#C0).      %% Start Of Frame N
-define(M_SOF1, 16#C1).      %% N indicates which compression process
-define(M_SOF2, 16#C2).      %% Only SOF0-SOF2 are now in common use
-define(M_SOF3, 16#C3).
-define(M_SOF5, 16#C5).      %% NB: codes C4 and CC are NOT SOF markers
-define(M_SOF6, 16#C6).
-define(M_SOF7, 16#C7).
-define(M_SOF9, 16#C9).
-define(M_SOF10,16#CA).
-define(M_SOF11,16#CB).
-define(M_SOF13,16#CD).
-define(M_SOF14,16#CE).
-define(M_SOF15,16#CF).
-define(M_DHT,  16#C4).
-define(M_RST0, 16#D0).
-define(M_SOI,  16#D8).       %% Start Of Image (beginning of datastream)
-define(M_EOI,  16#D9).       %% End Of Image (end of datastream)
-define(M_SOS,  16#DA).       %% Start Of Scan (begins compressed data)
-define(M_DQT,  16#DB).
-define(M_DRI,  16#DD).
-define(M_JFIF, 16#E0).       %% Jfif marker
-define(M_EXIF, 16#E1).       %% Exif marker
-define(M_COM,  16#FE).       %% COMment 

-spec magic(binary()) -> undefined | binary().
magic(<<?M_MARK, ?M_SOI, ?M_MARK, ?M_EXIF, _Len:16, "Exif", 0, 0, _/binary>>) -> <<"jpg">>;
magic(<<?M_MARK, ?M_SOI, ?M_MARK, ?M_JFIF, _Len:16, "JFIF", _, _, _/binary>>) -> <<"jpg">>;
magic(<<?M_MARK, ?M_SOI, ?M_MARK, ?M_DQT, _/binary>>) -> <<"jpg">>;
magic(<<?M_MARK, ?M_SOI, ?M_MARK, ?M_DHT, _/binary>>) -> <<"jpg">>;
magic(<<?M_MARK, ?M_SOI, ?M_MARK, ?M_SOF0, _/binary>>) -> <<"jpg">>;
magic(<<?M_MARK, ?M_SOI, ?M_MARK, ?M_SOS, _/binary>>) -> <<"jpg">>;
magic(<<?M_MARK, ?M_SOI, ?M_MARK, ?M_COM, _/binary>>) -> <<"jpg">>;
magic(<<?M_MARK, ?M_SOI, ?M_MARK, ?M_EXIF, _/binary>>) -> <<"jpg">>;
magic(<<$G, $I, $F, $8, $7, $a, _/binary>>) -> <<"gif">>;
magic(<<$G, $I, $F, $8, $9, $a, _/binary>>) -> <<"gif">>;
magic(<<137, $P, $N, $G, $\r, $\n, 26, $\n, _/binary>>) -> <<"png">>;
magic(_) -> undefined.


% web encode

-spec frame_to_web(binary()) -> binary().
frame_to_web(Data) ->
  FrameB = base64:encode(Data),
  <<"data:image/jpeg;base64,", FrameB/binary>>.

-spec frame_to_web(list(map()), string()) -> list(map()).
frame_to_web(List, Field) ->
  lists:map(fun(I) -> 
      maps:put(Field, frame_to_web(maps:get(Field, I, <<>>)), I)
    end, List).

% image size

-spec get_frame_size(atom(), binary()) -> {integer(), integer()}.
get_frame_size(jpeg, Image) ->
  get_jpeg_block(Image).

get_jpeg_block(<<?M_MARK, ?M_SOI, Rest/binary>>) ->
  get_jpeg_block(Rest);
get_jpeg_block(<<?M_MARK, ?M_SOS, _Rest/binary>>) ->
  {0, 0};
get_jpeg_block(<<?M_MARK, ?M_EOI, _Rest/binary>>) ->
  {0, 0};
get_jpeg_block(<<?M_MARK, ?M_SOF0, _:3/binary, Height:2/big-unsigned-integer-unit:8, Width:2/big-unsigned-integer-unit:8, _Rest/binary>>) ->
  {Width, Height};
get_jpeg_block(<<?M_MARK, ?M_SOF2, _:3/binary, Height:2/big-unsigned-integer-unit:8, Width:2/big-unsigned-integer-unit:8, _Rest/binary>>) ->
  {Width, Height};
get_jpeg_block(<<?M_MARK, _Block:8, Len:2/big-unsigned-integer-unit:8, Rest/binary>>) ->
  LenX = Len - 2,
  <<_Block:LenX/binary, More/binary>> = Rest,
  get_jpeg_block(More);
get_jpeg_block(<<_Byte:8, Rest/binary>>) ->
  get_jpeg_block(Rest);
get_jpeg_block(<<>>) ->
  {0, 0}.
