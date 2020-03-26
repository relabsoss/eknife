-module(cast).
-export([
          to_integer/1,
          to_integer/2,
          to_float/1,
          to_float/2,
          to_binary/1,
          to_list/1
        ]).

-include("eknife.hrl").


-spec to_integer(any()) -> integer().
to_integer(Val) -> 
  to_integer(Val, 0).

-spec to_integer(any(), integer()) -> integer().
to_integer(Int, _) when is_integer(Int) ->
  Int;
to_integer(IntB, Default) when is_binary(IntB) ->
  to_integer(IntB, fun binary_to_integer/1, Default);
to_integer(IntL, Default) when is_list(IntL) ->
  to_integer(IntL, fun list_to_integer/1, Default);
to_integer(_, Default) -> 
  Default.

to_integer(IntT, Conv, Default) ->
  try Conv(IntT)
  catch _:_ -> 
    Default
  end.

-spec to_float(any()) -> float().
to_float(Val) -> 
  to_float(Val, 0.0).

-spec to_float(any(), float()) -> float().
to_float(Float, _) when is_float(Float) ->
  Float;
to_float(Float, _) when is_integer(Float) ->
  float(Float);
to_float(FloatB, Default) when is_binary(FloatB) ->
  to_float(FloatB, fun binary_to_float/1, Default);
to_float(FloatL, Default) when is_list(FloatL) ->
  to_float(FloatL, fun list_to_float/1, Default);
to_float(_, Default) -> Default.

to_float(FloatT, Conv, Default) ->
  try Conv(FloatT)
  catch 
    error:badarg -> 
      float(to_integer(FloatT, Default));
    _:_ -> 
      Default
  end.

-spec to_binary(any()) -> binary().
to_binary(V) when is_binary(V) -> 
  V;
to_binary(V) when is_list(V) -> 
  unicode:characters_to_binary(V);
to_binary(V) when is_integer(V) -> 
  integer_to_binary(V);
to_binary(V) when is_float(V) -> 
  float_to_binary(V, [compact]);
to_binary(V) when is_atom(V) -> 
  atom_to_binary(V, utf8);
to_binary(true) -> 
  <<"TRUE">>;
to_binary(false) -> 
  <<"FALSE">>.

-spec to_list(any()) -> list().
to_list(V) when is_list(V) -> 
  V;
to_list(V) when is_binary(V) -> 
  unicode:characters_to_list(V);
to_list(V) when is_integer(V) -> 
  integer_to_list(V);
to_list(V) when is_float(V) -> 
  io_lib:format("~.2f",[V]);
to_list(V) when is_atom(V) -> 
  atom_to_list(V);
to_list(true) -> 
  "TRUE";
to_list(false) -> 
  "FALSE".
