-module(utils).
-export([
          stacktrace/1,     

          from_json/2,
          to_json/1,

          concat/2,

          to_integer/1,
          to_integer/2,
          to_float/1,
          to_float/2,
          to_binary/1,
          to_list/1,

          format_date/1,
          date_to_timestamp/1,
          timestamp_to_date/1,
          parse_date/1,
          time_to_seconds/1,
          time_str/0,
          time_str/1,

          map_to_json/1,
          list_to_json/1,
          proplist_to_map/1,

          hash/1,
          md5b/1,
          md5b64/1
        ]).

-include("eknife.hrl").

% debug

-spec stacktrace(list()) -> binary().
stacktrace(Trace) ->
  lists:foldl(fun
      ({Module, Fun, Params, _}, A) when is_list(Params) ->
        S = list_to_binary(lists:flatten(io_lib:format("~p:~p~p", [Module, Fun, Params]))),
        <<A/binary, S/binary>>;
      ({Module, Fun, Arity, Location}, A) ->
        File = filename:basename(proplists:get_value(file, Location, "_")),
        Line = proplists:get_value(line, Location, 0),
        S = list_to_binary(lists:flatten(io_lib:format(" -> ~p:~p/~p (~s:~p)", [Module, Fun, Arity, File, Line]))),
        <<A/binary, S/binary>>
    end, <<>>, Trace).

% transform

-spec from_json(binary(), any()) -> any().
from_json(Msg, Default) ->
  try ?FROM_JSON(Msg) of
    {error, Error} -> 
      ?LOG_ERROR("Error ~p in decoding ~p", [Error, Msg]),
      Default;
    {error, Error, Str} -> 
      ?LOG_ERROR("Error ~p in decoding ~p", [Error, Str]),
       Default;
    Data -> Data
  catch Exc:Exp:_ -> 
    ?LOG_ERROR("Exception ~p:~p in decoding of ~p", [Exc, Exp, Msg]),
    Default 
  end.

-spec to_json(list() | map()) -> binary().
to_json(Msg) ->
  try
    case ?TO_JSON(Msg) of
      Str when is_binary(Str) -> Str;
      Err -> 
        ?LOG_ERROR("Error encoding to JSON ~p in ~p", [Err, Msg]), 
        ?TO_JSON([])
    end
  catch 
    Exc:Exp:Stacktrace -> 
      ?LOG_ERROR("Exception ~p:~p in encoding of ~p\n~p", [Exc, Exp, Msg, Stacktrace]),
      <<"{}">>
  end.

% combine

-spec concat([binary()], binary()) -> binary().
concat([], _) -> 
  <<>>;
concat([E], _) -> 
  E;
concat([A1, A2 | T], Del) -> 
  concat([<<(to_binary(A1))/binary, Del/binary, (to_binary(A2))/binary>> | T], Del).

% types

-spec to_integer(any()) -> integer().
to_integer(Val) -> 
  cast:to_integer(Val).

-spec to_integer(any(), integer()) -> integer().
to_integer(Int, Int) ->
  cast:to_integer(Int, Int).

-spec to_float(any()) -> float().
to_float(Val) -> 
  cast:to_float(Val).

-spec to_float(any(), float()) -> float().
to_float(Float, Float) -> 
  cast:to_float(Float, Float).

-spec to_binary(any()) -> binary().
to_binary(Any) -> 
  cast:to_binary(Any).

-spec to_list(any()) -> list().
to_list(Any) -> 
  cast:to_list(Any).

% datetime

-define(EPOCH, calendar:datetime_to_gregorian_seconds({{1970,1,1},{0,0,0}})).

-spec format_date({{integer(), integer(), integer()}, {integer(), integer(), integer()}}) -> binary().
format_date(DateTime) -> 
  iso8601:format(DateTime).

-spec date_to_timestamp({{integer(), integer(), integer()}, {integer(), integer(), integer()}}) -> integer().
date_to_timestamp({Date, {H, M, S}}) ->
  RS = round(S),
  calendar:datetime_to_gregorian_seconds({Date, {H, M, RS}}) - ?EPOCH + (S - RS).

-spec timestamp_to_date(integer()) -> {{integer(), integer(), integer()}, {integer(), integer(), integer()}}.
timestamp_to_date(TS) ->
  RTS = math:floor(TS),
  {Date, {H, M, RS}} = calendar:gregorian_seconds_to_datetime(RTS + ?EPOCH),
  {Date, {H, M, RS + (TS - RTS)}}.

-spec parse_date({{integer(), integer(), integer()}, {integer(), integer(), integer()}} | string()) 
  -> {{integer(), integer(), integer()}, {integer(), integer(), integer()}}.
parse_date({{_, _, _}, {_, _, _}} = DateTime) ->
  DateTime;
parse_date(<<Y:4/binary, X, M:2/binary, X, D:2/binary>>) ->
  {{binary_to_integer(Y), binary_to_integer(M), binary_to_integer(D)}, {0,0,0}};
parse_date(Str) when is_list(Str) -> 
  parse_date(list_to_binary(Str));
parse_date(Str) ->
  case re:run(Str, "([0-9]{1,4})-([01][0-9])-([0-3][0-9])T([0-1][0-9]):([0-5][0-9]):([0-5][0-9].[0-9]+)Z", [global, {capture, all, binary}]) of
    {match, [[_, Year, Month, Day, Hour, Minute, Seconds]]} ->
      { {to_integer(Year), to_integer(Month), to_integer(Day)},
        {to_integer(Hour), to_integer(Minute), to_float(Seconds)}};
    _ ->
      iso8601:parse(Str)
  end.

-spec time_to_seconds({integer(), integer(), integer()} | binary()) -> integer().
time_to_seconds({H, M, S}) ->
  round(H * 3600 + M * 60 + S);
time_to_seconds(<<H:2/binary, ":", M:2/binary, ":", S:2/binary>>) ->
  time_to_seconds({binary_to_integer(H), binary_to_integer(M), binary_to_integer(S)});
time_to_seconds(_) ->
  undefined.

-spec time_str() -> string().
time_str() ->
  time_str(erlang:localtime()).

-spec time_str({{integer(), integer(), integer()}, {integer(), integer(), integer()}}) -> string().
time_str({{Y, M, D}, {H,  I, S}}) ->
  lists:flatten(io_lib:format("~4.10.0b-~2.10.0b-~2.10.0b_~2.10.0b-~2.10.0b-~2.10.0b", [Y, M, D, H, I, S])).

% maps

-spec map_to_json(map()) -> map().
map_to_json(M) ->
  maps:map(fun
      (_K, V) when is_map(V) -> 
        map_to_json(V);
      (_K, {{_, _, _}, {_, _, _}} = V) -> 
        format_date(V);
      (_K, V) when is_tuple(V) -> 
        list_to_json(tuple_to_list(V));
      (_K, V) when is_list(V) -> 
        list_to_json(V);
      (_K, V) -> 
        V
    end, M).

-spec list_to_json(list()) -> list().
list_to_json(L) ->
  lists:map(fun
      (I) when is_map(I) -> 
        map_to_json(I);
      ({{_, _, _}, {_, _, _}} = I) -> 
        format_date(I);
      (I) when is_tuple(I) -> 
        tuple_to_list(I);
      (I) -> 
        I 
    end, L).

-spec proplist_to_map(list()) -> map().
proplist_to_map(List) ->
  maps:from_list([{to_binary(K), to_binary(V)} || {K, V} <- List]).

% hash

-spec hash(binary()) -> binary().
hash(N) -> 
  list_to_binary(hexstring(crypto:hash(sha512, N))).

hexstring(<<X:128/big-unsigned-integer>>) -> 
  lists:flatten(io_lib:format("~32.16.0b", [X]));
hexstring(<<X:160/big-unsigned-integer>>) -> 
  lists:flatten(io_lib:format("~40.16.0b", [X]));
hexstring(<<X:256/big-unsigned-integer>>) -> 
  lists:flatten(io_lib:format("~64.16.0b", [X]));
hexstring(<<X:512/big-unsigned-integer>>) -> 
  lists:flatten(io_lib:format("~128.16.0b", [X])).

-spec md5b(binary()) -> binary().
md5b(S) ->
  list_to_binary(lists:flatten([io_lib:format("~2.16.0b", [C]) || <<C>> <= erlang:md5(S)])).

-spec md5b64(binary()) -> binary().
md5b64(S) ->
  binary:replace(
    binary:replace(
      base64:encode(crypto:hash(md5, S)), 
      [<<"+">>, <<"/">>, <<" ">>, <<"-">>], 
      <<"_">>, 
      [global]), 
    <<"=">>, 
    <<>>, 
    [global]).
