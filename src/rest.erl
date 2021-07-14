-module(rest).
-export([
          request/6,
          request/7
        ]).

-include("eknife.hrl").


-spec request(atom(), atom() | {atom(), any()}, list(), list(integer()), map(), map() | binary() | list()) -> 
  {ok, integer(), map(), map() | list() | binary() } | {error, integer(), map(), map() | list() | binary()} | {error, any()}.
request(Method, Type, URL, Expect, InHeaders, Body) ->
  request(Method, Type, URL, Expect, InHeaders, Body, []).

-spec request(atom(), atom() | {atom(), any()}, list(), list(integer()), map(), map() | binary() | list(), list(tuple())) -> 
  {ok, integer(), map(), map() | list() | binary() } | {error, integer(), map(), map() | list() | binary()} | {error, any()}.
request(Method, Type, URL, Expect, InHeaders, Body, TransportOptions) ->
  try request_throwable(Method, Type, URL, Expect, InHeaders, Body, TransportOptions) of
    Any -> 
      Any
  catch
    Type:Reason:Stacktrace  ->
      ?LOG_ERROR("Exception catched ~p:~p -> ~p. ~p", [Type, Reason, utils:stacktrace(Stacktrace), [Method, Type, URL, Expect, InHeaders]]),
      {error, exception}
  end.

request_throwable(Method, Type, URL, Expect, InHeaders, Body, TransportOptions) ->
  Headers = maps:merge(InHeaders, #{
      <<"Accept">> => get_access_type(Type) ++ ", */*;q=0.9",
      <<"Content-Type">> => get_content_type(Type)
    }),  
  URI = uri_string:parse(cast:to_list(URL)),
  Host = maps:get(host, URI, "localhost"),
  Port = maps:get(port, URI, 80),
  Path = maps:get(path, URI, "/"),
  QS = maps:get(query, URI, ""),
  {ok, ConnPid} = case TransportOptions of
    [] ->
      gun:open(Host, Port, #{ protocols => [http] });
    _ ->
      gun:open(Host, Port,#{ protocols => [http], transport => tls, transport_opts => TransportOptions })
  end,
  {ok, _Protocol} = gun:await_up(ConnPid, ?GUN_TIMEOUT),
  StreamRef = case lists:any(fun(I) -> I =:= Method end, [post, put]) of  
    true -> 
      EncBody = encode_body(Type, Body),
      gun:Method(ConnPid, Path ++ QS, maps:to_list(Headers#{ <<"content-length">> => byte_size(EncBody) }), EncBody);
    false when length(QS) =:= 0 ->
      gun:Method(ConnPid, lists:concat([Path, "?", binary_to_list(cow_qs:qs(maps:to_list(Body)))]), maps:to_list(Headers));
    false when length(QS) =/= 0 ->
      gun:Method(ConnPid, lists:concat([Path, QS, "&", binary_to_list(cow_qs:qs(maps:to_list(Body)))]), maps:to_list(Headers));
    false -> 
      gun:Method(ConnPid, Path ++ QS, maps:to_list(Headers))
  end,
  Resp = case gun:await(ConnPid, StreamRef, ?GUN_TIMEOUT) of
    {response, fin, Status, RespHeaders} ->
      case lists:any(fun(I) -> I =:= Status end, Expect) of
        true ->
          {ok, Status, RespHeaders, #{}};
        false when Expect =:= [] ->
          {ok, Status, RespHeaders, #{}};
        false ->
          {error, Status, RespHeaders, #{}}               
      end;    
    {response, nofin, Status, RespHeadersL} ->
      RespHeaders = maps:from_list(keys_to_lower(RespHeadersL)),
      case gun:await_body(ConnPid, StreamRef, ?GUN_TIMEOUT) of
        {ok, RespBody} -> 
          case lists:any(fun(I) -> I =:= Status end, Expect) of
            true ->
              {ok, Status, RespHeaders, parse(RespHeaders, RespBody)};
            false when Expect =:= [] ->
              {ok, Status, RespHeaders, parse(RespHeaders, RespBody)};
            false ->
              {error, Status, RespHeaders, parse(RespHeaders, RespBody)}               
          end;
        Error ->
          {error, Error}
      end;
    Any -> Any
  end,
  gun:shutdown(ConnPid),
  Resp.

keys_to_lower(L) -> [{string:lowercase(K), V} || {K, V} <- L].

get_access_type(html)   -> "text/html";
get_access_type(qs)     -> "application/x-www-form-urlencoded";
get_access_type(_)      -> "application/json".

get_content_type(video) -> "video/mp4"; 
get_content_type({multipart, _})  -> "multipart/form-data; boundary=xxxxxxxxXXXXXXXX";
get_content_type(html)  -> "text/html";
get_content_type(qs)    -> "application/x-www-form-urlencoded";
get_content_type(_)     -> "application/json".

encode_body(qs, Body) -> 
  cow_qs:qs(maps:to_list(Body));
encode_body(html, Body) -> 
  Body;
encode_body({multipart, FileName}, Body) -> 
  {ok, FileType, Mime} = get_mime_from_extension(filename:extension(FileName)),
  NewBody = <<<<"--xxxxxxxxXXXXXXXX\r\n"
    "Content-Disposition: form-data; name=\"">>/binary, FileType/binary, <<"\"; filename=\"">>/binary, FileName/binary,<<"\"\r\n"
    "Content-Type: ">>/binary, Mime/binary, <<"\r\n"
    "\r\n">>/binary,
    Body/binary,
    <<"\r\n"
    "--xxxxxxxxXXXXXXXX--">>/binary>>,
  NewBody;
encode_body({video, _FileName}, Body) -> 
  Body;
encode_body(_, Body) -> 
  utils:to_json(Body).

parse(Headers, Body) ->
  Unzip = case maps:get(<<"content-encoding">>, Headers, <<"plain">>) of
    <<"gzip">> -> 
      zlib:gunzip(Body);
    _ -> 
      Body
  end,
  case maps:get(<<"content-type">>, Headers, undefined) of
    undefined -> 
      parse_body("application/json", Unzip);
    Type -> 
      CType = hd(string:tokens(binary_to_list(Type), ";")),
      parse_body(CType, Unzip)
  end.

parse_body("application/json", Body) -> 
  utils:from_json(Body, #{}); 
parse_body("text/javascript", Body) -> 
  utils:from_json(Body, #{}); 
parse_body("application/x-www-form-urlencoded", Body) -> 
  maps:from_list(cow_qs:parse_qs(Body));
parse_body("text/plain", Body) -> 
  maps:from_list(cow_qs:parse_qs(Body)); % damn you, Facebook
parse_body(_, Body) -> 
  Body.

get_mime_from_extension(Extension) ->
  case Extension of
    <<".png">> ->
      {ok, <<"photo">>, <<"image/png">>};
    <<".jpg">> ->
      {ok, <<"photo">>, <<"image/jpeg">>};
    <<".jpeg">> ->
      {ok, <<"photo">>, <<"image/jpeg">>};
    <<".mp4">> ->
      {ok, <<"video">>, <<"video/*">>};
    <<".mpg">> ->
      {ok, <<"video">>, <<"video/*">>};
    _ ->
      {error, undefined, <<"">>}
  end.
