-module(rest).

-export([request/6, request/7, parse/2]).

-include("eknife.hrl").

-spec request(atom(), atom() | {atom(), any()}, list(),
              [integer()], map(), map() | binary() | list()) -> {ok,
                                                                 integer(), map(),
                                                                 map() | list() | binary()} |
                                                                {error, integer(), map(),
                                                                 map() | list() | binary()} |
                                                                {error, any()}.

request(Method, Type, URL, Expect, InHeaders, Body) ->
    request(Method, Type, URL, Expect, InHeaders, Body, []).

-spec request(atom(), atom() | {atom(), any()}, list(),
              [integer()], map(), map() | binary() | list(),
              [tuple()]) -> {ok, integer(), map(),
                             map() | list() | binary()} |
                            {error, integer(), map(), map() | list() | binary()} |
                            {error, any()}.

request(Method, Type, URL, Expect, InHeaders, Body,
        TransportOptions) ->
    try request_throwable(Method,
                          Type,
                          URL,
                          Expect,
                          InHeaders,
                          Body,
                          TransportOptions)
    of
        Any -> Any
    catch
        Type:Reason:Stacktrace ->
            ?LOG_ERROR("Exception catched ~p:~p -> ~p. ~p",
                       [Type,
                        Reason,
                        utils:stacktrace(Stacktrace),
                        [Method, Type, URL, Expect, InHeaders]]),
            {error, exception}
    end.

request_throwable(Method, Type, URL, Expect, InHeaders,
                  Body, TransportOptions) ->
    case url_parse(URL) of
        undefined -> {error, invalid_url};
        {Host, Port, Path, QS} ->
            Headers = maps:merge(keys_to_lower(InHeaders),
                                 #{<<"accept">> =>
                                       get_access_type(Type) ++ ", */*;q=0.9",
                                   <<"content-type">> => get_content_type(Type)}),
            ?LOG_DEBUG("Make a ~p connection to ~p:~p", [Host, Port]),
            {ok, ConnPid} = case TransportOptions of
                                [] -> gun:open(Host, Port, #{protocols => [http]});
                                _ ->
                                    gun:open(Host,
                                             Port,
                                             #{protocols => [http], transport => tls,
                                               transport_opts => TransportOptions})
                            end,
            {ok, Protocol} = gun:await_up(ConnPid, ?GUN_TIMEOUT),
            ?LOG_DEBUG("Connection is up - ~p", [Protocol]),
            StreamRef = case lists:any(fun (I) -> I =:= Method end,
                                       [post, put])
                            of
                            true ->
                                FullPath = Path ++ QS,
                                EncBody = encode_body(Type, Body),
                                BodySize = byte_size(EncBody), 
                                FullHeaders = maps:to_list(Headers#{<<"content-length">> => BodySize}), 
                                ?LOG_DEBUG("Make request ~p ~p with headers ~p and body size ~p", [Method, FullPath, FullHeaders, byte_size(EncBody)]),
                                case BodySize > (4 * 1024) of
                                    true ->
                                        ok;
                                    false ->
                                        ?LOG_DEBUG("Body: ~p", [EncBody])
                                end,
                                gun:Method(ConnPid,
                                           FullPath,
                                           FullHeaders,
                                           EncBody);
                            false when length(QS) =:= 0 ->
                                FullPath = lists:concat([Path,
                                                         "?",
                                                         binary_to_list(cow_qs:qs(maps:to_list(Body)))]),
                                FullHeaders = maps:to_list(Headers),
                                ?LOG_DEBUG("Make request ~p ~p with headers ~p", [Method, FullPath, FullHeaders]),
                                gun:Method(ConnPid,
                                           FullPath,
                                           FullHeaders);
                            false when length(QS) =/= 0 ->
                                FullPath = lists:concat([Path,
                                                         QS,
                                                         "&",
                                                         binary_to_list(cow_qs:qs(maps:to_list(Body)))]),
                                FullHeaders = maps:to_list(Headers),
                                ?LOG_DEBUG("Make request ~p ~p with headers ~p", [Method, FullPath, FullHeaders]),
                                gun:Method(ConnPid,
                                           FullPath,
                                           FullHeaders);
                            false ->
                                FullPath = Path ++ QS,
                                FullHeaders = maps:to_list(Headers),
                                ?LOG_DEBUG("Make request ~p ~p with headers ~p", [Method, FullPath, FullHeaders]),
                                gun:Method(ConnPid,
                                           FullPath,
                                           FullHeaders)
                        end,
            
            Resp = case gun:await(ConnPid, StreamRef, ?GUN_TIMEOUT)
                       of
                       {response, fin, Status, RespHeaders} ->
                           case lists:any(fun (I) -> I =:= Status end, Expect) of
                               true -> {ok, Status, RespHeaders, #{}};
                               false when Expect =:= [] ->
                                   {ok, Status, RespHeaders, #{}};
                               false -> {error, Status, RespHeaders, #{}}
                           end;
                       {response, nofin, Status, RespHeadersL} ->
                           RespHeaders =
                               maps:from_list(keys_to_lower(RespHeadersL)),
                           case gun:await_body(ConnPid, StreamRef, ?GUN_TIMEOUT) of
                               {ok, RespBody} ->
                                   case lists:any(fun (I) -> I =:= Status end, Expect) of
                                       true ->
                                           {ok, Status, RespHeaders, parse(RespHeaders, RespBody)};
                                       false when Expect =:= [] ->
                                           {ok, Status, RespHeaders, parse(RespHeaders, RespBody)};
                                       false ->
                                           {error,
                                            Status,
                                            RespHeaders,
                                            parse(RespHeaders, RespBody)}
                                   end;
                               Error -> {error, Error}
                           end;
                       Any -> Any
                   end,
            gun:shutdown(ConnPid),
            Resp
    end.

url_parse(URL) ->
    case uri_string:parse(cast:to_list(URL)) of
        URI when is_map(URI) ->
            {maps:get(host, URI, "localhost"),
             maps:get(port,
                      URI,
                      case maps:get(scheme, URI, "http") of
                          "https" -> 443;
                          _ -> 80
                      end),
             maps:get(path, URI, "/"),
             maps:get(query, URI, "")};
        Any ->
            ?LOG_DEBUG("Can't parse URL ~p traditional way - "
                       "~p, down to custom",
                       [URL, Any]),
            case re:run(URL,
                        "([a-z0-9]*):\\/\\/([^:\\/]+)(:[0-9]+)?([^?]*)"
                        "(\\??.*)?",
                        [{capture, all, list}])
                of
                {match, [_, "https", Host, [], Path, QS]} ->
                    {Host, 443, Path, QS};
                {match, [_, _, Host, [], Path, QS]} ->
                    {Host, 80, Path, QS};
                {match, [_, _Proto, Host, PortS, Path, QS]} ->
                    {Host, utils:to_integer(PortS, 80), Path, QS};
                Other ->
                    ?LOG_WARNING("Can't parse URL ~p - ~p", [URL, Other]),
                    undefined
            end
    end.

keys_to_lower(L) when is_list(L) ->
    [{string:lowercase(K), V} || {K, V} <- L];
keys_to_lower(M) when is_map(M) ->
    maps:from_list(keys_to_lower(maps:to_list(M))).

get_access_type(html) -> "text/html";
get_access_type(qs) ->
    "application/x-www-form-urlencoded";
get_access_type(_) -> "application/json".

get_content_type(video) -> "video/mp4";
get_content_type({multipart, _}) ->
    "multipart/form-data; boundary=xxxxxxxxXXXXXXXX";
get_content_type(html) -> "text/html";
get_content_type(qs) ->
    "application/x-www-form-urlencoded";
get_content_type(_) -> "application/json".

encode_body(qs, Body) -> cow_qs:qs(maps:to_list(Body));
encode_body(html, Body) -> Body;
encode_body({multipart, FileName}, Body) ->
    {ok, FileType, Mime} =
        get_mime_from_extension(filename:extension(FileName)),
    NewBody =
        <<<<"--xxxxxxxxXXXXXXXX\r\nContent-Disposition: "
            "form-data; name=\"">>/binary,
          FileType/binary, <<"\"; filename=\"">>/binary,
          FileName/binary, <<"\"\r\nContent-Type: ">>/binary,
          Mime/binary, <<"\r\n\r\n">>/binary, Body/binary,
          <<"\r\n--xxxxxxxxXXXXXXXX--">>/binary>>,
    NewBody;
encode_body({video, _FileName}, Body) -> Body;
encode_body(_, Body) -> utils:to_json(Body).

parse(Headers, Body) ->
    Unzip = case maps:get(<<"content-encoding">>,
                          Headers,
                          <<"plain">>)
                of
                <<"gzip">> -> zlib:gunzip(Body);
                _ -> Body
            end,
    case maps:get(<<"content-type">>, Headers, undefined) of
        undefined -> parse_body("application/json", Unzip);
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
% parse_body("text/plain", Body) ->
%   maps:from_list(cow_qs:parse_qs(Body)); % damn you, Facebook
parse_body(_, Body) -> Body.

get_mime_from_extension(Extension) ->
    case Extension of
        <<".png">> -> {ok, <<"photo">>, <<"image/png">>};
        <<".jpg">> -> {ok, <<"photo">>, <<"image/jpeg">>};
        <<".jpeg">> -> {ok, <<"photo">>, <<"image/jpeg">>};
        <<".mp4">> -> {ok, <<"video">>, <<"video/mp4">>};
        <<".mpg">> -> {ok, <<"video">>, <<"video/mpeg">>};
        <<".mp3">> -> {ok, <<"audio">>, <<"audio/mpeg">>};
        <<".m4a">> -> {ok, <<"audio">>, <<"audio/mp4">>};
        _ -> {error, undefined, <<"">>}
    end.
