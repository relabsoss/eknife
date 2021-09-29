-module(network).

-export([parse_ip/2]).

-include("eknife.hrl").

-spec parse_ip(binary() | list(), tuple()) -> tuple().

parse_ip(Address, Default) ->
    case inet:parse_address(Address) of
        {ok, ValidIP} -> ValidIP;
        Error ->
            ?LOG_WARNING("Can't parse ~p - ~p", [Address, Error]),
            Default
    end.
