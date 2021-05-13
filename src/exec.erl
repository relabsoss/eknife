-module(exec).
-export([
          drop_port/1
        ]).

-include("eknife.hrl").


-spec drop_port(undefined | port()) -> ok | undefined.
drop_port(undefined) ->
  undefined;
drop_port(Port) ->
  case erlang:port_info(Port) of
    undefined ->
      undefined;
    PList ->
      case proplists:get_value(os_pid, PList, undefined) of
        undefined ->
          erlang:port_close(Port);
        Pid ->
          os:cmd(utils:to_list(io_libc:format("kill -9 %d", [Pid])))
      end,
      ok
  end.
