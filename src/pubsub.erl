-module(pubsub).
-export([
          me/1,
          sub/1,
          subs/1,
          pub/2,
          unsub/1,
          lookup/1,
          lookups/1,
          lookup_sub/1,
          is_sub/1
        ]).


me(Reg) ->
	gproc:reg({n, l, Reg}).

sub(Event) -> 
  case is_sub(Event) of
    true -> 
      true;
    false -> 
      gproc:reg({p, l, Event})
  end.

subs(Reg) ->
  try gproc:lookup_pids({p, l, Reg}) of
    L when is_list(L) -> 
      L;
    _ -> 
      []
  catch 
  	_:_ -> 
      []
  end.

pub(Event, Msg) ->
  gproc:send({p, l, Event}, Msg).

unsub(Event) ->
  case is_sub(Event) of
    true -> 
      gproc:unreg({p, l, Event});
    false -> 
      true
  end.

lookup(Reg) ->
  try gproc:lookup_pid({n, l, Reg}) of
    I when is_pid(I) -> 
      I;
    _ -> 
      undefined
  catch 
  	_:_ -> udefined
  end.

lookups(Reg) ->
  try gproc:lookup_pids({n, l, Reg}) of
    L when is_list(L) -> L;
    _ -> 
      []
  catch 
  	_:_ -> 
      []
  end.

lookup_sub(Reg) ->
	gproc:lookup_pids({p, l, Reg}).

is_sub(Event) ->
	lists:any(fun({P, _}) -> P =:= self() end, gproc:lookup_local_properties(Event)).
