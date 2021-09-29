-include_lib("kernel/include/logger.hrl").

% timer

-define(S2MS(S), S * 1000).

-define(AFTER(Timeout, Event),
        {ok, _} = timer:send_after(Timeout, Event)).

-define(GUN_TIMEOUT, ?S2MS(60)).

% flow

-define(ASYNC(F), proc_lib:spawn(fun () -> F end)).

% supervisor

-define(CHILD(I, Type),
        {I, {I, start_link, []}, permanent, 50, Type, [I]}).

-define(CHILD(I, Type, Param),
        {I, {I, start_link, Param}, permanent, 50, Type, [I]}).

-define(CHILD(Id, I, Type, Param),
        {Id, {I, start_link, Param}, permanent, 50, Type, [I]}).

% json

-define(FROM_JSON(D), jiffy:decode(D, [return_maps])).

-define(TO_JSON(D), jiffy:encode(D)).

% pub/sub (gproc)

-define(ME(Reg), pubsub:me(Reg)).

-define(LOOKUP(Reg), pubsub:lookup(Reg)).

-define(LOOKUPS(Reg), pubsub:lookups(Reg)).

-define(PUB(Event, Msg), pubsub:pub(Event, Msg)).

-define(SUB(Event), pubsub:sub(Event)).

-define(UNSUB(Event), pubsub:unsub(Event)).

-define(LOOKUP_SUB(Reg), pubsub:lookup_sub(Reg)).

-define(IS_SUB(Event), pubsub:is_sub(Reg)).
