%% Copyright (c) 2015 Ulf Leopold.
-module(node).

-export([make/3]).

make(State, Get, Put) ->
  Pid = spawn(fun() -> put(state, State), node_loop(Get, Put) end),
  fun(Op) ->
      Ref = make_ref(),
      Pid ! {self(), Ref, Op},
      receive
        {Ref, Res} -> Res
      end
  end.

node_loop(Get, Put) ->
  receive
    {Pid, Ref, {get}}   ->
      State = get(state),
      Pid ! {Ref, Get(State)};
    {Pid, Ref, {put, L}}   ->
      State = get(state),
      put(state, Put(State, L)),
      Pid ! {Ref, ok};
    {Pid, Ref, {get_bloom}} ->
      State = get(state),
      Pid ! {Ref, reconcile:create_bloom(Get(State))};
    {Pid, Ref, {filter, B}} ->
      State = get(state),
      Pid ! {Ref, reconcile:filter(Get(State), B, [])}
  end,
  node_loop(Get, Put).
