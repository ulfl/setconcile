%% Copyright (c) 2015 Ulf Leopold.
-module(dataset).

-export([make/3]).

make(State, Get, Put) ->
  Pid = spawn(fun() -> put(state, State), loop(Get, Put) end),
  fun(Op) -> call(Pid, Op) end.

call(Pid, Op) ->
  Ref = make_ref(),
  Pid ! {self(), Ref, Op},
  receive
    {Ref, Res} -> Res
  end.

loop(Get, Put) ->
  receive
    %% Get local Bloom filter.
    {Pid, Ref, {get_bloom}} ->
      State = get(state),
      Pid ! {Ref, reconcile:create_bloom(Get(State))};

    %% Given a Bloom filter, start transfer to Dest.
    {Pid, Ref, {post_transfer, Bloom, Dest}}   ->
      State = get(state),
      L = reconcile:filter(Get(State), Bloom, []),
      Size = lists:foldl(fun(X, A) ->
                             Size = Dest({post_element, X}),
                             A + Size
                         end, 0, L),
      Pid ! {Ref, {length(L), Size}};

    %% Receive an element from remote.
    {Pid, Ref, {post_element, X}}   ->
      State = get(state),
      put(state, Put(State, [X])),
      Pid ! {Ref, byte_size(term_to_binary(X))};

    {Pid, Ref, {get_all}}   ->
      State = get(state),
      Pid ! {Ref, Get(State)}

  end,
  loop(Get, Put).
