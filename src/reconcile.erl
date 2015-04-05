%% Copyright (c) 2015 Ulf Leopold.
-module(reconcile).

-export([reconcile/3]).
-export([create_bloom/1]).
-export([filter/3]).

reconcile(NodeA, NodeB, MaxIts) ->
  reconcile(NodeA, NodeB, MaxIts, 0).

reconcile(_, _, 0, BloomSize)    ->
  io:format("Sum of bloom sizes: ~.2f MB~n", [BloomSize / (1024 * 1024)]),
  ok;
reconcile(NodeA, NodeB, MaxIts, Size) ->
  BloomA = NodeA({get_bloom}),
  L1 = NodeB({filter, BloomA}),
  NodeA({put, L1}),
  Len1 = length(L1),
  io:format("Added ~p to A~n", [Len1]),

  BloomB = NodeB({get_bloom}),
  L2 = NodeA({filter, BloomB}),
  NodeB({put, L2}),
  Len2 = length(L2),
  io:format("Added ~p to B~n", [Len2]),

  MaxIts1 = case (Len1 + Len2) =:= 0 of
              true  -> 1;
              false -> MaxIts
            end,
  reconcile(NodeA, NodeB, MaxIts1 - 1, Size + bloom_size(BloomA) +
              bloom_size(BloomB)).

bloom_size(B) -> byte_size(ebloom:serialize(B)).

create_bloom(L) ->
  N = length(L),
  {ok, B} = ebloom:new(N, 0.5, random:uniform(10000000)),
  create_bloom(L, B).

create_bloom([], B) ->
  B;
create_bloom([H | T], B) ->
  ebloom:insert(B, term_to_binary(H)),
  create_bloom(T, B).

filter([], _B, Remainder)      -> Remainder;
filter([H | T], B, Remainder)  ->
  case ebloom:contains(B, term_to_binary(H)) of
    true  -> filter(T, B, Remainder);
    false -> filter(T, B, [H | Remainder])
  end.
