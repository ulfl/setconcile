%% Copyright (c) 2015 Ulf Leopold.
-module(dataset_mini).

-export([test/0]).

test() ->
  Base = [{1, v1}, {2, v1}],
  L1 = [{3, v1}] ++ Base,
  L2 = [{4, v1}] ++ lists:keyreplace(2, 1, Base, {2, v2}),
  A = node:make(dict:from_list(L1), fun get/1, fun put/2),
  B = node:make(dict:from_list(L2), fun get/1, fun put/2),
  print_set("A", A),
  print_set("B", B),
  reconcile:reconcile(A, B, 10),
  print_set("A", A),
  print_set("B", B),
  verify(A, B).

get(State) -> dict:to_list(State).

put(State, L) ->
  lists:foldl(fun({K, V1}, S) ->
                  case dict:find(K, S) of
                    error    -> dict:store(K, V1, S);
                    {ok, V2} -> dict:store(K, resolve(V1, V2), S)
                  end
              end,
              State, L).

resolve(V1, V2) -> max(V1, V2).

verify(NodeA, NodeB) ->
  D1 = NodeA({get}),
  D2 = NodeB({get}),
  case lists:sort(D1) =:= lists:sort(D2) of
    true -> io:format("EQUAL!~n");
    false -> io:format("NOT EQUAL!~n")
  end.

print_set(Name, Node) ->
  D = lists:sort(Node({get})),
  io:format("~s: Size: ~p, Data: ~p~n", [Name, length(D), D]).
