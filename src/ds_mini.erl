%% Copyright (c) 2015 Ulf Leopold.
-module(ds_mini).

-export([setup/1]).
-export([remote/0]).
-export([local/0]).
-export([verify/1]).

%%%_* Remote test ======================================================
setup(Node) ->
  {L1, L2} = create(),
  L = case Node of
        a -> L1;
        b -> L2
      end,
  {ok, Ds} = dataset_local:start_link(dict:from_list(L), fun get/1, fun put/2),
  Ds.

remote() ->
  LocalDataset = misc:local_dataset(mini),
  RemoteDataset = misc:remote_dataset(mini),
  print_dataset("LocalDataset", LocalDataset),
  reconcile:reconcile(LocalDataset, RemoteDataset, 5),
  print_dataset("LocalDataset", LocalDataset),
  verify(LocalDataset).

%%%_* Local test =======================================================
local() ->
  A = setup(a),
  B = setup(b),
  print_dataset("A", A),
  print_dataset("B", B),
  reconcile:reconcile(A, B, 10),
  print_dataset("A", A),
  print_dataset("B", B),
  verify(A),
  verify(B).

%%%_* Helpers ==========================================================
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

create() ->
  Base = [{1, v1}, {2, v1}],
  A = [{3, v1}] ++ Base,
  B = [{4, v1}] ++ lists:keyreplace(2, 1, Base, {2, v2}),
  {A, B}.

verify(Dataset) ->
  Expected = [{1, v1}, {2, v2}, {3, v1}, {4, v1}],
  D = dataset:get_all(Dataset),
  case lists:sort(D) =:= lists:sort(Expected) of
    true -> lager:info("Correct!~n", []);
    false -> lager:info("INCORRECT!~n", [])
  end.

print_dataset(Name, Dataset) ->
  D = lists:sort(dataset:get_all(Dataset)),
  lager:info("~s: Size: ~p, Data: ~p~n", [Name, length(D), D]).
