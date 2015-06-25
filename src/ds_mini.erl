%% Copyright (c) 2015 Ulf Leopold.
-module(ds_mini).

-export([setup/1]).
-export([verify/1]).

%%%_* Remote test ======================================================
setup(Node) ->
  {L1, L2} = create(),
  L = case Node of
        a -> L1;
        b -> L2
      end,
  {ok, Ds} = dataset_local:start_link(mini, dict:from_list(L), fun prep/1,
                                      fun get/1, fun put/2, fun done/1),
  Ds.

%%%_* Helpers ==========================================================
prep(State) ->
  L = dict:to_list(State),
  print_dataset(L),
  {size(L, 0), State}.

get(State) -> dict:to_list(State).

put(State, {K, V1}) ->
  case dict:find(K, State) of
    error    -> dict:store(K, V1, State);
    {ok, V2} -> dict:store(K, resolve(V1, V2), State)
  end.

resolve(V1, V2) -> max(V1, V2).

done(State) ->
  L = dict:to_list(State),
  verify(L),
  print_dataset(L),
  State.

create() ->
  Base = [{1, v1}, {2, v1}],
  A = [{3, v1}] ++ Base,
  B = [{4, v1}] ++ lists:keyreplace(2, 1, Base, {2, v2}),
  {A, B}.

verify(L) ->
  Expected = [{1, v1}, {2, v2}, {3, v1}, {4, v1}],
  case lists:sort(L) =:= lists:sort(Expected) of
    true -> lager:info("Correct!~n", []);
    false -> lager:info("INCORRECT!~n", [])
  end.

print_dataset(L) ->
  L2 = lists:sort(L),
  lager:info("dataset: Size: ~p, Data: ~p~n", [length(L2), L2]).

size([], Size)      -> Size;
size([H | T], Size) -> size(T, Size + byte_size(term_to_binary(H))).
