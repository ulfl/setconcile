%% Copyright (c) 2015 Ulf Leopold.
-module(ds_symm).

-export([setup/1]).
-export([setup/4]).

%%%_* Remote test ======================================================
setup(Node) -> setup(Node, 10000, 0.2, 100).

setup(Node, N, P, B) ->
  {L1, L2, Expected} = symmetric_dataset:create(N, P, B),
  L = case Node of
        a -> L1;
        b -> L2
      end,
  {ok, Ds} = dataset_local:start_link(symm, dict:from_list(L), fun prep/1,
                                      fun get/1, fun get_vals/2, fun put/2,
                                      fun unprep/1),
  {Ds, Expected}.

%%%_* Helpers ==========================================================
prep(State) -> {size(dict:to_list(State), 0), State}.

get(State) -> dict:to_list(State).

get_vals(State, L) -> L.

put(State, {K, V1}) ->
  case dict:find(K, State) of
    error    -> dict:store(K, V1, State);
    {ok, V2} -> dict:store(K, resolve(V1, V2), State)
  end.

resolve(V1, V2) -> max(V1, V2).

unprep(State) ->
  Expected = misc:get_ds_config(symm, symm_expected),
  symmetric_dataset:verify(dict:to_list(State), Expected),
  State.

size([], Size)      -> Size;
size([H | T], Size) -> size(T, Size + byte_size(term_to_binary(H))).
