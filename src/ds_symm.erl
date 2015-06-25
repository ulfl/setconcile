%% Copyright (c) 2015 Ulf Leopold.
-module(ds_symm).

-export([setup/1]).
-export([setup/4]).

%%%_* Remote test ======================================================
setup(Node) -> setup(Node, 10000, 0.2, 100).

setup(Node, N, P, B) ->
  {L1, L2, Expected} = create(N, P, B),
  L = case Node of
        a -> L1;
        b -> L2
      end,
  {ok, Ds} = dataset_local:start_link(symm, dict:from_list(L), fun prep/1,
                                      fun get/1, fun put/2, fun done/1),
  {Ds, Expected}.

%%%_* Helpers ==========================================================
prep(State) -> {size(dict:to_list(State), 0), State}.

get(State) -> dict:to_list(State).

put(State, {K, V1}) ->
  case dict:find(K, State) of
    error    -> dict:store(K, V1, State);
    {ok, V2} -> dict:store(K, resolve(V1, V2), State)
  end.

resolve(V1, V2) -> max(V1, V2).

done(State) ->
  Expected = misc:get_ds_config(symm, symm_expected),
  verify(dict:to_list(State), Expected),
  State.

%% Return a new dataset of key/value pairs for nodes A and B. For each
%% node the pairs will have the following characteristics:
%%
%% - N*(1-P) pairs will be shared between A & B.
%%
%% - N*P/2 pairs will be split evenly between A and B and not be shared.
%%
%% - N*P/2 will have modified values. The keys will exist on both A and
%%   B but half will have new values on A and the other half have newer
%%   value on B.
%%
%% Total pairs on each node: N*(1-P) + N*P/4 + N*P/2 = N -
%% N*P/4. Minimal number of pairs to be exchanged (assuming a single
%% iteration with ideal bloom filters and the transfer starting from A
%% to B): Unique pairs on B and modified/unmodified pairs on B + unique
%% pairs on A and modified pairs on A = (N*P/4 + N*P/2) + (N*P/4 +
%% N*P/4) = 5N*P/4. Transfer_size/dataset_size ratio: (5N*P/4) / (N -
%% N*P/4) = (5P)/(4-P). With P = 0.2 we get a ratio of 0.26.
%%
create(N, P, BulkBytes) when (trunc(N*P) > 0) and (trunc(N*P) rem 4 =:= 0) ->
  Bulk = <<0:(8 * BulkBytes)>>,
  BaseSet = base([], N, Bulk),
  {NewOrUpdated, Shared} = lists:split(trunc(N * P), BaseSet),
  {New, Updated} = split_in_two(NewOrUpdated),
  {A, B} = new_or_updated(New, Updated),
  Expected = Shared ++ New ++ update(Updated),
  {A ++ Shared, B ++ Shared, Expected}.

base(L, 0, _Bulk) -> L;
base(L, N, Bulk)  ->
  K = crypto:hash(sha256, "ds1" ++ integer_to_list(N)),
  %K = integer_to_list(N),
  V = {1, Bulk},
  base([{K, V} | L], N - 1, Bulk).

new_or_updated(New, Updated) ->
  {UniqueA, UniqueB} = split_in_two(New),
  {L1, L2} = split_in_two(Updated),
  UpdatedA = update(L1) ++ L2,
  UpdatedB = L1 ++ update(L2),
  {UniqueA ++ UpdatedA, UniqueB ++ UpdatedB}.

split_in_two(L) -> lists:split(trunc(length(L) / 2), L).

update(L) -> [{K, {Time + 1, Bulk}} || {K, {Time, Bulk}} <- L].

verify(CurrentDataset, Expected) ->
  lager:info("num_elements_current=~p, num_elements_expected=~p",
             [length(CurrentDataset), length(Expected)]),
  case lists:sort(CurrentDataset) =:= lists:sort(Expected) of
    true -> lager:info("Correct!~n", []);
    false -> lager:info("INCORRECT!~n", [])
  end.

size([], Size)      -> Size;
size([H | T], Size) -> size(T, Size + byte_size(term_to_binary(H))).
