%% Copyright (c) 2015 Ulf Leopold.
-module(ds_symm).

-export([setup/1]).
-export([setup/4]).
-export([remote/0]).
-export([local/0]).

%%%_* Remote test ======================================================
setup(Node) -> setup(Node, 10000, 0.2, 100).

setup(Node, N, P, B) ->
  {L1, L2, Expected} = create(N, P, B),
  L = case Node of
        a -> L1;
        b -> L2
      end,
  {ok, Ds} = dataset_local:start_link(dict:from_list(L), fun get/1, fun put/2),
  {Ds, Expected}.

remote() ->
  {ok, Expected} = config:get(symm_expected),
  LocalDataset = misc:local_dataset(symm),
  RemoteDataset = misc:remote_dataset(symm),
  DatasetSize = size(dataset:get_all(LocalDataset), 0),
  {T, {ok, Its, Size, BloomSize}} =
    timer:tc(fun() -> reconcile:reconcile(LocalDataset, RemoteDataset, 30) end),
  lager:info("Done (time=~.2fs, its=~p, data_size=~.2f MB, "
             "bloom_size=~.2f MB)~n", [sec(T), Its, mb(Size), mb(BloomSize)]),
  lager:info("Size of local dataset (dataset_size=~.2fMB)~n", [mb(DatasetSize)]),
  lager:info("Ratio of transferred data to dataset size (size_ratio=~.2f)~n",
             [(Size + BloomSize) / DatasetSize]),
  verify(dataset:get_all(LocalDataset), Expected).

%%%_* Local test =======================================================
local() ->
  {A, ExpectedA} = setup(a),
  {B, ExpectedB} = setup(b),
  DatasetSizeA = size(dataset:get_all(A), 0),
  DatasetSizeB = size(dataset:get_all(B), 0),
  {ok, Its, Size, BloomSize} = reconcile:reconcile(A, B, 30),
  lager:info("Done (its=~p, data_size=~.2f MB, bloom_size=~.2f MB)~n",
             [Its, mb(Size), mb(BloomSize)]),
  lager:info("Size of dataset A: ~.2fMB~n", [mb(DatasetSizeA)]),
  lager:info("Size of dataset B: ~.2fMB~n", [mb(DatasetSizeB)]),
  lager:info("Ratio of transferred data to dataset A size (size_ratio=~.2f)~n",
             [(Size + BloomSize) / DatasetSizeA]),
  verify(dataset:get_all(A), ExpectedA),
  verify(dataset:get_all(B), ExpectedB).

%%%_* Helpers ==========================================================
get(State) -> dict:to_list(State).

put(State, {K, V1}) ->
  case dict:find(K, State) of
    error    -> dict:store(K, V1, State);
    {ok, V2} -> dict:store(K, resolve(V1, V2), State)
  end.

resolve(V1, V2) -> max(V1, V2).

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
%% N*P/4. Minimal number of pairs to be exchanged (assuming ideal bloom
%% filters): Unique pairs on B and modified/unmodified pairs on B +
%% unique pairs on A and modified pairs on A = (N*P/4 + N*P/2) + (N*P/4
%% + N*P/4) = 5N*P/4. Transfer_size/dataset_size ratio: (5N*P/4) / (N -
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

mb(X) -> X / (1024 * 1024).

sec(T) -> T / (1000 * 1000).
