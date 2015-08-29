%% Copyright (c) 2015 Ulf Leopold.
-module(reconcile).

-export([reconcile/1]).
-export([create_bloom/2]).
-export([filter/3]).

reconcile(DatasetName) ->
  LocalDataset = misc:local_dataset(DatasetName),
  RemoteDataset = misc:remote_dataset(DatasetName),

  %% Should execute these in parallel since they are potentially
  %% lengthy procedures.
  DatasetSize = dataset:prep(LocalDataset),
  dataset:prep(RemoteDataset),

  MaxIts = misc:get_ds_config(DatasetName, max_its),
  {T, {ok, Its, Size, BloomSize}} =
    timer:tc(fun() -> reconcile(LocalDataset, RemoteDataset, MaxIts) end),
  lager:info("Done (time=~.2fs, its=~p, data_size=~.2f MB, "
             "bloom_size=~.2f MB)~n", [sec(T), Its, mb(Size), mb(BloomSize)]),
  lager:info("Size of local dataset (dataset_size=~.2fMB)~n", [mb(DatasetSize)]),
  lager:info("Ratio of transferred data to dataset size (size_ratio=~.2f)~n",
             [(Size + BloomSize) / DatasetSize]),

  dataset:unprep(LocalDataset),
  dataset:unprep(RemoteDataset).

reconcile(LocalDataset, RemoteDataset, MaxIts) ->
  reconcile(LocalDataset, RemoteDataset, 0, MaxIts, fun converged/2, 0, 0).

reconcile(_, _, Its, MaxIts, _Converged, Size, BloomSize) when Its >= MaxIts ->
  {ok, Its, Size, BloomSize};
reconcile(LocalDataset, RemoteDataset, Its, MaxIts, Converged, Size,
          BloomSize) ->
  {Dt1, LocalBloom} = timer:tc(fun() -> dataset:get_bloom(LocalDataset) end),
  lager:info("Calculated local bloom (dt=~.2fs).", [sec(Dt1)]),

  {Dt2, {ReceiveCount, ReceiveSize}} =
    timer:tc(fun() -> dataset:post_transfer(RemoteDataset, LocalBloom,
                                            LocalDataset)
             end),
  lager:info("Received from remote (num_elements=~p, total_size=~pB, "
             "dt=~.2fs)~n", [ReceiveCount, ReceiveSize, sec(Dt2)]),

  {Dt3, RemoteBloom} = timer:tc(fun() -> dataset:get_bloom(RemoteDataset) end),
  lager:info("Calculated remote bloom (dt=~.2fs).", [sec(Dt3)]),

  {Dt4, {SendCount, SendSize}} =
    timer:tc(fun() -> dataset:post_transfer(LocalDataset, RemoteBloom,
                                            RemoteDataset)
             end),
  lager:info("Transferred to remote (num_elements=~p, total_size=~pB, "
             "dt=~.2fs)~n", [SendCount, SendSize, sec(Dt4)]),

  {MaxIts1, Converged1} = case Converged(ReceiveCount, SendCount) of
              true  -> {Its + 2, fun(_, _) -> false end};
              false -> {MaxIts, Converged}
            end,
  reconcile(LocalDataset, RemoteDataset, Its + 1, MaxIts1, Converged1,
            Size + ReceiveSize + SendSize,
            BloomSize + bloom_size(LocalBloom) + bloom_size(RemoteBloom)).

converged(C1, C2) -> C1 + C2 =:= 0.

bloom_size(B) -> byte_size(ebloom:serialize(B)).

create_bloom(L, FalseProbability) ->
  N = max(1, length(L)), % Don't allow N to be zero.
  {ok, B} = ebloom:new(N, FalseProbability, random:uniform(10000000)),
  populate_bloom(L, B).

populate_bloom([], B) ->
  B;
populate_bloom([H | T], B) ->
  ebloom:insert(B, term_to_binary(H)),
  populate_bloom(T, B).

filter([], _B, Remainder)      -> Remainder;
filter([H | T], B, Remainder)  ->
  case ebloom:contains(B, term_to_binary(H)) of
    true  -> filter(T, B, Remainder);
    false -> filter(T, B, [H | Remainder])
  end.

mb(X) -> X / (1024 * 1024).

sec(T) -> T / (1000 * 1000).
