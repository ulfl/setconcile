%% Copyright (c) 2015 Ulf Leopold.
-module(reconcile).

-export([reconcile/1]).
-export([create_bloom/2]).
-export([filter/3]).

reconcile(DsName) ->
  LocalDs = misc:local_dataset(DsName),
  RemoteDs = misc:remote_dataset(DsName),

  %% Should execute these in parallel since they are potentially
  %% lengthy procedures.
  DsSize = ds:prep(LocalDs),
  ds:prep(RemoteDs),

  MaxIts = misc:get_ds_config(DsName, max_its),
  {T, {ok, Its, Size, BloomSize}} =
    timer:tc(fun() -> reconcile(LocalDs, RemoteDs, MaxIts) end),
  lager:info("Done (time=~.2fs, its=~p, data_size=~.2f MB, "
             "bloom_size=~.2f MB)~n", [sec(T), Its, mb(Size), mb(BloomSize)]),
  lager:info("Size of local dataset (dataset_size=~.2fMB)~n", [mb(DsSize)]),
  lager:info("Ratio of transferred data to dataset size (size_ratio=~.2f)~n",
             [(Size + BloomSize) / DsSize]),

  ds:unprep(LocalDs),
  ds:unprep(RemoteDs).

reconcile(LocalDs, RemoteDs, MaxIts) ->
  reconcile(LocalDs, RemoteDs, 0, MaxIts, fun converged/2, 0, 0).

reconcile(_, _, Its, MaxIts, _Converged, Size, BloomSize) when Its >= MaxIts ->
  {ok, Its, Size, BloomSize};
reconcile(LocalDs, RemoteDs, Its, MaxIts, Converged, Size,
          BloomSize) ->
  {LocalBloom, ReceiveCount, ReceiveSize} = transfer(LocalDs,
                                                     RemoteDs, "local"),
  {RemoteBloom, SendCount, SendSize} = transfer(RemoteDs, LocalDs,
                                                "remote"),
  {MaxIts1, Converged1} = case Converged(ReceiveCount, SendCount) of
                            true  -> {Its + 2, fun(_, _) -> false end};
                            false -> {MaxIts, Converged}
                          end,
  reconcile(LocalDs, RemoteDs, Its + 1, MaxIts1, Converged1,
            Size + ReceiveSize + SendSize,
            BloomSize + bloom_size(LocalBloom) + bloom_size(RemoteBloom)).

transfer(SourceDs, DestDs, Name) ->
  {Dt1, Bloom} = timer:tc(fun() -> ds:get_bloom(DestDs) end),
  lager:info("Calculated ~s bloom (dt=~.2fs).", [Name, sec(Dt1)]),

  {Dt2, {SendCount, SendSize}} =
    timer:tc(fun() -> ds:transfer_missing(SourceDs, Bloom, DestDs) end),
  lager:info("Transferred elements to ~s (num_elements=~p, total_size=~pB, "
             "dt=~.2fs)~n", [Name, SendCount, SendSize, sec(Dt2)]),
  {Bloom, SendCount, SendSize}.

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
