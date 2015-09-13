%% Copyright (c) 2015 Ulf Leopold.
-module(reconcile).

-export([reconcile/1]).
-export([create_bloom/2]).
-export([filter/3]).

reconcile(DsName) ->
  LocalDs = misc:local_dataset(DsName),
  RemoteDs = misc:remote_dataset(DsName),

  %% Prep both sides simultaneously.
  lager:info("Prepping dataset for sync (dataset=~p).~n", [DsName]),
  {PrepTime, [DsSize, _]} =
    timer:tc(fun() ->
                 plists:map(fun(F) -> F() end,
                            [fun() -> ds:prep(LocalDs) end,
                             fun() -> ds:prep(RemoteDs) end],
                            {processes, 2})
             end),
  lager:info("Prepping done (time=~.2fs)~n", [sec(PrepTime)]),

  Converged = misc:get_ds_config(DsName, converged),
  {RecTime, {ok, Its, Stats}} =
    timer:tc(fun() -> reconcile(DsName, LocalDs, RemoteDs, Converged) end),

  #{data_size := DataSize, bloom_size := BloomSize, tx_cnt := TxCnt,
    rx_cnt := RxCnt} = Stats,
  lager:info("Reconciliation done (dataset=~p, time=~.2fs, its=~p, "
             "data_size=~.2f MB, bloom_size=~.2f MB, tx_cnt=~p, rx_cnt=~p)~n", 
             [DsName, sec(RecTime), Its, mb(DataSize), mb(BloomSize), TxCnt, 
              RxCnt]),
  lager:info("Size of local dataset (dataset=~p, dataset_size=~.2fMB)~n", 
             [DsName, mb(DsSize)]),
  lager:info("Ratio of transferred data to dataset size (dataset=~p, "
             "size_ratio=~.2f)~n", [DsName, (DataSize + BloomSize) / DsSize]),

  ds:unprep(LocalDs),
  ds:unprep(RemoteDs),
  Stats#{its => Its, ds_size => DsSize, prep_time => PrepTime,
         rec_time => RecTime}.

reconcile(DsName, LocalDs, RemoteDs, Converged) ->
  MaxIts = 25,
  Stats = #{data_size => 0, bloom_size => 0, tx_cnt => 0, rx_cnt => 0},
  reconcile(DsName, LocalDs, RemoteDs, 0, MaxIts, Converged, Stats).

reconcile(_, _, _, Its, MaxIts, _Converged, Stats) when Its >= MaxIts ->
  {ok, Its, Stats};
reconcile(DsName, LocalDs, RemoteDs, Its, MaxIts, Converged, Stats0) ->
  {LocalBloom, RxCnt, RxSize} = transfer(DsName, LocalDs, RemoteDs, "remote"),
  {RemoteBloom, TxCnt, TxSize} = transfer(DsName, RemoteDs, LocalDs, "local"),
  {MaxIts1, Converged1} = case Converged(Its, TxCnt, RxCnt) of
                            true  -> {Its + 2, fun(_, _, _) -> false end};
                            false -> {MaxIts, Converged}
                          end,
  Stats = Stats0#{data_size := maps:get(data_size, Stats0) + RxSize + TxSize,
                  bloom_size := maps:get(bloom_size, Stats0) +
                    bloom_size(LocalBloom) + bloom_size(RemoteBloom),
                  tx_cnt := maps:get(tx_cnt, Stats0) + TxCnt,
                  rx_cnt := maps:get(rx_cnt, Stats0) + RxCnt},
  reconcile(DsName, LocalDs, RemoteDs, Its + 1, MaxIts1, Converged1, Stats).

transfer(DsName, SourceDs, DestDs, DestStr) ->
  {Dt1, Bloom} = timer:tc(fun() -> ds:get_bloom(DestDs) end),
  lager:info("Calculated ~s bloom (dataset=~p, dt=~.2fs).", [DestStr, DsName,
                                                             sec(Dt1)]),

  {Dt2, {Cnt, Size}} =
    timer:tc(fun() -> ds:transfer_missing(SourceDs, Bloom, DestDs) end),
  lager:info("Transferred elements to ~s (dataset=~p, num_elements=~p, "
             "total_size=~pB, dt=~.2fs)~n", [DestStr, DsName, Cnt, Size, 
                                             sec(Dt2)]),
  {Bloom, Cnt, Size}.

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
