%% Copyright (c) 2015 Ulf Leopold.
-module(reconcile).

-export([reconcile/1]).
-export([bloom_create/2]).
-export([bloom_insert/2]).
-export([bloom_contains/2]).

reconcile(DsName) ->
  LocalDs = misc:local_dataset(DsName),
  RemoteDs = misc:remote_dataset(DsName),

  %% Prep both sides simultaneously.
  lager:info("Prepping dataset for sync (dataset=~p).", [DsName]),
  {PrepTime, [R1, R2]} =
    timer:tc(fun() ->
                 plists:map(fun(F) -> F() end,
                            [fun() -> ds:prep(LocalDs) end,
                             fun() -> ds:prep(RemoteDs) end],
                            {processes, 2})
             end),
  case (R1 =:= error_sync_in_progress) or (R2 =:= error_sync_in_progress) of
    false ->
      {ok, LocalDsSize} = R1,
      lager:info("Prepping done (time_s=~.2f).", [sec(PrepTime)]),
      Res = reconcile_and_print_stats(DsName, LocalDs, LocalDsSize, RemoteDs,
                                      PrepTime),
      ds:unprep(LocalDs),
      ds:unprep(RemoteDs),
      Res;
    true ->
      error("Sync attempt failed since a sync is already in progress.")
  end.

reconcile_and_print_stats(DsName, LocalDs, LocalDsSize, RemoteDs, PrepTime) ->
  try
    Converged = misc:get_ds_config(DsName, converged),
    {RecTime, {ok, Its, Stats}} =
      timer:tc(fun() -> reconcile(DsName, LocalDs, RemoteDs, Converged) end),

    #{data_size := DataSize, bloom_size := BloomSize, tx_cnt := TxCnt,
      rx_cnt := RxCnt} = Stats,
    lager:info("Reconciliation done (dataset=~p, time_s=~.2f, its=~p, "
               "data_size_mb=~.2f, bloom_size_mb=~.2f, tx_cnt=~p, rx_cnt=~p).",
               [DsName, sec(RecTime), Its, mb(DataSize), mb(BloomSize), TxCnt, 
                RxCnt]),
    lager:info("Size of local dataset (dataset=~p, dataset_size_mb=~.2f).",
               [DsName, mb(LocalDsSize)]),
    lager:info("Ratio of transferred data to dataset size (dataset=~p, "
               "size_ratio=~.3f).", [DsName, (DataSize + BloomSize) /
                                       LocalDsSize]),
    Stats#{its => Its, ds_size => LocalDsSize, prep_time => PrepTime,
           rec_time => RecTime}
  catch
    C:E -> lager:error("Error during reconciliation (error={~p, ~p}, stack=~p).",
                       [C, E, erlang:get_stacktrace()]),
           #{ds_size => LocalDsSize, prep_time => PrepTime}
  end.

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
  lager:info("Calculated ~s bloom (dataset=~p, time_s=~.2f).",
             [DestStr, DsName, sec(Dt1)]),

  {Dt2, {Cnt, Size}} =
    timer:tc(fun() -> ds:transfer_missing(SourceDs, Bloom, DestDs) end),
  lager:info("Transferred elements to ~s (dataset=~p, num_elements=~p, "
             "total_size_bytes=~p, time_s=~.2f).", [DestStr, DsName, Cnt, Size,
                                                    sec(Dt2)]),
  {Bloom, Cnt, Size}.

bloom_create(Size, FalseProbability) ->
  {ok, B} = ebloom:new(1 + Size, FalseProbability, random:uniform(10000000)),
  B.

bloom_size(Bloom) -> byte_size(ebloom:serialize(Bloom)).

bloom_insert(Bloom, Val) -> ebloom:insert(Bloom, term_to_binary(Val)).

bloom_contains(Bloom, Val) -> ebloom:contains(Bloom, term_to_binary(Val)).

mb(X) -> X / (1024 * 1024).

sec(T) -> T / (1000 * 1000).
