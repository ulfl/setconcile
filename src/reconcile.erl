%% Copyright (c) 2015 Ulf Leopold.
-module(reconcile).

-export([reconcile/3]).
-export([create_bloom/1]).
-export([filter/3]).

reconcile(LocalDataset, RemoteDataset, MaxIts) ->
  reconcile(LocalDataset, RemoteDataset, 0, MaxIts, fun converged/2, 0, 0).

reconcile(_, _, Its, MaxIts, _Converged, Size, BloomSize) when Its >= MaxIts ->
  {ok, Its, Size, BloomSize};
reconcile(LocalDataset, RemoteDataset, Its, MaxIts, Converged, Size,
          BloomSize) ->
  {Dt1, LocalBloom} = timer:tc(fun() -> dataset:get_bloom(LocalDataset) end),
  lager:info("Calculated local bloom (dt=~.2fs).", [s(Dt1)]),

  {Dt2, {ReceiveCount, ReceiveSize}} =
    timer:tc(fun() -> dataset:post_transfer(RemoteDataset, LocalBloom,
                                            LocalDataset)
             end),
  lager:info("Received from remote (num_elements=~p, total_size=~pB, "
             "dt=~.2fs)~n", [ReceiveCount, ReceiveSize, s(Dt2)]),

  {Dt3, RemoteBloom} = timer:tc(fun() -> dataset:get_bloom(RemoteDataset) end),
  lager:info("Calculated remote bloom (dt=~.2fs).", [s(Dt3)]),

  {Dt4, {SendCount, SendSize}} =
    timer:tc(fun() -> dataset:post_transfer(LocalDataset, RemoteBloom,
                                            RemoteDataset)
             end),
  lager:info("Transferred to remote (num_elements=~p, total_size=~pB, "
             "dt=~.2fs)~n", [SendCount, SendSize, s(Dt4)]),

  {MaxIts1, Converged1} = case Converged(ReceiveCount, SendCount) of
              true  -> {Its + 2, fun(_, _) -> false end};
              false -> {MaxIts, Converged}
            end,
  reconcile(LocalDataset, RemoteDataset, Its + 1, MaxIts1, Converged1,
            Size + ReceiveSize + SendSize,
            BloomSize + bloom_size(LocalBloom) + bloom_size(RemoteBloom)).

converged(C1, C2) -> C1 + C2 =:= 0.

bloom_size(B) -> byte_size(ebloom:serialize(B)).

create_bloom(L) ->
  N = length(L),
  {ok, FalseProbability} = config:get(bloom_false_probability),
  {ok, B} = ebloom:new(N, FalseProbability, random:uniform(10000000)),
  create_bloom(L, B).

create_bloom([], B) ->
  B;
create_bloom([H | T], B) ->
  ebloom:insert(B, term_to_binary(H)),
  create_bloom(T, B).

filter([], _B, Remainder)      -> Remainder;
filter([H | T], B, Remainder)  ->
  case ebloom:contains(B, term_to_binary(H)) of
    true  -> filter(T, B, Remainder);
    false -> filter(T, B, [H | Remainder])
  end.

s(T) -> T / (1000 * 1000).

%% t(Msg, Fun) ->
%%   {T, R} = timer:tc(Fun),
%%   lager:info(Msg, [T / (1000 * 1000)p]),
%%   R.
