%% Copyright (c) 2015 Ulf Leopold.
-module(dataset_symmetric).

-export([test/0]).

test() ->
  N = 1000000,
  P = 0.1,
  {L1, L2, Base} = create(N, P, 1),
  io:format("Size of dataset A: ~.2fMB~n", [size(L1, 0) / (1024*1024)]),
  io:format("Size of dataset B: ~.2fMB~n", [size(L2, 0) / (1024*1024)]),
  A = node:make(dict:from_list(L1), fun get/1, fun put/2),
  B = node:make(dict:from_list(L2), fun get/1, fun put/2),
  reconcile:reconcile(A, B, 20),
  verify(A({get}), Base, N, P),
  verify(B({get}), Base, N, P).

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
create(N, P, BulkSize) when (trunc(N*P) > 0) and (trunc(N*P) rem 4 =:= 0) ->
  Bulk = crypto:strong_rand_bytes(BulkSize),
  BaseSet = base([], N, Bulk),
  {NewOrUpdated, Shared} = lists:split(trunc(N * P), BaseSet),
  {A, B} = new_or_updated(NewOrUpdated),
  {A ++ Shared, B ++ Shared, BaseSet}.

base(L, 0, _Bulk) -> L;
base(L, N, Bulk)  ->
  K = crypto:hash(sha256, "ds1" ++ integer_to_list(N)),
  V = {1, Bulk},
  base([{K, V} | L], N - 1, Bulk).

new_or_updated(L) ->
  {New, Updated} = split_in_two(L),
  {UniqueA, UniqueB} = split_in_two(New),
  {UpdatedA, UpdatedB} = updated(Updated),
  {UniqueA ++ UpdatedA, UniqueB ++ UpdatedB}.

updated(L) ->
  {L1, L2} = split_in_two(L),
  {L1 ++ update(L2), update(L1) ++ L2}.

update(L) -> [{K, {Time + 1, Bulk}} || {K, {Time, Bulk}} <- L].

split_in_two(L) -> lists:split(trunc(length(L) / 2), L).

verify(CurrentDataset, BaseSet, N, P) ->
  {NewOrUpdated, Shared} = lists:split(trunc(N * P), BaseSet),
  {New, Updated} = split_in_two(NewOrUpdated),
  Expected = update(Updated) ++ New ++ Shared,
  case lists:sort(CurrentDataset) =:= lists:sort(Expected) of
    true -> io:format("EQUAL!~n");
    false -> io:format("NOT EQUAL!~n")
  end.

size([], Size)      -> Size;
size([H | T], Size) -> size(T, Size + byte_size(term_to_binary(H))).
