%% Copyright (c) 2015 Ulf Leopold.
-module(symmetric_dataset).

-export([create/3]).
-export([create_onesided/2]).
-export([verify/2]).

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
create(N, P, NumBulkBytes) when (trunc(N*P) > 0) and
                                (trunc(N*P) rem 4 =:= 0) and
                                (NumBulkBytes rem 16 =:= 0) ->
  random:seed({1, 2, 3}),
  BaseSet = base([], N, NumBulkBytes),
  {NewOrUpdated, Shared} = lists:split(trunc(N * P), BaseSet),
  {New, Updated} = split_in_two(NewOrUpdated),
  {A, B} = new_or_updated(New, Updated),
  Expected = Shared ++ New ++ update(Updated),
  {A ++ Shared, B ++ Shared, Expected}.

create_onesided(N, NumBulkBytes) when (NumBulkBytes rem 16 =:= 0) ->
  random:seed({1, 2, 3}),
  base([], N, NumBulkBytes).

base(L, 0, _NumBulkBytes) -> L;
base(L, N, NumBulkBytes)  ->
  K = crypto:hash(sha256, "ds1" ++ integer_to_list(N)),
  Seq = list_to_binary(lists:map(fun(_) -> random:uniform(255) end,
                                 lists:seq(1, 16))),
  Bulk = generate_bulk(Seq, NumBulkBytes),
  V = {1, Bulk},
  base([{K, V} | L], N - 1, NumBulkBytes).

generate_bulk(Seq, NumBulkBytes) ->
  generate_bulk(Seq, <<>>, NumBulkBytes div 16).

generate_bulk(_Seq, Acc, 0) ->
  Acc;
generate_bulk(Seq, Acc, Repeat) ->
  generate_bulk(Seq, <<Acc/binary, Seq/binary>>, Repeat - 1).

new_or_updated(New, Updated) ->
  {UniqueA, UniqueB} = split_in_two(New),
  {L1, L2} = split_in_two(Updated),
  UpdatedA = update(L1) ++ L2,
  UpdatedB = L1 ++ update(L2),
  {UniqueA ++ UpdatedA, UniqueB ++ UpdatedB}.

split_in_two(L) -> lists:split(trunc(length(L) / 2), L).

update(L) -> [{K, {Time + 1, Bulk}} || {K, {Time, Bulk}} <- L].

verify(Elements, ExpectedElements) ->
  lager:info("num_elements_current=~p, num_elements_expected=~p.",
             [length(Elements), length(ExpectedElements)]),
  compare(lists:sort(Elements), lists:sort(ExpectedElements)).

compare([], []) ->
  lager:info("Correct!~n", []),
  match;
compare([], [H|_]) ->
  lager:info("Incorrect. Expected elem ~p!~n", [H]);
compare([H|_], []) ->
  lager:info("Incorrect. Extra elem ~p!~n", [H]);
compare([H1|L1], [H2|L2]) ->
  case H1 =:= H2 of
    true -> compare(L1, L2);
    false -> lager:info("Incorrect. Elements ~p and ~p don't match!~n",
                        [H1, H2])
  end.
