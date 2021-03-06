%% Copyright (c) 2015 Ulf Leopold.
-module(dsdl_symm).

-export([new/4]).

new(Node, N, P, B) ->
  {L1, L2, Expected} = symmetric_dataset:create(N, P, B),
  L = case Node of
        a -> L1;
        b -> L2
      end,
  {{dict:from_list(L), Expected, false}, fun prep/2, fun count/1, fun fold/3,
   fun get_vals/2, fun put/2, fun unprep/1}.

%%%_* Internal =========================================================
prep(State = {_Dict, _Expected, true}, _Tmo) ->
  {error_sync_in_progress, State};
prep(_State = {Dict, Expected, false}, _Tmo) ->
  {{ok, misc:lsize(dict:to_list(Dict))}, {Dict, Expected, true}}.

count(_State = {Dict, _Expected, true}) -> dict:size(Dict).

fold(_State = {Dict, _Expected, true}, Fun, State) ->
  dict:fold(fun(K, V, S) -> Fun({K, V}, S) end, State, Dict).

get_vals(State, L) -> {L, State}.

put(_State = {Dict, Expected, true}, {K, V1}) ->
  case dict:find(K, Dict) of
    error    -> {dict:store(K, V1, Dict), Expected, true};
    {ok, V2} -> {dict:store(K, resolve(V1, V2), Dict), Expected, true}
  end.

resolve(V1, V2) -> max(V1, V2).

unprep(_State = {Dict, Expected, _Prepped}) ->
  symmetric_dataset:verify(dict:to_list(Dict), Expected),
  {Dict, Expected, false}.
