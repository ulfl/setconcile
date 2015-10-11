%% Copyright (c) 2015 Ulf Leopold.
-module(dsdl_symm).

-export([new/4]).

new(Node, N, P, B) ->
  {L1, L2, Expected} = symmetric_dataset:create(N, P, B),
  L = case Node of
        a -> L1;
        b -> L2
      end,
  {{dict:from_list(L), Expected}, fun prep/1, fun get/1, fun get_vals/2,
   fun put/2, fun unprep/1}.

%%%_* Internal =========================================================
prep(State = {Dict, _}) -> {size(dict:to_list(Dict), 0), State}.

get(_State = {Dict, _}) -> dict:to_list(Dict).

get_vals(_State, L) -> L.

put({Dict, Expected}, {K, V1}) ->
  case dict:find(K, Dict) of
    error    -> {dict:store(K, V1, Dict), Expected};
    {ok, V2} -> {dict:store(K, resolve(V1, V2), Dict), Expected}
  end.

resolve(V1, V2) -> max(V1, V2).

unprep(State = {Dict, Expected}) ->
  symmetric_dataset:verify(dict:to_list(Dict), Expected),
  State.

size([], Size)      -> Size;
size([H | T], Size) -> size(T, Size + byte_size(term_to_binary(H))).
