%% Copyright (c) 2015 Ulf Leopold.
-module(dsdl_mini).

-export([new/1]).

new(Node) ->
  {L1, L2} = create(),
  L = case Node of
        a -> L1;
        b -> L2
      end,
  {dict:from_list(L), fun prep/1, fun get/1, fun get_vals/2,
   fun put/2, fun unprep/1}.

%%%_* Internal =========================================================
prep(Dict) -> {size(dict:to_list(Dict), 0), Dict}.

get(Dict) -> dict:to_list(Dict).

get_vals(_Dict, L) -> L.

put(Dict, {K, V1}) ->
  case dict:find(K, Dict) of
    error    -> dict:store(K, V1, Dict);
    {ok, V2} -> dict:store(K, resolve(V1, V2), Dict)
  end.

resolve(V1, V2) -> max(V1, V2).

unprep(Dict) ->
  verify(dict:to_list(Dict)),
  Dict.

create() ->
  Base = [{1, v1}, {2, v1}],
  A = [{3, v1}] ++ Base,
  B = [{4, v1}] ++ lists:keyreplace(2, 1, Base, {2, v2}),
  {A, B}.

verify(L) ->
  Expected = [{1, v1}, {2, v2}, {3, v1}, {4, v1}],
  case lists:sort(L) =:= lists:sort(Expected) of
    true -> lager:info("Correct!~n", []);
    false -> lager:info("INCORRECT!~n", [])
  end.

size([], Size)      -> Size;
size([H | T], Size) -> size(T, Size + byte_size(term_to_binary(H))).
