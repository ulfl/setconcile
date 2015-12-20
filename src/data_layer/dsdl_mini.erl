%% Copyright (c) 2015 Ulf Leopold.
-module(dsdl_mini).

-export([new/1]).

new(Node) ->
  {L1, L2} = create(),
  L = case Node of
        a -> L1;
        b -> L2
      end,
  {dict:from_list(L), fun prep/2, fun count/1, fun fold/3, fun get_vals/2,
   fun put/2, fun unprep/1}.

%%%_* Internal =========================================================
prep(Dict, _Tmo) -> {{ok, misc:lsize(dict:to_list(Dict))}, Dict}.

count(Dict) -> dict:size(Dict).

fold(Dict, Fun, State) -> dict:fold(fun(K, V, S) -> Fun({K, V}, S) end, State,
                                    Dict).

get_vals(Dict, L) -> {L, Dict}.

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
