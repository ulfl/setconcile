%% Copyright (c) 2015 Ulf Leopold.
-module(ds_local).
-behaviour(gen_server).

%% API.
-export([new/2]).

%% Gen server callbacks.
-export([init/1]).
-export([handle_call/3]).
-export([handle_cast/2]).
-export([handle_info/2]).
-export([terminate/2]).
-export([code_change/3]).

new(Name, {DsdlState, Prep, Count, Fold, GetVals, Put, Unprep}) ->
  {ok, Ds} = gen_server:start_link(?MODULE, [Name, DsdlState, Prep, Count, Fold,
                                             GetVals, Put, Unprep], []),
  Ds.

%%%_* Gen server callbacks =============================================
init([Name, DsdlState, Prep, Count, Fold, GetVals, Put, Unprep]) ->
  {ok, #{ds_name => Name, dsdl_state => DsdlState, prep => Prep, count => Count,
         fold => Fold, get_vals => GetVals, put => Put, unprep => Unprep}}.

handle_call(prep, _From, #{ds_name := Name, dsdl_state := DsdlState0,
                           prep := Prep} = S) ->
  Tmo = misc:get_ds_config(Name, tmo_prep),
  {Result, DsdlState} = Prep(DsdlState0, Tmo),
  {reply, Result, S#{dsdl_state := DsdlState}};
handle_call(get_bloom, _From, #{ds_name := Name, dsdl_state := DsdlState,
                                count := Count, fold := Fold} = S) ->
  FalseProbability = misc:get_ds_config(Name, bloom_false_probability),
  Bloom = reconcile:bloom_create(Count(DsdlState), FalseProbability),
  Fold(DsdlState, fun(X = {_K, _V}, state) ->
                      reconcile:bloom_insert(Bloom, X),
                      state
                  end, state),
  {reply, {ok, Bloom}, S};
handle_call({transfer_missing, Bloom, DestDs}, _From,
            #{ds_name := Name, dsdl_state := DsdlState0, fold := Fold,
              get_vals := GetVals} = S) ->
  L = Fold(DsdlState0, fun(X = {_K, _V}, L) ->
                           case reconcile:bloom_contains(Bloom, X) of
                             true  -> L;
                             false -> [X | L]
                           end
                       end, []),
  MaxSize = misc:get_ds_config(Name, max_transfer_bundle),
  {Size, DsdlState} = foreach_bundle(fun(Bundle, {Size, Dsdl0}) ->
                                         {KeyVals, Dsdl} = GetVals(Dsdl0, Bundle),
                                         X = ds:store_elements(DestDs, KeyVals),
                                         {Size + X, Dsdl}
                                     end, L, MaxSize, {0, DsdlState0}),
  {reply, {ok, {length(L), Size}}, S#{dsdl_state := DsdlState}};
handle_call({store_elements, L}, _From, #{dsdl_state := DsdlState0,
                                          put := Put} = S) ->
  DsdlState = lists:foldl(fun(X, A) -> Put(A, X) end, DsdlState0, L),
  {reply, {ok, byte_size(term_to_binary(L))}, S#{dsdl_state := DsdlState}};
handle_call(unprep, _From, #{dsdl_state := DsdlState0, unprep := Unprep} = S) ->
  DsdlState = Unprep(DsdlState0),
  timer:send_after(3000, self(), collect_garbage),
  {reply, ok, S#{dsdl_state := DsdlState}}.

handle_cast(Msg, S) -> {stop, {unexpected_cast, Msg}, S}.

%% Collect garbage after dataset keyval references have been released.
handle_info(collect_garbage, S) ->
  erlang:garbage_collect(self()),
  {noreply, S};
handle_info(Msg, S)             ->
  {stop, {unexpected_info, Msg}, S}.

terminate(_Reason, _S) -> ok.

code_change(_OldVsn, S, _Extra) -> {ok, S}.

%%%_* Internal =========================================================

%% Call F for a bundle of MaxSize elements out of L at a time. The State
%% variable is threaded through all calls to F.
foreach_bundle(F, L, MaxSize, State) ->
  foreach_bundle(F, L, MaxSize, State, MaxSize, []).

foreach_bundle(F, [], _MaxSize, State, _Cnt, Acc) ->
  F(Acc, State);
foreach_bundle(F, L, MaxSize, State, 0, Acc) ->
  foreach_bundle(F, L, MaxSize, F(Acc, State), MaxSize, []);
foreach_bundle(F, [H | T], MaxSize, State, Cnt, Acc) ->
  foreach_bundle(F, T, MaxSize, State, Cnt - 1, [H | Acc]).

%%%_* Tests ============================================================
-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").
foreach_bundle_test() ->
  Cnt = misc:make_counter(),
  F = fun(L, State) ->
          case Cnt(get) of
            1 -> ?assertEqual([3, 2, 1], L);
            2 -> ?assertEqual([6, 5, 4], L);
            3 -> ?assertEqual([7], L)
          end,
          Cnt(inc),
          State + 1
      end,
  ?assertEqual(3, foreach_bundle(F, [1, 2, 3, 4, 5, 6, 7], 3, 0)).
-endif.
