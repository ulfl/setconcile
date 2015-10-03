%% Copyright (c) 2015 Ulf Leopold.
-module(dsdl_riak).

-export([new/3]).

new(DbIp, Bucket, Resolver) ->
  {{DbIp, Bucket, Resolver, no_pid, no_keys}, fun prep/1, fun get/1,
   fun get_vals/2, fun put/2, fun unprep/1}.

%%%_* Internal =========================================================

%% Prepare to start syncing.
prep({Ip, Bucket, Resolver, _, _}) ->
  Pid = riak_ops:connect(Ip),
  DoPrep = fun() ->
               KeyValsSize = map(Pid, Bucket),
               {KV, Size} = lists:foldl(
                              fun({Key, Size, Val}, {KeyVals, TotalSize}) ->
                                  {[{Key, Val} | KeyVals], Size + TotalSize}
                              end, {[], 0}, KeyValsSize),
               {dict:from_list(KV), Size}
           end,
  {Dt, {KeyVals, Size}} = timer:tc(fun() -> DoPrep() end),
  lager:info("dsdl_riak prep (num_elements=~p, size=~p, prep_time_s=~p).",
             [dict:size(KeyVals), Size, Dt / (1000 * 1000)]),
  {Size, {Ip, Bucket, Resolver, Pid, KeyVals}}.

%% Get the list of {Key, HashedVal} tuples.
get({_Ip, _Bucket, _Resolver, _Pid, KeyVals}) -> dict:to_list(KeyVals).

%% Given a list L of {Key, HashedVal} tuples, return the list of {Key,
%% Val}, i.e. unhashed values.
get_vals({_Ip, Bucket, Resolver, Pid, _KeyVals}, L) ->
  [{Key, riak_ops:get(Pid, Bucket, Key, Resolver)} || {Key, _HashedVal} <- L].

%% Store a {Key, Value} pair.
put({Ip, Bucket, Resolver, Pid, KeyVals}, {Key, Val}) ->
  NewVal = riak_ops:put(Pid, Bucket, Key, Val, Resolver),
  Hash = crypto:hash(sha, term_to_binary(NewVal)),
  {Ip, Bucket, Resolver, Pid, dict:store(Key, Hash, KeyVals)}.

%% Cleanup after syncing is done.
unprep({Ip, Bucket, Resolver, Pid, _Keys}) ->
  riak_ops:disconnect(Pid),
  {Ip, Bucket, Resolver, no_pid, no_keys}.

map(Pid, Bucket) ->
  F =
    "fun({error, notfound}, _, _)   -> [];
        (Obj, _, _Arg) ->
           case riak_object:get_values(Obj) of
              <<>>  ->
                [];
              [Val] ->
                Bin = term_to_binary(Val),
                [{riak_object:key(Obj), byte_size(Bin), crypto:sha(Val)}];
              _     ->
                []
            end
     end.",

  {Dt, L} =
    timer:tc(
      fun() ->
          Res = riakc_pb_socket:mapred(Pid, Bucket, [{map, {strfun, F}, "myarg",
                                                      true}], 3600000),
          L = case Res of
                {ok, [{0, X}]} -> X;
                {ok, []} -> []
              end,
          L
      end),
  L.
