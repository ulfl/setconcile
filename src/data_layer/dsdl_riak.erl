%% Copyright (c) 2015 Ulf Leopold.
-module(dsdl_riak).

-export([new/3]).
-export([new/4]).

new(DbIp, Bucket, Resolver) ->
  new(DbIp, Bucket, Resolver, default_map_fun()).

%% MapFunStr should be a string implementing a Riak map function.
%% The map operation must return either [{Key, Size, Sha}] where
%% Key is the object key, Size is the size of the value in bytes,
%% and Sha is the SHA-128 value of the object, or the empty list
%% if the value should be ignored.
new(DbIp, Bucket, Resolver, MapFunStr) ->
  {{DbIp, Bucket, Resolver, no_pid, no_keys, MapFunStr}, fun prep/1, fun get/1,
   fun get_vals/2, fun put/2, fun unprep/1}.

%%%_* Internal =========================================================

%% Prepare to start syncing.
prep({Ip, Bucket, Resolver, _, _, MapFunStr}) ->
  Pid = riak_ops:connect(Ip),
  DoPrep = fun() ->
               KeyValsSize = map(Pid, Bucket, MapFunStr),
               {KV, Size} = lists:foldl(
                              fun({Key, Size, Val}, {KeyVals, TotalSize}) ->
                                  {[{Key, Val} | KeyVals], Size + TotalSize}
                              end, {[], 0}, KeyValsSize),
               {dict:from_list(KV), Size}
           end,
  {Dt, {KeyVals, Size}} = timer:tc(fun() -> DoPrep() end),
  lager:info("dsdl_riak prep (num_elements=~p, size=~p, prep_time_s=~p).",
             [dict:size(KeyVals), Size, Dt / (1000 * 1000)]),
  {Size, {Ip, Bucket, Resolver, Pid, KeyVals, MapFunStr}}.

%% Get the list of {Key, HashedVal} tuples.
get({_Ip, _Bucket, _Resolver, _Pid, KeyVals, _MapFunStr}) ->
  dict:to_list(KeyVals).

%% Given a list L of {Key, HashedVal} tuples, return the list of {Key,
%% Val}, i.e. unhashed values.
get_vals({_Ip, Bucket, Resolver, Pid, _KeyVals, _MapFunStr}, L) ->
  [{Key, riak_ops:get(Pid, Bucket, Key, Resolver)} || {Key, _HashedVal} <- L].

%% Store a {Key, Value} pair.
put({Ip, Bucket, Resolver, Pid, KeyVals, _MapFunStr}, {Key, Val}) ->
  NewVal = riak_ops:put(Pid, Bucket, Key, Val, Resolver),
  Hash = crypto:hash(sha, term_to_binary(NewVal)),
  {Ip, Bucket, Resolver, Pid, dict:store(Key, Hash, KeyVals)}.

%% Cleanup after syncing is done.
unprep({Ip, Bucket, Resolver, Pid, _Keys, _MapFunStr}) ->
  riak_ops:disconnect(Pid),
  {Ip, Bucket, Resolver, no_pid, no_keys}.

map(Pid, Bucket, MapFunStr) ->
  Res = riakc_pb_socket:mapred(Pid, Bucket, [{map, {strfun, MapFunStr}, "myarg",
                                              true}], 3600000),
  case Res of
    {ok, [{0, X}]} -> X;
    {ok, []} -> []
  end.

default_map_fun() ->
  "fun({error, notfound}, _, _)   -> [];
      (Obj, _, _Arg) ->
         case riak_object:get_values(Obj) of
            <<>>  ->
              [];
            [Val] ->
              [{riak_object:key(Obj), byte_size(Val), crypto:sha(Val)}];
            _     ->
              []
          end
   end.".
