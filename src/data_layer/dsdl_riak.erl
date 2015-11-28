%% Copyright (c) 2015 Ulf Leopold.
-module(dsdl_riak).

-export([new/3]).
-export([new/4]).

-record(state, {
  ip :: string(),
  bucket :: binary(),
  resolver :: atom(),
  pid = no_pid :: pid() | no_pid,
  key_vals = no_keys :: dict:dict() | no_keys,
  map_fun_str :: string()
}).

new(DbIp, Bucket, Resolver) ->
  new(DbIp, Bucket, Resolver, default_map_fun()).

%% MapFunStr should be a string implementing a Riak map function. The
%% map operation must return either [{Key, Size, Sha}] (where Key is the
%% object key, Size is the size of the value in bytes, and Sha is the
%% SHA-128 value of the object), or return the empty list if the value
%% should be ignored.
new(DbIp, Bucket, Resolver, MapFunStr) ->
  State = #state{
             ip = DbIp,
             bucket = Bucket,
             resolver = Resolver,
             map_fun_str = MapFunStr
            },
  {State, fun prep/1, fun get/1, fun get_vals/2, fun put/2, fun unprep/1}.

%%%_* Internal =========================================================

%% Prepare to start syncing.
prep(#state{ip=Ip, bucket=Bucket, map_fun_str=MapFunStr}=State) ->
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
  {Size, State#state{pid=Pid, key_vals=KeyVals}}.

%% Get the list of {Key, HashedVal} tuples.
get(#state{key_vals=KeyVals}) ->
  dict:to_list(KeyVals).

%% Given a list L of {Key, HashedVal} tuples, return the list of {Key,
%% Val}, i.e. unhashed values.
get_vals(#state{bucket=Bucket, resolver=Resolver, pid=Pid}, L) ->
  [{Key, riak_ops:get(Pid, Bucket, Key, Resolver)} || {Key, _HashedVal} <- L].

%% Store a {Key, Value} pair.
put(#state{bucket=Bucket, resolver=Resolver, pid=Pid, key_vals=KeyVals}=State,
    {Key, Val}) ->
  NewVal = riak_ops:put(Pid, Bucket, Key, Val, Resolver),
  Hash = crypto:hash(sha, term_to_binary(NewVal)),
  State#state{key_vals=dict:store(Key, Hash, KeyVals)}.

%% Cleanup after syncing is done.
unprep(#state{pid=Pid}=State) ->
  riak_ops:disconnect(Pid),
  State#state{pid=no_pid, key_vals=no_keys}.

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
