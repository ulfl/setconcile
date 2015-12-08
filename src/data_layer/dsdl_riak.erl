%% Copyright (c) 2015 Ulf Leopold.
-module(dsdl_riak).

-export([new/3]).
-export([new/4]).

-record(state, {
          ip :: string(),
          bucket :: binary(),
          resolver :: fun(),
          pid = no_pid :: pid() | no_pid,
          key_vals = no_keys :: dict:dict() | no_keys,
          map_fun_str :: string()
         }).

new(DbIp, Bucket, Resolver) -> new(DbIp, Bucket, Resolver, default_map_fun()).

%% MapFunStr should be a string implementing a Riak map function. The
%% map operation must return either [{Key, Size, Sha}] (where Key is the
%% object key, Size is the size of the value in bytes, and Sha is the
%% SHA-128 value of the object), or return the empty list if the value
%% should be ignored.
new(DbIp, Bucket, Resolver, MapFunStr) ->
  State = #state{ip = DbIp, bucket = Bucket, resolver = Resolver,
                 map_fun_str = MapFunStr},
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
  lager:info("Prep done (num_elements=~p, size=~p, prep_time_s=~p).",
             [dict:size(KeyVals), Size, Dt / (1000 * 1000)]),
  {Size, State#state{pid=Pid, key_vals=KeyVals}}.

%% Get the list of {Key, HashedVal} tuples.
get(#state{key_vals=KeyVals}) -> dict:to_list(KeyVals).

%% Given a list L of {Key, HashedVal} tuples, return the list of {Key,
%% Val}, i.e. unhashed values. If a Key is found to be deleted, then
%% then it is not included in the final restult and it is also removed
%% from the current state.
get_vals(#state{} = State, L) -> do_get_vals(State, L, []).

do_get_vals(State, [], Result) -> {Result, State};
do_get_vals(#state{key_vals=KeyVals, pid=Pid, bucket=Bucket,
                   resolver=Resolver} = State0,
            [{Key, _HashedVal} | T], Result) ->
  case riak_ops:get(Pid, Bucket, Key, Resolver) of
    not_found ->
      State = State0#state{key_vals=dict:erase(Key, KeyVals)},
      lager:info("Key ~p not found. Deleted from state.", [Key]),
      do_get_vals(State, T, Result);
    {ok, Val} ->
      do_get_vals(State0, T, [{Key, Val} | Result])
  end.

%% Store a {Key, Value} pair. Write errors are ignored. I.e. there will
%% be a retry on the next sync iteration.
put(#state{bucket=Bucket, resolver=Resolver, pid=Pid, key_vals=KeyVals}=State,
    {Key, Val}) ->
  case riak_ops:put(Pid, Bucket, Key, Val, Resolver) of
    {ok, NewVal} ->
      Hash = crypto:hash(sha, term_to_binary(NewVal)),
      State#state{key_vals=dict:store(Key, Hash, KeyVals)};
    {error, Err} ->
      lager:error("Write error: ~p", [Err]),
      State
  end.

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
