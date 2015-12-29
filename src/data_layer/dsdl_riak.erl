%% Copyright (c) 2015 Ulf Leopold.
-module(dsdl_riak).

-export([new/3]).
-export([new/4]).

%% For perf tests.
-export([get_riak_data/3]).
-export([default_map_fun/0]).

-record(state, {
          ip :: string(),
          bucket :: binary(),
          resolver :: fun(),
          pid = no_pid :: pid() | no_pid,
          key_vals = no_keys :: gb_trees:tree() | no_keys,
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
  {State, fun prep/2, fun count/1, fun fold/3, fun get_vals/2, fun put/2,
   fun unprep/1}.

%%%_* Internal =========================================================

%% Prepare to start syncing.
prep(#state{pid=Pid, key_vals=KeyVals}=State0, _) ->
  case {Pid, KeyVals} of
    {no_pid, no_keys} ->
      {Size, State} = do_prep(State0),
      {{ok, Size}, State};
    {_, _} when is_pid(Pid) ->
      {error_sync_in_progress, State0}
  end.

do_prep(#state{ip=Ip, bucket=Bucket, map_fun_str=MapFunStr}=State) ->
  Pid = riak_ops:connect(Ip),
  {T, {ok, {KeyVals, Size}}} =
    timer:tc(fun() -> get_riak_data(Pid, Bucket, MapFunStr) end),
  lager:info("Prep done (num_elements=~p, size_bytes=~p, prep_time_s=~p).",
             [gb_trees:size(KeyVals), Size, sec(T)]),
  {Size, State#state{pid=Pid, key_vals=KeyVals}}.

count(#state{key_vals=KeyVals}) -> gb_trees:size(KeyVals).

%% Fold a function over the Key/HashedVal space.
fold(#state{key_vals=KeyVals}, Fun, State) ->
  fold_loop(gb_trees:iterator(KeyVals), Fun, State).

fold_loop(Itr, Fun, State) ->
  case gb_trees:next(Itr) of
    {K, V, Itr1} -> fold_loop(Itr1, Fun, Fun({K, V}, State));
    none         -> State
  end.

%% Given a list L of {Key, HashedVal} tuples, return the list of {Key,
%% Val}, i.e. unhashed values. If a Key is found to be deleted, then
%% then it is not included in the final result and it is also removed
%% from the current state.
get_vals(#state{} = State, L) -> do_get_vals(State, L, []).

do_get_vals(State, [], Result) -> {Result, State};
do_get_vals(#state{key_vals=KeyVals, pid=Pid, bucket=Bucket,
                   resolver=Resolver} = State0,
            [{Key, _HashedVal} | T], Result) ->
  case riak_ops:get(Pid, Bucket, Key, Resolver) of
    not_found ->
      State = State0#state{key_vals=gb_trees:delete(Key, KeyVals)},
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
      State#state{key_vals=gb_trees:enter(Key, Hash, KeyVals)};
    {error, Err} ->
      lager:error("Write error: ~p", [Err]),
      State
  end.

%% Cleanup after syncing is done.
unprep(#state{pid=Pid}=State) ->
  riak_ops:disconnect(Pid),
  State#state{pid=no_pid, key_vals=no_keys}.

get_riak_data(Pid, Bucket, MapFunStr) ->
  Parent = self(),
  spawn_link(
    fun() ->
        {ok, _Id} = riakc_pb_socket:mapred_stream(Pid, Bucket,
                                                  [{map, {strfun, MapFunStr},
                                                    "myarg", true}],
                                                  Parent, almost_infinity())
    end),
  {T, Res} = timer:tc(fun() -> mapred_receiver(0, gb_trees:empty()) end),
  lager:info("MapReduce done. (bucket=~p, time_s=~.2f)", [Bucket, sec(T)]),
  Res.

almost_infinity() -> 7 * 24 * 3600 * 1000.

mapred_receiver(Size, Tree) ->
  receive
    {_Id, {mapred, 0, Res}} ->
      {Tree1, ObjSize} = lists:foldl(fun({Key, ObjSize, Hash}, {T, S}) ->
                                         {gb_trees:insert(Key, Hash, T),
                                          S + ObjSize}
                                     end, {Tree, 0}, Res),
      mapred_receiver(Size + ObjSize, Tree1);
    {_Id, done}            -> {ok, {Tree, Size}};
    {_Id, {error, Reason}} -> {error, Reason}
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

sec(T) -> T / (1000 * 1000).
