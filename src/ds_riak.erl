%% Copyright (c) 2015 Ulf Leopold.
-module(ds_riak).

-export([setup/3]).

%% Configure resolver and strip function at this level.
setup(Name, Ip, Bucket) ->
  {ok, Ds} = dataset_local:start_link(Name, {Ip, Bucket, no_pid, no_keys},
                                      fun prep/1, fun get/1, fun get_vals/2,
                                      fun put/2, fun unprep/1),
  Ds.

%%%_* Internal =========================================================

%% Prepare to start syncing.
prep({Ip, Bucket, _, _}) ->
  Pid = riak_ops:connect(Ip),
  KeyVals0 = map(Pid, Bucket),
  KeyVals = dict:from_list(KeyVals0),
  Size = 1, % Could calculate ds size.
  {Size, {Ip, Bucket, Pid, KeyVals}}.

%% Get the list of {Key, HashedVal} tuples.
get({_Ip, _Bucket, _Pid, KeyVals}) -> dict:to_list(KeyVals).

%% Given a list L of {Key, HashedVal} tuples, return the list of {Key,
%% Val}, i.e. unhashed values. Since we don't use hashes we return L as
%% is.
get_vals({_Ip, Bucket, Pid, _KeyVals}, L) ->
  [{Key, riak_ops:get(Pid, Bucket, Key, fun resolve/2)} ||
    {Key, _HashedVal} <- L].

%% Store a {Key, Value} pair.
put({Ip, Bucket, Pid, KeyVals}, {Key, Val}) ->
  NewVal = riak_ops:put(Pid, Bucket, Key, Val, fun resolve/2),
  Hash = crypto:hash(sha, term_to_binary(NewVal)),
  {Ip, Bucket, Pid, dict:store(Key, Hash, KeyVals)}.

%% Cleanup after syncing is done.
unprep({Ip, Bucket, Pid, _Keys}) ->
  riak_ops:disconnect(Pid),
  {Ip, Bucket, no_pid, no_keys}.

resolve(V1, V2) -> max(V1, V2).

%% map_keys(Pid, Bucket, Keys) ->
%%   BucketKeyPairs = [{Bucket, Key} || Key <- Keys],
%%   map(Pid, BucketKeyPairs).

map(Pid, BucketOrBucketKeyPairs) ->
  F =
    "fun({error, notfound}, _, _)   -> [];
        (Obj, _, _Arg) ->
           case riak_object:get_values(Obj) of
              <<>>  ->
                [];
              [Val] ->
                [{riak_object:key(Obj), crypto:sha(Val)}];
              _     ->
                []
            end
     end.",

  {Dt, L} =
    timer:tc(
      fun() ->
          Res = riakc_pb_socket:mapred(Pid, BucketOrBucketKeyPairs,
                                          [{map, {strfun, F}, "myarg",
                                            true}], 3600000),
          L = case Res of
                {ok, [{0, X}]} -> X;
                {ok, []} -> []
              end,
          L
      end),
  io:format("mapred time: ~ps. length(L): ~p~n", [Dt / (1000 * 1000),
                                                  length(L)]),
  L.
