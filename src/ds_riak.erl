%% Copyright (c) 2015 Ulf Leopold.
-module(ds_riak).

-export([setup/3]).

setup(Name, Ip, Bucket) ->
  {ok, Ds} = dataset_local:start_link(Name, {Ip, Bucket, no_pid, no_keys},
                                      fun prep/1, fun get/1, fun get_vals/2,
                                      fun put/2, fun unprep/1),
  Ds.

%%%_* Internal =========================================================
prep({Ip, Bucket, _, _}) ->
  Pid = riak_ops:connect(Ip),
  Keys = riak_ops:keys(Pid, Bucket),
  Size = 1, % Could calculate ds size.
  {Size, {Ip, Bucket, Pid, Keys}}.

%% Return all {Key, Val} tuples. (get_all or perhaps get_bloom).
get({_Ip, Bucket, Pid, Keys}) ->
  map_keys(Pid, Bucket, Keys).

get_vals({_Ip, Bucket, Pid, _Keys}, L) ->
  [{Key, riak_ops:get(Pid, Bucket, Key)} || {Key, _HashedVal} <- L].

put({Ip, Bucket, Pid, Keys} = S, {Key, Val}) ->
  riak_ops:put(Pid, Bucket, Key, Val, fun resolve/2),
  {Ip, Bucket, Pid, [Key | Keys]}.

unprep({Ip, Bucket, Pid, _Keys}) ->
  riak_ops:disconnect(Pid),
  {Ip, Bucket, no_pid, no_keys}.

resolve(V1, V2) -> max(V1, V2).

map_keys(Pid, Bucket, Keys) ->
  BucketKeyPairs = [{Bucket, Key} || Key <- Keys],
  map(Pid, BucketKeyPairs).

map(Pid, BucketOrBucketKeyPairs) ->
  F =
    "fun({error, notfound}, _, _)   -> [];
        (Obj, _, _Arg) ->
           case riak_object:get_values(Obj) of
              <<>>  ->
                [];
              [Val] ->
                [{riak_object:key(Obj), crypto:sha256(Val)}];
              _     ->
                []
            end
     end.",

  {Dt, L} =
    timer:tc(
      fun() ->
          {ok, [{0, L}]} = riakc_pb_socket:mapred(Pid, BucketOrBucketKeyPairs,
                                                  [{map, {strfun, F}, "myarg",
                                                    true}],
                                                  3600000),
          L
      end),
  io:format("mapred time: ~ps. length(L): ~p~n", [Dt / (1000 * 1000),
                                                  length(L)]),
  L.
