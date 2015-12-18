%% Copyright (c) 2015 Ulf Leopold.
-module(riak_setup).

-export([symm/5]).
-export([symm_onesided/4]).
-export([verify/5]).
-export([mini/0]).
-export([clear/2]).
-export([count/2]).
-export([keys/2]).
-export([get/3]).
-export([nuclear/0]).

%% i.e. riak_setup:symm("127.0.0.1", <<"set_a">>, 1000, 0.1, 2048).
symm(Ip, Bucket, N, P, B) ->
  nuclear(),
  Pid = riak_ops:connect(Ip),
  riak_ops:configure_bucket(Pid, Bucket),
  io:format("Creating objects~n", []),
  {Dt, _} = timer:tc(fun() -> create_objects(Pid, Bucket, N, P, B) end),
  io:format("Setup time: ~ps~n", [Dt / (1000 * 1000)]),
  riak_ops:disconnect(Pid),
  ok.

%% i.e. riak_setup:symm_onesided("127.0.0.1", <<"set_a">>, 1000, 1024).
symm_onesided(Ip, Bucket, N, B) ->
  nuclear(),
  Pid = riak_ops:connect(Ip),
  riak_ops:configure_bucket(Pid, Bucket),
  io:format("Creating objects~n", []),
  {Dt, _} = timer:tc(fun() -> create_objects_onesided(Pid, Bucket, N, B) end),
  io:format("Setup time: ~ps~n", [Dt / (1000 * 1000)]),
  riak_ops:disconnect(Pid),
  ok.

%% i.e. riak_setup:verify("127.0.0.1", <<"set_a">>, 1000, 0.1, 2048).
verify(Ip, Bucket, N, P, B) ->
  Pid = riak_ops:connect(Ip),
  {_, _, Expected} = symmetric_dataset:create(N, P, B),
  Keys = riak_ops:keys(Pid, Bucket),
  KeyVals = [{Key, riak_ops:get(Pid, Bucket, Key, fun() -> error(sibling) end)} ||
              Key <- Keys],
  riak_ops:disconnect(Pid),
  symmetric_dataset:verify(KeyVals, Expected).

%% riak_setup:mini().
mini() ->
  Ip = "127.0.0.1",
  Bucket = <<"mini">>,
  Pid = riak_ops:connect(Ip),
  riak_ops:configure_bucket(Pid, Bucket),
  riak_ops:clear(Pid, Bucket),
  timer:sleep(5000),
  Resolve = fun(_, _) -> error(existing_object) end,
  case node_name() of
    a ->
      riak_ops:put(Pid, Bucket, <<"key1">>, 1, Resolve),
      riak_ops:put(Pid, Bucket, <<"key2">>, 2, Resolve),
      riak_ops:put(Pid, Bucket, <<"key_a">>, 1, Resolve);
    b ->
      riak_ops:put(Pid, Bucket, <<"key1">>, 2, Resolve),
      riak_ops:put(Pid, Bucket, <<"key2">>, 1, Resolve),
      riak_ops:put(Pid, Bucket, <<"key_b">>, 1, Resolve)
  end,
  riak_ops:disconnect(Pid).

%% i.e. riak_setup:clear("127.0.0.1", <<"set_a">>).
clear(Ip, Bucket) ->
  Pid = riak_ops:connect(Ip),
  riak_ops:clear(Pid, Bucket),
  riak_ops:disconnect(Pid),
  ok.

nuclear() ->
  os:cmd("sudo -H -u riak riak stop"),
  os:cmd("sudo rm -rf /var/lib/riak/bitcask/*"),
  os:cmd("sudo -H -u riak riak start"),
  io:format("Giving riak time to restart.~n"),
  timer:sleep(10000),
  "pong\n" = os:cmd("sudo -H -u riak riak ping").

%% i.e. riak_setup:count("127.0.0.1", <<"set_a">>).
count(Ip, Bucket) ->
  Pid = riak_ops:connect(Ip),
  io:format("Object in bucket: ~p~n", [riak_ops:count(Pid, Bucket)]),
  riak_ops:disconnect(Pid),
  ok.

%% i.e. riak_setup:keys("127.0.0.1", <<"set_a">>).
keys(Ip, Bucket) ->
  Pid = riak_ops:connect(Ip),
  L = riak_ops:keys(Pid, Bucket),
  riak_ops:disconnect(Pid),
  L.

%% i.e. riak_setup:get("127.0.0.1", <<"set_a">>, <<"key">>).
get(Ip, Bucket, Key) ->
  Pid = riak_ops:connect(Ip),
  V = riak_ops:get(Pid, Bucket, Key, fun(_, _) -> error(sibling) end),
  riak_ops:disconnect(Pid),
  V.

%%%_* Internal =========================================================

%% Contain dataset size in RAM by adding in bulk data just before the
%% write.
create_objects(Pid, Bucket, N, P, B) ->
  {L1, L2, Expected} = symmetric_dataset:create(N, P, 0),
  L = case node_name() of
        a -> L1;
        b -> L2
      end,
  Resolve = fun(_, _) -> error(existing_object) end,
  Bits = B * 8,
  Bulk = <<0:Bits>>,
  [riak_ops:put(Pid, Bucket, Key, {T, Bulk}, Resolve) || {Key, {T, _B}} <- L],
  ok.

create_objects_onesided(Pid, Bucket, N, B) ->
  L = symmetric_dataset:create_onesided(N, B),
  Resolve = fun(_, _) -> error(existing_object) end,
  [riak_ops:put(Pid, Bucket, Key, Value, Resolve) || {Key, Value} <- L],
  ok.

node_name() ->
  {ok, Name} = config:get_nested([node, name]),
  Name.
