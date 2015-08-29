-module(riak_setup).

-export([setup/5]).
-export([verify/5]).
-export([setup_mini/0]).
-export([clear/2]).
-export([count/2]).
-export([keys/2]).

%% i.e. riak_setup:setup("127.0.0.1", <<"set_a">>, 100000, 0.1, 2048).
setup(Ip, Bucket, N, P, B) ->
  Pid = riak_ops:connect(Ip),
  riak_ops:configure_bucket(Pid, Bucket),
  riak_ops:clear(Pid, Bucket),
  timer:sleep(5000),
  {Dt, _} = timer:tc(fun() -> create_objects(Pid, Bucket, N, P, B) end),
  io:format("Setup time: ~ps~n", [Dt / (1000 * 1000)]),
  ok.

%% i.e. riak_setup:verify("127.0.0.1", <<"set_a">>, 100000, 0.1, 2048).
verify(Ip, Bucket, N, P, B) ->
  Pid = riak_ops:connect(Ip),
  {_, _, Expected} = symmetric_dataset:create(N, P, B),
  Keys = riak_ops:keys(Pid, Bucket),
  KeyVals = [{Key, riak_ops:get(Pid, Bucket, Key, fun() -> error(sibling) end)} ||
              Key <- Keys],
  symmetric_dataset:verify(KeyVals, Expected).

%% riak_setup:setup_mini().
setup_mini() ->
  Ip = "127.0.0.1",
  Bucket = <<"mini">>,
  Pid = riak_ops:connect(Ip),
  riak_ops:configure_bucket(Pid, Bucket),
  riak_ops:clear(Pid, Bucket),
  timer:sleep(5000),
  Resolve = fun(_, _) -> error(existing_object) end,
  case node_name() of
    a ->
      riak_ops:put(Pid, Bucket, "key1", 1, Resolve),
      riak_ops:put(Pid, Bucket, "key2", 2, Resolve),
      riak_ops:put(Pid, Bucket, "key_a", 1, Resolve);
    b ->
      riak_ops:put(Pid, Bucket, "key1", 2, Resolve),
      riak_ops:put(Pid, Bucket, "key2", 1, Resolve),
      riak_ops:put(Pid, Bucket, "key_b", 1, Resolve)
  end.

%% i.e. riak_setup:clear("127.0.0.1", <<"set_a">>).
clear(Ip, Bucket) ->
  Pid = riak_ops:connect(Ip),
  riak_ops:clear(Pid, Bucket),
  ok.

%% i.e. riak_setup:count("127.0.0.1", <<"set_a">>).
count(Ip, Bucket) ->
  Pid = riak_ops:connect(Ip),
  io:format("Object in bucket: ~p~n", [riak_ops:count(Pid, Bucket)]),
  ok.

%% i.e. riak_setup:keys("127.0.0.1", <<"set_a">>).
keys(Ip, Bucket) ->
  Pid = riak_ops:connect(Ip),
  riak_ops:keys(Pid, Bucket).

%%%_* Internal =========================================================
create_objects(Pid, Bucket, N, P, B) ->
  {L1, L2, Expected} = symmetric_dataset:create(N, P, B),
  L = case node_name() of
        a -> L1;
        b -> L2
      end,
  Resolve = fun(_, _) -> error(existing_object) end,
  [riak_ops:put(Pid, Bucket, Key, Value, Resolve) || {Key, Value} <- L],
  Expected.

node_name() ->
  [Node1, _Host] = string:tokens(atom_to_list(node()), "@"),
  erlang:list_to_atom(Node1).
