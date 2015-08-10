-module(riak_ops).

-export([connect/1]).
-export([disconnect/1]).
-export([configure_bucket/2]).
-export([keys/2]).
-export([clear/2]).
-export([count/2]).
-export([put/5]).
-export([get/3]).

connect(Ip) ->
  {ok, Pid} = riakc_pb_socket:start(Ip, 8087, [{connect_timeout, 5000},
                                               {queue_if_disconnected, false},
                                               {auto_reconnect, false}]),
  Pid.

disconnect(Pid) -> ok = riakc_pb_socket:stop(Pid).

configure_bucket(Pid, Bucket) ->
  riakc_pb_socket:set_bucket(Pid, Bucket, [{n_val, 5}, {allow_mult, true}]).

keys(Pid, Bucket) ->
  {ok, L} = riakc_pb_socket:list_keys(Pid, Bucket),
  L.

clear(Pid, Bucket) ->
  L = keys(Pid, Bucket),
  [riakc_pb_socket:delete(Pid, Bucket, Key) || Key <- L],
  ok.

count(Pid, Bucket) -> length(keys(Pid, Bucket)).

put(Pid, Bucket, Key, Value, Resolve) ->
  NewObj = case riakc_pb_socket:get(Pid, Bucket, Key) of
             {error, notfound} ->
               riakc_obj:new(Bucket, Key, term_to_binary(Value));
             {ok, Obj} ->
               ExistingValue = binary_to_term(riakc_obj:get_value(Obj)),
               NewValue = Resolve(ExistingValue, Value),
               riakc_obj:update_value(Obj, term_to_binary(NewValue))
           end,
  ok = riakc_pb_socket:put(Pid, NewObj, [{w, quorum}, {dw, one}], 5000).

get(Pid, Bucket, Key) ->
  {ok, Obj} = riakc_pb_socket:get(Pid, Bucket, Key),
  binary_to_term(riakc_obj:get_value(Obj)).
