-module(riak_ops).

-export([connect/1]).
-export([disconnect/1]).
-export([configure_bucket/2]).
-export([keys/2]).
-export([clear/2]).
-export([count/2]).
-export([put/5]).
-export([get/4]).

connect(Ip) ->
  {ok, Pid} = riakc_pb_socket:start(Ip, 8087, [{connect_timeout, 5000},
                                               {queue_if_disconnected, false},
                                               {auto_reconnect, false}]),
  Pid.

disconnect(Pid) -> ok = riakc_pb_socket:stop(Pid).

configure_bucket(Pid, Bucket) ->
  riakc_pb_socket:set_bucket(Pid, Bucket, [{n_val, 1}, {allow_mult, true}]).

keys(Pid, Bucket) ->
  {ok, L} = riakc_pb_socket:list_keys(Pid, Bucket),
  L.

clear(Pid, Bucket) ->
  L = keys(Pid, Bucket),
  [riakc_pb_socket:delete(Pid, Bucket, Key) || Key <- L],
  ok.

count(Pid, Bucket) -> length(keys(Pid, Bucket)).

%% Key / Value has been received from the remote host and we need to
%% update the local DB. View this as a normal update, i.e. we read up
%% the current object (resolve siblings if necessary), make a
%% modification (i.e. resolving with the remote object), and write the
%% result to the DB. If there is no existing object, then we just create
%% a new one. The value that was stored is returned.
put(Pid, Bucket, Key, Value, Resolve) ->
  {NewObj, NewValue} =
    case riakc_pb_socket:get(Pid, Bucket, Key) of
      {error, notfound} ->
        {riakc_obj:new(Bucket, Key, term_to_binary(Value)), Value};
      {ok, Obj} ->
        Values = [binary_to_term(X) || X <- riakc_obj:get_values(Obj)],
        Value1 = do_resolve([Value | Values], Resolve),
        {riakc_obj:update_value(Obj, term_to_binary(Value1)), Value1}
    end,
  ok = riakc_pb_socket:put(Pid, NewObj, [{w, quorum}, {dw, one}], 5000),
  NewValue.

%% Get the current value for Key in the DB. In case there are siblings
%% they are resolved. FIXME: what if object deleted?
get(Pid, Bucket, Key, Resolve) ->
  {ok, Obj} = riakc_pb_socket:get(Pid, Bucket, Key),
  Values = [binary_to_term(X) || X <- riakc_obj:get_values(Obj)],
  do_resolve(Values, Resolve).

do_resolve([H], _Resolve)   ->
  H;
do_resolve([H|T], Resolve) ->
  F = fun(X, A) -> Resolve(X, A) end,
  lists:foldl(F, H, T).

