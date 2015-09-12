%% Copyright (c) 2015 Ulf Leopold.
-module(setup_db_handler).

-export([init/3]).
-export([handle/2]).
-export([terminate/3]).

init(_Transport, Req, []) -> {ok, Req, undefined}.

handle(Req, State) ->
  {Method, Req1} = cowboy_req:method(Req),
  {ok, Req2} = serve(Method, Req1),
  {ok, Req2, State}.

serve(<<"POST">>, Req) ->
  {N0, Req1} = cowboy_req:qs_val(<<"n">>, Req),
  {P0, Req2} = cowboy_req:qs_val(<<"p">>, Req1),
  {BulkBytes0, Req3} = cowboy_req:qs_val(<<"bb">>, Req2),
  N = b2i(N0),
  P = b2f(P0),
  BulkBytes = b2i(BulkBytes0),
  lager:info("Setting up DB (n=~p, p=~p, bb=~p)", [N, P, BulkBytes]),
  riak_setup:symm("127.0.0.1", <<"set_a">>, N, P, BulkBytes),
  cowboy_req:reply(200, [{<<"content-type">>, <<"text/plain">>}], "ok", Req3);
serve(_, Req) ->
  cowboy_req:reply(405, Req).

terminate(_Reason, _Req, _State) -> ok.

b2i(X) -> list_to_integer(binary_to_list(X)).
b2f(X) -> list_to_float(binary_to_list(X)).
