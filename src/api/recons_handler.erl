%% Copyright (c) 2015 Ulf Leopold.
-module(recons_handler).

-export([init/3]).
-export([handle/2]).
-export([terminate/3]).

init(_Transport, Req, []) -> {ok, Req, undefined}.

handle(Req, State) ->
  {Method, Req1} = cowboy_req:method(Req),
  {ok, Req2} = serve(Method, Req1),
  {ok, Req2, State}.

serve(<<"POST">>, Req) ->
  {Ds0, Req1} = cowboy_req:binding(set, Req),
  DsName = binary_to_existing_atom(Ds0, utf8),
  Res = reconcile:reconcile(DsName),
  Json = jsx:encode(Res),
  cowboy_req:reply(200, [{<<"content-type">>, <<"text/plain">>}], Json, Req1);
serve(_, Req) ->
  cowboy_req:reply(405, Req).

terminate(_Reason, _Req, _State) -> ok.

