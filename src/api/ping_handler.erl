%% Copyright (c) 2015 Ulf Leopold.
-module(ping_handler).

-export([init/3]).
-export([handle/2]).
-export([terminate/3]).

init(_Transport, Req, []) -> {ok, Req, undefined}.

handle(Req, State) ->
  {Method, Req1} = cowboy_req:method(Req),
  {ok, Req2} = serve(Method, Req1),
  {ok, Req2, State}.

serve(<<"GET">>, Req) ->
  {{Ip, _Port}, Req1} = cowboy_req:peer(Req),
  lager:info("ping_handler (peer=~p).", [Ip]),
  cowboy_req:reply(200, [{<<"content-type">>, <<"text/plain">>}], "ok", Req1);
serve(_, Req) ->
  cowboy_req:reply(405, Req).

terminate(_Reason, _Req, _State) -> ok.
