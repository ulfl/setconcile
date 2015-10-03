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
  try
    {Ds0, Req1} = cowboy_req:binding(set, Req),
    DsName = binary_to_existing_atom(Ds0, utf8),
    {{Ip, _Port}, Req2} = cowboy_req:peer(Req1),
    lager:info("recons_handler (dataset=~p, peer=~p).", [DsName, Ip]),
    Res = reconcile:reconcile(DsName),
    lager:info("recons_handler: reconciliation done."),
    Json = jsx:encode(Res),
    cowboy_req:reply(200, [{<<"content-type">>, <<"text/plain">>}], Json, Req1)
  catch
    C:E ->
      lager:error("recons_handler (error={~p, ~p}, stack=~p).",
                  [C, E, erlang:get_stacktrace()]),
      cowboy_req:reply(500, [], [], Req)
  end;
serve(_, Req) ->
  cowboy_req:reply(405, Req).

terminate(_Reason, _Req, _State) -> ok.

