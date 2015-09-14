%% Copyright (c) 2015 Ulf Leopold.
-module(transfers_handler).

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
    {ok, Body, Req2} = cowboy_req:body(Req1),
    LocalDs = misc:local_dataset(DsName),
    {ok, Bloom} = ebloom:deserialize(Body),
    {{Ip, _Port}, Req3} = cowboy_req:peer(Req2),
    lager:info("transfers handler: dataset=~p, peer=~p,", [DsName, Ip]),
    RemoteDs = misc:remote_dataset(DsName),
    {N, Size} = ds:transfer_missing(LocalDs, Bloom, RemoteDs),
    ds:stop(RemoteDs),
    cowboy_req:reply(200, [{<<"content-type">>, <<"text/plain">>}],
                     term_to_binary({N, Size}), Req3)
  catch
    _:_ -> cowboy_req:reply(500, [], [], Req)
  end;
serve(_, Req) ->
  cowboy_req:reply(405, Req).

terminate(_Reason, _Req, _State) -> ok.
