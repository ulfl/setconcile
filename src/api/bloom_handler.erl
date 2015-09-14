%% Copyright (c) 2015 Ulf Leopold.
-module(bloom_handler).

-export([init/3]).
-export([handle/2]).
-export([terminate/3]).

init(_Transport, Req, []) -> {ok, Req, undefined}.

handle(Req, State) ->
  {Method, Req1} = cowboy_req:method(Req),
  {ok, Req2} = serve(Method, Req1),
  {ok, Req2, State}.

serve(<<"GET">>, Req) ->
  try
    {DsName0, Req1} = cowboy_req:binding(set, Req),
    DsName = binary_to_existing_atom(DsName0, utf8),
    {Peer={_Ip, _Port}, Req2} = cowboy_req:peer(Req1),
    lager:info("bloom_handler: dataset=~p, peer=~p", [DsName, Peer]),
    D = misc:local_dataset(DsName),
    B = ds:get_bloom(D),
    cowboy_req:reply(200, [{<<"content-type">>, <<"application/octet-stream">>}],
                     ebloom:serialize(B), Req2)
  catch
    _:_ -> cowboy_req:reply(500, [], [], Req)
  end;
serve(_, Req) ->
  cowboy_req:reply(405, Req).

terminate(_Reason, _Req, _State) -> ok.
