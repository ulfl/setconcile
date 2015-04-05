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
  {Dataset0, Req1} = cowboy_req:binding(set, Req),
  DatasetName = binary_to_existing_atom(Dataset0, utf8),
  {ok, Body, Req2} = cowboy_req:body(Req1),
  LocalDataset = misc:local_dataset(DatasetName),
  {ok, Bloom} = ebloom:deserialize(Body),
  {{Ip, _Port}, Req3} = cowboy_req:peer(Req2),
  lager:info("transfers handler: Request from src_ip=~p, bloom=~p",
             [Ip, Bloom]),
  RemoteDataset = misc:remote_dataset(DatasetName),
  {N, Size} = LocalDataset({post_transfer, Bloom, RemoteDataset}),
  RemoteDataset({exit}),
  cowboy_req:reply(200, [{<<"content-type">>, <<"text/plain">>}],
                   term_to_binary({N, Size}), Req3);
serve(_, Req) ->
  cowboy_req:reply(405, Req).

terminate(_Reason, _Req, _State) -> ok.
