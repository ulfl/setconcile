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
  {Dataset0, Req1} = cowboy_req:binding(set, Req),
  Dataset = binary_to_existing_atom(Dataset0, utf8),
  {R={Ip, _Port}, _Req3} = cowboy_req:peer(Req1),
  lager:info("Bloom lookup. Dataset=~p, ~p", [Dataset, R]),
  D = misc:local_dataset(Dataset),
  B = D({get_bloom}),
  cowboy_req:reply(200, [{<<"content-type">>, <<"application/octet-stream">>}],
                   ebloom:serialize(B), Req);
serve(_, Req) ->
  cowboy_req:reply(405, Req).

terminate(_Reason, _Req, _State) -> ok.
