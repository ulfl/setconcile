%% Copyright (c) 2015 Ulf Leopold.
-module(element_handler).

-export([init/3]).
-export([handle/2]).
-export([terminate/3]).

init(_Transport, Req, []) -> {ok, Req, undefined}.

handle(Req, State) ->
  {Method, Req1} = cowboy_req:method(Req),
  {ok, Req2} = serve(Method, Req1),
  {ok, Req2, State}.

serve(<<"POST">>, Req) ->
  {DatasetName0, Req1} = cowboy_req:binding(set, Req),
  DatasetName = binary_to_existing_atom(DatasetName0, utf8),
  {ok, Body, Req2} = cowboy_req:body(Req1),
  D = misc:local_dataset(DatasetName),
  E = binary_to_term(Body),
  dataset:post_element(D, E),
  cowboy_req:reply(200, [{<<"content-type">>, <<"text/plain">>}], "ok", Req2);
serve(_, Req) ->
  cowboy_req:reply(405, Req).

terminate(_Reason, _Req, _State) -> ok.
