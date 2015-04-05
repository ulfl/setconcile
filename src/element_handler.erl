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
  {Dataset0, Req1} = cowboy_req:binding(set, Req),
  Dataset = binary_to_existing_atom(Dataset0, utf8),
  {ok, Body, Req2} = cowboy_req:body(Req1),
  D = misc:local_dataset(Dataset),
  E = binary_to_term(Body),
  %lager:info("element_handler: received ~p", [E]),
  D({post_element, E}),
  cowboy_req:reply(200, [{<<"content-type">>, <<"text/plain">>}], "ok", Req2);
serve(_, Req) ->
  cowboy_req:reply(405, Req).

terminate(_Reason, _Req, _State) -> ok.
