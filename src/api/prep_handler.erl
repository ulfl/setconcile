%% Copyright (c) 2015 Ulf Leopold.
-module(prep_handler).

-export([init/3]).
-export([handle/2]).
-export([terminate/3]).

init(_Transport, Req, []) -> {ok, Req, undefined}.

handle(Req, State) ->
  {Method, Req1} = cowboy_req:method(Req),
  {ok, Req2} = serve(Method, Req1),
  {ok, Req2, State}.

serve(<<"PUT">>, Req)    -> do_op(prep, Req);
serve(<<"DELETE">>, Req) -> do_op(unprep, Req);
serve(_, Req)            -> cowboy_req:reply(405, Req).

do_op(Op, Req) ->
  {DsName0, Req1} = cowboy_req:binding(set, Req),
  DsName = binary_to_existing_atom(DsName0, utf8),
  {{Ip, _Port}, Req2} = cowboy_req:peer(Req1),
  lager:info("prep_handler (~p): dataset=~p, peer=~p", [Op, DsName, Ip]),
  D = misc:local_dataset(DsName),
  case Op of
    prep -> ds:prep(D);
    unprep -> ds:unprep(D)
  end,
  cowboy_req:reply(200, [{<<"content-type">>, <<"text/plain">>}], "ok", Req2).

terminate(_Reason, _Req, _State) -> ok.
