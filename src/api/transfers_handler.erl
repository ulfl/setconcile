%% Copyright (c) 2015 Ulf Leopold.
-module(transfers_handler).

-export([init/3]).
-export([handle/2]).
-export([terminate/3]).

-define(MAX_BODY, 1024 * 1024 * 100). %% 100MB.

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
    lager:info("transfers handler (dataset=~p, peer=~p).", [DsName, Ip]),
    LocalDs = misc:local_dataset(DsName),
    {ok, Body, Req3} = cowboy_req:body(Req2, [{length, ?MAX_BODY},
                                              {read_timeout, infinity}]),
    {ok, Bloom} = ebloom:deserialize(Body),
    RemoteDs = misc:remote_dataset(DsName),
    {N, Size} = ds:transfer_missing(LocalDs, Bloom, RemoteDs),
    ds:stop(RemoteDs),
    cowboy_req:reply(200, [{<<"content-type">>, <<"text/plain">>}],
                     term_to_binary({N, Size}), Req3)
  catch
    C:E ->
      lager:error("transfers_handler (error={~p, ~p}, stack=~p).",
                  [C, E, erlang:get_stacktrace()]),
      cowboy_req:reply(500, [], [], Req)
  end;
serve(_, Req) ->
  cowboy_req:reply(405, Req).

terminate(_Reason, _Req, _State) -> ok.
