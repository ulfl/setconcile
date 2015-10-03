%% Copyright (c) 2015 Ulf Leopold.
-module(config_handler).

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
    {{Ip, _Port}, Req1} = cowboy_req:peer(Req),
    lager:info("config_handler (peer=~p).", [Ip]),
    {DsName0, Req2} = cowboy_req:qs_val(<<"ds">>, Req1),
    DsName = binary_to_atom(DsName0, utf8),
    {QsVals, Req3} = cowboy_req:qs_vals(Req2),
    lists:foreach(fun({Key, Val}) ->
                      case Key of
                        <<"ds">> -> ok;
                        <<"bloom_false_probability">> ->
                          Probability = binary_to_float(Val),
                          lager:info("Setting false probability to ~p.",
                                     [Probability]),
                          config:set_nested([datasets, DsName,
                                             bloom_false_probability],
                                            Probability);
                        _ -> ok
                      end
                  end, QsVals),
    cowboy_req:reply(200, [{<<"content-type">>, <<"text/plain">>}], "ok", Req3)
  catch
    C:E ->
      lager:error("config_handler (error={~p, ~p}, stack=~p).",
                  [C, E, erlang:get_stacktrace()]),
      cowboy_req:reply(500, [], [], Req)
  end;
serve(_, Req) ->
  cowboy_req:reply(405, Req).

terminate(_Reason, _Req, _State) -> ok.
