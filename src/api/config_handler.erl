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
  {DatasetName0, Req1} = cowboy_req:qs_val(<<"ds">>, Req),
  DatasetName = binary_to_atom(DatasetName0, utf8),
  {QsVals, Req2} = cowboy_req:qs_vals(Req1),
  lists:foreach(fun({Key, Val}) ->
                    case Key of
                      <<"ds">> -> ok;
                      <<"bloom_false_probability">> ->
                        Probability = binary_to_float(Val),
                        lager:info("Setting false probability to ~p",
                                   [Probability]),
                        config:set_nested([datasets, DatasetName,
                                           bloom_false_probability],
                                          Probability);
                      _ -> ok
                    end
                end, QsVals),
  cowboy_req:reply(200, [{<<"content-type">>, <<"text/plain">>}], "ok", Req2);
serve(_, Req) ->
  cowboy_req:reply(405, Req).

terminate(_Reason, _Req, _State) -> ok.
