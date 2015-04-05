-module(setconcile_app).
-behaviour(application).

-export([start/2]).
-export([stop/1]).

start(_Type, _Args) ->
  lager:info("Starting Setconcile."),
  Dispatch = cowboy_router:compile(
               [
                {'_', [
                       {"/", ping_handler, []}
                      ]}
               ]),
  {ok, _} = cowboy:start_http(http, 100, [{port, 8080}],
                              [{env, [{dispatch, Dispatch}]}]).

stop(_State) ->
  ok.
