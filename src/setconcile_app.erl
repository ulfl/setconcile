%% Copyright (c) 2015 Ulf Leopold.
-module(setconcile_app).
-behaviour(application).

-export([start/2]).
-export([stop/1]).

start(_Type, _Args) ->
  Dispatch = cowboy_router:compile(
               [
                {'_', [
                       {"/api/ping", ping_handler, []},
                       {"/api/datasets/:set/bloom", bloom_handler, []},
                       {"/api/datasets/:set/transfers", transfers_handler, []},
                       {"/api/datasets/:set", element_handler, []}
                      ]}
               ]),

  {ok, NodeCfg} = config:get(node),
  Port = maps:get(port, NodeCfg),
  lager:info("Starting Setconcile on port ~p", [Port]),

  %% Start the web server.
  {ok, _} = cowboy:start_http(http, 100, [{port, Port}],
                              [{env, [{dispatch, Dispatch}]}]).

stop(_State) ->
  ok.
