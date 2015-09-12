%% Copyright (c) 2015 Ulf Leopold.
-module(setconcile_app).
-behaviour(application).

-export([start/2]).
-export([stop/1]).

start(_Type, _Args) ->
  Dispatch = cowboy_router:compile(
               [
                {'_', [
                       %% External.
                       {"/api/ping", ping_handler, []},

                       %% Trigger reconciliation of the given dataset
                       %% (POST).
                       {"/api/datasets/:set/recons", recons_handler, []},

                       %% Prep a dataset for reconciliation (PUT) or unprep
                       %% the dataset (DELETE).
                       {"/api/datasets/:set/prep", prep_handler, []},

                       %% GET the current bloom filter for the given
                       %% dataset.
                       {"/api/datasets/:set/bloom", bloom_handler, []},

                       %% Create a transfer (POST). The request contains
                       %% a bloom filter containing all the elements on
                       %% the requesting node. The request will be
                       %% serviced by POSTing back all elements not in
                       %% the bloom filter to the requesting node.
                       {"/api/datasets/:set/transfers", transfers_handler, []},

                       %% POST elements to be included in the given set.
                       {"/api/datasets/:set", element_handler, []},
 
                       %% Debug interface.
                       {"/api/debug/setup_db/", setup_db_handler, []},
                       {"/api/debug/config/", config_handler, []}
                      ]}
               ]),

  {ok, NodeCfg} = config:get(node),
  Port = maps:get(port, NodeCfg),
  lager:info("Starting Setconcile on port ~p", [Port]),

  %% Start the web server.
  {ok, _} = cowboy:start_http(http, 100, [{port, Port}],
                              [{env, [{dispatch, Dispatch}]},
                               {max_keepalive,  1000},
                               {timeout, 60 * 1000}]).

stop(_State) ->
  ok.
