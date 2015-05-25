%% Copyright (c) 2015 Ulf Leopold.
-module(misc).

-export([local_dataset/1]).
-export([remote_dataset/1]).

local_dataset(Name) ->
  {ok, Datasets} = config:get(datasets),
  maps:get(Name, Datasets).

remote_dataset(Dataset) ->
  {ok, NodeCfg} = config:get(node),
  Port = maps:get(remote_port, NodeCfg),
  Host = maps:get(remote_host, NodeCfg),
  {ok, Ds} = dataset_remote:start_link(Host, Port, Dataset),
  Ds.

