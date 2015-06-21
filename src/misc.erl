%% Copyright (c) 2015 Ulf Leopold.
-module(misc).

-export([local_dataset/1]).
-export([remote_dataset/1]).
-export([get_ds_config/2]).

local_dataset(DatasetName) -> get_ds_config(DatasetName, dataset).

remote_dataset(Name) ->
  {ok, Port} = config:get_nested([node, remote_port]),
  {ok, Host} = config:get_nested([node, remote_host]),
  {ok, Ds} = dataset_remote:start_link(Host, Port, Name),
  Ds.

get_ds_config(DatasetName, ConfigName) ->
  {ok, Cfg} = config:get_nested([datasets, DatasetName, ConfigName]),
  Cfg.
