%% Copyright (c) 2015 Ulf Leopold.
-module(misc).

-export([local_dataset/1]).
-export([remote_dataset/1]).
-export([get_ds_config/2]).
-export([foreach_ds_config/1]).

local_dataset(DsName) -> get_ds_config(DsName, dataset).

remote_dataset(Name) ->
  {ok, Port} = config:get_nested([node, remote_port]),
  {ok, Host} = config:get_nested([node, remote_host]),
  {ok, Ds} = ds_remote:start_link(Host, Port, Name),
  Ds.

get_ds_config(DsName, ConfigName) ->
  {ok, Cfg} = config:get_nested([datasets, DsName, ConfigName]),
  Cfg.

foreach_ds_config(Fun) ->
  {ok, Datasets} = config:get_nested([datasets]),
  maps:fold(fun(K, V, _) -> Fun(K, V) end, 0, Datasets).
