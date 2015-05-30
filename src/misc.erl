%% Copyright (c) 2015 Ulf Leopold.
-module(misc).

-export([local_dataset/1]).
-export([remote_dataset/1]).

local_dataset(Name) ->
  {ok, Ds} = config:get_nested([datasets, Name]),
  Ds.

remote_dataset(Name) ->
  {ok, Port} = config:get_nested([node, remote_port]),
  {ok, Host} = config:get_nested([node, remote_host]),
  {ok, Ds} = dataset_remote:start_link(Host, Port, Name),
  Ds.
