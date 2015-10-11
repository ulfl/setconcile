%% Copyright (c) 2015 Ulf Leopold.
-module(config_sup).
-behaviour(supervisor).

-export([start_link/0]).
-export([init/1]).

-define(CHILD(I, Type), {I, {I, start_link, []}, permanent, 2000, Type, [I]}).

start_link() -> supervisor:start_link({local, ?MODULE}, ?MODULE, []).

init([]) -> {ok, {{one_for_all, 10, 10}, [?CHILD(config, worker)]}}.
