%% Copyright (c) 2015 Ulf Leopold.
-module(config).
-behaviour(gen_server).

-export([start_link/0]).
-export([get/1]).
-export([get_nested/1]).
-export([set/2]).
-export([set_nested/2]).

-export([init/1]).
-export([handle_call/3]).
-export([handle_cast/2]).
-export([handle_info/2]).
-export([code_change/3]).
-export([terminate/2]).

%%%_* API ==============================================================
start_link() -> gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

get(Key) -> gen_server:call(?MODULE, {get, Key}).

get_nested(Keys) -> gen_server:call(?MODULE, {get_nested, Keys}).

set(Key, Val) -> gen_server:call(?MODULE, {set, Key, Val}).

set_nested(Keys, Val) -> gen_server:call(?MODULE, {set_nested, Keys, Val}).

%%%_* Gen server callbacks =============================================
init([]) ->
  {ok, Path} = application:get_env(config_file),
  {ok, Data} = file:read_file(Path),
  {ok, Scanned, _} = erl_scan:string(binary_to_list(Data)),
  {ok, Parsed} = erl_parse:parse_exprs(Scanned),
  {value, Result, _} = erl_eval:exprs(Parsed, []),
  {ok, #{config => Result}}.

handle_call({get, Key}, _From, #{config := ConfigMap} = S) ->
  R = get_nested([Key], ConfigMap),
  {reply, R, S};
handle_call({get_nested, Keys}, _From, #{config := ConfigMap} = S) ->
  R = get_nested(Keys, ConfigMap),
  {reply, R, S};
handle_call({set, Key, Val}, _From, #{config := ConfigMap}) ->
  {reply, ok, #{config => maps:put(Key, Val, ConfigMap)}};
handle_call({set_nested, Keys, Val}, _From, #{config := ConfigMap}) ->
  R = set_nested(Keys, Val, ConfigMap),
  {reply, ok, #{config => R}}.

handle_cast(Msg, State) -> {stop, {unexpected_cast, Msg}, State}.

handle_info(Msg, State) -> {stop, {unexpected_info, Msg}, State}.

code_change(_OldVsn, State, _Extra) -> {ok, State}.

terminate(_Reason, _State) -> ok.

%%%_* Helpers ==========================================================
get_nested([], _Map)     -> error;
get_nested([H | T], Map) ->
  case {maps:is_key(H, Map), T} of
    {true, []} -> {ok,  maps:get(H, Map)};
    {true, _}  -> get_nested(T, maps:get(H, Map));
    {false, _} -> error
  end.

set_nested([], _Val, _Map)    -> error;
set_nested([H | T], Val, Map) ->
  case {maps:is_key(H, Map), T} of
    {true, []} -> maps:put(H, Val, Map);
    {true, _}  -> maps:put(H, set_nested(T, Val, maps:get(H, Map)), Map);
    {false, _} -> error({key_not_found, H})
  end.
