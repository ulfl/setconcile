%% Copyright (c) 2015 Ulf Leopold.
-module(dataset_remote).

%% API.
-export([start_link/3]).
-export([get_bloom/1]).
-export([post_transfer/3]).
-export([post_element/2]).
-export([get_all/1]).
-export([ping/1]).
-export([stop/1]).

%% Gen server callbacks.
-export([init/1]).
-export([handle_call/3]).
-export([handle_cast/2]).
-export([handle_info/2]).
-export([terminate/2]).
-export([code_change/3]).

start_link(Host, Port, DatasetName) ->
  gen_server:start_link(?MODULE, [Host, Port, DatasetName], []).

get_bloom(Dataset) ->
  {ok, Bloom} = gen_server:call(Dataset, get_bloom, infinity),
  Bloom.

post_transfer(Dataset, Bloom, Dest) ->
  {ok, {Len, Size}} = gen_server:call(Dataset, {post_transfer, Bloom, Dest},
                                      infinity),
  {Len, Size}.

post_element(Dataset, Element) ->
  {ok, Size} = gen_server:call(Dataset, {post_element, Element}, infinity),
  Size.

ping(Dataset) -> gen_server:call(Dataset, ping, infinity).

stop(Dataset) -> gen_server:call(Dataset, stop).

%%%_* Gen server callbacks =============================================
get_all(Dataset) ->
  {ok, Elements} = gen_server:call(Dataset, get_all),
  Elements.

init([Host, Port, Name]) ->
  {ok, #{host => Host, port => Port, dataset_name => Name}}.

handle_call(ping, _From, #{host := Host, port := Port} = S) ->
  {ok, Body} = http_get(Host, Port, "/api/ping", 5000),
  {reply, {ok, Body}, S};
handle_call(get_bloom, _From, #{host := Host, port := Port,
                                dataset_name := Name} = S) ->
  {ok, Tmo} = config:get(tmo_get_bloom),
  {ok, Body} = http_get(Host, Port, fmt("/api/datasets/~p/bloom", [Name]), Tmo),
  {ok, Bloom} = ebloom:deserialize(Body),
  {reply, {ok, Bloom}, S};
handle_call({post_transfer, Bloom, _Dest}, _From,
            #{host := Host, port := Port, dataset_name := Name} = S) ->
  {ok, Tmo} = config:get(tmo_post_transfer),
  {ok, Body} = http_post(Host, Port, fmt("/api/datasets/~p/transfers", [Name]),
                         ebloom:serialize(Bloom), Tmo),
  {reply, {ok, binary_to_term(Body)}, S};
handle_call({post_element, Element}, _From,
            #{host := Host, port := Port, dataset_name := Name} = S) ->
  {ok, Tmo} = config:get(tmo_post_element),
  Elem = term_to_binary(Element),
  {ok, _} = http_post(Host, Port, fmt("/api/datasets/~p/", [Name]), Elem, Tmo),
  {reply, {ok, byte_size(Elem)}, S};
handle_call(get_all, _From, S) ->
  {reply, {ok, []}, S};
handle_call(stop, _From, State) ->
  {stop, normal, ok, State}.

handle_cast(Msg, S) -> {stop, {unexpected_cast, Msg}, S}.

handle_info(Msg, S) -> {stop, {unexpected_info, Msg}, S}.

terminate(_Reason, _State) -> ok.

code_change(_OldVsn, State, _Extra) -> {ok, State}.

%%%_* Helpers ==========================================================
http_get(Host, Port, Path, Tmo) ->
  try
    Url = fmt("http://~s:~p~s", [Host, Port, Path]),
    {ok, {_Res, _Headers, Body}} = lhttpc:request(Url, "GET", [], Tmo),
    {ok, Body}
  catch
    C:E -> lager:info("http_get: ~p:~p", [C, E]), error
  end.

http_post(Host, Port, Path, Body, Tmo) ->
  try
    Url = fmt("http://~s:~p~s", [Host, Port, Path]),
    {ok, {_Res, _Headers, ResBody}} = lhttpc:request(Url, "POST", [], Body, Tmo),
    {ok, ResBody}
  catch
    C:E -> lager:info("http_post: ~p:~p", [C, E]), error
  end.

fmt(Str, Args) -> lists:flatten(io_lib:format(Str, Args)).
