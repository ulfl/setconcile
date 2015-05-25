%% Copyright (c) 2015 Ulf Leopold.
-module(dataset_local).
-behaviour(gen_server).

%% API.
-export([start_link/3]).

%% Gen server callbacks.
-export([init/1]).
-export([handle_call/3]).
-export([handle_cast/2]).
-export([handle_info/2]).
-export([terminate/2]).
-export([code_change/3]).

start_link(State, Get, Put) ->
  gen_server:start_link(?MODULE, [State, Get, Put], []).

%%%_* Gen server callbacks =============================================
init([State, Get, Put]) -> {ok, #{state => State, get => Get, put => Put}}.

handle_call(get_bloom, _From, #{state := State, get := Get} = S) ->
  Bloom = reconcile:create_bloom(Get(State)),
  {reply, {ok, Bloom}, S};
handle_call({post_transfer, Bloom, Dest}, _From,
            #{state := State, get := Get} = S) ->
  L = reconcile:filter(Get(State), Bloom, []),
  Size = lists:foldl(fun(X, A) ->
                         Size = dataset:post_element(Dest, X),
                         A + Size
                     end, 0, L),
  {reply, {ok, {length(L), Size}}, S};
handle_call({post_element, Element}, _From, #{state := State0,
                                              put := Put} = S) ->
  State = Put(State0, [Element]),
  {reply, {ok, byte_size(term_to_binary(Element))}, S#{state := State}};
handle_call(get_all, _From, #{state := State, get := Get} = S) ->
  {reply, {ok, Get(State)}, S}.

handle_cast(Msg, S) -> {stop, {unexpected_cast, Msg}, S}.

handle_info(Msg, S) -> {stop, {unexpected_info, Msg}, S}.

terminate(_Reason, _State) -> ok.

code_change(_OldVsn, State, _Extra) -> {ok, State}.
