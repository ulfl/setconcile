%% Copyright (c) 2015 Ulf Leopold.
-module(dataset_local).
-behaviour(gen_server).

%% API.
-export([start_link/4]).

%% Gen server callbacks.
-export([init/1]).
-export([handle_call/3]).
-export([handle_cast/2]).
-export([handle_info/2]).
-export([terminate/2]).
-export([code_change/3]).

start_link(Name, State, Get, Put) ->
  gen_server:start_link(?MODULE, [Name, State, Get, Put], []).

%%%_* Gen server callbacks =============================================
init([Name, State, Get, Put]) ->
  {ok, #{dataset_name => Name, state => State, get => Get, put => Put}}.

handle_call(get_bloom, _From, #{state := State, get := Get} = S) ->
  Bloom = reconcile:create_bloom(Get(State)),
  {reply, {ok, Bloom}, S};
handle_call({post_transfer, Bloom, Dest}, _From,
            #{dataset_name := Name, state := State, get := Get} = S) ->
  L = reconcile:filter(Get(State), Bloom, []),
  MaxSize = misc:get_ds_config(Name, max_transfer_bundle),
  Size = bundle(L, fun(X) -> dataset:post_elements(Dest, X) end, MaxSize),
  {reply, {ok, {length(L), Size}}, S};
handle_call({post_elements, L}, _From, #{state := State0,
                                              put := Put} = S) ->
  State = lists:foldl(fun(X, A) -> Put(A, X) end, State0, L),
  {reply, {ok, byte_size(term_to_binary(L))}, S#{state := State}};
handle_call(get_all, _From, #{state := State, get := Get} = S) ->
  {reply, {ok, Get(State)}, S}.

handle_cast(Msg, S) -> {stop, {unexpected_cast, Msg}, S}.

handle_info(Msg, S) -> {stop, {unexpected_info, Msg}, S}.

terminate(_Reason, _State) -> ok.

code_change(_OldVsn, State, _Extra) -> {ok, State}.

%%%_* Helpers ==========================================================
bundle(L, F, MaxSize) -> bundle(MaxSize, L, [], F, 0, MaxSize).

bundle(_, [], A, F, S, _MaxSize) ->
  S + F(A);
bundle(0, L, A, F, S, MaxSize) ->
  bundle(MaxSize, L, [], F, S + F(A), MaxSize);
bundle(X, [H | T], A, F, S, MaxSize) ->
  bundle(X - 1, T, [H | A], F, S, MaxSize).
