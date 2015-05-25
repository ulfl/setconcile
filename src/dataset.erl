%% Copyright (c) 2015 Ulf Leopold.
-module(dataset).

%% API.
-export([get_bloom/1]).
-export([post_transfer/3]).
-export([post_element/2]).
-export([get_all/1]).
-export([ping/1]).
-export([stop/1]).

%%%_* Common API =======================================================
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

%%%_* Only dataset_local ===============================================
get_all(Dataset) ->
  {ok, Elements} = gen_server:call(Dataset, get_all, infinity),
  Elements.

%%%_* Only dataset_remote ==============================================
ping(Dataset) -> gen_server:call(Dataset, ping, infinity).

stop(Dataset) -> gen_server:call(Dataset, stop).
