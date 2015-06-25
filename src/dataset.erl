%% Copyright (c) 2015 Ulf Leopold.
-module(dataset).

%% API.
-export([prep/1]).
-export([get_bloom/1]).
-export([post_transfer/3]).
-export([post_elements/2]).
-export([unprep/1]).
-export([ping/1]).
-export([stop/1]).

%%%_* Common API =======================================================
prep(Dataset) -> _Size = gen_server:call(Dataset, prep, infinity).

get_bloom(Dataset) ->
  {ok, Bloom} = gen_server:call(Dataset, get_bloom, infinity),
  Bloom.

post_transfer(Dataset, Bloom, Dest) ->
  {ok, {Len, Size}} = gen_server:call(Dataset, {post_transfer, Bloom, Dest},
                                      infinity),
  {Len, Size}.

post_elements(Dataset, Element) ->
  {ok, Size} = gen_server:call(Dataset, {post_elements, Element}, infinity),
  Size.

unprep(Dataset) -> gen_server:call(Dataset, unprep, infinity).

%%%_* Only dataset_remote ==============================================
ping(Dataset) -> gen_server:call(Dataset, ping, infinity).

stop(Dataset) -> gen_server:call(Dataset, stop).
