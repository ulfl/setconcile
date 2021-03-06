%% Copyright (c) 2015 Ulf Leopold.
-module(ds).

%% API.
-export([prep/1]).
-export([get_bloom/1]).
-export([transfer_missing/3]).
-export([store_elements/2]).
-export([unprep/1]).
-export([ping/1]).
-export([stop/1]).

-include("setconcile.hrl").

-spec prep(dataset()) -> {ok, integer()} | ok | error_sync_in_progress.
-spec get_bloom(dataset()) -> reference().
-spec transfer_missing(dataset(), reference(), dataset()) -> {non_neg_integer(),
                                                              non_neg_integer()}.
-spec store_elements(dataset(), [term()]) -> non_neg_integer().
-spec unprep(dataset()) -> ok.

%%%_* Common API =======================================================

%% Prepare a dataset for syncing. Returns the size of the dataset in
%% bytes or, for remote datasets, just 'ok'.
prep(Ds) -> gen_server:call(Ds, prep, infinity).

%% Get a bloom filter populated with the elements for this dataset.
get_bloom(Ds) ->
  {ok, Bloom} = gen_server:call(Ds, get_bloom, infinity),
  Bloom.

%% Transfer elements in 'SrcDs' that are not in 'Bloom' to 'DestDs'.
transfer_missing(SrcDs, Bloom, DestDs) ->
  {ok, {Len, Size}} = gen_server:call(SrcDs, {transfer_missing, Bloom, DestDs},
                                      infinity),
  {Len, Size}.

%% Store 'Elements' in 'Ds'.
store_elements(Ds, Elements) ->
  {ok, Size} = gen_server:call(Ds, {store_elements, Elements}, infinity),
  Size.

unprep(Ds) -> gen_server:call(Ds, unprep, infinity).

%%%_* Only dataset_remote ==============================================
ping(Ds) -> gen_server:call(Ds, ping, infinity).

stop(Ds) -> gen_server:call(Ds, stop).
