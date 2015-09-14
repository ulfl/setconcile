%% Copyright (c) 2015 Ulf Leopold.
-module(ds_remote).

%% API.
-export([start_link/3]).

%% Gen server callbacks.
-export([init/1]).
-export([handle_call/3]).
-export([handle_cast/2]).
-export([handle_info/2]).
-export([terminate/2]).
-export([code_change/3]).

start_link(Host, Port, DsName) ->
  gen_server:start_link(?MODULE, [Host, Port, DsName], []).

%%%_* Gen server callbacks =============================================
init([Host, Port, Name]) ->
  {ok, #{host => Host, port => Port, ds_name => Name}}.

handle_call(ping, _From, #{host := Host, port := Port} = S) ->
  {ok, Body} = http(get, Host, Port, "/api/ping", <<>>, 5000),
  {reply, {ok, Body}, S};
handle_call(prep, _From, #{host := Host, port := Port,
                           ds_name := Name} = S) ->
  Tmo = misc:get_ds_config(Name, tmo_prep),
  {ok, _} = http(put, Host, Port, fmt("/api/datasets/~p/prep", [Name]), <<>>,
                 Tmo),
  {reply, ok, S};
handle_call(get_bloom, _From, #{host := Host, port := Port,
                                ds_name := Name} = S) ->
  Tmo = misc:get_ds_config(Name, tmo_get_bloom),
  {ok, Body} = http(get, Host, Port, fmt("/api/datasets/~p/bloom", [Name]),
                    <<>>, Tmo),
  {ok, Bloom} = ebloom:deserialize(Body),
  {reply, {ok, Bloom}, S};
handle_call({transfer_missing, Bloom, _Dest}, _From,
            #{host := Host, port := Port, ds_name := Name} = S) ->
  Tmo = misc:get_ds_config(Name, tmo_transfer_elements),
  {ok, Body} = http(post, Host, Port, fmt("/api/datasets/~p/transfers", [Name]),
                    ebloom:serialize(Bloom), Tmo),
  {reply, {ok, binary_to_term(Body)}, S};
handle_call({store_elements, L}, _From,
            #{host := Host, port := Port, ds_name := Name} = S) ->
  Tmo = misc:get_ds_config(Name, tmo_transfer_elements),
  Data = term_to_binary(L),
  {ok, _} = http(post, Host, Port, fmt("/api/datasets/~p/", [Name]), Data, Tmo),
  {reply, {ok, byte_size(Data)}, S};
handle_call(unprep, _From, #{host := Host, port := Port,
                             ds_name := Name} = S) ->
  Tmo = misc:get_ds_config(Name, tmo_unprep),
  {ok, _} = http(delete, Host, Port, fmt("/api/datasets/~p/prep", [Name]), <<>>,
                 Tmo),
  {reply, ok, S};
handle_call(stop, _From, State) ->
  {stop, normal, ok, State}.

handle_cast(Msg, S) -> {stop, {unexpected_cast, Msg}, S}.

handle_info(Msg, S) -> {stop, {unexpected_info, Msg}, S}.

terminate(_Reason, _State) -> ok.

code_change(_OldVsn, State, _Extra) -> {ok, State}.

%%%_* Helpers ==========================================================
http(Op, Host, Port, Path, Data, Tmo) ->
  try
    Url = fmt("http://~s:~p~s", [Host, Port, Path]),
    {ok, Code, _RespHeaders, Ref} = hackney:request(Op, Url, [], Data,
                                                    [{pool, default},
                                                     {recv_timeout, Tmo}]),
    case Code of
      200 -> {ok, _RespBody} = hackney:body(Ref);
      _   -> error
    end
  catch
    C:E -> lager:info("http ~p: ~p:~p", [Op, C, E]), error
  end.

fmt(Str, Args) -> lists:flatten(io_lib:format(Str, Args)).
