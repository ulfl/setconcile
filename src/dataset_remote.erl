%% Copyright (c) 2015 Ulf Leopold.
-module(dataset_remote).

-export([make/3]).

make(Host, Port, Dataset) ->
  Pid = spawn(fun() -> loop(Host, Port, Dataset) end),
  fun(Op) -> call(Pid, Op) end.

call(Pid, Op) ->
  Ref = make_ref(),
  Pid ! {self(), Ref, Op},
  receive
    {Ref, Res} -> Res
  end.

loop(Host, Port, Dataset) ->
  receive
    {Pid, Ref, {ping}}   ->
      Response = http_get(Host, Port, "/api/ping", 3000),
      Pid ! {Ref, Response};

    {Pid, Ref, {get_bloom}} ->
      Body = http_get(Host, Port, fmt("/api/datasets/~p/bloom", [Dataset]),
                      1000 * 3600),
      {ok, Bloom} = ebloom:deserialize(Body),
      Pid ! {Ref, Bloom};

    {Pid, Ref, {post_transfer, Bloom, _Dest}}   ->
      Body = http_post(Host, Port, fmt("/api/datasets/~p/transfers", [Dataset]),
                       ebloom:serialize(Bloom), 1000 * 3600),
      Pid ! {Ref, binary_to_term(Body)};

    {Pid, Ref, {post_element, Elem0}}   ->
      Elem = term_to_binary(Elem0),
      _Body = http_post(Host, Port, fmt("/api/datasets/~p/", [Dataset]),
                        Elem, 1000 * 60),
      Pid ! {Ref, byte_size(Elem)};

    {Pid, Ref, {get_all}}   ->
      Pid ! {Ref, ok};

    {Pid, Ref, {exit}}   ->
      Pid ! {Ref, ok},
      exit(done)
  end,
  loop(Host, Port, Dataset).


http_get(Host, Port, Path, Tmo) ->
  try
    Url = fmt("http://~s:~p~s", [Host, Port, Path]),
    {ok, {_Res, _Headers, Body}} = lhttpc:request(Url, "GET", [], Tmo),
    Body
  catch
    C:E -> lager:info("http_get: ~p:~p", [C, E]), error
  end.

http_post(Host, Port, Path, Body, Tmo) ->
  try
    Url = fmt("http://~s:~p~s", [Host, Port, Path]),
    {ok, {_Res, _Headers, ResBody}} = lhttpc:request(Url, "POST", [], Body, Tmo),
    ResBody
  catch
    C:E -> lager:info("http_post: ~p:~p", [C, E]), error
  end.

fmt(Str, Args) -> lists:flatten(io_lib:format(Str, Args)).
