%% Copyright (c) 2015 Ulf Leopold.
-module(misc).

-export([local_dataset/1]).
-export([remote_dataset/1]).
-export([get_ds_config/2]).
-export([foreach_ds_config/1]).
-export([make_counter/0]).
-export([lsize/1]).

local_dataset(DsName) -> get_ds_config(DsName, dataset).

remote_dataset(Name) ->
  {ok, Port} = config:get_nested([node, remote_port]),
  {ok, Host} = config:get_nested([node, remote_host]),
  {ok, Ds} = ds_remote:start_link(Host, Port, Name),
  Ds.

get_ds_config(DsName, ConfigName) ->
  {ok, Cfg} = config:get_nested([datasets, DsName, ConfigName]),
  Cfg.

foreach_ds_config(Fun) ->
  {ok, Datasets} = config:get_nested([datasets]),
  maps:fold(fun(K, V, _) -> Fun(K, V) end, 0, Datasets).

make_counter() ->
  Loop = fun Loop() ->
             receive
               {Pid, Ref, inc} -> Cnt = get(count), put(count, Cnt + 1),
                                  Pid ! {Ref, Cnt};
               {Pid, Ref, get} -> Cnt = get(count), Pid ! {Ref, Cnt}
             end,
             Loop()
         end,
  Pid = spawn(fun() -> put(count, 1), Loop() end),
  fun(inc) ->
      Ref = make_ref(),
      Pid ! {self(), Ref, inc},
      receive
        {Ref, Cnt} -> Cnt
      end;
     (get) ->
      Ref = make_ref(),
      Pid ! {self(), Ref, get},
      receive
        {Ref, Cnt} -> Cnt
      end
  end.

%% Byte size of list.
lsize(L) -> lsize(L, 0).

lsize([], Size)      -> Size;
lsize([H | T], Size) -> lsize(T, Size + byte_size(term_to_binary(H))).
