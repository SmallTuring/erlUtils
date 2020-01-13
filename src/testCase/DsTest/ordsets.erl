-module(ordsets).
-compile([nowarn_unused_function, nowarn_unused_vars, nowarn_export_all]).

-export([start/2]).

start(Num, Pid) ->
   Ds = init(Num),
   erlang:statistics(wall_clock),
   NewDsI = insert(Num, Ds),
   {_, TimeI} = erlang:statistics(wall_clock),
   NewDsR = read(Num, NewDsI),
   {_, TimeR} = erlang:statistics(wall_clock),
   NewOrdSetU = update(Num, NewDsR),
   {_, TimeU} = erlang:statistics(wall_clock),
   NewOrdSetF = for(Num, NewOrdSetU),
   {_, TimeF} = erlang:statistics(wall_clock),
   delete(Num, NewOrdSetF),
   {_, TimeD} = erlang:statistics(wall_clock),
   erlang:send(Pid, {over, self(), TimeI, TimeR, not_support, TimeF, TimeD}),
   exit(normal).

init(_Num) ->
   ordsets:new().

insert(0, Ds) ->
   Ds;
insert(Num, Ds) ->
   Key = utTestDs:makeK(Num),
   Value = utTestDs:makeV(Num),
   NewDs = ordsets:add_element(Key, Ds),
   insert(Num - 1, NewDs).

read(0, Ds) ->
   Ds;
read(Num, Ds) ->
   Key = utTestDs:makeK(Num),
   Value = ordsets:is_element(Key, Ds),
   read(Num - 1, Ds).

update(0, Ds) ->
   Ds.

for(Num, Ds) ->
   Fun =
      fun(Key, Acc) ->
         Key
      end,
   List = ordsets:fold(Fun, [], Ds),
   Ds.

delete(0, Ds) ->
   ok;
delete(Num, Ds) ->
   Key = utTestDs:makeK(Num),
   NewDs = ordsets:del_element(Key, Ds),
   delete(Num - 1, NewDs).


