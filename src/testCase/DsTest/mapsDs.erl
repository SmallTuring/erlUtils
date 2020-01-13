-module(mapsDs).
-compile([nowarn_unused_function, nowarn_unused_vars, nowarn_export_all]).

-export([start/2]).

start(Num, Pid) ->
   Ds = init(Num),
   erlang:statistics(wall_clock),
   NewDsI = insert(Num, Ds),
   {_, TimeI} = erlang:statistics(wall_clock),
   NewDsR = read(Num, NewDsI),
   {_, TimeR} = erlang:statistics(wall_clock),
   NewDsU = update(Num, NewDsR),
   {_, TimeU} = erlang:statistics(wall_clock),
   NewDsF = for(Num, NewDsU),
   {_, TimeF} = erlang:statistics(wall_clock),
   delete(Num, NewDsF),
   {_, TimeD} = erlang:statistics(wall_clock),
   erlang:send(Pid, {over, self(), TimeI, TimeR, TimeU, TimeF, TimeD}),
   exit(normal).

init(_Num) ->
   maps:new().

insert(0, Ds) ->
   Ds;
insert(Num, Ds) ->
   Key = utTestDs:makeK(Num),
   Value = utTestDs:makeV(Num),
   NewDs = maps:put(Key, Value, Ds),
   insert(Num - 1, NewDs).

read(0, Ds) ->
   Ds;
read(Num, Ds) ->
   Key = utTestDs:makeK(Num),
   case Ds of
      #{Key := Value} ->
         Value;
      _ ->
         undefined
   end,
   read(Num - 1, Ds).

update(0, Ds) ->
   Ds;
update(Num, Ds) ->
   Key = utTestDs:makeK(Num),
   Value = utTestDs:makeV2(Num),
   NewDs = maps:update(Key, Value, Ds),
   update(Num - 1, NewDs).

for(Num, Ds) ->
   Fun =
      fun(Key, Value, Acc) ->
         Value
      end,
   List = maps:fold(Fun, [], Ds),
   Ds.

delete(0, Ds) ->
   ok;
delete(Num, Ds) ->
   Key = utTestDs:makeK(Num),
   NewDs = maps:remove(Key, Ds),
   delete(Num - 1, NewDs).


