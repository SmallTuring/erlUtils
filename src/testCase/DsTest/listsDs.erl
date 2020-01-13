-module(listsDs).
-compile([nowarn_unused_function, nowarn_unused_vars, nowarn_export_all]).

-export([start/2]).

start(Num, Pid) ->
   Ds = init(Num),
   erlang:statistics(wall_clock),
   NewDsI = insert(Num, Ds),
   {_, TimeI} = erlang:statistics(wall_clock),
   NewArrR = read(Num, NewDsI),
   {_, TimeR} = erlang:statistics(wall_clock),
   NewDsU = update(Num, NewArrR),
   {_, TimeU} = erlang:statistics(wall_clock),
   NewDsF = for(NewDsU, NewDsU),
   {_, TimeF} = erlang:statistics(wall_clock),
   delete(Num, NewDsF),
   {_, TimeD} = erlang:statistics(wall_clock),
   erlang:send(Pid, {over, self(), TimeI, TimeR, TimeU, TimeF, TimeD}),
   exit(normal).

init(_Num) ->
   [].

insert(0, Ds) ->
   Ds;
insert(Num, Ds) ->
   Key = utTestDs:makeK(Num),
   Value = utTestDs:makeV(Num),
   insert(Num - 1, [{Key, Value} | Ds]).

read(0, Ds) ->
   Ds;
read(Num, Ds) ->
   Key = utTestDs:makeK(Num),
   Value = lists:keyfind(Key, 1, Ds),
   read(Num - 1, Ds).

update(0, Ds) ->
   Ds;
update(Num, Ds) ->
   Key = utTestDs:makeK(Num),
   Value = utTestDs:makeV2(Num),
   NewDs = lists:keyreplace(Key, 1, Ds, {Key, Value}),
   update(Num - 1, NewDs).

for([], Array) ->
   Array;
for([H | T], Array) ->
   for(T, Array).

delete(0, _List) ->
   ok;
delete(Num, Ds) ->
   Key = utTestDs:makeK(Num),
   NewDs = lists:keydelete(Key, 1, Ds),
   delete(Num - 1, NewDs).


