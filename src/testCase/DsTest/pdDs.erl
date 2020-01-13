-module(pdDs).
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
   erlang:send(Pid, {over, self(), TimeI, TimeR, TimeU, TimeF, not_support}),
   exit(normal).

init(Num) ->
   undefined.

insert(0, Ds) ->
   Ds;
insert(Num, Ds) ->
   Key = utTestDs:makeK(Num),
   Value = utTestDs:makeV(Num),
   erlang:put(Key, Value),
   insert(Num - 1, Ds).

read(0, Ds) ->
   Ds;
read(Num, Ds) ->
   Key = utTestDs:makeK(Num),
   Value = erlang:get(Key),
   V = if Value == 1 -> 1; true -> 2 end,
   read(Num - 1, Ds).

update(0, Ds) ->
   Ds;
update(Num, Ds) ->
   Key = utTestDs:makeK(Num),
   Value = utTestDs:makeV2(Num),
   erlang:put(Key, Value),
   update(Num - 1, Ds).

for(Num, Ds) ->
   Keylist = erlang:get(),
   for1(Keylist, undefined),
   Ds.

for1([], V) ->
   ok;
for1([H | T], _V) ->
   V = erlang:get(H),
   for1(T, V).

delete(0, Ds) ->
   ok;
delete(Num, Ds) ->
   Key = utTestDs:makeK(Num),
   erlang:erase(Key),
   delete(Num - 1, Ds).


