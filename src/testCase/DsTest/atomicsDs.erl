-module(atomicsDs).
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
   atomics:new(Num, []).

insert(0, Ds) ->
   Ds;
insert(Num, Ds) ->
   atomics:put(Ds, Num, Num),
   insert(Num - 1, Ds).

read(0, Ds) ->
   Ds;
read(Num, Ds) ->
   Value = atomics:get(Ds, Num),
   read(Num - 1, Ds).

update(0, Ds) ->
   Ds;
update(Num, Ds) ->
   NewDs = atomics:add(Ds, Num, 1),
   update(Num - 1, NewDs).

for(0, Ds) ->
   Ds;
for(Num, Ds) ->
   atomics:get(Ds, Num),
   for(Num - 1, Ds).

delete(0, Ds) ->
   ok.


