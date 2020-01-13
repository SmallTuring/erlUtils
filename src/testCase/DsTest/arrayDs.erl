-module(arrayDs).
-compile([nowarn_unused_function, nowarn_unused_vars, nowarn_export_all]).

-export([start/2]).

start(Num, Pid) ->
   Ds = init(Num),
   erlang:statistics(wall_clock),
   NewDsI = insert(Num - 1, Ds),
   {_, TimeI} = erlang:statistics(wall_clock),
   NewDsR = read(Num - 1, NewDsI),
   {_, TimeR} = erlang:statistics(wall_clock),
   NewDsU = update(Num - 1, NewDsR),
   {_, TimeU} = erlang:statistics(wall_clock),
   NewDsF = for(Num - 1, NewDsU),
   {_, TimeF} = erlang:statistics(wall_clock),
   delete(Num - 1, NewDsF),
   {_, TimeD} = erlang:statistics(wall_clock),
   erlang:send(Pid, {over, self(), TimeI, TimeR, TimeU, TimeF, not_support}),
   exit(normal).

init(Num) ->
   array:new(Num, fixed).

insert(0, Ds) ->
   Key = utTestDs:makeK(0),
   array:set(0, utTestDs:makeV(0), Ds);
insert(Num, Ds) ->
   Key = utTestDs:makeK(Num),
   NewDs = array:set(Num, utTestDs:makeV(Num), Ds),
   insert(Num - 1, NewDs).

read(0, Ds) ->
   Key = utTestDs:makeK(0),
   Value = array:get(0, Ds),
   Ds;
read(Num, Ds) ->
   Key = utTestDs:makeK(Num),
   Value = array:get(Num, Ds),
   read(Num - 1, Ds).

update(0, Ds) ->
   Key = utTestDs:makeK(0),
   array:set(0, utTestDs:makeV2(0), Ds);
update(Num, Ds) ->
   Key = utTestDs:makeK(Num),
   NewDs = array:set(Num, utTestDs:makeV2(Num), Ds),
   update(Num - 1, NewDs).

for(0, Ds) ->
   Value = array:get(0, Ds),
   Ds;
for(Num, Ds) ->
   Value = array:get(Num, Ds),
   for(Num - 1, Ds).

delete(Num, Ds) ->
   ok.


