-module(persistentTermDs).
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
   erlang:send(Pid, {over, self(), TimeI, TimeR, not_support, not_support, not_support}),
   exit(normal).

init(Num) ->
   undefined.

insert(0, Ds) ->
   Ds;
insert(Num, Ds) ->
   Key = utTestDs:makeK(Num),
   Value = utTestDs:makeV(Num),
   persistent_term:put(Key, Value),
   insert(Num - 1, Ds).

read(0, Ds) ->
   Ds;
read(Num, Ds) ->
   Key = utTestDs:makeK(Num),
   persistent_term:get(Key),
   read(Num - 1, Ds).

update(0, Ds) ->
   Ds.

for(Num, Ds) ->
   Ds.

delete(0, Ds) ->
   ok.


