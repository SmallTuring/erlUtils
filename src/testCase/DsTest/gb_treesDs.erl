-module(gb_treesDs).
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
   gb_trees:empty().

insert(0, Ds) ->
   Ds;
insert(Num, Ds) ->
   Key = utTestDs:makeK(Num),
   Value = utTestDs:makeV(Num),
   NewDs = gb_trees:enter(Key, Value, Ds),
   insert(Num - 1, NewDs).

read(0, Ds) ->
   Ds;
read(Num, Ds) ->
   Key = utTestDs:makeK(Num),
   Value = gb_trees:lookup(Key, Ds),
   read(Num - 1, Ds).

update(0, Ds) ->
   Ds;
update(Num, Ds) ->
   Key = utTestDs:makeK(Num),
   Value = utTestDs:makeV2(Num),
   NewDs = gb_trees:update(Key, Value, Ds),
   update(Num - 1, NewDs).

for(Num, Ds) ->
   List = gb_trees:to_list(Ds),
   for1(List),
   Ds.

for1([]) ->
   ok;
for1([H | T]) ->
   for1(T).

delete(0, Ds) ->
   ok;
delete(Num, Ds) ->
   Key = utTestDs:makeK(Num),
   NewDs = gb_trees:delete_any(Key, Ds),
   delete(Num - 1, NewDs).


