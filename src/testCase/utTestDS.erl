-module(utTestDS).
-compile([export_all, nowarn_unused_function, nowarn_unused_vars, nowarn_export_all]).

%% 用于测试erlang各种数据结构 读写遍历等操作的效率
%%  lists ，maps 和record是erlang最为常用的数据结构，lists使用方便简单，maps则查询高效，record则需要预定义，
%% 可扩展性差，各有各的优。本文做一下lists和maps的性能对比（再对比一下dict），代码如下（record操作不便则不做比较）。

%%通过注释部分代码做以下测试
%%timer:tc(lib_test, test_struct, [10000,#{}]).
%%timer:tc(lib_test, test_struct, [10000,[]]).
test_struct(0, R) ->
   Fun = fun({K, V}) -> K + 1, V + 1 end, lists:foreach(Fun, R),      %%遍历测试
   Fun = fun(K, V) -> K + 1, V + 1 end, maps:map(Fun, R),
   ok;
test_struct(Num, R) ->
   NewR = [{Num, Num} | R], lists:keyfind(5000, 1, NewR),   %%插入查询测试
   NewR = R#{Num=>Num}, maps:get(5000, NewR, 0),
   test_struct(Num - 1, NewR).
%% 做10000次的插入查询测试结果：
%%
%%     lists 50736微秒
%%     maps 4670微秒
%%     dict 60236微秒
%% 做10000次的遍历结果：
%%
%%     lists 523微秒
%%     maps 8337微秒
%%     dict 4426微秒
%% 对比总结：
%%
%%     对比测试数据maps在查询性能上比lists高10倍以上， 而在遍历上lists则更优。对于频繁插入和查询的数据，maps是最佳的选择，
%%     lists则适用于广播列表之类需要遍历的数据。除此之外，个人觉得在使用上lists 比maps更为方便，因为lists模块提供了大量实用的函数，
%%     单单在排序上，maps的实用性就不如lists了，所以在数据结构选择上就需要多多斟酌。另外record在查询上使用的是模式匹配，性能只会更高，
%%     但需要提前定义字段，可扩展性差，在热更这块有不少坑，maps也可以用模式匹配查询，但也要确保key值存在，不然就nomatch，
%%     但整体上maps更优于record，故建议用maps替代record。
