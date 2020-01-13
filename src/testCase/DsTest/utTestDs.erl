-module(utTestDs).
-compile([export_all, nowarn_unused_function, nowarn_unused_vars, nowarn_export_all]).

-define(V_NUM, [8, 16, 32, 64, 128, 256, 516, 1024, 2048, 4096, 8192, 16384, 32768, 65536, 131072, 524288, 1048576]).
%-define(V_NUM, [8, 16, 32, 64, 128, 256, 516, 1024]).
-define(DsList, [utArrayDs, utTupleDs, utListsDs, utMapsDs, utPdDs, utEtsSetDs, utEtsOrdDs, utDictDs, utSetsDs, utGb_setsDs, utOrddictDs, utOrdsetsDs, utAtomicsDs, utPTermDs]).

recRet(Type, Time) ->
   erlang:put(Type, Time).

getRet(Type) ->
   erlang:get(Type).

makeK(N) ->
   case N rem 4 of
      0 ->
         N;
      1 ->
         {N, <<"test-testDs">>};
      2 ->
         {N, 8686};
      3 ->
         {N, test}
   end.

makeV(N) ->
   case N rem 4 of
      0 ->
         N;
      1 ->
         {N, <<"test-testDs">>};
      2 ->
         {N, 8787.87878, <<"test-testDs">>};
      3 ->
         {N, test, [list, 123, 456.789, "test"], {23231, "gggggg"}, <<"12345678901234567890">>}
   end.

makeV2(N) ->
   case N rem 4 of
      0 ->
         {N, 8787.87878, <<"test-testDs">>};
      1 ->
         {N, test, [list, 123, 456.789, "test"], {23231, "gggggg"}, <<"12345678901234567890">>};
      2 ->
         N;
      3 ->
         {N, <<"test-testDs">>}
   end.

start() ->
   %% erlang:process_flag(trap_exit, true),
   io:format("Ds benchmark..."),
   runDs(?DsList, ?V_NUM).

runDs([Ds | T], VNumList) ->
   printTitle(),
   runNum(VNumList, Ds),
   runDs(T, VNumList);
runDs([], _VNumList) ->
   ok.

runNum([Num | T], Ds) ->
   runExe(Num, Ds),
   runNum(T, Ds);
runNum([], _Ds) ->
   ok.

runExe(Num, Ds) ->
   Pid = erlang:spawn_link(Ds, start, [Num, self()]),
   receive
      {over, Pid, Insert, Read, Update, For, Delete} ->
         {_, DsName} = lists:split(2, atom_to_list(Ds)),
         io:format("~-9.s ~8.s ~12.s ~12.s ~10.s ~12.s ~10.s ~14.s ~10.s ~12.s ~12.s ~12.s ~n",
            [DsName, integer_to_list(Num), timeToStr(Insert), calcPer(Insert, Num), timeToStr(Read), calcPer(Read, Num), timeToStr(Update), calcPer(Update, Num), timeToStr(For), calcPer(For, Num), timeToStr(Delete), calcPer(Delete, Num)]);
      _ShutDown ->
         io:format("Ds test shutDown ~p ~p~n",[Ds, Num])
   end.

-define(S, 1000000000).
-define(MS, 1000000).
-define(US, 1000).
-define(NS, 1).

timeToStr(not_support) ->
   <<"noSupport">>;
timeToStr(skip) ->
   <<"skip">>;
timeToStr(Time) when Time > ?S ->
   float_to_list(Time/?S, [{decimals, 2}, compact]) ++ "s";
timeToStr(Time) when Time > ?MS ->
   float_to_list(Time/?MS, [{decimals, 2}, compact]) ++ "ms";
timeToStr(Time) when Time > ?US ->
   float_to_list(Time/?US, [{decimals, 2}, compact]) ++ "us";
timeToStr(Time) ->
   integer_to_list(Time) ++ "ns".

calcPer(not_support, _Num) ->
   <<"notSupport">>;
calcPer(skip, _Num) ->
   <<"skip">>;
calcPer(Time, Num) ->
   float_to_list(Time / Num, [{decimals, 2}, compact]) ++ "ns".

printTitle() ->
   io:format("~n~-9.s ~8.s ~12.s ~12.s ~10.s ~12.s ~10.s ~14.s ~10.s ~12.s ~12.s ~12.s ~n",
      ["DsName", "V_Num", "insert", "insert/per", "read", "read/per", "update", "update/per", "for", "for/per", "delete", "delete/per"]),
   io:format("~s ~n",[[$= || _ <- lists:seq(1, 145)]]).

