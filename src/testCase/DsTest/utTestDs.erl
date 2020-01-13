-module(utTestDs).
-compile([export_all, nowarn_unused_function, nowarn_unused_vars, nowarn_export_all]).

-define(V_NUM, [8, 16, 32, 64, 128, 256, 516, 1024, 2048, 4096, 8192, 16384, 32768, 65536, 131072, 524288, 1048576]).
-define(DsList, [arrayDs, tupleDs, listsDs, setsDs, gb_setsDs, gb_treesDs, dictDs, etsSetDs, etsOrdDs, mapsDs, orddictDs, ordsetsDs, pdDs, atomicsDs, persistentTermDs]).

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
   io:format("Ds benchmark...~n~n" ++ "DsName    V_Num    insert    read    update     for    delete ~n" ++ [$= || _ <- lists:seq(1, 51)] ++ "~n", []),
   runDs(?DsList, ?V_NUM).

runDs([Ds | T], VNumList) ->
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
         io:format("~-10s ~8B ~8B ~8B ~8B ~8B ~8B~n", [Ds, Num, Insert, Read, Update, For, Delete]);
      _ShutDown ->
         io:format("Ds test shutDown ~p ~p~n",[Ds, Num])
   end.
