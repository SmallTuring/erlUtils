-module(utTestPerformance).


-compile([export_all, nowarn_unused_function, nowarn_unused_vars, nowarn_export_all]).

-record(cycleData, {
   module = test
   , isEnter = false
   , hibernateAfter = infinity
   , isHibernate = false
   , lastStatus = init_status
   , lastState = #{11 => 555, 44 => 434}
   , postponed = [1, 3, "dffd", "fdf"]
   , timers = #{etime => {aaa, fdfd}}
}).

tt1(N) ->
   tt1(N, #cycleData{}, [123421, 434, 34], {12222, 343, "fdfd"}, fff, "ArgD").

tt1(0, CycleData, ArgA, ArgB, ArgC, ArgD) ->
   ok;
tt1(N, #cycleData{lastState = LastState, isEnter = IsEnter, hibernateAfter = HibernateAfter, module = Module} = CycleData, ArgA, ArgB, ArgC, ArgD) ->
   I1 = is_atom(LastState),
   I2 = is_atom(IsEnter),
   I3 = is_atom(HibernateAfter),
   I4 = is_atom(Module),
   _Ret = I1 andalso I2 andalso I3 andalso I4 andalso true,
   tt1(N - 1, CycleData, ArgA, ArgB, ArgC, ArgD).

tt2(N) ->
   tt2(N, #cycleData{}, [123421, 434, 34], {12222, 343, "fdfd"}, fff, "ArgD").

tt2(0, CycleData, ArgA, ArgB, ArgC, ArgD) ->
   ok;
tt2(N, CycleData, ArgA, ArgB, ArgC, ArgD) ->
   LastState = element(#cycleData.lastState, CycleData),
   IsEnter = element(#cycleData.isEnter, CycleData),
   HibernateAfter = element(#cycleData.hibernateAfter, CycleData),
   Module = element(#cycleData.module, CycleData),
   I1 = is_atom(LastState),
   I2 = is_atom(IsEnter),
   I3 = is_atom(HibernateAfter),
   I4 = is_atom(Module),
   _Ret = I1 andalso I2 andalso I3 andalso I4 andalso true,
   tt2(N - 1, CycleData, ArgA, ArgB, ArgC, ArgD).

tt3(N) ->
   tt3(N, #cycleData{}, [123421, 434, 34], {12222, 343, "fdfd"}, fff, "ArgD").

tt3(0, CycleData, ArgA, ArgB, ArgC, ArgD) ->
   ok;
tt3(N, CycleData, ArgA, ArgB, ArgC, ArgD) ->
   NewCycleData = CycleData#cycleData{module = tttt, lastState = #{11 => 22, 22 => 33}, isEnter = false},
   tt3(N - 1, NewCycleData, ArgA, ArgB, ArgC, ArgD).

tt4(N) ->
   tt4(N, #cycleData{}, [123421, 434, 34], {12222, 343, "fdfd"}, fff, "ArgD").

tt4(0, CycleData, ArgA, ArgB, ArgC, ArgD) ->
   ok;
tt4(N, CycleData, ArgA, ArgB, ArgC, ArgD) ->
   New1 = setelement(#cycleData.module, CycleData, tttt),
   New2 = setelement(#cycleData.lastState, New1, #{11 => 22, 22 => 33}),
   New3 = setelement(#cycleData.isEnter, New2, false),
   tt4(N - 1, New3, ArgA, ArgB, ArgC, ArgD).

tt5(N) ->
   tt5(N, #cycleData{}, [123421, 434, 34]).

tt5(0, CycleData, AA) ->
   ok;
tt5(N, CycleData, AA) ->
   tt5(N - 1, CycleData, AA).

tt6(N) ->
   tt6(N, test, false, infinity, false, init_status, #{11 => 555, 44 => 434}, [1, 3, "dffd", "fdf"], #{etime => {aaa, fdfd}}, [123421, 434, 34]).

tt6(0, A1, B, C, D, E, F, G, H, AA) ->
   ok;
tt6(N, A1, B, C, D, E, F, G, H, AA) ->
   tt6(N - 1, A1, B, C, D, E, F, G, H, AA).

tt7(0) ->
   ok;
tt7(N) ->
   tt7(N - 1).


tt8(N) ->
   tt8(N, #cycleData{}).

tt8(0, CycleData) ->
   ok;
tt8(N, #cycleData{module = Module, lastState = Lasst, postponed = Postponed} = AA) ->
   A = setelement(#cycleData.module, AA, ttt),
   B = setelement(#cycleData.isEnter, A, trye),
   %% B = setelement(#cycleData.lastState, A, #{22 => 555, 55 => 434}),
   %% C = setelement(#cycleData.postponed, B, [1,3,"fdf", "dffd"]),
   tt8(N - 1, B).

tt88(N) ->
   tt88(N, #cycleData{}).

tt88(0, CycleData) ->
   ok;
tt88(N, #cycleData{module = Module, lastState = Lasst, postponed = Postponed} = AA) ->
   %%C = setelement(#cycleData.postponed, AA, [1,3,"fdf", "dffd"]),
   %%B = setelement(#cycleData.lastState, C, #{22 => 555, 55 => 434}),
   B = setelement(#cycleData.isEnter, AA, trye),
   A = setelement(#cycleData.module, B, ttt),
   tt88(N - 1, A).

tt888(N) ->
   tt888(N, #cycleData{}).

tt888(0, CycleData) ->
   ok;
tt888(N, #cycleData{module = Module, lastState = Lasst, postponed = Postponed} = AA) ->
   A = AA#cycleData{isEnter = trye, module = ttt},
   tt888(N - 1, A).

tt9(N) ->
   Data = #cycleData{},
   put(a, Data#cycleData.module),
   put(b, Data#cycleData.isEnter),
   put(c, Data#cycleData.hibernateAfter),
   put(d, Data#cycleData.isHibernate),
   put(e, Data#cycleData.lastStatus),
   put(f, Data#cycleData.lastState),
   put(g, Data#cycleData.postponed),
   tt10(N).

tt10(0) ->
   ok;
tt10(N) ->
   A = get(a),
   put(a, ttt),
   B = get(f),
   put(f, #{22 => 555, 55 => 434}),
   C = get(g),
   put(g, [1, 3, "fdf", "dffd"]),
   _Ret = A orelse B orelse C orelse true,
   tt10(N - 1).

c6(N) ->
   cc6(N, 0).

cc6(0, F) ->
   F;
cc6(N, _F) ->
   F = N + 0.0,
   cc6(N - 1, F).

c7(N) ->
   cc7(N, 0).

cc7(0, F) ->
   F;
cc7(N, _F) ->
   F = float(N),
   cc7(N - 1, F).

s1(0, Fun) ->
   ok;
s1(N, Fun) ->
   ?MODULE:Fun(),
   s1(N - 1, Fun).

st1() ->
   size(<<"fdfdfdd:fdffd:\rn\n:fdfd fd df df dfddfdf">>).

st2() ->
   byte_size(<<"fdfdfdd:fdffd:\rn\n:fdfd fd df df dfddfdf">>).

st3() ->
   iolist_size(<<"fdfdfdd:fdffd:\rn\n:fdfd fd df df d:fddfdf">>).

st4() ->
   size(<<"fdfdfdd:fdffd:\rn\n:fdfd fd df df dfddfdf">>).

gm(0, Fun) ->
   ok;
gm(N, Fun) ->
   [?MODULE:Fun(M) || M <- [1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12]],
   gm(N - 1, Fun).

%% 这个更快
getMonth(1) ->
   <<"Jan">>;
getMonth(2) ->
   <<"Feb">>;
getMonth(3) ->
   <<"Mar">>;
getMonth(4) ->
   <<"Apr">>;
getMonth(5) ->
   <<"May">>;
getMonth(6) ->
   <<"Jun">>;
getMonth(7) ->
   <<"Jul">>;
getMonth(8) ->
   <<"Aug">>;
getMonth(9) ->
   <<"Sep">>;
getMonth(10) ->
   <<"Oct">>;
getMonth(11) ->
   <<"Nov">>;
getMonth(12) ->
   <<"Dec">>.

getMonth2(Month) ->
   element(Month, {<<"Jan">>, <<"Feb">>, <<"Mar">>, <<"Apr">>, <<"May">>, <<"Jun">>, <<"Jul">>, <<"Aug">>, <<"Sep">>, <<"Oct">>, <<"Nov">>, <<"Dec">>}).

-define(Month, #{1 => <<"Jan">>, 2 => <<"Feb">>, 3 => <<"Mar">>, 4 => <<"Apr">>, 5 => <<"May">>, 6 => <<"Jun">>, 7 => <<"Jul">>, 8 => <<"Aug">>, 9 => <<"Sep">>, 10 => <<"Oct">>, 11 => <<"Nov">>, 12 => <<"Dec">>}).
getMonth3(Month) ->
   case ?Month of
      #{Month := MonthStr} ->
         MonthStr;
      _ ->
         <<"">>
   end.

-define(List, [1, 2, 3234235, <<"fdsfasf">>,  <<"fdsfasf111111111111111111111111">>, [3434,43,434], tryrer, {rqwrer, 342144}, #{23424 => "fdsfsdafsaf"}, {432143, "fdsaf", 76767}]).

ht(0, _Fun) ->
   ok;
ht(N, Fun) ->
   [?MODULE:Fun(Term) || Term <- ?List],
   ht(N - 1, Fun).

hash1(Term) ->
   erlang:phash(Term, 256).

hash2(Term) ->
   erlang:phash2(Term, 256).

hashn1(Term) ->
   nifHashb:hash1(Term, 256).

hashn2(Term) ->
   nifHashb:hash2(Term, 256).

hashn3(Term) ->
   nifHashb:hash3(Term, 256).

ht1(0, _Fun, Term) ->
   ok;
ht1(N, Fun, Term) ->
   ?MODULE:Fun(Term),
   ht1(N - 1, Fun, Term).

hash3(Term) ->
   erlang:phash(Term, 256).

hash4(Term) ->
   erlang:phash2(Term, 256).

ttT(0, Fun) ->
   ?MODULE:Fun(0);
ttT(N, Fun) ->
   ?MODULE:Fun(N),
   ttT(N - 1, Fun).

nifT(N) ->
   [nifArray:test(Term) || Term <- ?List].

nifT1(N) ->
   [nifArray:test1(Term) || Term <- ?List].

cb(N, Len, Fun) ->
   Bin = utGenTerm:genBinary(Len),
   cddo(N, Bin, Fun).

cddo(0, Bin, Fun) ->
   nifHashb:Fun(Bin, Bin);
cddo(N, Bin, Fun) ->
   nifHashb:Fun(Bin, Bin),
   cddo(N - 1, Bin, Fun).


