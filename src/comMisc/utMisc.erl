-module(utMisc).

-compile([export_all, nowarn_export_all]).

%% 日志记录函数
fileLog(T, F, A, Mod, Line) ->
   {ok, Fl} = file:open("logs/error_log.txt", [write, append]),
   Format = list_to_binary("#" ++ T ++ " ~s[~w:~w] " ++ F ++ "\r\n~n"),
   {{Y, M, D}, {H, I, S}} = erlang:localtime(),
   Date = list_to_binary([integer_to_list(Y), "-", integer_to_list(M), "-", integer_to_list(D), " ", integer_to_list(H), ":", integer_to_list(I), ":", integer_to_list(S)]),
   io:format(Fl, unicode:characters_to_list(Format), [Date, Mod, Line] ++ A),
   file:close(Fl).

compile_base_data(Table, ModName, IDPoses) ->
   ModNameString = com_util:term_to_string(ModName),
   HeadString =
      "-module(" ++ ModNameString ++ ").
		-compile(export_all).
		",
   BaseDataList = db_base:select_all(Table, "*", []),
   ContentString =
      lists:foldl(fun(BaseData0, PreString) ->
         FunChange =
            fun(Field) ->
               if is_integer(Field) -> Field;
                  true ->
                     case com_util:bitstring_to_term(Field) of
                        undefined ->
                           Field;
                        Term ->
                           Term
                     end
               end
            end,
         BaseData = [FunChange(Item) || Item <- BaseData0],
         Base = list_to_tuple([Table | BaseData]),
         BaseString = com_util:term_to_string(Base),
         IDs = [element(Pos, Base) || Pos <- IDPoses],
         IDList0 = lists:foldl(fun(ID, PreString2) ->
            IDList =
               if erlang:is_integer(ID) ->
                  integer_to_list(ID);
                  true ->
                     ID
               end,
            PreString2 ++ "," ++ IDList
                               end, [], IDs),
         [_ | IDList] = IDList0,
         PreString ++
            "get(" ++
            IDList ++
            ") ->" ++
            BaseString ++
            ";
            "
                  end
         , "", BaseDataList),

   _List0 = [",_" || _Pos <- IDPoses],
   [_ | _List] = lists:flatten(_List0),
   ErrorString = "get(" ++ _List ++ ") -> undefined.
	",
   FinalString = HeadString ++ ContentString ++ ErrorString,
   %% ?PRINT("string=~s~n",[FinalString]),
   try
      {Mod, Code} = dynamic_compile:from_string(FinalString),
      code:load_binary(Mod, ModNameString ++ ".erl", Code)
   catch
      Type:Error -> io:format("Error compiling (~p): ~p~n", [Type, Error])
   end,
   ok.



