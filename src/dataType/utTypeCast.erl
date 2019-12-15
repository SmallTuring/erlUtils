-module(utTypeCast).

-compile([export_all, nowarn_export_all]).

toFloat(Value) when is_list(Value) -> list_to_float(Value);
toFloat(Value) when is_binary(Value) -> binary_to_float(Value);
toFloat(Value) when is_integer(Value) -> erlang:float(Value);
toFloat(Value) when is_float(Value) -> Value.

toList(undefined) -> "undefined";
toList(null) -> "null";
toList(Value) when is_tuple(Value) -> tuple_to_list(Value);
toList(Value) when is_binary(Value) -> binary_to_list(Value);
toList(Value) when is_bitstring(Value) -> bitstring_to_list(Value);
toList(Value) when is_integer(Value) -> integer_to_list(Value);
toList(Value) when is_float(Value) -> float_to_list(Value, [{decimals, 6}, compact]);
toList(Value) when is_atom(Value) -> atom_to_list(Value);
toList(Value) when is_list(Value) -> Value;
toList([Tuple | PropList] = Value) when is_list(PropList) and is_tuple(Tuple) ->
   lists:map(fun({K, V}) -> {toList(K), toList(V)} end, [Value]).

%% to_list(Term) when is_binary(Term) ->
%%    case unicode:characters_to_binary(Term, utf8, utf8) of
%%       Term ->
%%          unicode:characters_to_list(Term);
%%       _ ->
%%          binary_to_list(Term)
%%    end.

toBinary(Value) when is_integer(Value) -> integer_to_binary(Value);
toBinary(Value) when is_list(Value) -> list_to_binary(Value);
toBinary(Value) when is_float(Value) -> float_to_binary(Value, [{decimals, 6}, compact]);
toBinary(Value) when is_atom(Value) -> atom_to_binary(Value, utf8);
toBinary(Value) when is_binary(Value) -> Value;
toBinary([Tuple | PropList] = Value) when is_list(PropList) and is_tuple(Tuple) ->
   lists:map(fun({K, V}) -> {toBinary(K), toBinary(V)} end, Value);
toBinary(Value) -> term_to_binary(Value).

toInteger(undefined) -> undefined;
toInteger(Value) when is_float(Value) -> trunc(Value);
toInteger(Value) when is_list(Value) -> list_to_integer(Value);
toInteger(Value) when is_binary(Value) -> binary_to_integer(Value);
toInteger(Value) when is_tuple(Value) -> toInteger(tuple_to_list(Value));
toInteger(Value) when is_integer(Value) -> Value.


dataType(Data) when is_list(Data) -> list;
dataType(Data) when is_integer(Data) -> integer;
dataType(Data) when is_binary(Data) -> binary;
dataType(Data) when is_function(Data) -> function;
dataType(Data) when is_tuple(Data) -> tuple;
dataType(Data) when is_atom(Data) -> atom;
dataType(Data) when is_bitstring(Data) -> bitstring;
dataType(Data) when is_boolean(Data) -> boolean;
dataType(Data) when is_float(Data) -> float;
dataType(Data) when is_number(Data) -> number;
dataType(Data) when is_pid(Data) -> pid;
dataType(Data) when is_port(Data) -> port;
dataType(_Data) -> not_know.

%% Trim the binary
-spec trim(Bin :: binary()) -> binary().
trim(Bin) when is_binary(Bin) -> trimHead(trimTail(Bin)).

%% Trim head of binary
-spec trimHead(Bin :: binary()) -> binary().
trimHead(<<>>) -> <<>>;
trimHead(<<C, BinTail/binary>> = Bin) ->
   case is_whitespace(C) of
      true -> trimHead(BinTail);
      false -> Bin
   end.

%% Trim tail of binary
-spec trimTail(Bin :: binary()) -> binary().
trimTail(<<>>) -> <<>>;
trimTail(Bin) ->
   Size = byte_size(Bin) - 1,
   <<BinHead:Size/binary, C>> = Bin,
   case is_whitespace(C) of
      true -> trimTail(BinHead);
      false -> Bin
   end.

%% Check if the char is a whitespace
-spec is_whitespace(char()) -> true | false.
is_whitespace($\s) -> true;
is_whitespace($\t) -> true;
is_whitespace($\n) -> true;
is_whitespace($\r) -> true;
is_whitespace(_) -> false.

%% term序列化, term转为string
term_to_string(Bin) when is_binary(Bin) ->
   binary_to_list(Bin);
term_to_string(Term) ->
   case catch io_lib:format("~w", [Term]) of
      {'EXIT', _} -> lists:flatten(io_lib:format("~p", [Term]));
      GoodString -> lists:flatten(GoodString)
   end.

%% term反序列化, string转换为term
string_to_term(String) ->
   case erl_scan:string(String ++ ".") of
      {ok, Tokens, _} ->
         case erl_parse:parse_term(Tokens) of
            {ok, Term} -> Term;
            _Err -> String
         end;
      _Error ->
         undefined
   end.

%% 列表转utf8编码的列表
listToUtfString(List) ->
   unicode:characters_to_list(erlang:list_to_binary(List), utf8).

%%汉字unicode编码范围 0x4e00 - 0x9fa5
% (4 * 16 * 16 * 16 + 14 * 16 * 16)
-define(UNICODE_CHINESE_BEGIN, 16#4e00).
% (9 * 16 * 16 * 16 + 15 * 16 * 16 + 10 * 16 + 5)
-define(UNICODE_CHINESE_END, 16#9fa5).

%% desc   获取字符串汉字和非汉字的个数
%% parm   UTF8String  			UTF8编码的字符串
%% return {汉字个数,非汉字个数}
getChineseNum(UTF8String) ->
   UnicodeList = unicode:characters_to_list(list_to_binary(UTF8String)),
   Fun = fun(Num, {Sum}) ->
      case Num >= ?UNICODE_CHINESE_BEGIN andalso Num =< ?UNICODE_CHINESE_END of
         true ->
            {Sum + 1};
         false ->
            {Sum}
      end
         end,
   {ChineseCount} = lists:foldl(Fun, {0}, UnicodeList),
   OtherCount = length(UnicodeList) - ChineseCount,
   {ChineseCount, OtherCount}.

termToBase64(Term) ->
   base64:encode(term_to_binary(Term)).

base64ToTerm(Base64String) ->
   binary_to_term(base64:decode(Base64String)).
