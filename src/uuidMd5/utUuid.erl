-module(utUuid).

-compile([export_all, nowarn_export_all]).

-spec randStr(integer()) -> string().
randStr(Length) ->
   [
      begin
         Char = 47 + rand:uniform(75),
         if
            Char =< 57 -> Char;
            Char =< 59 -> 58 - rand:uniform(10);
            Char =< 64 -> 64 + rand:uniform(26);
            Char =< 90 -> Char;
            Char =< 96 -> 96 + rand:uniform(26);
            true -> Char
         end
      end || _ <- lists:seq(1, Length)].

-spec uuid() -> binary.
uuid() ->
   erlang:md5(term_to_binary({erlang:system_time(nanosecond), rand:uniform(134217727), make_ref()})).

-spec uuidHex() -> binary.
uuidHex() ->
   utMd5:getMd5Hex(term_to_binary({erlang:system_time(nanosecond), rand:uniform(134217727), make_ref()})).
