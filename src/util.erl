-module(util).
-export([binary_join/2, now_to_sec/0, now_to_sec/1, sec_to_now/1]).

-spec binary_join([binary()], binary()) -> binary().
binary_join([], _Sep) -> <<>>;
binary_join([Part], _Sep) -> Part;
binary_join(List, Sep) ->
	lists:foldr(
	fun(A, B) ->
		if
			bit_size(B) > 0 -> <<A/binary, Sep/binary, B/binary>>;
			true -> A
		end
	end, <<>>, List).

now_to_sec() -> now_to_sec(erlang:timestamp()).
now_to_sec({Mega,Sec,_Milli}) -> Mega*1000000+Sec.
sec_to_now(Time) ->
	Mega = trunc(Time/1000000),
	{Mega, Time-Mega*1000000, 0}.
