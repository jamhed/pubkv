-module(db_util).
-export([all/1]).
-include_lib("stdlib/include/qlc.hrl").

all(Table) ->
	Q = qlc:q([ S || S <- mnesia:table(Table)]),
	{atomic, Records} = mnesia:transaction(fun() -> qlc:e(Q) end),
	Records.