-module(db_sha).
-export([set/2, get/1, delete/1, all/0]).
-include_lib("stdlib/include/qlc.hrl").
-include("db.hrl").

map_key(Key) -> crypto:hash(sha256, Key).

set(Key, Value) when is_binary(Key), is_binary(Value) ->
	F = fun() ->
		mnesia:write(#sha{id=map_key(Key), value=Value})
	end,
	{atomic, ok} = mnesia:transaction(F),
	ok.

get(Key) when is_binary(Key) ->
	F = fun() ->
		mnesia:select(sha, [{#sha{id=map_key(Key), _='_', type='$1',value='$2'}, [], [['$1','$2']]}])
	end,
	{atomic, V} = mnesia:transaction(F),
	{key, V}.

delete(Key) when is_binary(Key) ->
	mnesia:transaction(fun() -> mnesia:delete({sha, map_key(Key)}) end),
	ok.

all() ->
	Q = qlc:q([ S || S <- mnesia:table(sha)]),
	{atomic, Records} = mnesia:transaction(fun() -> qlc:e(Q) end),
	Records.