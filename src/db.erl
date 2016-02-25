-module(db).
-export([setup_mnesia/1, get_kv/2, set_kv/5, delete_k/2, delete_k/1, get_obsolete/0]).
-export([get_obsolete/1, default_ttl/0, all/0]).
-include_lib("stdlib/include/qlc.hrl").
-include("db.hrl").

-define(DEFAULT_TTL, 24*3600).

default_ttl() -> ?DEFAULT_TTL.

setup_mnesia(Nodes) ->
	mnesia:create_table(store, [
		{disc_copies, Nodes},
		{index, [uuid]},
		{attributes, record_info(fields, store)}
	]).

get_kv(Uuid, undefined) when is_binary(Uuid) ->
	Q = qlc:q([ Key || #store{uuid=U, key=Key} <- mnesia:table(store), Uuid == U, is_binary(Key)]),
	{atomic, Records} = mnesia:transaction(fun() -> qlc:e(Q) end),
	{list, Records};
get_kv(Uuid, Key) when is_binary(Uuid) ->
	F = fun() ->
		mnesia:select(store, [{#store{id={Uuid,Key}, _='_', type='$1', value='$2'}, [], [['$1','$2']]}])
	end,
	{atomic, V} = mnesia:transaction(F),
	{key, V};
get_kv(_, _) -> error.

set_kv(Uuid, Key, Type, Data, TTL) when is_binary(Uuid), is_binary(Type), is_binary(Data) ->
	F = fun() ->
		mnesia:write(#store{id={Uuid,Key}, uuid=Uuid, key=Key, type=Type, value=Data, ttl=TTL, stamp=util:now_to_sec()})
	end,
	{atomic, ok} = mnesia:transaction(F),
	gen_event:notify(evh_uuid, {update, Uuid, Key}),
	ok;
set_kv(_Uuid, _Key, _Type, _Data, _TTL) -> ?INFO("set:~p", [_Key]), error.

delete_k(Id = {Uuid, Key}) ->
	mnesia:transaction(fun() -> mnesia:delete({store, Id}) end),
	gen_event:notify(evh_uuid, {delete, Uuid, Key}),
	ok.

delete_k(Uuid, undefined) when is_binary(Uuid) ->
	F = fun() ->
		Keys = mnesia:select(store, [{#store{id='$1', uuid=Uuid, _='_'}, [], ['$1']}]),
		lists:foreach(fun(K) -> mnesia:delete({store, K}) end, Keys),
		ok
	end,
	{atomic, ok} = mnesia:transaction(F),
	ok;
delete_k(Uuid, K) when is_binary(Uuid) ->
	delete_k({Uuid, K});
delete_k(_, _) -> error.

get_obsolete() -> get_obsolete(util:now_to_sec()).

-spec get_obsolete(Time::non_neg_integer()) -> [#store{}].
get_obsolete(Time) ->
	Q = qlc:q([ Id || #store{id=Id} = #store{stamp=Stamp, ttl=TTL} <- mnesia:table(store), is_integer(TTL), Stamp + TTL < Time]),
	{atomic, Records} = mnesia:transaction(fun() -> qlc:e(Q) end),
	Records.

all() ->
	Q = qlc:q([ S || S <- mnesia:table(store)]),
	{atomic, Records} = mnesia:transaction(fun() -> qlc:e(Q) end),
	Records.