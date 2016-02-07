-module(db).
-export([setup_mnesia/0, get_kv/2, set_kv/4, delete_k/2]).

-record(store, {id, uuid, key, type, value}).

setup_mnesia() ->
	Nodes = [node()],
	mnesia:stop(),
	mnesia:create_schema(Nodes),
	mnesia:start(),
	mnesia:create_table(store, [
		{disc_copies, Nodes},
		{index, [uuid]},
		{attributes, record_info(fields, store)}
	]).

get_kv(Uuid, Key) when is_binary(Uuid), is_binary(Key) ->
	F = fun() ->
		mnesia:select(store, [{#store{id={Uuid,Key}, _='_', type='$1', value='$2'}, [], [['$1','$2']]}])
	end,
	{atomic, V} = mnesia:transaction(F),
	case V of
		[] -> {404, <<"text/plain">>, <<"not found">>};
		[[Type, Value]] -> {200, Type, Value}
	end;
get_kv(Uuid, undefined) when is_binary(Uuid) ->
	F = fun() ->
		mnesia:select(store, [{#store{uuid=Uuid, _='_', key='$1'}, [], ['$1']}])
	end,
	{atomic, V} = mnesia:transaction(F),
	case V of
		[] -> {200, <<"application/json">>, <<"[]">>};
		List ->
			Join = binary_join(lists:map(fun(B) -> <<"\"", B/binary, "\"">> end, List), <<",">>),
			{200, <<"application/json">>, <<"[", Join/binary, "]">>}
	end;
get_kv(_, _) -> {406, <<"text/plain">>, <<"not acceptable">>}.

set_kv(Uuid, Key, Type, Data) when is_binary(Uuid), is_binary(Key), is_binary(Type), is_binary(Data) ->
	F = fun() ->
		mnesia:write(#store{id={Uuid,Key}, uuid=Uuid, key=Key, type=Type, value=Data})
	end,
	{atomic, ok} = mnesia:transaction(F),
	{200, <<"text/plain">>, <<"ok">>};
set_kv(_ , _, _, _ ) -> {406, <<"text/plain">>, <<"not acceptable">>}.

delete_k(Uuid, K) when is_binary(Uuid), is_binary(K) ->
	F = fun() ->
		mnesia:delete({store, {Uuid, K}})
	end,
	{atomic, ok} = mnesia:transaction(F),
	{200, <<"text/plain">>, <<"ok">>};
delete_k(Uuid, undefined) when is_binary(Uuid) ->
	F = fun() ->
		Keys = mnesia:select(store, [{#store{id='$1', uuid=Uuid, _='_'}, [], ['$1']}]),
		lists:foreach(fun(K) -> mnesia:delete({store, K}) end, Keys),
		ok
	end,
	{atomic, ok} = mnesia:transaction(F),
	{200, <<"text/plain">>, <<"ok">>};
delete_k(_, _) -> {406, <<"text/plain">>, <<"not acceptable">>}.

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