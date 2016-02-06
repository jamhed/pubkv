-module(pubkv_http_handler).
-behaviour(cowboy_http_handler).

-export([init/3, handle/2, terminate/3, setup_mnesia/0]).

-define(INFO(S,A), error_logger:info_msg(S, A)).

-record(state, {}).

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

uuid() ->
	erlang:list_to_binary(uuid:uuid_to_string(uuid:get_v4(), standard)).

check_uuid(Uuid) ->
	UuidBin = uuid:string_to_uuid(Uuid),
	case uuid:is_v4(UuidBin) of
		true -> UuidBin;
		fasle -> false
	end.

init(_, Req, _Opts) ->
	{ok, Req, #state{}}.

handle(Req, State=#state{}) ->
	{Method, _} = cowboy_req:method(Req),
	{Path, _} = cowboy_req:path(Req),
	{Code, Type, Body} = handle_req(Method, Path, Req),
	Req2 = cowboy_req:set_resp_header(<<"content-type">>, Type, Req),
	Req3 = cowboy_req:set_resp_body(Body, Req2),
	{ok, Req4} = cowboy_req:reply(Code, Req3),
	{ok, Req4, State}.

terminate(_Reason, _Req, _State) ->
	ok.

handle_req(<<"GET">>, <<"/uuid">>, _Req) ->
	{200, <<"text/plain">>, uuid()};

handle_req(<<"GET">>, _, Req) ->
	{Uuid, _} = cowboy_req:binding(uuid, Req),
	{Key, _} = cowboy_req:binding(key, Req),
	get_kv(check_uuid(Uuid), Key);

handle_req(<<"DELETE">>, _, Req) ->
	{Uuid, _} = cowboy_req:binding(uuid, Req),
	{Key, _} = cowboy_req:binding(key, Req),
	delete_k(check_uuid(Uuid), Key);

handle_req(<<"PUT">>, _, Req) ->
	{Uuid, _} = cowboy_req:binding(uuid, Req),
	{Key, _} = cowboy_req:binding(key, Req),
	{ok, {Type, SubType, _}, _} = cowboy_req:parse_header(<<"content-type">>, Req),
	{ok, Data, _} = cowboy_req:body(Req),
	set_kv(check_uuid(Uuid), Key, <<Type/binary,"/",SubType/binary>>, Data);

handle_req(Method, Path, _Req) ->
	{405, <<"text/plain">>, <<"not supported method ", Method/binary, " to ", Path/binary>>}.

% implementation

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