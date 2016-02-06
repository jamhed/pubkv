-module(pubkv_http_handler).
-behaviour(cowboy_http_handler).

-export([init/3, handle/2, terminate/3, setup_mnesia/0]).

-define(INFO(S,A), error_logger:info_msg(S, A)).

-record(state, {}).

-record(store, {uuid, key, value}).

setup_mnesia() ->
	Nodes = [node()],
	mnesia:stop(),
	mnesia:create_schema(Nodes),
	mnesia:start(),
	mnesia:create_table(store, [
		{disc_copies, Nodes},
		{index, [key]},
		{attributes, record_info(fields, store)}
	]).

uuid() ->
	erlang:list_to_binary(uuid:uuid_to_string(uuid:get_v4(), standard)).

check_uuid(Uuid) ->
	uuid:is_v4(uuid:string_to_uuid(erlang:binary_to_list(Uuid))).

init(_, Req, _Opts) ->
	{ok, Req, #state{}}.

handle(Req, State=#state{}) ->
	{Method, _} = cowboy_req:method(Req),
	{Path, _} = cowboy_req:path(Req),
	{Type, Body} = handle_req(Method, Path, Req),
	Req2 = cowboy_req:set_resp_header(<<"content-type">>, Type, Req),
	Req3 = cowboy_req:set_resp_body(Body, Req2),
	{ok, Req4} = cowboy_req:reply(200, Req3),
	{ok, Req4, State}.

terminate(_Reason, _Req, _State) ->
	ok.

handle_req(<<"GET">>, <<"/uuid">>, _Req) ->
	{<<"text/plain">>, uuid()};

handle_req(<<"GET">>, _, Req) ->
	{Uuid, _} = cowboy_req:binding(uuid, Req),
	{Key, _} = cowboy_req:binding(key, Req),
	get_kv(Uuid, Key);

handle_req(<<"POST">>, _, Req) ->
	{Uuid, _} = cowboy_req:binding(uuid, Req),
	{Key, _} = cowboy_req:binding(key, Req),
	{ok, Data, _} = cowboy_req:body(Req),
	set_kv(Uuid, Key, Data);

handle_req(Method, Path, _Req) ->
	{<<"text/plain">>, <<"not supported method ", Method/binary, " to ", Path/binary>>}.

get_kv(Uuid, Key) ->
	case check_uuid(Uuid) of 
		true ->
			UuidStr = erlang:binary_to_list(Uuid),
			KeyStr = erlang:binary_to_list(Key),
			F = fun() ->
				mnesia:select(store, [{#store{uuid=UuidStr, key=KeyStr, value='$1'}, [], ['$1']}])
			end,
			{atomic, Value} = mnesia:transaction(F),
			{<<"application/json">>, Value};
		false -> {<<"text/plain">>, <<"not uuid">>}
	end.

set_kv(Uuid, Key, Data) ->
	case check_uuid(Uuid) of 
		true ->
			UuidStr = erlang:binary_to_list(Uuid),
			KeyStr = erlang:binary_to_list(Key),
			F = fun() ->
				mnesia:write(#store{uuid=UuidStr, key=KeyStr, value=Data})
			end,
			{atomic, ok} = mnesia:transaction(F),
			{<<"text/plain">>, <<"ok">>};
		false -> {<<"text/plain">>, <<"not uuid">>}
	end.