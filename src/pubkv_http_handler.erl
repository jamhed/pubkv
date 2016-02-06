-module(pubkv_http_handler).
-behaviour(cowboy_http_handler).

-export([init/3, handle/2, terminate/3]).

-define(INFO(S,A), error_logger:info_msg(S, A)).

-record(state, {}).

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
	db:get_kv(check_uuid(Uuid), Key);

handle_req(<<"DELETE">>, _, Req) ->
	{Uuid, _} = cowboy_req:binding(uuid, Req),
	{Key, _} = cowboy_req:binding(key, Req),
	db:delete_k(check_uuid(Uuid), Key);

handle_req(<<"PUT">>, _, Req) ->
	{Uuid, _} = cowboy_req:binding(uuid, Req),
	{Key, _} = cowboy_req:binding(key, Req),
	{ok, {Type, SubType, _}, _} = cowboy_req:parse_header(<<"content-type">>, Req),
	{ok, Data, _} = cowboy_req:body(Req),
	db:set_kv(check_uuid(Uuid), Key, <<Type/binary,"/",SubType/binary>>, Data);

handle_req(Method, Path, _Req) ->
	{405, <<"text/plain">>, <<"not supported method ", Method/binary, " to ", Path/binary>>}.

