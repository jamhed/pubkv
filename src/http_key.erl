-module(http_key).
-behaviour(cowboy_http_handler).

-export([init/3, handle/2, terminate/3]).

init(_, Req, _Opts) -> {ok, Req, []}.

handle(Req, State) ->
	{Method, _} = cowboy_req:method(Req),
	{Code, Type, Body} = handle_req(Method, Req),
	{ok, cmon:set_response(Req, Code, Type, Body), State}.

terminate(_Reason, _Req, _State) -> ok.

handle_req(<<"GET">>, Req) ->
	{Uuid, _} = cowboy_req:binding(uuid, Req),
	{Key, _} = cowboy_req:binding(key, Req),
	db:get_kv(translate_ro(cmon:check_uuid(Uuid)), Key);

handle_req(<<"DELETE">>, Req) ->
	{Uuid, _} = cowboy_req:binding(uuid, Req),
	{Key, _} = cowboy_req:binding(key, Req),
	db:delete_k(cmon:check_uuid(Uuid), Key);

handle_req(<<"PUT">>, Req) ->
	{Uuid, _} = cowboy_req:binding(uuid, Req),
	{Key, _} = cowboy_req:binding(key, Req),
	{ok, {Type, SubType, _}, _} = cowboy_req:parse_header(<<"content-type">>, Req),
	{ok, Data, _} = cowboy_req:body(Req),
	db:set_kv(cmon:check_uuid(Uuid), Key, <<Type/binary,"/",SubType/binary>>, Data);

handle_req(Method, _Req) ->
	{405, <<"text/plain">>, <<"not supported method ", Method/binary>> }.

translate_ro(Uuid) -> translate_ro(db_ro:translate(Uuid), Uuid).
translate_ro([Uuid], _) -> Uuid;
translate_ro([], Origin) -> Origin.