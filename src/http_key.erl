-module(http_key).
-behaviour(cowboy_http_handler).
-export([init/3, handle/2, terminate/3]).
-include("db.hrl").

init(_, Req, _Opts) -> {ok, Req, []}.

handle(Req, State) ->
	{Method, _} = cowboy_req:method(Req),
	case Method of
		<<"OPTIONS">> ->
			{ok, cmon:set_cors(Req), State};
		_ ->
			{Code, Type, Body} = wrap_handle_req(Method, Req),
			{ok, cmon:set_response(Req, Code, Type, Body), State}
	end.

wrap_handle_req(Method, Req) ->
	try handle_req(Method, Req) of
		Re -> Re
	catch
		_:_ -> wrap_response(error)
	end.

terminate(_Reason, _Req, _State) -> ok.

wrap_key_response({key, V}) ->
	case V of
		[] -> {404, <<"text/plain">>, <<"not found">>};
		[[Type, Value]] -> {200, Type, Value}
	end;
wrap_key_response({list, V}) ->
	case V of
		[] -> {200, <<"application/json">>, <<"[]">>};
		List ->
			Join = util:binary_join(lists:map(fun(B) -> <<"\"", B/binary, "\"">> end, List), <<",">>),
			{200, <<"application/json">>, <<"[", Join/binary, "]">>}
	end;
wrap_key_response(_) ->
	{406, <<"text/plain">>, <<"not acceptable">>}.

wrap_response(ok) ->
	{200, <<"text/plain">>, <<"ok">>};
wrap_response(_) ->
	{406, <<"text/plain">>, <<"not acceptable">>}.

handle_req(<<"GET">>, Req) ->
	{Uuid, _} = cowboy_req:binding(uuid, Req),
	{Key, _} = cowboy_req:binding(key, Req),
	wrap_key_response(db:get_kv(translate_ro(cmon:check_uuid(Uuid)), Key));

handle_req(<<"DELETE">>, Req) ->
	{Uuid, _} = cowboy_req:binding(uuid, Req),
	{Key, _} = cowboy_req:binding(key, Req),
	wrap_response(db:delete_k(cmon:check_uuid(Uuid), Key));

handle_req(<<"PUT">>, Req) ->
	{Uuid, _} = cowboy_req:binding(uuid, Req),
	{Key, _} = cowboy_req:binding(key, Req),
	{ok, {Type, SubType, _}, _} = cowboy_req:parse_header(<<"content-type">>, Req),
	{ok, Data, _} = cowboy_req:body(Req),
	Re = db:set_kv(cmon:check_uuid(Uuid), Key, <<Type/binary,"/",SubType/binary>>, Data, get_ttl(Req)),
	wrap_response(Re);

handle_req(Method, _Req) ->
	{405, <<"text/plain">>, <<"not supported method ", Method/binary>> }.

get_ttl(Req) -> 
	{TTL, _} = cowboy_req:qs_val(<<"ttl">>, Req, db:default_ttl()),
	case TTL of
		<<"keep">> -> keep;
		_ -> min(erlang:binary_to_integer(TTL), db:default_ttl())
	end.

translate_ro(Uuid) -> translate_ro(db_ro:translate(Uuid), Uuid).
translate_ro([Uuid], _) -> Uuid;
translate_ro([], Origin) -> Origin.