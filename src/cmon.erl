-module(cmon).
-include("db.hrl").
-export([uuid/0, uuid/1, check_uuid/1, set_response/4, set_cors/1]).
-export([wrap_response/1, wrap_key_response/1]).
-export([get_ttl/1, get_content_type/1, get_uuid_and_key/1, get_uuid/1]).

uuid() ->
	erlang:list_to_binary(uuid:uuid_to_string(uuid:get_v4(), standard)).

uuid(Uuid) ->
	erlang:list_to_binary(uuid:uuid_to_string(Uuid, standard)).

check_uuid(Uuid) ->
	UuidBin = uuid:string_to_uuid(Uuid),
	case uuid:is_v4(UuidBin) of
		true -> UuidBin;
		_ -> false
	end.

set_response(Req, Code, Type, Body) ->
	Req1 = cowboy_req:set_resp_header(<<"access-control-allow-origin">>, <<"*">>, Req),
	Req2 = cowboy_req:set_resp_header(<<"content-type">>, Type, Req1),
	Req3 = cowboy_req:set_resp_body(Body, Req2),
	{ok, Req4} = cowboy_req:reply(Code, Req3),
	Req4.

set_cors(Req) ->
	{Method, _} = cowboy_req:header(<<"access-control-request-method">>, Req, <<"*">>),
	{Headers, _} = cowboy_req:header(<<"access-control-request-headers">>, Req, <<"*">>),
	Cors = [
		{ <<"access-control-allow-origin">>, <<"*">> },
		{ <<"access-control-allow-credentials">>, <<"true">> },
		{ <<"access-control-allow-methods">>, Method },
		{ <<"access-control-allow-headers">>, Headers }
	],
	Req1 = lists:foldl(
		fun({Header, Value}, AccReq) -> 
			cowboy_req:set_resp_header(Header, Value, AccReq)
		end,
		Req, Cors),
	{ok, Req2} = cowboy_req:reply(204, Req1),
	Req2.

wrap_key_response({key, V}) ->
	case V of
		[] -> {404, <<"text/plain">>, <<"not found">>};
		[[undefined, Value]] -> {200, <<"application/json">>, Value};
		[[Type, Value]] -> {200, Type, Value}
	end;
wrap_key_response({hash, Props}) ->
	case Props of
		[] -> {200, <<"application/json">>, <<"[]">>};
		List ->
			Join = util:binary_join(lists:map(fun({K,V}) -> <<"\"", K/binary, "\": ", V/binary>> end, List), <<",">>),
			{200, <<"application/json">>, <<"{", Join/binary, "}">>}
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

get_uuid_and_key(Req) ->
	{Uuid, _} = cowboy_req:binding(uuid, Req),
	{PathInfo, _} = cowboy_req:path_info(Req),
	Key = util:binary_join(PathInfo, <<"/">>),
	{check_uuid(Uuid), Key}.

get_uuid(Req) ->
	{Uuid, _} = cowboy_req:binding(uuid, Req),
	check_uuid(Uuid).

get_content_type(Req) ->
	case cowboy_req:parse_header(<<"content-type">>, Req) of
		{ok, {Type, SubType, _}, _} -> {Type, SubType};
		_ -> {<<"application">>, <<"octet-stream">>}
	end.

get_ttl(Req) -> 
	{TTL, _} = cowboy_req:qs_val(<<"ttl">>, Req, erlang:integer_to_binary(db:default_ttl())),
	case TTL of
		<<"keep">> -> keep;
		_ -> min(erlang:binary_to_integer(TTL), db:default_ttl())
	end.