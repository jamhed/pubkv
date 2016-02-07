-module(cmon).
-include("db.hrl").
-export([uuid/0, uuid/1, check_uuid/1, set_response/4, set_cors/1]).

uuid() ->
	erlang:list_to_binary(uuid:uuid_to_string(uuid:get_v4(), standard)).

uuid(Uuid) ->
	erlang:list_to_binary(uuid:uuid_to_string(Uuid, standard)).

check_uuid(Uuid) ->
	UuidBin = uuid:string_to_uuid(Uuid),
	case uuid:is_v4(UuidBin) of
		true -> UuidBin;
		fasle -> false
	end.

set_response(Req, Code, Type, Body) ->
	Req2 = cowboy_req:set_resp_header(<<"content-type">>, Type, Req),
	Req3 = cowboy_req:set_resp_body(Body, Req2),
	{ok, Req4} = cowboy_req:reply(Code, Req3),
	Req4.

set_cors(Req) ->
	{Method, _} = cowboy_req:header(<<"access-control-requst-method">>, Req, <<"*">>),
	{Headers, _} = cowboy_req:header(<<"access-control-requst-headers">>, Req, <<"*">>),
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