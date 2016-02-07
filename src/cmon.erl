-module(cmon).
-export([uuid/0, uuid/1, check_uuid/1, set_response/4]).

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