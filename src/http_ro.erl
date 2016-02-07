-module(http_ro).
-behaviour(cowboy_http_handler).

-export([init/3, handle/2, terminate/3]).

init(_, Req, _Opts) -> {ok, Req, []}.

handle(Req, State) ->
	{Method, _} = cowboy_req:method(Req),
	case Method of
		<<"OPTIONS">> ->
			{ok, cmon:set_cors(Req), State};
		_ ->
			{Code, Type, Body} = handle_req(Method, Req),
			{ok, cmon:set_response(Req, Code, Type, Body), State}
	end.

terminate(_Reason, _Req, _State) -> ok.

handle_req(<<"GET">>, Req) ->
	{Uuid, _} = cowboy_req:binding(uuid, Req),
	{200, <<"text/plain">>, cmon:uuid(db_ro:mk_alias(cmon:check_uuid(Uuid)))};

handle_req(Method, _Req) ->
	{405, <<"text/plain">>, <<"not supported method ", Method/binary>> }.