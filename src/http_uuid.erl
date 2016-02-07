-module(http_uuid).
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

handle_req(<<"GET">>, _Req) ->
	{200, <<"text/plain">>, cmon:uuid()};

handle_req(Method, _Req) ->
	{405, <<"text/plain">>, <<"not supported method ", Method/binary>> }.