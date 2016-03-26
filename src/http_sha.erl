-module(http_sha).
-behaviour(cowboy_http_handler).
-include("db.hrl").

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

get_key(Req) ->
	{[Key], _} = cowboy_req:path_info(Req),
	Key.

handle_req(<<"GET">>, Req) ->
	Key = get_key(Req),
	cmon:wrap_key_response(db_sha:get(Key));

handle_req(<<"PUT">>, Req) ->
	Key = get_key(Req),
	{ok, Value, _} = cowboy_req:body(Req),
	cmon:wrap_response(db_sha:set(Key, Value));

handle_req(<<"DELETE">>, Req) ->
	Key = get_key(Req),
	cmon:wrap_response(db_sha:delete(Key)).