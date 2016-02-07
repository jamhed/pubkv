-module(send_file).
-behaviour(cowboy_http_handler).
-export([init/3, handle/2, terminate/3]).
-define(INFO(S,A), error_logger:info_msg(S, A)).

init(_, Req, Opts) ->
	?INFO("OPTS: ~p", [Opts]),
	{ok, Req, Opts}.

file_or_path([File], _) -> File;
file_or_path([], Req) ->
	{Path, _} = cowboy_req:path(Req),
	erlang:binary_to_list(Path).

handle(Req, Opts) ->
	File = file_or_path(Opts, Req),
	?INFO("FILE: ~p", [File]),
	F = fun (Socket, Transport) -> Transport:sendfile(Socket, "../../" ++ File) end,
	Req2 = cowboy_req:set_resp_header(<<"content-type">>, <<"text/plain">>, Req),
	Req3 = cowboy_req:set_resp_body_fun(F, Req2),
	{ok, Req4} = cowboy_req:reply(200, Req3),
	{ok, Req4, [File]}.

terminate(_Reason, _Req, _State) ->
	ok.
