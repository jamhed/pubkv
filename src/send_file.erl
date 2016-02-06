-module(send_file).
-behaviour(cowboy_http_handler).
-export([init/3, handle/2, terminate/3]).
-define(INFO(S,A), error_logger:info_msg(S, A)).

init(_, Req, _Opts) ->
	{ok, Req, []}.

handle(Req, State) ->

	F = fun (Socket, Transport) -> Transport:sendfile(Socket, "../../README.md") end,
	Req2 = cowboy_req:set_resp_header(<<"content-type">>, <<"text/plain">>, Req),
	Req3 = cowboy_req:set_resp_body_fun(F, Req2),
	{ok, Req4} = cowboy_req:reply(200, Req3),
	{ok, Req4, State}.

terminate(_Reason, _Req, _State) ->
	ok.
