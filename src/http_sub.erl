-module(http_sub).
-behaviour(cowboy_loop_handler).
-export([init/3, info/3, terminate/3]).
-include("db.hrl").

-record(state, {}).

init({_TransportName, _ProtocolName}, Req, []) ->
	Headers = [{<<"content-type">>, <<"text/event-stream">>}],
	{ok, Req2} = cowboy_req:chunked_reply(200, Headers, Req),
	gen_event:add_handler(evh_uuid, evh_uuid, [self(), cmon:get_uuid(Req2)]),
	{loop, Req2, #state{}}.

info({message, Action, Key}, Req, State) ->
	?INFO("action: ~p key:~p~n", [Action, Key]),
	ok = cowboy_req:chunk([erlang:atom_to_binary(Action, utf8), " ", Key, "\n\n"], Req),
	{loop, Req, State}.

terminate(_Reason, _Req, _State) -> ok.