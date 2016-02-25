-module(http_sub).
-behaviour(cowboy_loop_handler).
-export([init/3, info/3, terminate/3]).
-include("db.hrl").

-record(state, {uuid}).

init({_TransportName, _ProtocolName}, Req, []) ->
	Headers = [{<<"content-type">>, <<"text/event-stream">>}],
	{ok, Req2} = cowboy_req:chunked_reply(200, Headers, Req),
	Uuid = cmon:get_uuid(Req2),
	gen_event:add_handler(evh_uuid, {evh_uuid, Uuid}, [self(), Uuid]),
	{loop, Req2, #state{uuid=Uuid}}.

info({message, Action, {hashed, _Key}}, Req, State) ->
	ok = cowboy_req:chunk(["hashed", " ", erlang:atom_to_binary(Action, utf8), "\n\n"], Req),
	{loop, Req, State};

info({message, Action, Key}, Req, State) ->
	ok = cowboy_req:chunk([erlang:atom_to_binary(Action, utf8), " ", Key, "\n\n"], Req),
	{loop, Req, State}.

terminate(_Reason, _Req, #state{uuid=Uuid}) ->
	gen_event:delete_handler(evh_uuid, {evh_uuid, Uuid}, []),
	ok.