-module(evh_uuid).
-export([init/1, handle_event/2, terminate/2]).

-record(state, {pid, uuid}).

init([Pid, Uuid]) ->
	{ok, #state{pid=Pid, uuid=Uuid}}.

handle_event({Action, Uuid, Key}, S=#state{uuid=Uuid, pid=Pid}) ->
	Pid ! {message, Action, Key},
	{ok, S}.

terminate(_Args, _State) -> ok.