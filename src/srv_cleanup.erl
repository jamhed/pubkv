-module(srv_cleanup).
-behaviour(gen_server).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).
-export([start_link/0]).
-include("db.hrl").

-define(CHECK_TIMEOUT, 1000).

-record(state, {}).

start_link() ->
	gen_server:start_link(?MODULE, [], []).

self_notify() ->
	erlang:send_after(?CHECK_TIMEOUT, self(), {timeout}).

delete_obsolete_records() ->
	lists:foreach(fun(K) -> db:delete_k(K) end, db:get_obsolete()).

init([]) ->
	self_notify(),
	{ok, #state{}}.

handle_info({timeout}, S=#state{}) ->
	delete_obsolete_records(),
	self_notify(),
	{noreply, S};

handle_info(_Info, S=#state{}) -> {noreply, S}.

handle_cast(_Msg, S=#state{}) -> {noreply, S}.
handle_call(_Request, _From, S=#state{}) -> {reply, ok, S}.
terminate(_Reason, _S) -> ok.
code_change(_OldVsn, S=#state{}, _Extra) -> {ok, S}.
