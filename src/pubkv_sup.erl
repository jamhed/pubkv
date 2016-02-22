-module(pubkv_sup).
-behaviour(supervisor).

-export([start_link/0]).
-export([init/1]).

start_link() ->
	supervisor:start_link({local, ?MODULE}, ?MODULE, []).

init(_Args) ->
	SupFlags = #{strategy => one_for_one, intensity => 1, period => 5},
	SrvCleanup = 
		#{id => srv_cleanup, start => {srv_cleanup, start_link, []}, restart => permanent, type => worker, modules => [srv_cleanup]},
	{ok, {SupFlags, [SrvCleanup]}}.