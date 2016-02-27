-module(pubkv_sup).
-behaviour(supervisor).

-export([start_link/0]).
-export([init/1]).

start_link() ->
	supervisor:start_link({local, ?MODULE}, ?MODULE, []).

start_if_master() ->
   start_if_master(application:get_env(pubkv,type)).

start_if_master({ok, master}) ->
[ 
   #{id => srv_cleanup, start => {srv_cleanup, start_link, []}, restart => permanent, type => worker, modules => [srv_cleanup]}
];
start_if_master(_) -> [].

init(_Args) ->
	SupFlags = #{strategy => one_for_one, intensity => 1, period => 5},
	{ok, {SupFlags, start_if_master()}}.
