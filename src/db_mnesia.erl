-module(db_mnesia).
-export([setup_mnesia/0]).

setup_mnesia() ->
	Nodes = [node()],
	mnesia:stop(),
	mnesia:create_schema(Nodes),
	mnesia:start(),
	db:setup_mnesia(Nodes),
	db_ro:setup_mnesia(Nodes).