-module(db_mnesia).
-export([setup_mnesia/0]).

get_nodes() -> get_nodes(nodes()).

get_nodes([]) -> [node()];
get_nodes(Nodes) -> Nodes.

setup_mnesia() ->
   Master = application:get_env(pubkv,type),
   error_logger:info_msg("Type:~p~n", [Master]),
   connect_nodes(Master),
   setup_mnesia(Master).

setup_mnesia({ok, master}) ->
	Nodes = get_nodes(),
	mnesia:stop(),
	mnesia:create_schema(Nodes),
	mnesia:start(),
	db:setup_mnesia(Nodes),
	db_ro:setup_mnesia(Nodes);
setup_mnesia(_) -> 
	mnesia:start().

get_slaves() ->
   get_slaves(application:get_env(pubkv,slaves)).
get_slaves({ok,Nodes}) when is_list(Nodes) ->
   Nodes;
get_slaves(_) -> [].

connect_nodes({ok, master}) ->
   Slaves = get_slaves(),
   Re = [ net_adm:ping(Node) || Node <- Slaves ],
   error_logger:info_msg("Slaves: ~p~n", [lists:zip(Slaves, Re)]);
connect_nodes(_) -> skip.
