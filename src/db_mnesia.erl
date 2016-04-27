-module(db_mnesia).
-export([setup_mnesia/0]).
-include("db.hrl").

tables() ->
	[
		{store, [uuid], record_info(fields, store)},
		{alias, [], record_info(fields, alias)},
		{sha, [], record_info(fields, sha)},
		{user, [facebook_id, email], record_info(fields, user)}
	].

create_tables() -> create_tables(tables()).
create_tables([]) -> ok;
create_tables([ {Table, Index, Fields} | Tables ]) ->
	mnesia:create_table(Table, [ {disc_copies, [node()]}, {index, Index}, {attributes, Fields} ]),
	create_tables(Tables).

setup_mnesia() ->
	Master = application:get_env(pubkv,type),
	connect_nodes(Master),
	setup_mnesia(Master).

setup_mnesia({ok, master}) ->
	mnesia:change_table_copy_type(schema, node(), disc_copies),
	mnesia:change_config(extra_db_nodes, nodes()),
	create_tables(),
	mnesia:wait_for_tables([store, alias], 5000),
	lists:foreach(fun(Node) ->
		mnesia:change_table_copy_type(schema, Node, disc_copies),
		lists:foreach(fun({Table, _, _}) ->
			mnesia:add_table_copy(Table, Node, disc_copies)
		end, tables())
	end, nodes());
setup_mnesia(_) -> skip.

get_slaves() ->
	get_slaves(application:get_env(pubkv,slaves)).
get_slaves({ok, Nodes}) when is_list(Nodes) ->
	Nodes;
get_slaves(_) -> [].

connect_nodes({ok, master}) ->
	Slaves = get_slaves(),
	Re = [ net_adm:ping(Node) || Node <- Slaves ],
	error_logger:info_msg("Type: master, Slaves: ~p~n", [lists:zip(Slaves, Re)]);
connect_nodes(_) ->
	error_logger:info_msg("Type: slave", []),
	skip.
