-module(db_ro).
-export([mk_alias/1, translate/1, setup_mnesia/1]).
-include("db.hrl").

setup_mnesia(Nodes) ->
	mnesia:create_table(alias, [
		{disc_copies, Nodes},
		{attributes, record_info(fields, alias)}
	]).

mk_alias(false) -> false;
mk_alias(Uuid) when is_binary(Uuid) ->
	Alias = uuid:get_v4(),
	F = fun() ->
		mnesia:write(#alias{from=Alias, to=Uuid})
	end,
	mnesia:transaction(F),
	Alias.

translate(false) -> false;
translate(Uuid) when is_binary(Uuid) ->
	F = fun() ->
		mnesia:select(alias, [ {#alias{from=Uuid, to='$1'}, [], ['$1']} ])
	end,
	{atomic, V} = mnesia:transaction(F),
	V.