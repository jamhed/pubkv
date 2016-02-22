-record(alias, {from, to}).
-record(store, {
	id,
	uuid,
	stamp,
	ttl = 0,
	key,
	type,
	value
}).

-define(INFO(S,A), error_logger:info_msg(S, A)).