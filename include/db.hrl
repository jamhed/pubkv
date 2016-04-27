-record(alias, {from, to}).
-record(store, {
	id,
	uuid,
	stamp = util:now_to_sec(),
	ttl = 0,
	key,
	type,
	value
}).

-record(sha, {
	id,
	stamp = util:now_to_sec(),
	ttl = 0,
	type,
	value
}).

-record(user, {
	id,
	email,
	facebook_id,
	password
}).

-define(INFO(S,A), error_logger:info_msg(S, A)).

