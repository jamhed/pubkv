-module(pubkv_app).
-behaviour(application).

-export([start/2]).
-export([stop/1]).

start(_Type, _Args) ->
	db_mnesia:setup_mnesia(),
	Dispatch = cowboy_router:compile([
		{'_', [
			{"/", send_file, ["README.md"]},
			{"/uuid", http_uuid, []},
			{"/key/:uuid/[...]", http_key, []},
			{"/key/:uuid", http_key, []},
			{"/skey/:uuid/[...]", http_key, [sha256]},
			{"/skey/:uuid", http_key, [sha256]},
			{"/alias/:uuid", http_ro, []},
			{'_', send_file, []}
		]}
	]),
	{ok, _} =
		cowboy:start_http(my_http_listener, 100,
			[{port, 10080}],
			[{env, [{dispatch, Dispatch}]}]
		),
	pubkv_sup:start_link().

stop(_State) ->
	ok.
