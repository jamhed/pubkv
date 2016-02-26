-module(pubkv_app).
-behaviour(application).

-export([start/2]).
-export([stop/1]).

start(_Type, _Args) ->
	db_mnesia:setup_mnesia(),
	Dispatch = cowboy_router:compile([
		{'_', [
			{"/", cowboy_static, {priv_file, pubkv, "README.md", [{mimetypes, {<<"text">>, <<"plain">>, []}}]}},
			{"/uuid", http_uuid, []},
			{"/key/:uuid/[...]", http_key, []},
			{"/key/:uuid", http_key, []},
			{"/skey/:uuid/[...]", http_key, [sha256]},
			{"/skey/:uuid", http_key, [sha256]},
			{"/alias/:uuid", http_ro, []},
			{"/watch/:uuid", http_sub, []},
			{"/[...]", cowboy_static, {priv_dir, pubkv, ""}}
		]}
	]),
	{ok, _} =
		cowboy:start_http(my_http_listener, 100,
			[{port, 10080}],
			[{env, [{dispatch, Dispatch}]}]
		),
	gen_event:start({local, evh_uuid}),
	pubkv_sup:start_link().

stop(_State) ->
	ok.
