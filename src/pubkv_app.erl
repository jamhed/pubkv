-module(pubkv_app).
-behaviour(application).

-export([start/2]).
-export([stop/1]).

start(_Type, _Args) ->
   pubkv_http_handler:setup_mnesia(),
   Dispatch = cowboy_router:compile([
      {'_', [
         {"/uuid", pubkv_http_handler, []},
         {"/key/:uuid/:key", pubkv_http_handler, []}
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
