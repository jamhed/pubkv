-module(http_key).
-behaviour(cowboy_http_handler).
-export([init/3, handle/2, terminate/3]).
-include("db.hrl").

-record(state, {hash=none}).

init(_, Req, [Hash]) -> {ok, Req, #state{hash=Hash}};
init(_, Req, []) -> {ok, Req, #state{}}.

handle(Req, State) ->
	{Method, _} = cowboy_req:method(Req),
	case Method of
		<<"OPTIONS">> ->
			{ok, cmon:set_cors(Req), State};
		_ ->
			{Code, Type, Body} = wrap_handle_req(Method, Req, State),
			{ok, cmon:set_response(Req, Code, Type, Body), State}
	end.

wrap_handle_req(Method, Req, State) ->
	try handle_req(Method, Req, State) of
		Re -> Re
	catch
		C:E ->
			?INFO("error ~p ~p ~p", [C, E, erlang:get_stacktrace()]),
			cmon:wrap_response(error)
	end.

terminate(_Reason, _Req, _State) -> ok.

skey(_, _, <<"">>) -> undefined;
skey(none, _Uuid, Key) -> Key;
skey(_, _Uuid, undefined) -> undefined;
skey(sha256, Uuid, Key) when is_binary(Uuid), is_binary(Key) -> {hashed, crypto:hash(sha256, <<Uuid/binary, Key/binary>>)};
skey(sha256, Uuid, Key) -> ?INFO("uuid:~p key:~p", [Uuid, Key]),  exit(bad_key_type).

handle_req(<<"GET">>, Req, #state{hash=HashType}) ->
	{Uuid, Key} = cmon:get_uuid_and_key(Req),
	{Type, _} = cowboy_req:qs_val(<<"data">>, Req, <<"">>),
	Re = case {Key, Type} of
		{<<"">>, <<"hash">>} ->
			db:get_k_as_hash(translate_ro(Uuid));
		{<<"">>, _} ->
			db:get_k_as_list(translate_ro(Uuid));
		{_, _} -> 
			db:get_kv(translate_ro(Uuid), skey(HashType, Uuid, Key))
	end,
	cmon:wrap_key_response(Re);

handle_req(<<"DELETE">>, Req, #state{hash=Hash}) ->
	{Uuid, Key} = cmon:get_uuid_and_key(Req),
	cmon:wrap_response(db:delete_k(Uuid, skey(Hash, Uuid, Key)));

handle_req(<<"PUT">>, Req, #state{hash=Hash}) ->
	{Uuid, Key} = cmon:get_uuid_and_key(Req),
	{Type, SubType} = cmon:get_content_type(Req),
	{ok, Data, _} = cowboy_req:body(Req),
	Re = db:set_kv(Uuid, skey(Hash, Uuid, Key), <<Type/binary,"/",SubType/binary>>, Data, cmon:get_ttl(Req)),
	cmon:wrap_response(Re);

handle_req(Method, _Req, _State) ->
	{405, <<"text/plain">>, <<"not supported method ", Method/binary>> }.

translate_ro(Uuid) -> translate_ro(db_ro:translate(Uuid), Uuid).
translate_ro([Uuid], _) -> Uuid;
translate_ro([], Origin) -> Origin. 