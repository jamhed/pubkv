-module(facebook).
-export([auth/2]).

auth(Fbid, Token) when is_binary(Fbid), is_binary(Token) ->
	Base = <<"https://graph.facebook.com/me?access_token=">>,
	{ok, {_Status, _Header, Body}} = httpc:request(binary_to_list(<<Base/binary, Token/binary>>)),
	{Props} = jiffy:decode(Body),
	case proplists:get_value(<<"id">>, Props) of
		Fbid -> ok;
		_ -> no_match
	end.
