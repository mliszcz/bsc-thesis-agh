%% @author Michal Liszcz
%% @doc Client API module for Storage Server

-module(storage).
-include("shared.hrl").
-define(SERVER, ?DIST_SERVER).

-export([
	create/6,
	read/5,
	update/6,
	delete/5,
	list/5,
	find/5
	]).


create(Node, User, Owner, Path, Hmac, Data) ->
	dist_call(Node, {create, User, Owner, Path, Hmac, Data}).

read(Node, User, Owner, Path, Hmac) ->
	dist_call(Node, {read, User, Owner, Path, Hmac, none}).

update(Node, User, Owner, Path, Hmac, Data) ->
	dist_call(Node, {update, User, Owner, Path, Hmac, Data}).

delete(Node, User, Owner, Path, Hmac) ->
	dist_call(Node, {delete, User, Owner, Path, Hmac, none}).

list(Node, User, Owner, Path, Hmac) ->
	dist_call(Node, {list, User, Owner, Path, Hmac, none}).

find(Node, User, Owner, Path, Hmac) ->
	dist_call(Node, {find, User, Owner, Path, Hmac, none}).


dist_call(Node, {Type, User, Owner, Path, Hmac, Data}) ->
	try gen_server:call({?SERVER, Node}, {request,
			#request{	type = Type,
						user = User,
						addr = {Owner, Path},
						data = Data,
						hmac = Hmac
					}
		}, ?TIMEOUT)
	catch
		_:{timeout, _} -> {error, timeout};
		_:{{nodedown, _}, _} -> {error, nodedown};
		_:_	-> {error, unknown}
	end.
