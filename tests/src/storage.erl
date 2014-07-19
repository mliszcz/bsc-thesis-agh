%% @author Michal Liszcz
%% @doc Client API module for Storage Server

-module(storage).
-include("shared.hrl").
-define(SERVER, ?DIST_SERVER).

-export([
	create/3,
	read/2,
	update/3,
	delete/2,
	list/2,
	find/2
	]).


create(Node, Path, Data) ->
	dist_call(Node, {create, "user01", Path, Data}).

read(Node, Path) ->
	dist_call(Node, {read, "user01", Path, none}).

update(Node, Path, Data) ->
	dist_call(Node, {update, "user01", Path, Data}).

delete(Node, Path) ->
	dist_call(Node, {delete, "user01", Path, none}).

list(Node, Path) ->
	dist_call(Node, {list, "user01", Path, none}).

find(Node, Path) ->
	dist_call(Node, {find, "user01", Path, none}).


dist_call(Node, {Type, User, Path, Data}) ->
	try gen_server:call({?SERVER, Node}, {request,
			#request{	type = Type,
						user = User,
						path = Path,
						data = Data
					}
		}, ?TIMEOUT)
	catch
		_:{timeout, _} -> {error, timeout};
		_:{{nodedown, _}, _} -> {error, nodedown};
		_:_	-> {error, unknown}
	end.
