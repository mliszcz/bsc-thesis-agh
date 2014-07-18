%% @author Michal Liszcz
%% @doc Client API module for Storage Server

-module(storage_client_api).
-include("shared.hrl").
-define(SERVER, ?DIST_SERVER).

-export([
	request_create/3,
	request_read/2,
	request_update/3,
	request_delete/2,
	request_list/2,
	request_find/2
	]).

request_create(Node, Path, Data) ->
	dist_call(Node, {create, "user01", Path, Data}).

request_read(Node, Path) ->
	dist_call(Node, {read, "user01", Path, none}).

request_update(Node, Path, Data) ->
	dist_call(Node, {update, "user01", Path, Data}).

request_delete(Node, Path) ->
	dist_call(Node, {delete, "user01", Path, none}).

request_list(Node, Path) ->
	dist_call(Node, {list, "user01", Path, none}).

request_find(Node, Path) ->
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
		exit:{timeout, _} -> {error, timeout};
		_:{{nodedown, _}, _} -> {error, nodedown};
		_:_	-> {error, unknown}
	end.
