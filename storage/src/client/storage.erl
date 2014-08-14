%% @author Michal Liszcz
%% @doc Client API module for Storage Server

-module(storage).
-include("shared.hrl").
-define(SERVER, ?DIST_SERVER).

-export([
	create/5,
	read/4,
	update/5,
	delete/4,
	list/4,
	find/4
	]).


create(Node, User, Path, Hmac, Data) ->
	dist_call(Node, {create, User, Path, Hmac, Data}).

read(Node, User, Path, Hmac) ->
	dist_call(Node, {read, User, Path, Hmac, none}).

update(Node, User, Path, Hmac, Data) ->
	dist_call(Node, {update, User, Path, Hmac, Data}).

delete(Node, User, Path, Hmac) ->
	dist_call(Node, {delete, User, Path, Hmac, none}).

list(Node, User, Path, Hmac) ->
	dist_call(Node, {list, User, Path, Hmac, none}).

find(Node, User, Path, Hmac) ->
	dist_call(Node, {find, User, Path, Hmac, none}).


dist_call(Node, {Type, User, Path, Hmac, Data}) ->
	try gen_server:call({?SERVER, Node}, {request,
			#request{	type = Type,
						user = User,
						path = Path,
						data = Data,
						hmac = Hmac
					}
		}, ?TIMEOUT)
	catch
		_:{timeout, _} -> {error, timeout};
		_:{{nodedown, _}, _} -> {error, nodedown};
		_:_	-> {error, unknown}
	end.


% sign_request(#request{}=Request, Hmac) ->
% 	% TBD: use this one or call auth_srv({sign_request, ...})
% 	SignedRequest = Request#request{hmac = calculate_hmac(Request, Hmac)},
% 	SignedRequest.


% calculate_hmac(
% 	#request{
% 		type=Type,
% 		user=UserId,
% 		path=Path
% 	}, Hmac) ->
% 	crypto:hmac(sha, Hmac, list_to_binary([
% 		atom_to_list(Type),
% 		integer_to_list(UserId),
% 		Path
% 		])).
