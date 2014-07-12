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
	request_list/2
	]).

request_create(Node, Path, Data) ->
	gen_server:call({?SERVER, Node}, {request,
		#request{	type = create,
					user = "user01",
					path = Path,
					data = Data
				}
		}).

request_read(Node, Path) ->
	gen_server:call({?SERVER, Node}, {request,
		#request{	type = read,
					user = "user01",
					path = Path
				}
		}).

request_update(Node, Path, Data) ->
	gen_server:call({?SERVER, Node}, {request,
		#request{	type = update,
					user = "user01",
					path = Path,
					data = Data
				}
		}).

request_delete(Node, Path) ->
	gen_server:call({?SERVER, Node}, {request,
			#request{	type = delete,
						user = "user01",
						path = Path
					}
		}).

request_list(Node, Path) ->
	gen_server:call({?SERVER, Node}, {request,
			#request{	type = list,
						user = "user01",
						path = Path
					}
		}).
