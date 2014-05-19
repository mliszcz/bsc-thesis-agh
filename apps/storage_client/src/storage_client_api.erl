%% @author Michal Liszcz
%% @doc Client API module for Storage Server

-module(storage_client_api).
-include("shared.hrl").
-define(SERVER, ?DIST_SERVER).

-export([request_create/3, request_read/2, request_list/2, request_put/4, request_get/2]).

request_create(Node, VPath, RawData) ->
	gen_server:call({?SERVER, Node},
		#request{	action	= create,
					user_id	= "user01",
					v_path	= VPath,
					options	= #create_opts{	data = RawData }
				}).

request_read(Node, VPath) ->
	gen_server:call({?SERVER, Node},
		#request{	action	= read,
					user_id	= "user01",
					v_path	= VPath
				}).

request_put(Node, VPath, RawData, NewVPath) ->
	gen_server:call({?SERVER, Node}, {request,
		#rreq{	action	= put,
				user_id	= "user01",
				v_path	= VPath,
				put_path = NewVPath,	% may be none
				put_data = RawData		% may be none
			}
		}).

request_get(Node, VPath) ->
	gen_server:call({?SERVER, Node}, {request,
		#rreq{	action	= get,
				user_id	= "user01",
				v_path	= VPath
			}
		}).

request_list(Node, _UserId) ->
	gen_server:call({?SERVER, Node}, {request,
		#rreq{	action	= lst,
				user_id	= "user01",
				v_path	= "/"
			}
		}).
