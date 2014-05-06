%% @author Michal Liszcz
%% @doc Client API module for Storage Server

-module(storage_client_api).
-include("shared.hrl").
-define(SERVER, storage_core_srv).
% -define(HANDLERS, proc_handlers).

-export([request_create/2, request_read/1]).

request_create(V_path, RawData) ->
	gen_server:call({?SERVER, 'ds@michal-pc'},
		#request{	action	= create,
					user_id	= "user01",
					v_path	= V_path,
					options	= #create_opts{	data = RawData }
				}).

request_read(V_path) ->
	gen_server:call({?SERVER, 'ds@michal-pc'},
		#request{	action	= read,
					user_id	= "user01",
					v_path	= V_path
				}).
