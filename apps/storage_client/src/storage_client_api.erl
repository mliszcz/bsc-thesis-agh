%% @author Michal Liszcz
%% @doc Client API module for Storage Server

-module(storage_client_api).
-include("shared.hrl").
-define(SERVER, storage_core_srv).
% -define(HANDLERS, proc_handlers).

-export([echo/0, request/0]).

echo() ->
	gen_server:call({?SERVER, 'ds@michal-pc'}, none, 1000).

request() ->
	gen_server:call({?SERVER, 'ds@michal-pc'},
		#request{	action	= write,
					user_id	= michal,
					v_path	= "path/to/file",
					options	= none }
				).
