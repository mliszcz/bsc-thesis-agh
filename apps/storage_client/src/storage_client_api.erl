%% @author Michal Liszcz
%% @doc Client API module for Storage Server

-module(storage_client_api).
%% -include("shared.hrl").
% -define(HANDLERS, proc_handlers).

-export([echo/0]).

echo() ->
	gen_server:call({storage_core_srv, 'ds@michal-pc'}, none, 1000).