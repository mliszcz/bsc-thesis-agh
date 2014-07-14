%% @author Michal Liszcz
%% @doc Storage Server node application

-module(storage_app).
-behaviour(application).

%% Application callbacks
-export([start/2, stop/1]).

%% ===================================================================
%% Application callbacks
%% ===================================================================

start(_StartType, [ConfigFilePath | _OtherArgs]) ->
	case file:consult(ConfigFilePath) of
		{ok, Config} ->
			globals:init(),	% @FIXME remove globals
			dict:fold(fun(K, V, _) -> util:set_env(K, V) end, 0, dict:from_list(Config)),
			storage_sup:start_link();
		{error, _} ->
			{error, config_not_found}
	end.

stop(_State) ->
	log:info("application stopped!"),
	globals:deinit(),
	ok.
