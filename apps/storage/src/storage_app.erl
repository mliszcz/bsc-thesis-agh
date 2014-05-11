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
	{ok, Config} = file:consult(ConfigFilePath),
	dict:fold(fun(K, V, _) -> util:set_env(K, V) end, 0, dict:from_list(Config)),
	storage_sup:start_link().

stop(_State) ->
	ok.
