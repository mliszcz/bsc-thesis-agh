%% @author Michal Liszcz
%% @doc Storage Server node application

-module(storage_app).
-include("shared.hrl").
-behaviour(application).

%% Application callbacks
-export([start/2, stop/1]).


-ifdef(profile).
-define(PROF_START(F), fprof:trace(start, F)).
-define(PROF_STOP(F), {fprof:trace(stop), fprof:profile(file, F)}).
-define(PROF_ANALYSE(F), fprof:analyse([{dest, F}, {cols, 120}])).
-else.
-define(PROF_START(F), true).
-define(PROF_STOP(F), true).
-define(PROF_ANALYSE(F), true).
-endif.



%% ===================================================================
%% Application callbacks
%% ===================================================================

start(_StartType, [ConfigFilePath | _OtherArgs]) ->

	case file:consult(ConfigFilePath) of
		{ok, Config} ->

			?PROF_START("fprof.trace"),

			globals:init(),	% @FIXME remove globals
			dict:fold(fun(K, V, _) -> util:set_env(K, V) end, 0, dict:from_list(Config)),
			storage_sup:start_link();

		{error, _} ->
			{error, config_not_found}
	end.


stop(_State) ->
	?LOG_INFO("application stopped!"),
	globals:deinit(),

	?PROF_STOP("fprof.trace"),
	?PROF_ANALYSE("fprof.analysis"),

	ok.
