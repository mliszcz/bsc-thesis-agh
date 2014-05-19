%% @author Michal
%% @doc Event logger

-module(log).

%% ====================================================================
%% API functions
%% ====================================================================
-export([info/1, info/2, warn/1, warn/2, error/1, error/2]).

info(Msg) -> info(Msg, []).
warn(Msg) -> warn(Msg, []).
error(Msg) -> ?MODULE:error(Msg, []).

info(Msg, Data) ->
	case should_log(info) of
		true -> error_logger:info_msg(Msg, Data);
		_ -> none
	end.

warn(Msg, Data) ->
	case should_log(warn) of
		true -> error_logger:warning_msg(Msg, Data);
		_ -> none
	end.

error(Msg, Data) ->
	case should_log(error) of
		true -> error_logger:error_msg(Msg, Data);
		_ -> none
	end.

should_log(Level) ->
	case {Level, util:get_env(app_log_level)} of
		{_, info}		-> true;
		{info, warn}	-> false;
		{_, warn}		-> true;
		{error, error}	-> true;
		{_, error}		-> false;
		_				-> true
	end.