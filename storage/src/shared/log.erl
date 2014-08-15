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
	log_stdout(info, Msg, Data).

warn(Msg, Data) ->
	log_stdout(warn, Msg, Data).

error(Msg, Data) ->
	log_stdout(error, Msg, Data).

should_log(Level) ->
	case {Level, util:get_env(app_log_level)} of
		{_,		info }	-> true;
		{info,	warn }	-> false;
		{_,		warn }	-> true;
		{error,	error}	-> true;
		{_,		error}	-> false;
		_				-> true
	end.

backtrace(Nth) ->
	catch throw(away),
	try lists:nth(Nth, erlang:get_stacktrace()) of
		{Module, Fun, Arity, _} -> {Module, Fun, Arity}
	catch
		% error:{function_clause, _} -> {unknown, unknown, '?'} % WHY U NO WORK???
		_:_ -> {unknown, unknown, '?'}
	end.

log_stdout(Level, Msg, Data) ->
	case should_log(Level) of
		true ->
			{_, {H,M,S}} = erlang:localtime(),
			{Mod, Fun, Arity} = backtrace(3),
			io:format("[~p] ~2..0b:~2..0b:~2..0b (~p:~p/~p): ", [Level, H, M, S, Mod, Fun, Arity]),
			io:format(Msg++"~n", Data);
		_ -> none
	end.
