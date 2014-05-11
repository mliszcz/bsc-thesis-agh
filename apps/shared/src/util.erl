%% @author Michal
%% @doc Helper functions and other stuff that fits nowhere else

-module(util).

%% ====================================================================
%% API functions
%% ====================================================================
-export([timestamp/0, get_app/0, set_env/2, get_env/1]).

%% @def Current timestamp in milliseconds 
timestamp() ->
	{Mega,Sec,Micro} = erlang:now(),
	round(((Mega*1000000+Sec)*1000000+Micro)/1000).

get_app() ->
	case application:get_application() of
		{ok, App} -> App;
		_ -> undefined
	end.

set_env(Key, Val) ->
	application:set_env(get_app(), Key, Val).

get_env(Key) ->
	case application:get_env(Key) of
		{ok, Val} -> Val;
		_ -> undefined
	end.

