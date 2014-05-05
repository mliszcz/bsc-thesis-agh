%% @author Michal
%% @doc Useful stuff


-module(util).

%% ====================================================================
%% API functions
%% ====================================================================
-export([timestamp/0]).

%% @def Current timestamp in milliseconds 
timestamp() ->
	{Mega,Sec,Micro} = erlang:now(),
	round(((Mega*1000000+Sec)*1000000+Micro)/1000).


%% ====================================================================
%% Internal functions
%% ====================================================================
