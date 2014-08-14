%% @author Michal
%% @doc Helper functions and other stuff that fits nowhere else

-module(util).

%% ====================================================================
%% API functions
%% ====================================================================
-export([timestamp/0,
		 get_app/0,
		 set_env/2,
		 get_env/1,
		 term_to_binary_string/1,
		 binary_to_hex_string/1,
		 hex_string_to_binary/1]).

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

term_to_binary_string(Term) ->
	list_to_binary(lists:flatten(io_lib:format("~p", [Term]))).

binary_to_hex_string(Binary) when is_binary(Binary) ->
	lists:flatten(lists:map(
		fun(X) -> io_lib:format("~2.16.0b", [X]) end, 
	binary_to_list(Binary))).

hex_string_to_binary([]) -> << >>;
hex_string_to_binary([F,S|T]) ->
	Int = list_to_integer([F,S], 16),
	Rest = hex_string_to_binary(T),
	<< Int/integer, Rest/binary >>.
