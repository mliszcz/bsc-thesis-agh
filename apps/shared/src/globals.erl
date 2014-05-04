%% @author Michal
%% @doc Set of per-node global variables


-module(globals).
-define(GLOBALS, globalVars).
%% ====================================================================
%% API functions
%% ====================================================================
-export([init/0, deinit/0, get/1, set/2]).

init() ->
	ets:new(?GLOBALS, [named_table, public, {heir, whereis(init), nothing} ]).

deinit() ->
	ets:delete(?GLOBALS).

get(Name) ->
	case ets:lookup(?GLOBALS, Name) of
		[{ Name, Var }] -> Var;
		[] -> undefined
	end.

set(Name, Var) ->
	ets:insert(?GLOBALS, {Name, Var}),
	ok.

%% ====================================================================
%% Internal functions
%% ====================================================================
