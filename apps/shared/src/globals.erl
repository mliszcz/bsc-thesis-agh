%% @author Michal
%% @doc Set of per-node global variables

-module(globals).
-define(GLOBALS, globalVars).

%% ====================================================================
%% API functions
%% ====================================================================
-export([init/0, deinit/0, get/1, set/2]).	%% @deprecated
-export([init/1, deinit/1, get/2, set/3]).

init(TableName) ->
	ets:new(TableName, [named_table, public, {heir, whereis(init), nothing} ]).

init() ->
	init(?GLOBALS).

deinit(TableName) ->
	ets:delete(TableName).

deinit() ->
	deinit(?GLOBALS).

get(TableName, Name) ->
	case ets:lookup(TableName, Name) of
		[{ Name, Var }] -> Var;
		[] -> undefined
	end.

get(Name) ->
	get(?GLOBALS, Name).

set(TableName, Name, Var) ->
	ets:insert(TableName, {Name, Var}),
	ok.

set(Name, Var) ->
	set(?GLOBALS, Name, Var).
