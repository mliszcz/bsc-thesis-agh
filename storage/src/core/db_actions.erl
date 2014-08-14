%% @author Michal Liszcz
%% @doc interface for user actions log (SQL database)

-module(db_actions).
-include("shared.hrl").

-define(DBNAME, db_actions_srv).

%% ====================================================================
%% API functions
%% ====================================================================
-export([init/1,
		 deinit/0,
		 create/1
		]).

init(DatabaseLocation) ->

	{ok, _Pid} = sqlite3:open(?DBNAME, [{file, DatabaseLocation}]),

	sqlite3:sql_exec_script(?DBNAME,
		"CREATE TABLE IF NOT EXISTS actions (
			id 				TEXT 		NOT NULL PRIMARY KEY,
			user_id 		TEXT 		NOT NULL,
			file_id 		TEXT 		NOT NULL,
			action_time 	INTEGER 	NOT NULL,
			action_type 	TEXT 		NOT NULL,
		);").


deinit() ->
	sqlite3:close(?DBNAME).


create(#action{} = Entity) ->

	NewEntity = Entity#action {id = ?UUID_SERVER:generate()},

	sqlite3:sql_exec(?DBNAME,
		"INSERT INTO actions (id, user_id, file_id, action_time, action_type)
					VALUES (:uuid, :u_id, :f_id, :time, :type);", [
						{':uuid',  NewEntity#action.id},
						{':u_id',  NewEntity#action.user_id},
						{':f_id',  NewEntity#action.file_id},
						{':time',  NewEntity#action.action_time},
						{':type',  NewEntity#action.action_type}
		]),

	{ok, NewEntity}.


%% ====================================================================
%% Internal functions
%% ====================================================================
