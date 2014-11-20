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
		 create/1,
		 store/1,

		% statistics
		 get_write_ratio/1,
		 get_avg_size/1
		]).

init(DatabaseLocation) ->

	{ok, _Pid} = ?SQLITE3_CONNECT(?DBNAME, DatabaseLocation),

	sqlite3:sql_exec_script(?DBNAME,
		"CREATE TABLE IF NOT EXISTS actions (
			id 				TEXT 		NOT NULL PRIMARY KEY,
			user_id 		TEXT 		NOT NULL,
			file_id 		TEXT 		NOT NULL,
			weight 			INTEGER 	NOT NULL,
			action_time 	INTEGER 	NOT NULL,
			action_type 	TEXT 		NOT NULL
		);").


deinit() ->
	sqlite3:close(?DBNAME).


create(#action{} = Entity) ->

	NewEntity = Entity#action {id = ?UUID_SERVER:generate()},

	sqlite3:sql_exec(?DBNAME,
		<<"INSERT INTO actions (id, user_id, file_id, weight, action_time, action_type)
					VALUES (:uuid, :u_id, :f_id, :wght, :time, :type);">>, [
						{':uuid',  NewEntity#action.id},
						{':u_id',  NewEntity#action.user_id},
						{':f_id',  NewEntity#action.file_id},
						{':wght',  NewEntity#action.weight},
						{':time',  NewEntity#action.action_time},
						{':type',  NewEntity#action.action_type}
		]),

	{ok, NewEntity}.


store(#request{type=Type, user=User, addr={Owner, Path}}) ->
	case db_files:select(Owner, Path) of
		{ok, File} ->
			create(#action{
				user_id = User,
				file_id = File#file.id,
				weight = File#file.bytes,
				action_time = util:timestamp(),
				action_type = atom_to_list(Type)
				});
		_ ->
			pass
	end.


get_write_ratio(Name) ->

	%TODO file sizes shall be taken into consideration

	WriteNum = case sqlite3:sql_exec(?DBNAME,
		<<"SELECT count(*) from actions
					WHERE user_id = :uid AND
					(action_type = 'create' OR action_type = 'update');">>, [
					{':uid', Name}
		]) of
		[{columns, _}, {rows, [{Num}]}] -> Num;
		_ -> 0
	end,

	AllOpsNum = case sqlite3:sql_exec(?DBNAME,
		<<"SELECT count(*) from actions
					WHERE user_id = :uid;">>, [
					{':uid', Name}
		]) of
		[{columns, _}, {rows, [{Num2}]}] -> Num2;
		_ -> 1
	end,

	WriteNum/AllOpsNum.


get_avg_size(Name) ->
	case sqlite3:sql_exec(?DBNAME,
		<<"SELECT avg(weight) from actions
					WHERE user_id = :uid;">>, [
					{':uid', Name}
		]) of
		[{columns, _}, {rows, [{Num}]}] -> Num;
		_ -> 0
	end.

%% ====================================================================
%% Internal functions
%% ====================================================================
