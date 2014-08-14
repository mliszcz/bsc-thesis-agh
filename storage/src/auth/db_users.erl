%% @author Michal Liszcz
%% @doc DAO for user entities (SQL database)

-module(db_users).
-include("shared.hrl").

-define(DBNAME, db_users_srv).

%% ====================================================================
%% API functions
%% ====================================================================
-export([init/1,
		 deinit/0,
		 create/1,
		 update/1,
		 delete/1,
		 select/1,
		 select_by_name/1,
		 select_all/0,
		 exists/1,
		 exists_by_name/1
		]).

init(DatabaseLocation) ->

	{ok, _Pid} = sqlite3:open(?DBNAME, [{file, DatabaseLocation}]),

	sqlite3:sql_exec_script(?DBNAME,
		"CREATE TABLE IF NOT EXISTS users (
			id 				INTEGER 	NOT NULL PRIMARY KEY AUTOINCREMENT,
			name 			TEXT 		NOT NULL UNIQUE,
			secret 			TEXT 		NOT NULL,
			create_time 	INTEGER 	NOT NULL,
			storage_grant	INTEGER 	NOT NULL DEFAULT 0
		);"),

	case select_by_name("derp") of
		{ok, _} -> pass;
		{error, _} ->
			User = instantiate_entity({0, "derp", "82f63b78", 0, 0}),
			{ok, #user{name = "derp"}} = create(User)
	end.


deinit() ->
	sqlite3:close(?DBNAME).


create(#user{} = Entity) ->

	{rowid, NewId} = sqlite3:sql_exec(?DBNAME,
		"INSERT INTO users (name, secret, create_time, storage_grant)
					VALUES (:name, :secrt, :ctime, :grant);", [
						{':name',  Entity#user.name},
						{':secrt', Entity#user.secret},
						{':ctime', Entity#user.create_time},
						{':grant', Entity#user.storage_grant}
		]),

	{ok, Entity#user {id=NewId}}.


update(#user{} = Entity) ->

	sqlite3:sql_exec(?DBNAME,
		"UPDATE users SET
						name 			= :name,
						secret 			= :secrt,
						create_time 	= :ctime,
						storage_grant 	= :grant
					WHERE id = :ident;", [
						{':name',  Entity#user.name},
						{':secrt', Entity#user.secret},
						{':ctime', Entity#user.create_time},
						{':grant', Entity#user.storage_grant}
		]),

	{ok, Entity}.


delete(#user{} = Entity) ->
	sqlite3:sql_exec(?DBNAME, "DELETE FROM users WHERE id = :id;", [{':id', Entity#user.id}]).


select(Id) ->

	case sqlite3:sql_exec(?DBNAME, 
		"SELECT id, name, secret, create_time, storage_grant
				FROM users WHERE id = :id;", [{':id', Id}]) of

		[{columns, _}, {rows, [RowTuple]}] -> {ok, instantiate_entity(RowTuple)};
		_ -> {error, not_found}
	end.


select_by_name(Name) ->

	case sqlite3:sql_exec(?DBNAME, 
		"SELECT id, name, secret, create_time, storage_grant
				FROM users WHERE name = :name;", [{':name', Name}]) of

		[{columns, _}, {rows, [RowTuple]}] -> {ok, instantiate_entity(RowTuple)};
		_ -> {error, not_found}
	end.


select_all() ->
	
	case sqlite3:sql_exec(?DBNAME, 
		"SELECT id, secret, create_time, storage_grant FROM users;") of

		[{columns, _}, {rows, ListOfRows}] -> {ok, [instantiate_entity(Row) || Row <- ListOfRows]};
		_ -> {ok, []}
	end.


exists(Id) ->

	case sqlite3:sql_exec(?DBNAME, 
		"SELECT EXISTS (SELECT 1 FROM users WHERE id = :id LIMIT 1);", [{':id', Id}]) of

		[{columns, _}, {rows, [{1}]}] -> true;
		_ -> false
	end.


exists_by_name(Name) ->

	case sqlite3:sql_exec(?DBNAME, 
		"SELECT EXISTS (SELECT 1 FROM users WHERE name = :name LIMIT 1);", [{':name', Name}]) of

		[{columns, _}, {rows, [{1}]}] -> true;
		_ -> false
	end.


%% ====================================================================
%% Internal functions
%% ====================================================================

instantiate_entity({Id, Name, Secret, CreatTime, StorGrant}) ->
	#user{	id 				= Id,
			name 			= Name,
			secret 			= Secret,
			create_time 	= CreatTime,
			storage_grant 	= StorGrant
			}.
