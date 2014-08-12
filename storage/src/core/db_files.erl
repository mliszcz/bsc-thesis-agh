%% @author Michal Liszcz
%% @doc DAO for file entities (SQL database)

-module(db_files).
-include("shared.hrl").

-define(DBNAME, db_files_srv).

%% ====================================================================
%% API functions
%% ====================================================================
-export([init/1,
		 deinit/0,
		 create/1,
		 update/1,
		 delete/1,
		 select/2,
		 select_by_owner/1,
		 select_all/0,
		 calculate_total_size/0
		]).

init(DatabaseLocation) ->

	{ok, Pid} = sqlite3:open(?DBNAME, [{file, DatabaseLocation}]),

	sqlite3:sql_exec_script(?DBNAME,
		"CREATE TABLE IF NOT EXISTS files (
			id 				INTEGER 	NOT NULL PRIMARY KEY AUTOINCREMENT,
			vpath 			TEXT 		NOT NULL,
			owner 			INTEGER 	NOT NULL,
			bytes 			INTEGER 	NOT NULL,
			location 		TEXT 		NOT NULL,
			access_mode 	INTEGER 	NOT NULL,
			create_time 	INTEGER 	NOT NULL,

			UNIQUE (vpath, owner)
		);").


deinit() ->
	sqlite3:close(?DBNAME).


create(#file{} = Entity) ->

	NewEntity = Entity#file {
		location = uuid:generate(),
		create_time = util:timestamp()
		},

	sqlite3:sql_exec(?DBNAME,
		"INSERT INTO files (vpath, owner, bytes, location, access_mode, create_time)
					VALUES (:vpath, :owner, :bytes, :locat, :acces, :ctime);", [
						{':vpath', Entity#file.vpath},
						{':owner', Entity#file.owner},
						{':bytes', Entity#file.bytes},
						{':locat', Entity#file.location},
						{':acces', Entity#file.access_mode},
						{':ctime', Entity#file.create_time}
		]),

	[{columns, ["id"]}, {rows, [{NewId}]}] = sqlite3:sql_exec(?DBNAME,
		"SELECT id FROM files WHERE vpath = :vpath AND owner = :owner;", [
			{':vpath', Entity#file.vpath},
			{':owner', Entity#file.owner},
		]),

	NewEntity#file {id=NewId}.


update(#file{} = Entity) ->

	sqlite3:sql_exec(?DBNAME,
		"UPDATE files SET
						vpath 		= :vpath,
						owner 		= :owner,
						bytes 		= :bytes,
						location 	= :locat,
						access_mode = :acces,
						create_time = :ctime
					WHERE id = :ident;", [
						{':vpath', Entity#file.vpath},
						{':owner', Entity#file.owner},
						{':bytes', Entity#file.bytes},
						{':locat', Entity#file.location},
						{':acces', Entity#file.access_mode},
						{':ctime', Entity#file.create_time},
						{':ident', Entity#file.id}
		]),

	Entity.


delete(#filedesc{} = File) ->
	sqlite3:sql_exec(?DBNAME, "DELETE FROM files WHERE id = :id;", [{':id', Entity#file.id}]).


select(VPath, Owner) ->

	case sqlite3:sql_exec(?DBNAME, 
		"SELECT id, vpath, owner, bytes, location, access_mode, create_time
				FROM files WHERE vpath = :vpath AND owner = :owner;", [
				{':vpath', VPath},
				{':owner', Owner}
				]) of

		[{columns, _}, {rows, [RowTuple]}] -> {ok, instantiate_file(RowTuple)};
		_ -> {error, not_found}
	end.


select_by_owner(Owner) ->
	
	case sqlite3:sql_exec(?DBNAME, 
		"SELECT id, vpath, owner, bytes, location, access_mode, create_time
				FROM files WHERE owner = :owner;", [{':owner', Owner}]) of

		[{columns, _}, {rows, ListOfRows}] -> {ok, [instantiate_file(Row) || Row <- ListOfRows]};
		_ -> {ok, []}
	end.


select_all() ->
	
	case sqlite3:sql_exec(?DBNAME, 
		"SELECT id, vpath, owner, bytes, location, access_mode, create_time FROM files;") of

		[{columns, _}, {rows, ListOfRows}] -> {ok, [instantiate_file(Row) || Row <- ListOfRows]};
		_ -> {ok, []}
	end.


calculate_total_size() ->

	case sqlite3:sql_exec(?DBNAME, "SELECT sum(bytes) FROM files;") of
		[{columns, _}, {rows, [{TotalSize}]}] -> TotalSize;
		_ -> 0
	end

%% ====================================================================
%% Internal functions
%% ====================================================================

instantiate_file({Id, VPath, Owner, Bytes, Locat, AccMode, CreatTime}) ->
	#file{	id 			= Id,
			vpath 		= VPath,
			owner 		= Owner,
			bytes 		= Bytes,
			location 	= Locat,
			access_mode = AccMode,
			create_time = CreatTime
			}.
