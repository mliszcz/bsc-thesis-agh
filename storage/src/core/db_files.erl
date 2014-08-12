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
		 exists/2,
		 calculate_total_size/0
		]).

init(DatabaseLocation) ->

	{ok, Pid} = sqlite3:open(?DBNAME, [{file, DatabaseLocation}]),

	sqlite3:sql_exec_script(?DBNAME,
		"CREATE TABLE IF NOT EXISTS files (
			id 				INTEGER 	NOT NULL PRIMARY KEY AUTOINCREMENT,
			owner 			INTEGER 	NOT NULL,
			vpath 			TEXT 		NOT NULL,
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

	{rowid, NewId} = sqlite3:sql_exec(?DBNAME,
		"INSERT INTO files (owner, vpath, bytes, location, access_mode, create_time)
					VALUES (:owner, :vpath, :bytes, :locat, :acces, :ctime);", [
						{':owner', NewEntity#file.owner},
						{':vpath', NewEntity#file.vpath},
						{':bytes', NewEntity#file.bytes},
						{':locat', NewEntity#file.location},
						{':acces', NewEntity#file.access_mode},
						{':ctime', NewEntity#file.create_time}
		]),

	[{columns, ["id"]}, {rows, [{NewId}]}] = sqlite3:sql_exec(?DBNAME,
		"SELECT id FROM files WHERE owner = :owner AND vpath = :vpath;", [
			{':owner', Entity#file.owner},
			{':vpath', Entity#file.vpath}
		]),

	{ok, NewEntity#file {id=NewId}}.


update(#file{} = Entity) ->

	sqlite3:sql_exec(?DBNAME,
		"UPDATE files SET
						owner 		= :owner,
						vpath 		= :vpath,
						bytes 		= :bytes,
						location 	= :locat,
						access_mode = :acces,
						create_time = :ctime
					WHERE id = :ident;", [
						{':owner', Entity#file.owner},
						{':vpath', Entity#file.vpath},
						{':bytes', Entity#file.bytes},
						{':locat', Entity#file.location},
						{':acces', Entity#file.access_mode},
						{':ctime', Entity#file.create_time},
						{':ident', Entity#file.id}
		]),

	{ok, Entity}.


delete(#file{} = Entity) ->
	sqlite3:sql_exec(?DBNAME, "DELETE FROM files WHERE id = :id;", [{':id', Entity#file.id}]).


select(Owner, VPath) ->

	case sqlite3:sql_exec(?DBNAME, 
		"SELECT id, owner, vpath, bytes, location, access_mode, create_time
				FROM files WHERE owner = :owner AND vpath = :vpath;", [
				{':owner', Owner},
				{':vpath', VPath}
				]) of

		[{columns, _}, {rows, [RowTuple]}] -> {ok, instantiate_file(RowTuple)};
		_ -> {error, not_found}
	end.


select_by_owner(Owner) ->
	
	case sqlite3:sql_exec(?DBNAME, 
		"SELECT id, owner, vpath, bytes, location, access_mode, create_time
				FROM files WHERE owner = :owner;", [{':owner', Owner}]) of

		[{columns, _}, {rows, ListOfRows}] -> {ok, [instantiate_file(Row) || Row <- ListOfRows]};
		_ -> {ok, []}
	end.


select_all() ->
	
	case sqlite3:sql_exec(?DBNAME, 
		"SELECT id, owner, vpath, bytes, location, access_mode, create_time FROM files;") of

		[{columns, _}, {rows, ListOfRows}] -> {ok, [instantiate_file(Row) || Row <- ListOfRows]};
		_ -> {ok, []}
	end.


exists(Owner, VPath) ->

	case sqlite3:sql_exec(?DBNAME, 
		"SELECT EXISTS (SELECT 1 FROM files WHERE owner = :owner AND vpath = :vpath LIMIT 1);", [
		{':owner', Owner},
		{':vpath', VPath}
		]) of

		[{columns, _}, {rows, [{1}]}] -> true;
		_ -> false
	end.


calculate_total_size() ->

	log:info("QUERYING SIZE"),
	log:info("RESULT ~p", [sqlite3:sql_exec(?DBNAME, "SELECT sum(bytes) FROM files;")]),

	case sqlite3:sql_exec(?DBNAME, "SELECT sum(bytes) FROM files;") of
		[{columns, _}, {rows, [{TotalSize}]}] -> TotalSize;
		_ -> 0
	end.

%% ====================================================================
%% Internal functions
%% ====================================================================

instantiate_file({Id, Owner, VPath, Bytes, Locat, AccMode, CreatTime}) ->
	#file{	id 			= Id,
			owner 		= Owner,
			vpath 		= VPath,
			bytes 		= Bytes,
			location 	= Locat,
			access_mode = AccMode,
			create_time = CreatTime
			}.
