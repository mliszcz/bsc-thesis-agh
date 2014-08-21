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

	% emysql:add_pool(?DBNAME, 100,
 %           "storage", "storage", "localhost", 3306,
 %           "storage", utf8),

	% emysql:execute(?DBNAME, "DROP TABLE IF EXISTS files;"),
	% emysql:execute(?DBNAME,
	% 	"CREATE TABLE IF NOT EXISTS files (
	% 		id 				VARCHAR(32) 	NOT NULL PRIMARY KEY,
	% 		owner 			VARCHAR(32) 	NOT NULL,
	% 		vpath 			VARCHAR(32) 	NOT NULL,
	% 		bytes 			INTEGER 		NOT NULL,
	% 		access_mode 	INTEGER 		NOT NULL,
	% 		create_time 	INTEGER 		NOT NULL,

	% 		UNIQUE INDEX (vpath, owner)
	% 	);").

	{ok, _Pid} = ?SQLITE3_CONNECT(?DBNAME, DatabaseLocation),

	sqlite3:sql_exec_script(?DBNAME,
		<<"CREATE TABLE IF NOT EXISTS files (
			id 				TEXT 		NOT NULL PRIMARY KEY,
			owner 			TEXT 		NOT NULL,
			vpath 			TEXT 		NOT NULL,
			bytes 			INTEGER 	NOT NULL,
			access_mode 	INTEGER 	NOT NULL,
			create_time 	INTEGER 	NOT NULL,

			UNIQUE (vpath, owner)
		);">>).


deinit() ->
	% emysql:remove_pool(?DBNAME).
	sqlite3:close(?DBNAME).


create(#file{} = Entity) ->

	NewEntity = Entity#file {
		id = ?UUID_SERVER:generate(),
		create_time = util:timestamp()
		},

	% emysql:execute(?DBNAME,
	% 	<<"INSERT INTO files (id, owner, vpath, bytes, access_mode, create_time)
	% 				VALUES (?, ?, ?, ?, ?, ?);">>, [
	% 					NewEntity#file.id,
	% 					NewEntity#file.owner,
	% 					NewEntity#file.vpath,
	% 					NewEntity#file.bytes,
	% 					NewEntity#file.access_mode,
	% 					NewEntity#file.create_time
	% 	], 1000),

	sqlite3:sql_exec(?DBNAME,
		<<"INSERT INTO files (id, owner, vpath, bytes, access_mode, create_time)
					VALUES (:ident, :owner, :vpath, :bytes, :acces, :ctime);">>, [
						{':ident', NewEntity#file.id},
						{':owner', NewEntity#file.owner},
						{':vpath', NewEntity#file.vpath},
						{':bytes', NewEntity#file.bytes},
						{':acces', NewEntity#file.access_mode},
						{':ctime', NewEntity#file.create_time}
		]),

	{ok, NewEntity}.


update(#file{} = Entity) ->

	% emysql:execute(?DBNAME,
	% 	"UPDATE files SET
	% 					owner 		= ?,
	% 					vpath 		= ?,
	% 					bytes 		= ?,
	% 					access_mode = ?,
	% 					create_time = ?
	% 				WHERE id = ?;", [
	% 					Entity#file.owner,
	% 					Entity#file.vpath,
	% 					Entity#file.bytes,
	% 					Entity#file.access_mode,
	% 					Entity#file.create_time,
	% 					Entity#file.id
	% 	]),

	sqlite3:sql_exec(?DBNAME,
		<<"UPDATE files SET
						owner 		= :owner,
						vpath 		= :vpath,
						bytes 		= :bytes,
						access_mode = :acces,
						create_time = :ctime
					WHERE id = :ident;">>, [
						{':owner', Entity#file.owner},
						{':vpath', Entity#file.vpath},
						{':bytes', Entity#file.bytes},
						{':acces', Entity#file.access_mode},
						{':ctime', Entity#file.create_time},
						{':ident', Entity#file.id}
		]),

	{ok, Entity}.


delete(#file{} = Entity) ->
	% emysql:execute(?DBNAME, "DELETE FROM files WHERE id = ?;", [Entity#file.id]).
	sqlite3:sql_exec(?DBNAME, <<"DELETE FROM files WHERE id = :id;">>, [{':id', Entity#file.id}]).


select(Owner, VPath) ->

	% case emysql:execute(?DBNAME, 
	% 	<<"SELECT id, owner, vpath, bytes, access_mode, create_time
	% 			FROM files WHERE owner = ? AND vpath = ?;">>, [
	% 			Owner,
	% 			VPath
	% 			]) of

	% 	Res ->
	% 		Rec = emysql_util:as_record(Res, file, record_info(fields, file)),
	% 		{ok, hd(Rec)};
	% 	_ -> {error, not_found}
	% end.

	case sqlite3:sql_exec(?DBNAME, 
		<<"SELECT id, owner, vpath, bytes, access_mode, create_time
				FROM files WHERE owner = :owner AND vpath = :vpath;">>, [
				{':owner', Owner},
				{':vpath', VPath}
				]) of

		[{columns, _}, {rows, [RowTuple]}] -> {ok, instantiate_file(RowTuple)};
		_ -> {error, not_found}
	end.


select_by_owner(Owner) ->
	
	case sqlite3:sql_exec(?DBNAME, 
		<<"SELECT id, owner, vpath, bytes, access_mode, create_time
				FROM files WHERE owner = :owner;">>, [{':owner', Owner}]) of

		[{columns, _}, {rows, ListOfRows}] -> {ok, [instantiate_file(Row) || Row <- ListOfRows]};
		_ -> {ok, []}
	end.

	% case emysql:execute(?DBNAME, 
	% 	"SELECT id, owner, vpath, bytes, access_mode, create_time
	% 			FROM files WHERE owner = :owner;", [{':owner', Owner}]) of

	% 	Res -> {ok, emysql_util:as_record(Res, file, record_info(fields, file))};
	% 	_ -> {ok, []}
	% end.


select_all() ->
	case sqlite3:sql_exec(?DBNAME, 
		<<"SELECT id, owner, vpath, bytes, access_mode, create_time FROM files;">>) of

		[{columns, _}, {rows, ListOfRows}] -> {ok, [instantiate_file(Row) || Row <- ListOfRows]};
		_ -> {ok, []}
	end.


exists(Owner, VPath) ->
	case sqlite3:sql_exec(?DBNAME, 
		<<"SELECT EXISTS (SELECT 1 FROM files WHERE owner = :owner AND vpath = :vpath LIMIT 1);">>, [
		{':owner', Owner},
		{':vpath', VPath}
		]) of

		[{columns, _}, {rows, [{1}]}] -> true;
		_ -> false
	end.


calculate_total_size() ->
	case sqlite3:sql_exec(?DBNAME, <<"SELECT sum(bytes) FROM files;">>) of
		[{columns, _}, {rows, [{TotalSize}]}] -> TotalSize;
		_ -> 0
	end.

%% ====================================================================
%% Internal functions
%% ====================================================================

instantiate_file({Id, Owner, VPath, Bytes, AccMode, CreatTime}) ->
	#file{	id 			= binary_to_list(Id),
			owner 		= binary_to_list(Owner),
			vpath 		= binary_to_list(VPath),
			bytes 		= Bytes,
			access_mode = AccMode,
			create_time = CreatTime
			}.
