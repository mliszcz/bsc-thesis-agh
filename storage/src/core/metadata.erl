%% @author Michal
%% @doc Local files metadata storage (memory / disk)


-module(metadata).
-include("shared.hrl").

-define(DBNAME, metadata).
-define(TABNAME, files).

%% ====================================================================
%% API functions
%% ====================================================================
-export([init/1,
		 deinit/0,
		 dump/0,
		 create/1,
		 modify/1,
		 delete/1,
		 get/2,
		 get/1,
		 to_list/0
		]).

init(DatabaseLocation) ->
	{ok, Pid} = sqlite3:open(?DBNAME, [{file, DatabaseLocation}]),

	% TableInfo = [	{path, text, [primary_key]},
	% 				{owner, text, [primary_key]},
	% 				{size, integer, [not_null]},
	% 				{access, integer, [not_null]},
	% 				{time, time}
	% ]

	case sqlite3:table_info(?DBNAME, ?TABNAME) of
		table_does_not_exist ->
			% ok = sqlite3:create_table(metadata, files, TableInfo);
			Query = io_lib:format("CREATE TABLE ~p (
						path 			TEXT 		NOT NULL,
						user 			TEXT 		NOT NULL,
						size 			INTEGER 	NOT NULL,
						mode 			TEXT 		NOT NULL,
						last_access 	INTEGER 	NOT NULL,
						internal_id 	TEXT 		UNIQUE NOT NULL,

						PRIMARY KEY (path, user)
					);", [?TABNAME]),
			sqlite3:sql_exec_script(?DBNAME, Query);
		_ -> pass
	end.

deinit() ->
	sqlite3:close(?DBNAME).

% init(FileMetaLocation) ->
% 	ets:new(memDb, [named_table, public, { keypos, #filedesc.internal_id }]),
% 	dets:open_file(perDb, [
% 						   	{ file, FileMetaLocation },
% 						  	{ keypos, #filedesc.internal_id }
% 					]),
% 	dets:to_ets(perDb, memDb),
% 	ok.

% deinit() ->
% 	ets:delete(memDb),
% 	dets:close(perDb),
% 	ok.

create(#filedesc{} = File) ->
	InsFile = File#filedesc{ internal_id = uuid:generate() },
	modify(InsFile).

modify(#filedesc{} = File) ->
	Query = io_lib:format("INSERT OR REPLACE INTO ~p
							(path, user, size, mode, last_access, internal_id)
							VALUES (:path, :user, :size, :mode, :lacc, :uuid)", [?TABNAME]),
	Result = sqlite3:sql_exec(?DBNAME, Query, [
		{':path', File#filedesc.path},
		{':user', File#filedesc.user},
		{':size', File#filedesc.size},
		{':mode', "private"},
		{':lacc', 0},
		% {':lacc', File#filedesc.last_access},
		{':uuid', File#filedesc.internal_id}
	]),
	get(File#filedesc.user, File#filedesc.path).

% modify(#filedesc{} = File) ->
% 	ets:insert(memDb, File),
% 	dets:insert(perDb, File),
% 	{ ok, File }.

delete(#filedesc{} = File) ->
	sqlite3:delete(?DBNAME, ?TABNAME, {internal_id, File#filedesc.internal_id}).
	% Query = io_lib:format("DELETE FROM ~p WHERE internal_id = :uuid", [?TABNAME]),
	% 						(path, user, size, mode, last_access, internal_id)
	% 						VALUES (:path, :user, :size, :mode, :lacc, :uuid)", [?TABNAME]),
	% ets:delete(memDb, File#filedesc.internal_id),
	% dets:delete(perDb, File#filedesc.internal_id).

get(UserId, VPath) ->
	Query = io_lib:format("SELECT * FROM ~p WHERE path = :path AND user = :user", [?TABNAME]),
	case sqlite3:sql_exec(?DBNAME, Query, [
		{':path', VPath},
		{':user', UserId}
	]) of
		[{columns, Columns}, {rows, [{Path, User, Size, Mode, Lacc, Uuid}]}] ->
			{ok, #filedesc{	user = User,
						path = Path,
						size = Size,
						mode = Mode,
						last_access = Lacc,
						internal_id = Uuid
					}};
		_ -> { error, not_found }
	end.
	% case ets:match_object(memDb,
	% 		#filedesc{
	% 			user = UserId,
	% 			path = VPath,
	% 			_ = '_'
	% 		}) of
	% 	[File]	-> { ok, File };
	% 	[]		-> { error, not_found };
	% 	_		-> { error, too_many }
	% end.

get(UserId) ->
	{ok, []}.
	% {ok, ets:match_object(memDb, #filedesc{ user = UserId, _ = '_' })}.

to_list() ->
	[].
	% ets:tab2list(memDb).

dump() -> pass.
	% ets:foldl(fun(Elem, _Acc) ->
	% 				  Name = Elem#filedesc.path,
	% 				  Id = Elem#filedesc.internal_id,
	% 				  io:format("file ~s as ~s~n", [Name, Id]),
	% 				  _Acc
	% 		  end, [], memDb).

%% ====================================================================
%% Internal functions
%% ====================================================================
