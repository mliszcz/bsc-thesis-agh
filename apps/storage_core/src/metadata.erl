%% @author Michal
%% @doc Local files metadata storage (memory / disk)


-module(metadata).
-include("shared.hrl").

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

init(FileMetaLocation) ->
	ets:new(memDb, [named_table, public, { keypos, #file.local_id }]),
	dets:open_file(perDb, [
						   	{ file, FileMetaLocation },
						  	{ keypos, #file.local_id }
					]),
	dets:to_ets(perDb, memDb),
	ok.

deinit() ->
	ets:delete(memDb),
	dets:close(perDb),
	ok.

create(#file{} = File) ->
	InsFile = File#file{ local_id = uuid:generate() },
	modify(InsFile).

modify(#file{} = File) ->
	ets:insert(memDb, File),
	dets:insert(perDb, File),
	{ ok, File }.

delete(#file{} = File) ->
	ets:delete(memDb, File#file.local_id),
	dets:delete(perDb, File#file.local_id).

get(UserId, VPath) ->
	case ets:match_object(memDb, #file{ owner_id = UserId,
										v_path = VPath,
										_ = '_'
									  }) of
		[File]	-> { ok, File };
		[]		-> { error, not_found };
		_		-> { error, too_many }
	end.

get(UserId) ->
	ets:match_object(memDb, #file{ owner_id = UserId, _ = '_' }).

to_list() ->
	ets:tab2list(memDb).

dump() ->
	ets:foldl(fun(Elem, _Acc) ->
					  Name = Elem#file.v_path,
					  Id = Elem#file.local_id,
					  io:format("file ~s as ~s~n", [Name, Id]),
					  _Acc
			  end, [], memDb).

%% ====================================================================
%% Internal functions
%% ====================================================================
