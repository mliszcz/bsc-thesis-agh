%% @author Michal Liszcz
%% @doc Core Server backend

-module(core).
-include("shared.hrl").

-export([ handle_req/1 ]).

handle_req(
	#request{
		type=create,
		user=UserId,
		path=VPath,
		data=PData
	}=_Request) ->
	log:info("create ~s", [VPath]),
	case metadata:get(UserId, VPath) of
		{ok,	_} ->
			log:warn("file exists!"),
			{error, file_exists};

		{error,	_} ->
			File = #filedesc {
				path = VPath,
				user = UserId,
				size = byte_size(PData),
				last_access	= util:timestamp()
				},
		
			{ok, #filedesc{internal_id=NewId, size=Size}} = metadata:create(File),
			globals:set(fill, globals:get(fill)+Size),
			globals:set(reserv, globals:get(reserv)-Size),
		
			files:write(NewId, PData),
			log:info("file ~s  created!", [VPath]),
			{ok, created}
	end;

handle_req(
	#request{
	type=update,
	user=UserId,
	path=VPath,
	data=Data
	}=_Request) ->
	log:info("update ~s", [VPath]),
	case metadata:get(UserId, VPath) of
		{error,	_} ->
			log:warn("file not exists"),
			{error, not_found};

		{ok,	File} ->
			metadata:modify(File#filedesc{last_access=util:timestamp()}),
			globals:set(fill, globals:get(fill)+byte_size(Data)),
			globals:set(fill, globals:get(fill)-File#filedesc.size),
			metadata:modify(File#filedesc{size=byte_size(Data)}),
			files:write(File#filedesc.internal_id, Data),
			{ok, updated}
	end;

handle_req(
	#request{
		type=read,
		user=UserId,
		path=VPath
	}) ->
	log:info("read ~s", [VPath]),
	case metadata:get(UserId, VPath) of
		{error, _} -> log:warn("file not exists"), {error, not_found};	
		{ok, File} ->
			metadata:modify(File#filedesc{last_access=util:timestamp()}),
			{ok, Data} = files:read(File#filedesc.internal_id),
			log:info("serving ~s, (~w bytes)", [VPath, byte_size(Data)]),
			{ok, Data}
	end;

handle_req(
	#request{
		type=delete,
		user=UserId,
		path=VPath
	}) ->
	log:info("delete ~s", [VPath]),
	case metadata:get(UserId, VPath) of
		{error, _} -> log:warn("file not exists"), {error, not_found};	
		{ok, File} ->
			globals:set(fill, globals:get(fill)-File#filedesc.size),
			metadata:delete(File),
			files:delete(File#filedesc.internal_id),
			{ok, deleted}
	end;

handle_req(
	#request{
		type=list,
		user=UserId
	}) ->
	log:info("list"),
	metadata:get(UserId).


%% ------------------------------------------------------------------
%% Internal Function Definitions
%% ------------------------------------------------------------------
