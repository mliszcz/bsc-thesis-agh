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

	case db_files:exists(VPath, UserId) of

		true ->

			log:warn("file exists!"),
			{error, file_exists};

		false ->

			File = #file{	vpath 		= VPath,
							owner 		= UserId,
							bytes 		= byte_size(PData),
							access_mode = 0
						},

			{ok, #file{bytes=Size, location=Location}} = db_files:create(File),

			globals:set(fill, globals:get(fill)+Size),
			globals:set(reserv, globals:get(reserv)-Size),
		
			files:write(Location, PData),
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

	case db_files:select(VPath, UserId) of

		{error,	_} ->

			log:warn("file not exists"),
			{error, not_found};

		{ok,	File} ->

			NewSize = byte_size(Data),
			globals:set(fill, globals:get(fill)+NewSize),
			globals:set(fill, globals:get(fill)-File#file.bytes),
			db_files:update(File#file{bytes=NewSize}),

			files:write(File#file.location, Data),
			{ok, updated}
	end;


handle_req(
	#request{
		type=read,
		user=UserId,
		path=VPath
	}) ->
	log:info("read ~s", [VPath]),
	case db_files:select(VPath, UserId) of
		{error, _} -> log:warn("file not exists"), {error, not_found};	
		{ok, File} ->
			{ok, Data} = files:read(File#file.location),
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
	case db_files:select(VPath, UserId) of
		{error, _} -> log:warn("file not exists"), {error, not_found};	
		{ok, File} ->
			globals:set(fill, globals:get(fill)-File#file.bytes),
			db_files:delete(File),
			files:delete(File#file.location),
			{ok, deleted}
	end;

handle_req(
	#request{
		type=list,
		user=UserId
	}) ->
	log:info("list"),
	db_files:select_by_owner(UserId);

handle_req(
	#request{
		type=find,
		user=User,
		path=Path
	}) ->
	log:info("find ~s", [Path]),
	case db_files:exists(Path, User) of
		true -> {ok, node()};
		false -> {error, not_found}
	end.


%% ------------------------------------------------------------------
%% Internal Function Definitions
%% ------------------------------------------------------------------
