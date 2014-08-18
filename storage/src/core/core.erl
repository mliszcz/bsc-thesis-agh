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
	?LOG_INFO("create ~s", [VPath]),

	case db_files:exists(UserId, VPath) of

		true ->

			?LOG_WARN("file exists!"),
			{error, file_exists};

		false ->

			File = #file{	vpath 		= VPath,
							owner 		= UserId,
							bytes 		= byte_size(PData),
							access_mode = 0
						},

			{ok, #file{bytes=Size, id=Location}} = db_files:create(File),

			globals:set(fill, globals:get(fill)+Size),
			globals:set(reserv, globals:get(reserv)-Size),
		
			files:write(Location, PData),
			?LOG_INFO("file ~s  created!", [VPath]),
			{ok, created}
	end;


handle_req(
	#request{
	type=update,
	user=UserId,
	path=VPath,
	data=Data
	}=_Request) ->
	?LOG_INFO("update ~s", [VPath]),

	case db_files:select(UserId, VPath) of

		{error,	_} ->

			?LOG_WARN("file not exists"),
			{error, not_found};

		{ok,	File} ->

			NewSize = byte_size(Data),
			globals:set(fill, globals:get(fill)+NewSize),
			globals:set(fill, globals:get(fill)-File#file.bytes),
			db_files:update(File#file{bytes=NewSize}),

			files:write(File#file.id, Data),
			{ok, updated}
	end;


handle_req(
	#request{
		type=read,
		user=UserId,
		path=VPath
	}) ->
	?LOG_INFO("read ~s", [VPath]),
	case db_files:select(UserId, VPath) of
		{error, _} -> ?LOG_WARN("file not exists"), {error, not_found};	
		{ok, File} ->
			% {ok, Data} = files:read(File#file.id),
			% ?LOG_INFO("serving ~s, (~w bytes)", [VPath, byte_size(Data)]),
			{ok, << >>}
			% {ok, Data}
	end;

handle_req(
	#request{
		type=delete,
		user=UserId,
		path=VPath
	}) ->
	?LOG_INFO("delete ~s", [VPath]),
	case db_files:select(UserId, VPath) of
		{error, _} -> ?LOG_WARN("file not exists"), {error, not_found};	
		{ok, File} ->
			globals:set(fill, globals:get(fill)-File#file.bytes),
			db_files:delete(File),
			files:delete(File#file.id),
			{ok, deleted}
	end;

handle_req(
	#request{
		type=list,
		user=UserId
	}) ->
	?LOG_INFO("list"),
	db_files:select_by_owner(UserId);

handle_req(
	#request{
		type=find,
		user=User,
		path=Path
	}) ->
	?LOG_INFO("find ~s", [Path]),
	case db_files:exists( User, Path) of
		true -> {ok, node()};
		false -> {error, not_found}
	end.


%% ------------------------------------------------------------------
%% Internal Function Definitions
%% ------------------------------------------------------------------
