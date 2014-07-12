%% @author Michal Liszcz
%% @doc Core Server backend

-module(core).
-include("shared.hrl").

-export([ handle_req/1 ]).

handle_req(
	#request{
		type=create,
		user=UserId,
		path=VPath
	}=Request) ->
	log:info("create ~s", [VPath]),
	case metadata:get(UserId, VPath) of
		{ok,	_} -> log:warn("file exists!"), {error, file_exists};
		{error,	_} -> create_file(Request)
	end;

handle_req(
	#request{
	type=update,
	user=UserId,
	path=VPath
	}=Request) ->
	log:info("update ~s", [VPath]),
	case metadata:get(UserId, VPath) of
		{error,	_} -> log:warn("file not exists!"), {error, not_found};
		{ok,	_} -> create_file(Request)
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
	end.


%% ------------------------------------------------------------------
%% Internal Function Definitions
%% ------------------------------------------------------------------


create_file(
	#request{
		user=UserId,
		path=VPath,
		data=PData
	}) ->

	File = #filedesc {
		path = VPath,
		user = UserId,
		size = byte_size(PData),
		last_access	= util:timestamp()
		},
	
	{ok, #filedesc{internal_id=NewId}} = metadata:create(File),
	%% storage reserved on dispatching
	%% globals:set(fill, globals:get(fill)+byte_size(Data)),
	
	files:write(NewId, PData),
	log:info("file ~s  created!", [VPath]),
	{ok, created}.
