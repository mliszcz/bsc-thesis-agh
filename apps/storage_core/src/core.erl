%% @author Michal Liszcz
%% @doc Core Server backend

-module(core).
-include("shared.hrl").

-export([ handle_req/1 ]).

handle_req(
	#rreq{
		action=get,
		user_id=UserId,
		v_path=VPath
	}=Request) ->
	log:info("CORE: GET request ~s~n", [VPath]),
	case metadata:get(UserId, VPath) of
		{error, _} ->
			log:warn("CORE: nonexistent file requested ~s~n", [VPath]),
			{error, not_found};	
		{ok, File} ->
			metadata:modify(File#file{last_access=util:timestamp()}),
			{ ok, Data } = files:read(File#file.local_id),
			log:info("CORE: serving ~s, (~w bytes)~n", [VPath, byte_size(Data)]),
			{ok, Data}
	end;


handle_req(
	#rreq{
		action=put,
		user_id=UserId,
		v_path=VPath,
		put_path=PPath,
		put_data=PData
	}=Request) ->
	log:info("CORE: PUT request ~s~n", [VPath]),
	case metadata:get(UserId, VPath) of
		{error, _} ->
			log:info("CORE: nonexistent file requested ~s, creating...~n", [VPath]),
			create_file(Request);
		{ok, File} ->
			log:info("CORE: existing file requested ~s, updating...~n", [VPath]),
			update_file(Request)
	end;


handle_req(
	#rreq{
		action=del,
		user_id=UserId,
		v_path=VPath
	}=Request) ->

	log:info("CORE: deleting ~s!~n", [VPath]),

	{ok, File} = metadata:get(UserId, VPath),
	globals:set(fill, globals:get(fill)-File#file.size),
	
	metadata:delete(File),
	files:delete(File#file.local_id),	
	{ok, deleted}.


%% ------------------------------------------------------------------
%% Internal Function Definitions
%% ------------------------------------------------------------------


create_file(
	#rreq{
		action=put,
		user_id=UserId,
		v_path=VPath,
		put_path=PPath,
		put_data=PData
	}=Request) ->

	File = #file{owner_id		= UserId,
				 last_access	= util:timestamp(),
				 size			= byte_size(PData),
				 v_path			= VPath},
	
	{ok, #file{local_id=NewId}} = metadata:create(File),
	%% storage reserved on dispatching
	%% globals:set(fill, globals:get(fill)+byte_size(Data)),
	
	files:write(NewId, PData),
	log:info("CORE: file ~s  created!~n", [VPath]),
	{ok, created}.


update_file(
	#rreq{
		action=put,
		user_id=UserId,
		v_path=VPath,
		put_path=PPath,
		put_data=PData
	}=Request) ->

	Path = case PPath of
		none -> VPath;
		_ ->PPath
	end,

	Data = case PData of
		none ->
			{ok, OldData} = handle_req(Request#rreq{action=get}),
			OldData;
		_ -> PData
	end,

	log:info("CORE: recreating ~s  on update!~n", [VPath]),
	create_file(Request#rreq{v_path=Path, put_data=Data}),
	%% FIXME update metadata
	%% TODO refactor that CRAP
	{ok, updated}.
