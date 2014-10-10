%% @author Michal Liszcz
%% @doc REST interface for Storage Server

-module(storage_http_srv).
-include("shared.hrl").
-behaviour(gen_server).
-define(SERVER, ?MODULE).

%% ------------------------------------------------------------------
%% API Function Exports
%% ------------------------------------------------------------------

-export([start_link/0, stop/0, listen/1, handle_request/1]).

%% ------------------------------------------------------------------
%% gen_server Function Exports
%% ------------------------------------------------------------------

-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

%% ------------------------------------------------------------------
%% API Function Definitions
%% ------------------------------------------------------------------

start_link() ->
	gen_server:start_link({local, ?SERVER}, ?MODULE, [], []).

stop() ->
	?LOG_INFO("shutdown"),
	gen_server:cast(?SERVER, stop).	%% @FIXME this is never called

%% ------------------------------------------------------------------
%% gen_server Function Definitions
%% ------------------------------------------------------------------

init(_Args) ->
	?LOG_INFO("starting http listener"),
	{ok, ListenSock} = gen_tcp:listen(
		util:get_env(http_port),
		[binary, {active, false}, {packet, http}, {reuseaddr, true}]),
	spawn_link(?MODULE, listen, [ListenSock]),
	{ok, ListenSock}.

handle_call(_Request, _From, State) ->
	{reply, ok, State}.

handle_cast(stop, State) ->
	?LOG_INFO("stopping"),
	gen_tcp:close(State),
	{stop, normal, State}.

handle_info(_Info, State) ->
	{noreply, State}.

terminate(_Reason, State) ->
	?LOG_INFO("closing"),
	gen_tcp:close(State),
	ok.

code_change(_OldVsn, State, _Extra) ->
	{ok, State}.

%% ------------------------------------------------------------------
%% Internal Function Definitions
%% ------------------------------------------------------------------
 

listen(ListenSock) ->
	case gen_tcp:accept(ListenSock) of
		{ok, Sock} -> spawn(?MODULE, handle_request, [Sock]), listen(ListenSock);
		{error, _} -> ?LOG_ERROR("tcp accept failed")
	end.	


handle_request(Sock) ->

	{ok, {http_request, Method, {_, FullPath}, _Version}} = gen_tcp:recv(Sock, 0),

	?LOG_INFO("received ~p from ~s (~s)", [Method, http_utils:hostaddr(Sock), FullPath]),

	Response = case string:tokens(FullPath, "/") of

		["storage", Owner | PathParts] 		-> handle_context_storage(Method, {Owner, string:join(PathParts, "/")}, Sock);
		["manager" | _] 					-> handle_context_manager(Method);
		[] 									-> handle_context_manager(Method);
		["favicon.ico"] 					-> handle_other();
		_ 									-> handle_other()
	end,

	http_utils:send_response(Sock, Response),
	gen_tcp:close(Sock).


% TODO load this only once
handle_context_manager('GET') ->
	{ok, BinManager} = file:read_file(code:priv_dir(util:get_app())++"/manager.html"),
	{'OK', html, [], BinManager};
handle_context_manager(_) ->
	handle_other().


handle_context_storage(Method, {Owner, Path}, Sock) ->
	{Headers, BinData} = http_utils:parse_request(Sock),
	try extract_credentials(Headers) of
		{User, HmacStr} -> handle_context_storage_method(Method, User, Owner, Path, string:to_upper(HmacStr), BinData)
	catch
		_:_ -> handle_other()
	end.


%%
%% method-specific handlers
%%

extract_credentials(Headers) ->

	% this call will raise exception if anything goes wrong

	{match, [[UserIdStr, HmacStr]]} = re:run(
		dict:fetch('Authorization', Headers),
		% "^HMAC\\s+([a-f0-9]+):([a-f0-9]+)$",
		"^HMAC\\s+(\\w+):([a-f0-9]+)$",
		[global, {capture, all_but_first, list}]
	),

	{UserIdStr, HmacStr}.


handle_context_storage_method('POST', User, Owner, Path, Hmac, BinData) ->

	case storage:create(node(), User, Owner, Path, Hmac, BinData) of
		{ok,	created}				-> {'Created', 		text, [], << >>};
		{error,	timeout}				-> {'NotAllowed',	text, [], << >>};
		{error,	file_exists}			-> {'NotAllowed',	text, [], << >>};
		{error, authenticaiton_failed}	-> {'Unauthorized', text, [], << >>};
		{error,	_}						-> {'BadRequest',	text, [], << >>}
	end;


handle_context_storage_method('GET', User, Owner, Path, Hmac, _BinData) when Path == "" ->

	case storage:list(node(), User, Owner, "/", Hmac) of
		{ok,	ErlList}				-> {'OK', 			json, [], list_to_binary(file_list_to_json(ErlList))};
		{error, authenticaiton_failed}	-> {'Unauthorized', text, [], << >>};
		{error,	_}						-> {'BadRequest',	text, [], << >>}
	end;


handle_context_storage_method('GET', User, Owner, Path, Hmac, _BinData) ->

	case storage:read(node(), User, Owner, Path, Hmac) of
		{ok,	RawData}				-> {'OK',			file, [], RawData};
		{error,	timeout}				-> {'NotFound',		text, [], << >>};
		{error, not_found}				-> {'NotFound', 	text, [], << >>};
		{error, authenticaiton_failed}	-> {'Unauthorized', text, [], << >>};
		{error,	_}						-> {'BadRequest', 	text, [], << >>}
	end;


handle_context_storage_method('PUT', User, Owner, Path, Hmac, BinData) ->

	case storage:update(node(), User, Owner, Path, Hmac, BinData) of
		{ok,	_}						-> {'Accepted',		text, [], << >>};
		{error, not_found}				-> {'NotFound',		text, [], << >>};
		{error, authenticaiton_failed}	-> {'Unauthorized', text, [], << >>};
		{error,	_}						-> {'BadRequest',	text, [], << >>}
	end;


handle_context_storage_method('DELETE', User, Owner, Path, Hmac, _BinData) ->

	case storage:delete(node(), User, Owner, Path, Hmac) of
		{ok,	deleted}				-> {'Accepted',		text, [], << >>};
		{error,	not_found}				-> {'NotFound',		text, [], << >>};
		{error, authenticaiton_failed}	-> {'Unauthorized', text, [], << >>};
		{error,	_}						-> {'BadRequest',	text, [], << >>}
	end;


handle_context_storage_method('HEAD', User, Owner, Path, Hmac, _BinData) ->

	case storage:find(node(), User, Owner, Path, Hmac) of
		{ok,	Node	}				-> {'OK',			file, [], util:term_to_binary_string(Node)};
		{error, not_found}				-> {'NotFound',		text, [], << >>};
		{error, authenticaiton_failed}	-> {'Unauthorized', text, [], << >>};
		{error,	_}						-> {'BadRequest',	text, [], << >>}
	end;

handle_context_storage_method(_Method, _User, _Owner, _Path, _Hmac, _BinData) ->
	handle_other().


handle_other() ->
	?LOG_WARN("unsupported operation requested"),
	{'BadRequest', text, [], << >>}.


%%
%% THIS IS CRAP AND NEEDS REFACTORING
%%

file_list_to_json(ErlList) ->
	MulList = [format_node_entry(N) || N <- ErlList],
	FlatList = lists:append(MulList),
	"[" ++ string:join(FlatList, ", ") ++ "]".

format_node_entry({Node, FileList}) ->
	N = atom_to_list(Node),
	[ "{\"node\":\"" ++ N ++ "\"," ++ format_file_entry(F) ++ "}" || F <- FileList ].

format_file_entry(#file{
	id 			= Id, 			% list()
	owner 		= _Owner, 		% list()
	vpath 		= VPath,		% list()
	bytes 		= Bytes,		% integer()
	access_mode = _AccMod, 		% integer()
	create_time = CreatTim		% integer()
	}) ->
	io_lib:format("\"id\":\"~s\",\"vpath\":\"~s\",\"bytes\":~p,\"created\":~p",
		[Id, VPath, Bytes, CreatTim]).
