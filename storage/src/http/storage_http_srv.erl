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
	log:info("shutdown"),
	gen_server:cast(?SERVER, stop).	%% @FIXME this is never called

%% ------------------------------------------------------------------
%% gen_server Function Definitions
%% ------------------------------------------------------------------

init(_Args) ->
	log:info("starting http listener"),
	{ok, ListenSock} = gen_tcp:listen(
		util:get_env(http_port),
		[binary, {active, false}, {packet, http}, {reuseaddr, true}]),
	spawn_link(?MODULE, listen, [ListenSock]),
	{ok, ListenSock}.

handle_call(_Request, _From, State) ->
	{reply, ok, State}.

handle_cast(stop, State) ->
	log:info("stopping"),
	gen_tcp:close(State),
	{stop, normal, State}.

handle_info(_Info, State) ->
	{noreply, State}.

terminate(_Reason, State) ->
	log:info("closing"),
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
		{error, _} -> log:error("tcp accept failed")
	end.	


handle_request(Sock) ->

	{ok, {http_request, Method, {_, FullPath}, _Version}} = gen_tcp:recv(Sock, 0),

	Response = case string:tokens(FullPath, "/") of

		[Context | Elements] ->

			Path = string:join(Elements, "/"),

			log:info("accepted ~p from ~s (~s)", [Method, http_utils:hostaddr(Sock), FullPath]),

			case Context of
				"storage" 		-> handle_context_storage(Method, Path, Sock);
				"favicon.ico" 	-> handle_other(Path);
				_ 				-> handle_other(Path)
			end;

		[] -> handle_other("/")
	end,

	http_utils:send_response(Sock, Response),
	gen_tcp:close(Sock).


handle_context_storage(Method, Path, Sock) ->

	{Headers, BinData} = http_utils:parse_request(Sock),

	try extract_credentials(Headers) of
		{User, Hmac} -> 
			case Method of
				'PUT'	->    handle_put(User, Path, Hmac, BinData);
				'GET'	->    handle_get(User, Path, Hmac, BinData);
				'POST'	->   handle_post(User, Path, Hmac, BinData);
				'DELETE'-> handle_delete(User, Path, Hmac, BinData);
				'HEAD'	->   handle_head(User, Path, Hmac, BinData);
				_		->  handle_other(Path)
			end
	catch
		_:_ -> handle_other(Path)
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


handle_post(User, Path, Hmac, BinData) ->

	case storage:create(node(), User, Path, Hmac, BinData) of
		{ok,	created}		-> {'Created',		self};
		{error,	timeout}		-> {'NotAllowed',	self};
		{error,	file_exists}	-> {'NotAllowed',	self};
		{error, authenticaiton_failed}	-> {'Unauthorized', self};
		{error,	_}				-> {'BadRequest',	self}
	end.


handle_get(User, Path, Hmac, _BinData) when Path == "/" ->

	case storage:list(node(), User, none_path, Hmac) of
		{ok,	ErlList}	-> {'OK',			util:term_to_binary_string(ErlList)};
		{error, authenticaiton_failed}	-> {'Unauthorized', self};
		{error,	_}			-> {'BadRequest',	self}
	end;


handle_get(User, Path, Hmac, _BinData) ->

	case storage:read(node(), User, Path, Hmac) of
		{ok,	RawData}	-> {'OK',			RawData};
		{error,	timeout}	-> {'NotFound',		self};
		{error, not_found}	-> {'NotFound', 	self};
		{error, authenticaiton_failed}	-> {'Unauthorized', self};
		{error,	_}			-> {'BadRequest', 	self}
	end.


handle_put(User, Path, Hmac, BinData) ->

	case storage:update(node(), User, Path, Hmac, BinData) of
		{ok,	_}			-> {'Accepted',		self};
		{error, not_found}	-> {'NotFound',		self};
		{error, authenticaiton_failed}	-> {'Unauthorized', self};
		{error,	_}			-> {'BadRequest',	self}
	end.


handle_delete(User, Path, Hmac, _BinData) ->

	case storage:delete(node(), User, Path, Hmac) of
		{ok,	deleted}	-> {'Accepted',		self};
		{error,	not_found}	-> {'NotFound',		self};
		{error, authenticaiton_failed}	-> {'Unauthorized', self};
		{error,	_}			-> {'BadRequest',	self}
	end.


handle_head(User, Path, Hmac, _BinData) ->

	case storage:find(node(), User, Path, Hmac) of
		{ok,	Node	}	-> {'OK',			util:term_to_binary_string(Node)};
		{error, not_found}	-> {'NotFound',		self};
		{error, authenticaiton_failed}	-> {'Unauthorized', self};
		{error,	_}			-> {'BadRequest',	self}
	end.


handle_other(_Path) ->
	log:warn("unsupported operation requested"),
	{'BadRequest', self}.
