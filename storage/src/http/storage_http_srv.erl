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

	case string:tokens(FullPath, "/") of

		[Context | Elements] ->
			Path = string:join(Elements, "/"),
			log:info("accepted ~p from ~s (~s)", [Method, http_utils:hostaddr(Sock), FullPath]),

			case Context of
				"storage" 		-> handle_context_storage(Method, Path, Sock);
				"favicon.ico" 	-> handle_other(Sock, Path);
				_ 				-> handle_other(Sock, Path)
			end;

		[] -> handle_other(Sock, "/")
	end,

	gen_tcp:close(Sock).


handle_context_storage(Method, Path, Sock) ->
	case Method of
		'PUT'	-> handle_put(Sock, Path);
		'GET'	-> handle_get(Sock, Path);
		'POST'	-> handle_post(Sock, Path);
		'DELETE'-> handle_delete(Sock, Path);
		'HEAD'	-> handle_head(Sock, Path);
		_		-> handle_other(Sock, Path)
	end.

%%
%% method-specific handlers
%%

fetch_auth(Headers) ->
	try dict:fetch('Authorization', Headers) of
		AuthStr ->
			case re:run(AuthStr, "^HMAC\\s+(\\d+):([a-f0-9]+)$",
				[global, {capture, all_but_first, list}]) of

				{match, [[UserIdStr, HmacStr]]} ->
					log:info("hmac validated"),
					{list_to_integer(UserIdStr), list_to_binary(HmacStr)};
				_ ->
					log:info("hmac parsing failed"),
					{unknown, unknown}
			end
	catch
			_:_ ->
				log:info("hmac empty"),
				{unknown, unknown}
	end.


handle_post(Sock, Path) ->

	{Headers, BinData} = http_utils:parse_request(Sock),
	{User, Hmac} = fetch_auth(Headers),

	case storage:create(node(), User, Path, Hmac, BinData) of
		{ok,	created}		-> http_utils:send_response(Sock, 'Created',	text);
		{error,	timeout}		-> http_utils:send_response(Sock, 'NotAllowed',	text);
		{error,	file_exists}	-> http_utils:send_response(Sock, 'NotAllowed',	text);
		{error,	_}				-> http_utils:send_response(Sock, 'BadRequest',	text)
	end.

handle_get(Sock, Path) when Path == "/" ->
	{Headers, _} = http_utils:parse_request(Sock),
	{User, Hmac} = fetch_auth(Headers),
	case storage:list(node(), User, none_path, Hmac) of
		{ok,	ErlList}	-> http_utils:send_response(Sock, 'OK',			text, list_to_binary(lists:flatten(io_lib:format("~p", [ErlList]))));
		{error,	_}			-> http_utils:send_response(Sock, 'BadRequest',	text)
	end;

handle_get(Sock, Path) ->
	{Headers, _} = http_utils:parse_request(Sock),
	{User, Hmac} = fetch_auth(Headers),
	case storage:read(node(), User, Path, Hmac) of
		{ok,	RawData}	-> http_utils:send_response(Sock, 'OK',			file, RawData);
		{error,	timeout}	-> http_utils:send_response(Sock, 'NotFound',	text);
		{error, not_found}	-> http_utils:send_response(Sock, 'NotFound',	text);
		{error,	_}			-> http_utils:send_response(Sock, 'BadRequest',	text)
	end.

handle_put(Sock, Path) ->
	{Headers, BinData} = http_utils:parse_request(Sock),
	{User, Hmac} = fetch_auth(Headers),
	case storage:update(node(), User, Path, Hmac, BinData) of
		{ok,	_}			-> http_utils:send_response(Sock, 'Accepted',	text);
		{error, not_found}	-> http_utils:send_response(Sock, 'NotFound',	text);
		{error,	_}			-> http_utils:send_response(Sock, 'BadRequest',	text)
	end.

handle_delete(Sock, Path) ->
	{Headers, _} = http_utils:parse_request(Sock),
	{User, Hmac} = fetch_auth(Headers),
	case storage:delete(node(), User, Path, Hmac) of
		{ok,	deleted}	-> http_utils:send_response(Sock, 'Accepted',	text);
		{error,	not_found}	-> http_utils:send_response(Sock, 'NotFound',	text);
		{error,	_}			-> http_utils:send_response(Sock, 'BadRequest',	text)
	end.

handle_head(Sock, Path) ->
	{Headers, _} = http_utils:parse_request(Sock),
	{User, Hmac} = fetch_auth(Headers),
	case storage:find(node(), User, Path, Hmac) of
		{ok,	Node	}	-> http_utils:send_response(Sock, 'OK',			text, list_to_binary(lists:flatten(io_lib:format("~p", [Node]))));
		{error, not_found}	-> http_utils:send_response(Sock, 'NotFound',	text);
		{error,	_}			-> http_utils:send_response(Sock, 'BadRequest',	text)
	end.

handle_other(Sock, _Path) ->
	log:warn("unsupported operation requested"),
	http_utils:send_response(Sock, 'BadRequest', text).
