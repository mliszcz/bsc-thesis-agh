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
	{ok, {http_request, Method, {_, Path}, _Version}} = gen_tcp:recv(Sock, 0),
	log:info("accepted ~p from ~s (~s)", [Method, http_utils:hostaddr(Sock), Path]),
	case Method of
		'PUT'	-> handle_put(Sock, Path);
		'GET'	-> handle_get(Sock, Path);
		'POST'	-> handle_post(Sock, Path);
		'DELETE'-> handle_delete(Sock, Path);
		'HEAD'	-> handle_head(Sock, Path);
		_		-> handle_other(Sock, Path)
	end,
	% case {Method, Path} of
	% 	{'PUT',		_				} -> handle_put(Sock, Path);	% create
	% 	{'GET',		"/"				} -> handle_list(Sock, Path);	% 
	% 	{'GET',		"/favicon.ico"	} -> handle_other(Sock, Path);	% browser crap
	% 	{'GET',		_				} -> handle_get(Sock, Path);	% read
	% 	{'POST',	_				} -> handle_post(Sock, Path);	% update
	% 	{'DELETE',	_				} -> handle_delete(Sock, Path);	% delete
	% 	{'HEAD',	_ 				} -> handle_head(Sock, Path);	% find
	% 	_							  -> handle_other(Sock, Path)
	% end,
	gen_tcp:close(Sock).

%%
%% method-specific handlers
%%

handle_put(Sock, Path) ->
	{_Headers, BinData} = http_utils:parse_request(Sock),
	case storage:create(node(), Path, BinData) of
		{ok,	created}		-> http_utils:send_response(Sock, 'Created',	text);
		{error,	timeout}		-> http_utils:send_response(Sock, 'NotAllowed',	text);
		{error,	file_exists}	-> http_utils:send_response(Sock, 'NotAllowed',	text);
		{error,	_}				-> http_utils:send_response(Sock, 'BadRequest',	text)
	end.

handle_get(Sock, Path) when Path == "/" ->
	{_Headers, _} = http_utils:parse_request(Sock),
	case storage:list(node(), none_path) of
		{ok,	ErlList}	-> http_utils:send_response(Sock, 'OK',			text, list_to_binary(lists:flatten(io_lib:format("~p", [ErlList]))));
		{error,	_}			-> http_utils:send_response(Sock, 'BadRequest',	text)
	end;

handle_get(Sock, Path) when Path == "/favicon.ico" ->
	http_utils:send_response(Sock, 'NotFound', text);

handle_get(Sock, Path) ->
	{_Headers, _} = http_utils:parse_request(Sock),
	case storage:read(node(), Path) of
		{ok,	RawData}	-> http_utils:send_response(Sock, 'OK',			file, RawData);
		{error,	timeout}	-> http_utils:send_response(Sock, 'NotFound',	text);
		{error, not_found}	-> http_utils:send_response(Sock, 'NotFound',	text);
		{error,	_}			-> http_utils:send_response(Sock, 'BadRequest',	text)
	end.

handle_post(Sock, Path) ->
	{_Headers, BinData} = http_utils:parse_request(Sock),
	case storage:update(node(), Path, BinData) of
		{ok,	_}			-> http_utils:send_response(Sock, 'Accepted',	text);
		{error, not_found}	-> http_utils:send_response(Sock, 'NotFound',	text);
		{error,	_}			-> http_utils:send_response(Sock, 'BadRequest',	text)
	end.

handle_delete(Sock, Path) ->
	{_Headers, _} = http_utils:parse_request(Sock),
	case storage:delete(node(), Path) of
		{ok,	deleted}	-> http_utils:send_response(Sock, 'Accepted',	text);
		{error,	not_found}	-> http_utils:send_response(Sock, 'NotFound',	text);
		{error,	_}			-> http_utils:send_response(Sock, 'BadRequest',	text)
	end.

handle_head(Sock, Path) ->
	{_Headers, _} = http_utils:parse_request(Sock),
	case storage:find(node(), Path) of
		{ok,	Node	}	-> http_utils:send_response(Sock, 'OK',			text, list_to_binary(lists:flatten(io_lib:format("~p", [Node]))));
		{error, not_found}	-> http_utils:send_response(Sock, 'NotFound',	text);
		{error,	_}			-> http_utils:send_response(Sock, 'BadRequest',	text)
	end.

handle_other(Sock, _Path) ->
	log:warn("unsupported operation requested"),
	http_utils:send_response(Sock, 'BadRequest', text).
