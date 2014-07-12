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
	gen_server:cast(?SERVER, stop).	%% @FIXME this is never called

%% ------------------------------------------------------------------
%% gen_server Function Definitions
%% ------------------------------------------------------------------

init(_Args) ->
	log:info("starting http listener"),
	{ok, ListenSock} = gen_tcp:listen(
		util:get_env(http_port),
		[list, {active, false}, {packet,http}, {reuseaddr, true}]),
	spawn_link(?MODULE, listen, [ListenSock]),
	{ok, ListenSock}.

handle_call(_Request, _From, State) ->
	{reply, ok, State}.

handle_cast(stop, State) ->
	gen_tcp:close(State),
	{stop, normal, State}.

handle_info(_Info, State) ->
	{noreply, State}.

terminate(_Reason, State) ->
	gen_tcp:close(State),
	log:info("closing"),
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
	log:info("accepted ~p from ~s (~s)", [Method, http_util:hostaddr(Sock), Path]),
	case (Method) of
		'PUT'	-> handle_put(Sock, Path);		% create
		'GET'	-> handle_get(Sock, Path);		% read
		'POST'	-> handle_post(Sock, Path);		% update
		'DELETE'-> handle_delete(Sock, Path);	% delete
		_		-> handle_other(Sock, Path)
	end,
	gen_tcp:close(Sock).

%%
%% method-specific handlers
%%

handle_put(Sock, Path) ->
	{_Headers, StrData} = http_util:parse_request(Sock),
	case storage_client_api:request_create(node(), Path, list_to_binary(StrData)) of
		{ok,	created}		-> http_util:send_response(Sock, 'Created');
		{error,	file_exists}	-> http_util:send_response(Sock, 'NotAllowed');
		{error,	_}				-> http_util:send_response(Sock, 'BadRequest')
	end.

handle_get(Sock, Path) ->
	{_Headers, _} = http_util:parse_request(Sock),
	case storage_client_api:request_read(node(), Path) of
		{ok,	RawData}	-> http_util:send_response(Sock, 'OK', binary_to_list(RawData));
		{error, not_found}	-> http_util:send_response(Sock, 'NotFound');
		{error,	_}			-> http_util:send_response(Sock, 'BadRequest')
	end.

handle_post(Sock, Path) ->
	{_Headers, StrData} = http_util:parse_request(Sock),
	case storage_client_api:request_update(node(), Path, list_to_binary(StrData)) of
		{ok,	_}			-> http_util:send_response(Sock, 'Accepted');
		{error, not_found}	-> http_util:send_response(Sock, 'NotFound');
		{error,	_}			-> http_util:send_response(Sock, 'BadRequest')
	end.

handle_delete(Sock, Path) ->
	{_Headers, _} = http_util:parse_request(Sock),
	case storage_client_api:request_delete(node(), Path) of
		{ok,	deleted}	-> http_util:send_response(Sock, 'Accepted');
		{error,	not_found}	-> http_util:send_response(Sock, 'NotFound');
		{error,	_}			-> http_util:send_response(Sock, 'BadRequest')
	end.

handle_other(Sock, _Path) ->
	log:warn("unsupported operation requested"),
	http_util:send_response(Sock, 'BadRequest').
