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
	{ok, ListenSock} = gen_tcp:listen(util:get_env(http_port), [list, {active, false}, {packet,http}, {reuseaddr, true}]),
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
	io:format("closed~n", []),
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
	log:info("accepted ~p from ~s (~s)", [Method, hostname(Sock), Path]),
	case (Method) of
		'PUT'	-> handle_put_file(Sock, Path);
		'GET'	-> handle_get_file(Sock, Path);
		_		-> send_unsupported_error(Sock)
	end,
	gen_tcp:close(Sock).


fetch_headers(Sock, Headers) ->
	case gen_tcp:recv(Sock, 0, ?TIMEOUT) of
		{ok, {http_header, _, Header, _, Value}} -> fetch_headers(Sock, dict:store(Header, Value, Headers));
		{ok, http_eoh} -> Headers
	end.

fetch_body(Sock, Length) ->
	inet:setopts(Sock, [{packet, raw}]),
	{ok, Body} = gen_tcp:recv(Sock, Length),
	Body.

handle_put_file(Sock, Path) ->
	Headers = fetch_headers(Sock, dict:new()),
	Length = dict:fetch('Content-Length', Headers),
	RawData = fetch_body(Sock, list_to_integer(Length)),
	storage_client_api:request_put(node(), Path, list_to_binary(RawData), none),
	send_accept(Sock).

handle_get_file(Sock, Path) ->
	send_binary(Sock,
		case Path of
			"/" ->
				{ok, RawList} = storage_client_api:request_list(node(), Path),
				list_to_binary(lists:flatten(io_lib:format("~p",[RawList])));
			_   ->
				{ok, RawData} = storage_client_api:request_get(node(), Path),
				RawData
		end).

send_binary(Sock, RawData) ->
	log:info("responding to ~s with HTTP/1.0 200 OK", [hostname(Sock)]),
	StrData = binary_to_list(RawData),
	gen_tcp:send(Sock, iolist_to_binary(io_lib:fwrite("HTTP/1.0 200 OK\r\nContent-Length: ~p\r\n\r\n~s", [length(StrData), StrData]))).

send_accept(Sock) ->
	log:info("responding to ~s with HTTP/1.1 202 Accepted", [hostname(Sock)]),
	gen_tcp:send(Sock, "HTTP/1.1 202 Accepted\r\nConnection: close\r\nContent-Type: text/html; charset=UTF-8\r\nCache-Control: no-cache\r\n\r\n").
	
send_unsupported_error(Sock) ->
	log:warn("responding to ~s with HTTP/1.1 405 Method Not Allowed", [hostname(Sock)]),
	gen_tcp:send(Sock, "HTTP/1.1 405 Method Not Allowed\r\nConnection: close\r\nAllow: POST\r\nContent-Type: text/html; charset=UTF-8\r\nCache-Control: no-cache\r\n\r\n").

hostname(Socket) ->
	case inet:peername(Socket) of
 		{ok, {{O1, O2, O3, O4}, Port}} -> io_lib:format("~p.~p.~p.~p:~p",[O1, O2, O3, O4, Port]);
 		{error, _} -> "UNKNOWN-HOST"
 	end.
