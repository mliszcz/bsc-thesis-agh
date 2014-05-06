%% @author Michal Liszcz
%% @doc REST interface for Storage Server

-module(storage_http_srv).
-export([start/0, handle_request/1]).
-define(TIMEOUT, 60000).

start() ->
	start(8081).

 start(Port)->
	{ok, ListenSock} = gen_tcp:listen(Port, [list,{active, false},{packet,http}]),
	loop(ListenSock).
 
 loop(ListenSock) ->
 	{ok, Sock} = gen_tcp:accept(ListenSock),
	spawn(?MODULE, handle_request, [Sock]),
	loop(ListenSock).

 handle_request(Sock) ->
 	{ok, {http_request, Method, {_, Path}, _Version}} = gen_tcp:recv(Sock, 0),
 	case (Method) of
 		'PUT' -> handle_put_file(Sock, Path);
 		'GET' -> handle_get_file(Sock, Path);
 		_ -> send_unsupported_error(Sock)
 	end.

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
	storage_client_api:request_create(Path, list_to_binary(RawData)),
	io:format("writing file ~s~n", [Path]),
	send_accept(Sock).

handle_get_file(Sock, Path) ->
	{ok, RawData} = storage_client_api:request_read(Path),
	send_binary(Sock, RawData).

send_binary(Sock, RawData) ->
	StrData = binary_to_list(RawData),
	gen_tcp:send(Sock, iolist_to_binary(io_lib:fwrite("HTTP/1.0 200 OK\r\nContent-Length: ~p\r\n\r\n~s", [length(StrData), StrData]))).

 send_accept(Sock) ->
	gen_tcp:send(Sock, "HTTP/1.1 202 Accepted\r\nConnection: close\r\nContent-Type: text/html; charset=UTF-8\r\nCache-Control: no-cache\r\n\r\n"),
	gen_tcp:close(Sock).
	
 send_unsupported_error(Sock) ->
	gen_tcp:send(Sock, "HTTP/1.1 405 Method Not Allowed\r\nConnection: close\r\nAllow: POST\r\nContent-Type: text/html; charset=UTF-8\r\nCache-Control: no-cache\r\n\r\n"),
	gen_tcp:close(Sock).
