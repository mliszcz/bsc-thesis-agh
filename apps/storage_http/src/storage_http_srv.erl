-module(storage_http_srv).
-export([start/0, handle_request/1]).
-define(TIMEOUT, 60000).

% start() ->
% 	spawn(fun () -> {ok, Sock} = gen_tcp:listen(8081, [{active, false}]), 
% 	loop(Sock) end).

% loop(Sock) ->
% 	{ok, Conn} = gen_tcp:accept(Sock),
% 	Handler = spawn(fun () -> handle(Conn) end),
% 	gen_tcp:controlling_process(Conn, Handler),
% 	loop(Sock).

% handle(Conn) ->
% 	gen_tcp:send(Conn, response("Hello World")),
% 	gen_tcp:close(Conn).

% response(Str) ->
% 	B = iolist_to_binary(Str),
% 	iolist_to_binary(
% 		io_lib:fwrite(
% 		"HTTP/1.0 200 OK\nContent-Type: text/html\nContent-Length: ~p\n\n~s",
% 		[size(B), B])).

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
 		'PUT' -> handle_write_file(Sock, Path);
 		_ -> send_unsupported_error(Sock)
 	end.

fetch_headers(Sock, Headers) ->
	case gen_tcp:recv(Sock, 0, ?TIMEOUT) of
		{ok, {http_header, _, Header, _, Value}} -> parse_headers(Sock, dict:store(Header, Value, Headers));
		{ok, http_eoh} -> io:format("got eoh~n", []), Headers
	end.

fetch_body(Sock, Length) ->
	inet:setopts(Sock, [{packet, raw}]),
	{ok, Body} = gen_tcp:recv(Sock, Length),
	Body.

handle_write_file(Sock, Path) ->
	Headers = fetch_headers(Sock, dict:new()),
	Length = dict:fetch('Content-Length', Headers),
	PostBody= fetch_body(Sock, list_to_integer(Length)),
	% api call here
	io:fwrite(PostBody),
	send_accept(Sock).

 send_accept(Sock) ->
	gen_tcp:send(Sock, "HTTP/1.1 202 Accepted\r\nConnection: close\r\nContent-Type: text/html; charset=UTF-8\r\nCache-Control: no-cache\r\n\r\n"),
	gen_tcp:close(Sock).
	
 send_unsupported_error(Sock) ->
	gen_tcp:send(Sock, "HTTP/1.1 405 Method Not Allowed\r\nConnection: close\r\nAllow: POST\r\nContent-Type: text/html; charset=UTF-8\r\nCache-Control: no-cache\r\n\r\n"),
	gen_tcp:close(Sock).
