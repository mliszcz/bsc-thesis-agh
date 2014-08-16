%% @author Michal Liszcz
%% @doc Module for processing http request and responses

-module(http_utils).
-include("shared.hrl").

%% ====================================================================
%% API functions
%% ====================================================================
-export([hostaddr/1, parse_request/1, send_response/2]).

hostaddr(Socket) ->
	case inet:peername(Socket) of
		{ok, {{O1, O2, O3, O4}, Port}} -> io_lib:format("~p.~p.~p.~p:~p",[O1, O2, O3, O4, Port]);
		{error, _} -> "UNKNOWN-HOST"
	end.


%%
%% HTTP request parsing
%%

fetch_headers(Sock, Headers) ->
	case gen_tcp:recv(Sock, 0, ?TIMEOUT) of
		{ok, {http_header, _, Header, _, Value}} -> fetch_headers(Sock, dict:store(Header, Value, Headers));
		{ok, http_eoh} -> Headers
	end.


fetch_body(Sock, Length) ->
	inet:setopts(Sock, [{packet, raw}]),
	{ok, Body} = chunked_recv(Sock, << >>, Length),
	% {ok, Body} = gen_tcp:recv(Sock, Length),	% this fails for large binaries (100+ MB)
	Body.


parse_request(Sock) ->
	Headers = fetch_headers(Sock, dict:new()),
	try dict:fetch('Content-Length', Headers) of
		Length -> {Headers, fetch_body(Sock, list_to_integer(Length))}
	catch
		_:_ -> {Headers, << >>}
	end.


-define(CHUNK_SIZE, 67108864). % 64 MB 16777216).	% 16 MB
chunked_recv(Sock, Buffer, Size) ->
	case Size =< ?CHUNK_SIZE of
		true ->
			{ok, Bin} = gen_tcp:recv(Sock, Size, 10*?TIMEOUT),
			{ok, <<Buffer/binary, Bin/binary>>};
		false ->
			{ok, Bin} = gen_tcp:recv(Sock, ?CHUNK_SIZE, 10*?TIMEOUT),
			chunked_recv(Sock, <<Buffer/binary, Bin/binary>>, Size-?CHUNK_SIZE)
	end.


send_response(Sock, {HttpStatus, ContentType, Headers, Body})
	when is_atom(HttpStatus), is_atom(ContentType), is_list(Headers), is_binary(Body) ->

	Resp = case HttpStatus of
		'OK'			-> "HTTP/1.0 200 OK";
		'Created'		-> "HTTP/1.0 201 Created";
		'Accepted'		-> "HTTP/1.1 202 Accepted";
		'NoContent'		-> "HTTP/1.0 204 No Content";
		'Redirect'		-> "HTTP/1.1 302 Found";
		'BadRequest'	-> "HTTP/1.0 400 Bad Request";
		'Unauthorized'	-> "HTTP/1.0 401 Unauthorized";
		'NotAllowed'	-> "HTTP/1.1 405 Method Not Allowed";
		'NotFound'		-> "HTTP/1.0 404 Not Found";
		'ServerError'	-> "HTTP/1.0 500 Internal Server Error"
	end,

	Mime = case ContentType of
		text	-> "text/plain";
		html 	-> "text/html";
		json 	-> "application/json";
		file	-> "application/octet-stream";
		_		-> "application/octet-stream"
	end,

	ResponseBody = case Body of
		<< >>	-> list_to_binary(Resp);
		_		-> Body
	end,

	Status = case Body of
		<< >> -> io_lib:fwrite("~s\r\n~s\r\n", [Resp, build_headers("", Headers)]);
		Body  -> io_lib:fwrite("~s\r\n~sContent-Type: ~s; charset=UTF-8\r\nContent-Length: ~p\r\n\r\n",
			[Resp, build_headers("", Headers), Mime, byte_size(ResponseBody)])
	end,

	BinHeaders = iolist_to_binary(Status),

	gen_tcp:send(Sock, << BinHeaders/binary, ResponseBody/binary >>).

build_headers(Buffer, []) -> Buffer;
build_headers(Buffer, [{H,V}|T]) ->
	build_headers(io_lib:fwrite("~s~s: ~s\r\n", [Buffer, H, V]), T).


% send_response(Sock, {HttpStatus, self}) ->
% 	send_response(Sock, HttpStatus, text);

% send_response(Sock, {HttpStatus, BinData}) ->
% 	send_response(Sock, HttpStatus, file, BinData).


% send_response(Sock, HttpStatus, MimeType) ->
% 	send_response(Sock, HttpStatus, MimeType, << >>).


% send_response(Sock, HttpStatus, MimeType, BinData) ->
% 	Resp = case HttpStatus of
% 		'OK'			-> "HTTP/1.0 200 OK";
% 		'Created'		-> "HTTP/1.0 201 Created";
% 		'Accepted'		-> "HTTP/1.1 202 Accepted";
% 		'NoContent'		-> "HTTP/1.0 204 No Content";
% 		'BadRequest'	-> "HTTP/1.0 400 Bad Request";
% 		'Unauthorized'	-> "HTTP/1.0 401 Unauthorized";
% 		'NotAllowed'	-> "HTTP/1.1 405 Method Not Allowed";
% 		'NotFound'		-> "HTTP/1.0 404 Not Found";
% 		'ServerError'	-> "HTTP/1.0 500 Internal Server Error"
% 	end,
% 	Mime = case MimeType of
% 		text	-> "text/plain";
% 		html 	-> "text/html"
% 		file	-> "application/octet-stream";
% 		_		-> "application/octet-stream"
% 	end,
% 	ResponseBody = case BinData of
% 		<< >>	-> list_to_binary(Resp);
% 		_		-> BinData
% 	end,

% 	Headers = iolist_to_binary(io_lib:fwrite(
% 			"~s\r\nContent-Type: ~s; charset=UTF-8\r\nContent-Length: ~p\r\n\r\n",
% 			[Resp, Mime, byte_size(ResponseBody)])),

% 	gen_tcp:send(Sock, << Headers/binary, ResponseBody/binary >>).
