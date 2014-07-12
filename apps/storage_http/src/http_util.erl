%% @author Michal Liszcz
%% @doc Module for processing http request and responses

-module(http_util).
-include("shared.hrl").

%% ====================================================================
%% API functions
%% ====================================================================
-export([hostaddr/1, parse_request/1, send_response/2, send_response/3]).

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
	{ok, Body} = gen_tcp:recv(Sock, Length),
	Body.

parse_request(Sock) ->
	Headers = fetch_headers(Sock, dict:new()),

	try dict:fetch('Content-Length', Headers) of
		Length -> {Headers, fetch_body(Sock, list_to_integer(Length))}
	catch
		_:_ -> {Headers, ""}
	end.

send_response(Sock, HttpStatus) ->
	send_response(Sock, HttpStatus, "").

send_response(Sock, HttpStatus, StrData) ->
	Resp = case HttpStatus of
		'OK'			-> "HTTP/1.0 200 OK";
		'Created'		-> "HTTP/1.0 201 Created";
		'Accepted'		-> "HTTP/1.1 202 Accepted";
		'NoContent'		-> "HTTP/1.0 204 No Content";
		'BadRequest'	-> "HTTP/1.0 400 Bad Request";
		'Unauthorized'	-> "HTTP/1.0 401 Unauthorized";
		'NotAllowed'	-> "HTTP/1.1 405 Method Not Allowed";
		'NotFound'		-> "HTTP/1.0 404 Not Found";
		'ServerError'	-> "HTTP/1.0 500 Internal Server Error"
	end,
	MimeType = case StrData of
		"" -> "text/plain";
		_ -> "application/other"
	end,
	ResponseBody = case StrData of
		"" -> Resp;
		_ -> StrData
	end,
	gen_tcp:send(Sock, iolist_to_binary(
		io_lib:fwrite(
			"~s\r\nContent-Type: ~s; charset=UTF-8\r\nContent-Length: ~p\r\n\r\n~s",
			[Resp, MimeType, length(ResponseBody), ResponseBody]))).
