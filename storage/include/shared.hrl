%% @author Michal
%% @doc Shared data structures

-define(TIMEOUT, 30000).


% node server modules
-define(CORE_SERVER, storage_core_srv).
-define(DIST_SERVER, storage_dist_srv).
-define(AUTH_SERVER, storage_auth_srv).
-define(HTTP_SERVER, storage_http_srv).


-record(file, {
	id, 			% integer()
	owner, 			% integer()
	vpath, 			% list()
	bytes, 			% integer()
	location, 		% list()
	access_mode, 	% integer()
	create_time 	% integer()
	}).


-record(user, {
	id, 			% integer()
	name,			% list()
	secret, 		% list()
	create_time, 	% integer()
	storage_grant 	% integer()
	}).


-record(action, {
	id, 			% integer()
	user_id, 		% integer()
	file_id, 		% integer()
	time, 			% integer()
	type 			% integer() = C|R|U|D|L|F
	}).


-record(request, {
	type, 			% create | read | update | delete | list | find
	user, 			% integer()
	path = none, 	% list()
	hmac = none, 	% binary()
	data = none, 	% binary()
	opts = none 	% -unused-
	}).
