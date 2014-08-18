%% @author Michal
%% @doc Shared data structures

-define(TIMEOUT, 30000).


% node server modules
-define(UUID_SERVER, storage_uuid_srv).
-define(CORE_SERVER, storage_core_srv).
-define(AUTH_SERVER, storage_auth_srv).
-define(DIST_SERVER, storage_dist_srv).
-define(HTTP_SERVER, storage_http_srv).


-ifdef(log).
-define(LOG_INFO(M), 		log:info(M)).
-define(LOG_INFO(M, D), 	log:info(M, D)).
-define(LOG_WARN(M), 		log:warn(M)).
-define(LOG_WARN(M, D), 	log:warn(M, D)).
-define(LOG_ERROR(M), 		log:error(M)).
-define(LOG_ERROR(M, D), 	log:error(M, D)).
-else.
-define(LOG_INFO(M), 		true).
-define(LOG_INFO(M, D), 	true).
-define(LOG_WARN(M), 		true).
-define(LOG_WARN(M, D), 	true).
-define(LOG_ERROR(M), 		true).
-define(LOG_ERROR(M, D), 	true).
-endif.


-record(file, {
	id, 			% list()
	owner, 			% list()
	vpath, 			% list()
	bytes, 			% integer()
	access_mode, 	% integer()
	create_time 	% integer()
	}).


-record(user, {
	id, 			% list()
	name,			% list()
	secret, 		% list()
	create_time, 	% integer()
	storage_grant 	% integer()
	}).


-record(action, {
	id, 			% list()
	user_id, 		% list()
	file_id, 		% list()
	action_time, 	% integer()
	action_type 	% integer() = C|R|U|D|L|F
	}).


-record(request, {
	type, 			% create | read | update | delete | list | find
	user, 			% list()
	path = none, 	% list()
	hmac = none, 	% list()
	data = none, 	% binary()
	opts = none 	% -unused-
	}).
