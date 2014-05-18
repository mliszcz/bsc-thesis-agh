%% @author Michal
%% @doc Shared data structures


-define(STORAGE_PROC, storage).
-define(SYSTEM_PROC, system).
-define(TIMEOUT, 1000).

% node server modules
-define(CORE_SERVER, storage_core_srv).
-define(DIST_SERVER, storage_dist_srv).
-define(HTTP_SERVER, storage_http_srv).


-record(file, {
			   		v_path	= "/empty",	% string			// THIS AND THIS
			   		owner_id,			% string (uuid)		// FORMS PRIMARY KEY

					local_id,			% string (uuid) 	// **ONLY INTERNAL USE !!!!**

					size,				% in bytes
					last_access			% util:timestamp()
					
					}).

-record(request, {
				  	v_path = "/empty",	% string
					user_id,			% string

					action,				% atom read | write | create | delete | move | list
					broadcast = true,	% atom true | false
					options				% request-specific record
					}).

-record(create_opts, {
					  	force_loc = false,	% atom true | false
						data				% raw data
						}).

-record(write_opts, {
						v_path = false,	% false = no update
						data = false	%
						}).

-record(move_opts, {
						new_loc		% atom (node)
						}).


%% new, simplified request structure

-record(rreq, {
	v_path,	% v_path = string()
	action,	% action = get | put | del

	user_id,	% user_id = string()

	put_path = none,	% put_path = string(), new v_path
	put_loct = none,	% put_loct = atom(), node()
	put_data = none,	% put_data = binary()

	options = none		% reserved for future use

	}).
