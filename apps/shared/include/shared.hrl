%% @author Michal
%% @doc Shared data structures


-define(STORAGE_PROC, storage).
-define(SYSTEM_PROC, system).
-define(TIMEOUT, 1000).

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
