%% @author Michal Liszcz
%% @doc testing framework fixture that sets up tested cluster

-module(fixture).

-export([ setup/0, teardown/1, config/2 ]).


config(Option, Config) ->
	{ok, Value} = dict:find(Option, Config),
	Value.

% cwd MUST be restored if we still want to
% load modules from elrang binary path pa
context_run(ContextDir, Fun) ->
	{ok, Cwd} = file:get_cwd(),
	shell_default:cd(ContextDir),
	Fun(),
	shell_default:cd(Cwd).

setup() ->

	{ok, ConfigList} = file:consult("test.config"),
	Config = dict:from_list(ConfigList),

	erlang:set_cookie(node(), config(cookie, Config)),

	filelib:ensure_dir(config(logfile, Config)),

	TempFile = "/tmp/storage_benchmark_test_file",
	os:cmd(io_lib:format("openssl rand -out ~s $(( ~p ))", [TempFile, config(file_size, Config)])),

	context_run(config(cluster, Config), fun() ->
		Clustertool = "./cluster.sh ",
		os:cmd(Clustertool++" stop"),
		case config(setup_clean, Config) of
			true  -> os:cmd(Clustertool++" clean");
			false -> pass
		end,
		os:cmd(Clustertool++" start"),
		timer:sleep(config(start_delay, Config))
	end),

	dict:append(file_name, TempFile, Config).


teardown(Config) ->

	context_run(config(cluster, Config), fun() ->
		Clustertool = "./cluster.sh",
		os:cmd(Clustertool++" stop"),
		case config(teardown_clean, Config) of
			true  -> os:cmd(Clustertool++" clean");
			false -> pass
		end
	end),

	ok.
