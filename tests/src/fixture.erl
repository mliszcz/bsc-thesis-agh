
-module(fixture).

-export([ setup/0, teardown/1, config/2 ]).


config(Option, Config) ->
	{ok, Value} = dict:find(Option, Config),
	Value.


setup() ->

	{ok, ConfigList} = file:consult("test.config"),
	Config = dict:from_list(ConfigList),

	TempFile = "/tmp/storage_benchmark_test_file",
	os:cmd(io_lib:format("openssl rand -out ~s $(( ~p ))", [TempFile, config(file_size, Config)])),

	shell_default:cd(config(cluster, Config)),
	Clustertool = "./cluster.sh",

	os:cmd(Clustertool++" stop"),
	case config(setup_clean, Config) of
		true  -> os:cmd(Clustertool++" clean");
		false -> pass
	end,
	os:cmd(Clustertool++" start"),

	dict:append(file_name, TempFile, Config).


teardown(Config) ->

	shell_default:cd(config(cluster, Config)),
	Clustertool = "./cluster.sh",

	os:cmd(Clustertool++" stop"),
	case config(teardown_clean, Config) of
		true  -> os:cmd(Clustertool++" clean");
		false -> pass
	end,

	ok.
