
all: clean compile

rel: release

compile:
	./rebar compile

clean:
	./rebar clean

release: clean
	./rebar compile generate
	cp node.config storage/rel/storage/node.config
	sed -i 's/-name.*/-sname $(NAME)/g' storage/rel/storage/releases/0.1.0/vm.args
	sed -i 's/-setcookie.*/-setcookie $(COOKIE)/g' storage/rel/storage/releases/0.1.0/vm.args
