
RELEASE_DIR=storage/rel/storage/releases/0.1.0

all: clean compile

rel: release

compile:
	./rebar compile

clean:
	./rebar clean

release: all
	./rebar generate
	cp node.config storage/rel/storage/node.config
	sed -i 's/-s\?name.*/-sname $(NAME)/g' $(RELEASE_DIR)/vm.args
	sed -i 's/-setcookie.*/-setcookie $(COOKIE)/g' $(RELEASE_DIR)/vm.args
