inz
===

Praca Inzynierska

### URGENT TODOS
* make use of core gen_server state (stop using global vars)
* handle constraint violations and other db errors

### cluster deployment
* change clustertool generator options in *clustertool/make_cluster.properties*
* generate cluster using *clustertool/make_cluster.sh*
* *node.config* and *releases/0.1.0/vm.args* in each node dir may require some changes

### third-party code
* erlang-sqlite  
  https://github.com/alexeyr/erlang-sqlite3
*