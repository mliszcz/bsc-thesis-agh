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

### REST API
One can access files using HTTP requests and REST API. Supported methods:
* `POST` - create
* `GET` - read
* `PUT` - update
* `DELETE` - delete
* `HEAD` - search (in progress)

#### POST
```
POST /storage/path/to/my/file.dat HTTP/1.0
Authorization: HMAC 123456:0xfbdb1d1b18aa6c08324b7d64b71fb76370690e1d
Content-Type: application/octet-stream
Content-Length: 2048
<<binary data>>
```
where:
* file *path/to/my/file.dat* will be created
* *123456* is User ID
* HMAC is calculated from concatenated words *'create'*, path and user id using md5(*password*) as key  
  `hmac_sha1(md5("secret"), "createpath/to/my/file.dat123456") = 0xfbdb1d1b18aa6c08324b7d64b71fb76370690e1d`

