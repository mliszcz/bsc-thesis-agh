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
* `HEAD` - search (work in progress)

#### `/storage` context
Example

```
POST /storage/path/to/my/file.dat HTTP/1.1
Host: ds-01.storage.example.com:9001
Authorization: HMAC 0147d6e0a2d028d24438c90c27e6d149:6c51adc384572536d9c8a9dbcfbebf590942771f
Content-Type: application/octet-stream
Content-Length: 2048
<<binary data>>
```

where:
* *path/to/my/file.dat* is virtual file path
* *0147d6e0a2d028d24438c90c27e6d149* is user id
* *6c51adc384572536d9c8a9dbcfbebf590942771f* is *HMAC*  
  HMAC is calculated from request body (method, path, user id) and hashed using md5(*password*) as a key  
  e.g.: `hmac_sha1(md5("secret"), "POSTpath/to/my/file.dat0147d6e0a2d028d24438c90c27e6d149") = 6c51adc384572536d9c8a9dbcfbebf590942771f`

#### `/user` context

**NOT SUPPORTED YET**

Create user (derp:secret) request

```
POST /user/derp HTTP/1.1
Host: ds-01.storage.example.com:9001
Content-Type: text/plain; charset=UTF-8
Content-Length: 6
secret
```

In response we get new user id

```
HTTP/1.0 201 Created
Content-Type: text/plain; charset=UTF-8
Content-Length: 32
0147d6e0a2d028d24438c90c27e6d149
```