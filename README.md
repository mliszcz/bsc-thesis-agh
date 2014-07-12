inz
===

Praca Inżynierska

### URGENT TODOS

* reserve cap for *update* requests
* make core server stateful (stop using global vars)

### NOTE
Project is **UNDER DEVELOPEMENT** and should not be used in prod env.
This is not final way to start the server and will change many times.

### How to run it

```
git clone git@github.com:mliszcz/inz.git
./rebar compile
./run_env.sh
>application:start(storage).
```

