#!/bin/bash

COOKIE=ciacho
NAME=ds

erl -setcookie $COOKIE -sname $NAME -pa storage/ebin erlang-sqlite3/ebin Emysql/ebin
