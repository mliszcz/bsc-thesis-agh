#!/bin/bash

COOKIE=ciacho
NAME=http
erl -setcookie $COOKIE -sname $NAME -pa apps/storage_http/ebin -s storage_http_srv start
