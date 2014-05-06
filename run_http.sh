#!/bin/bash

COOKIE=ciacho
NAME=http
erl -setcookie $COOKIE -sname $NAME -pa apps/shared/ebin apps/storage_client/ebin apps/storage_http/ebin -s storage_http_srv start
