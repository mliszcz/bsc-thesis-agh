#!/bin/bash

COOKIE=ciacho
NAME=ds

erl -setcookie $COOKIE -sname $NAME -pa apps/shared/ebin apps/*/ebin -s storage_core_srv start_link
