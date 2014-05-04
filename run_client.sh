#!/bin/bash

COOKIE=ciacho
NAME=client

erl -setcookie $COOKIE -sname $NAME -pa apps/shared/ebin apps/storage_client/ebin
