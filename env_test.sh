#!/bin/bash

COOKIE=ciastko
NAME=client

cp storage/ebin/storage.beam tests/ebin
erl -setcookie $COOKIE -sname $NAME -pa tests/ebin 
