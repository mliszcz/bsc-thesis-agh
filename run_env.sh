#!/bin/bash

COOKIE=ciacho
NAME=ds

erl -setcookie $COOKIE -sname $NAME -pa apps/*/ebin 
