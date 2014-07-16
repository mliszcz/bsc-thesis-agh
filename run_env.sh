#!/bin/bash

COOKIE=ciacho
NAME=ds

erl  +hms 700000000 -setcookie $COOKIE -sname $NAME -pa storage/ebin 
