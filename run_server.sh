#!/bin/bash

erl -setcookie ciacho -sname ds -pa apps/*/ebin -s storage_core_srv start_link
