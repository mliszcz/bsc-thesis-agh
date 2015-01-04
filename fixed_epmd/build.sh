#!/bin/bash

erlc -I . -Derlang_daemon_port=4369 -Depmd_dist_high=9101 -Depmd_dist_low=9101 erl_epmd.erl
