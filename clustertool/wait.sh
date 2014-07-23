#!/bin/bash

CWD="$( cd "$( dirname "$0" )" && pwd )"

NODENUM=$(find $CWD -maxdepth 1 -mindepth 1 -type d | wc -l)

for node in $(find $CWD -maxdepth 1 -mindepth 1 -type d)
do
	printf "waiting for $node to configure ..."
	while true
	do
		COUNT1=$(grep -c "has joined the cluster" $node/log/erlang.log.1 2>/dev/null)
		COUNT2=$(grep -c "has joined the cluster" $node/log/erlang.log.2 2>/dev/null)
		COUNT3=$(grep -c "has joined the cluster" $node/log/erlang.log.3 2>/dev/null)
		COUNT4=$(grep -c "has joined the cluster" $node/log/erlang.log.3 2>/dev/null)
		COUNT=$(( COUNT1 + COUNT2 + COUNT3 + COUNT4 ))

		(( COUNT % NODENUM == 0 )) && break
		sleep 1
	done
	printf " done!\n"
done
