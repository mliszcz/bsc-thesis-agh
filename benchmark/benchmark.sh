#!/bin/bash

# 
# benchmark.sh storage benchmark script
# author: Michal Liszcz
# 
# this script generates clusters of various size (1, 5, 10, ..)
# and then runs series of tests against each generated cluster
#
# number of handled requests (CRUD) per second is measured
# requests are spawned from configurable number of threads
# various file sizes are used during the tests (4KB - 1GB)
# 

TMP="/tmp"
CWD="$( cd "$( dirname "$0" )/.." && pwd )"
HOSTNAME=$(hostname -s)

function create_cluster {
	# creates cluster of size $1
	cd $CWD/clustertool
	cp make_cluster.properties make_cluster.properties.old
	echo "NODES=$1" >> make_cluster.properties
	echo "COOKIE=benchmark" >> make_cluster.properties
	echo "PREFIX=ds" >> make_cluster.properties

	printf "creating cluster ..."
	res=$(./make_cluster.sh $TMP)
	printf " done!\n"
	
	mv make_cluster.properties.old make_cluster.properties
	cd $CWD 
}

function create_test_config {
	cd $CWD
	cat > test.config <<EOF
{gateway, "ds1@$HOSTNAME"}.
{cluster, "$CWD/storage_cluster"}.
{cookie, 'benchmark'}.

{threads, $1}.
{iterations, $2}.

{file_size, $3}.

{setup_clean, $4}.
{teardown_clean, $5}.

{start_delay, 3000}.

{verbose, true}.
{logfile, "$CWD/benchmark/out/$6"}.
EOF
}

function execute_beam {
	cd $CWD
	erl -sname client -pa tests/ebin storage/ebin -s $1 $2
}

_4K=$((   4    * 1024 ))
_512K=$(( 512  * 1024 ))
_1M=$((   1024 * 1024 ))
_32M=$((  32   * $_1M ))
_128M=$(( 128  * $_1M ))
_512M=$(( 512  * $_1M ))

function prepare_fixture {
	# $1 - nodes
	# $2 - threads
	# $3 - iterations
	# $4 - size var name
	# $5, $6 - setup / teardown cleanup

	create_cluster $1
	create_test_config $2 $3 ${!4} \
		true true "$1n_$2t$4.log"
}

function test_create {
	prepare_fixture 5 3 10 _32M true true
	execute_beam test_base shell_create
}

test_create
