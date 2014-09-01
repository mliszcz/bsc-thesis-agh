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

if [ -z $1 ]; then
	echo "usage $0 cluster/out/dir"
	exit 1
fi

TMP=$1
CWD="$( cd "$( dirname "$0" )/.." && pwd )"
HOSTNAME=$(hostname -s)

function create_cluster {
	# creates cluster of size $1
	# $2 - storage space limit
	# $3 - per-handler memory limit

	# dk why this is needed, cause clustertool should remove old dir
	rm -rf $TMP/storage_cluster 2>/dev/null

	cd $CWD/clustertool
	cp make_cluster.properties make_cluster.properties.old
	echo "NODES=$1" >> make_cluster.properties
	echo "COOKIE=benchmark" >> make_cluster.properties
	echo "PREFIX=ds" >> make_cluster.properties
	echo "QUOTA=$2" >> make_cluster.properties
	echo "MEMORY=$3" >> make_cluster.properties

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
{cluster, "$TMP/storage_cluster"}.
{cookie, 'benchmark'}.

{threads, $1}.
{iterations, $2}.

{file_size, $3}.

{setup_clean, $4}.
{teardown_clean, $5}.

{start_delay, 8000}.

{verbose, true}.
{logfile, "$CWD/benchmark/out/$6"}.
EOF
}

function execute_beam {
	cd $CWD
	erl -sname client -setcookie benchmark -pa benchmark/ebin storage/ebin -s $1 $2
}

_4K=$((      4 * 1024 ))
_512K=$((  512 * 1024 ))
_1M=$((   1024 * 1024 ))
_4M=$((      4 * $_1M ))
_16M=$((    16 * $_1M ))
_32M=$((    32 * $_1M ))
_64M=$((    64 * $_1M ))
_128M=$((  128 * $_1M ))
_512M=$((  512 * $_1M ))

function prepare_fixture {
	# $1 - nodes
	# $2 - threads
	# $3 - iterations
	# $4 - size var name
	# $5, $6 - setup / teardown cleanup
	# $7 - logfile prefix

	# create_cluster $1 				# call it explicitly!
	create_test_config $2 $3 ${!4} \
		  $5 $6 "$7-$1n.log"
		# $5 $6 "$7-$1n_$2t$4.log"
}

function test_cycle {
	# $1 - nodes
	# $2 - threads
	# $3 - iterations
	# $4 - size var name

	# $5, $6 - storage and mem limit

	create_cluster $1 $5 $6

	cd $TMP/storage_cluster/ && ./cluster.sh clean && cd $CWD
	sleep 2
	# cd $TMP/storage_cluster/ && ./cluster.sh start && ./wait.sh && cd $CWD
	cd $TMP/storage_cluster/ && ./cluster.sh start && cd $CWD
	sleep 8

	prepare_fixture $1 $2 $3 $4 true false 'create'
	execute_beam test_base shell_create

	prepare_fixture $1 $2 $3 $4 false false 'read'
	execute_beam test_base shell_read

	prepare_fixture $1 $2 $3 $4 false true 'update'
	execute_beam test_base shell_update

	cd $TMP/storage_cluster/ && ./cluster.sh stop && cd $CWD
	cd $TMP/storage_cluster/ && ./cluster.sh clean && cd $CWD
}


# BENCHMARK PARAMETERS
NODES=(1 2 5 10 20 50)
THREADS=(1 2 5 10 20 50)
SIZES=(_4K _512K _1M _32M _128M _512M)

NODES=(1)
THREADS=(8)
SIZES=(_16M)
ITER_LIM=10

[[ -n "$2" ]] && NODES=($2)
[[ -n "$3" ]] && THREADS=($3)
[[ -n "$4" ]] && SIZES=($4)

[[ -n "$5" ]] && ITER_LIM=($5)

QUOTA=$(( 250 * 1024 * 1024 * 1024 ))	# 250 GB, max available disk space
MEMORY=$(( 5  * 1024 * 1024 * 1024 ))	# 5 GB, max available memory


for node in ${NODES[@]}
do
	for size in ${SIZES[@]}
	do
		for thread in ${THREADS[@]}
		do
			echo "running cycle: $node nodes, $size sample, $thread threads"

			# ITER=$(( QUOTA / ( ${!size} * thread ) ))
			# (( ITER > $ITER_LIM )) && ITER=$ITER_LIM
			ITER=$ITER_LIM
			echo "$ITER iterations"

			DSK_LIM=$(( QUOTA / node ))
			MEM_LIM=$(( MEMORY / ( 2 * thread ) ))

			test_cycle $node $thread $ITER $size $DSK_LIM $MEM_LIM

		done
	done
done


# mathematica-compatilbe results

for log in $(ls $CWD/benchmark/out)
do
	sed -i 's/\[/{/g' $CWD/benchmark/out/$log
	sed -i 's/\]/}/g' $CWD/benchmark/out/$log
done
