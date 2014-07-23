#!/bin/bash

# 
# make_cluster.sh storage cluster generator
# author: Michal Liszcz
# 
# inside directory passed as argument, creates a storage_cluster dir, which contains
# a number of storage nodes, each packed as a release, with own Erlang VM and config
# then, copies cluster.sh, which allows you to control the cluster, into to that dir
#

if [ -z $1 ]; then
	echo "usage $0 cluster/out/dir"
	exit 1
fi


OUT_DIR=$1/storage_cluster
rm -rf $OUT_DIR 2>/dev/null
mkdir -p $OUT_DIR


cp cluster.sh $OUT_DIR/cluster.sh
chmod +x $OUT_DIR/cluster.sh

cp wait.sh $OUT_DIR/wait.sh
chmod +x $OUT_DIR/wait.sh


# just in case ...
cp ../storage/test/accept.sh $OUT_DIR/accept.sh


# default options
NODES=3
QUOTA=1073741824	# 1 GB
MEMORY=134217728	# 128 MB
PREFIX=ds			# this will generate names like $(PREFIX)1, $(PREFIX)2, ...
COOKIE=ciastko
INIT_NODE="${PREFIX}1@$(hostname -s)"
PORTS=9001

source make_cluster.properties


sed -i "s/LEADER=.*/LEADER=${INIT_NODE%%@$(hostname -s)}/g" $OUT_DIR/wait.sh


CURR=$(pwd) ; cd ..
make release NAME="derp" COOKIE=$COOKIE
cd $CURR


NODENUM=1
PORTNUM=$PORTS


while [ $(( NODES-- )) -ne 0 ]; do

	NAME=${PREFIX}${NODENUM}
	TARG=$OUT_DIR/$NAME

	cp -r ../storage/rel/storage $TARG

	sed -i "s/-s\?name.*/-sname $NAME/g" $TARG/releases/0.1.0/vm.args

	rm $TARG/node.config 2>/dev/null
	touch $TARG/node.config

	cat >> $TARG/node.config <</EOF
{app_log_level,			info}.
{core_work_dir,			"work_dir"}.
{core_storage_quota,	$QUOTA}.
{core_memory_quota,		$MEMORY}.
{dist_initial_node,		"$INIT_NODE"}.	
{http_port,				$PORTNUM}.
/EOF

	echo "node $NAME created in $TARG!"
	
	(( NODENUM++ ))
	(( PORTNUM++))

done
