
if [ -z $1 ]; then
	echo "usage $0 cluster/out/dir"
	exit 1
fi

OUT_DIR=$1/storage_cluster

rm -rf $OUT_DIR 2>/dev/null
mkdir -p $OUT_DIR

source cluster.properties

NODENUM=1
PORTNUM=$LOW_PORT

while [ $(( $NODES )) -ne 0 ];  do

	NAME=$PREFIX$NODENUM
	TARG=$OUT_DIR/$NAME

	CURR=$(pwd) ; cd ..
	make release NAME=$NAME COOKIE=$COOKIE
	cd $CURR

	cp -r ../storage/rel/storage $TARG

	rm $TARG/node.config 2>/dev/null
	touch $TARG/node.config

	echo '{app_log_level, info}.'				> $TARG/node.config
	echo '{core_work_dir, "work_dir"}.'			> $TARG/node.config
	echo '{core_storage_quota, $QUOTA}.	% 1GB'	> $TARG/node.config
	echo '{dist_initial_node, "$INIT_NODE"}.'	> $TARG/node.config
	echo '{http_port, $PORTNUM}.'				> $TARG/node.config

	echo "node $NAME created in $TARG!"
	
	(( NODES-- ))
	(( NODENUM++ ))
	(( PORTNUM++))

done
