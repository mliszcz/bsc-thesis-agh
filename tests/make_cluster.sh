

if [ -z $1 ]; then
	echo "usage $0 cluster/out/dir"
	exit 1
fi


OUT_DIR=$1/storage_cluster
rm -rf $OUT_DIR 2>/dev/null
mkdir -p $OUT_DIR


# default options
NODES=3
QUOTA=1073741824	# 1 GB
PREFIX=ds			# this will generate names $(PREFIX)1, $(PREFIX)2, ...
COOKIE=ciastko
INIT_NODE="${PREFIX}1@$(hostname -s)"
PORTS=9001

source cluster.properties


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
{core_storage_quota,	$QUOTA}.		% 1GB
{dist_initial_node,		"$INIT_NODE"}.	
{http_port,				$PORTNUM}.
/EOF

	echo "node $NAME created in $TARG!"
	
	(( NODENUM++ ))
	(( PORTNUM++))

done
