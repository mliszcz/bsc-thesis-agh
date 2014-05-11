
if [ -n "$2" ]; then
	curl -XPUT --data-binary @"$1" localhost:8090/"$2"
else
	echo "usage: $0 local_file virtual_path";
fi
