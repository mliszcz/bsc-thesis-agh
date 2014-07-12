
if [ -n "$1" ]; then
	curl -XDELETE localhost:8090/"$1"
else
	echo "usage: $0 virtual_path";
fi
