
if [ -n "$1" ]; then
	curl -XDELETE localhost:8090/"$1" \
		 -H "Authorization: HMAC 1:82f63b78"
else
	echo "usage: $0 virtual_path";
fi
