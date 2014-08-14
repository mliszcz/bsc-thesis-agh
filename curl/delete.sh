
if [ -n "$1" ]; then

	hmac=$(echo -n "delete1${2}" | openssl sha1 -hmac "82f63b78")
	hmac=${hmac##"(stdin)= "}

	curl -XDELETE localhost:8090/"$1" \
		 -H "Authorization: HMAC 1:$hmac"
else
	echo "usage: $0 virtual_path";
fi
