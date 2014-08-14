
if [ -n "$2" ]; then

	hmac=$(echo -n "update1${2}" | openssl sha1 -hmac "82f63b78")
	hmac=${hmac##"(stdin)= "}

	curl -XPUT localhost:8090/storage/"$2"		\
		 -H "Authorization: HMAC 1:$hmac"		\
		 --data-binary @"$1"
else
	echo "usage: $0 local_file virtual_path";
fi
