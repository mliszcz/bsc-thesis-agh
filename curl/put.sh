
if [ -n "$2" ]; then

	echo -n "value" | openssl sha1 -hmac "key"

	
	curl -XPUT localhost:8090/storage/"$2"		\
		 -H "Authorization: HMAC 1:82f63b78"	\
		 --data-binary @"$1"
else
	echo "usage: $0 local_file virtual_path";
fi
