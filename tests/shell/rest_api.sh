
function curl_put {
	# $1 - node host
	# $2 - virtual path
	# $3 - local file
	curl -XPUT --data-binary @"$3" "$1"/"$2" > /dev/null 2>&1
}

function curl_post {
	# $1 - node host
	# $2 - virtual path
	# $3 - local file
	curl -XPOST --data-binary @"$3" "$1"/"$2" > /dev/null 2>&1
}

function curl_get {
	# $1 - node host
	# $2 - virtual path
	# $3 - local file
	curl  --output "$3" "$1"/"$2" > /dev/null 2>&1
}

function curl_delete {
	# $1 - node host
	# $2 - virtual path
	curl -XDELETE "$1"/"$2" > /dev/null 2>&1
}
