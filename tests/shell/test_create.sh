
source ./rest_api.sh


THREADS=3
ITERATIONS=5
FILESIZE=512	# KB

TEST_INPUT="/tmp/storage_accept_in"
TEST_OUTPUT="/tmp/storage_accept_out"
TEST_REMOTE="storage/test/accept/test_file.dat"

rm -f $TEST_INPUT > /dev/null 2>&1
rm -f $TEST_OUTPUT > /dev/null 2>&1

function random_file {
	openssl rand -out $2 $(( $1 * 2**10 ))
}


function thread {

	# $1 - thread id

	local i=0

	local start=$(date +"%s.%N")

	while (( i < ITERATIONS ))
	do
		echo "thread $1 iter $i"
		sleep 2

		(( i++ ))
	done

	local end=$(date +"%s.%N")

	delta=$(echo "$end-$start" | bc)
	echo "thread $1 done in $delta"
	# echo "$(( end-start ))"

}




random_file $FILESIZE $TEST_INPUT

c=0

while (( c < THREADS ))
do
	thread $c &

	(( c++ ))
done

echo "loop done"

# utime="$( TIMEFORMAT='%lR';time ( ls ) 2>&1 1>/dev/null )"