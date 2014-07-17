
#!/bin/bash

# 
# accept.sh storage node acceptance test
# author: Michal Liszcz
# 
# test PUT, POST, GET & DELETE methods
#

function random {
	cat /dev/urandom | tr -dc 'a-zA-Z0-9' | fold -w $1 | head -n 1
}

function pass {
	printf "$1\t[OK]"
}

function fail {
	printf "$1\t[FAIL] result: $2"
	exit 1
}

STORAGE_NODE="localhost:8090"

TEST_INPUT="/tmp/storage_accept_in"
TEST_OUTPUT="/tmp/storage_accept_out"
TEST_REMOTE="storage/test/accept/test_file.dat"

rm -f $TEST_INPUT > /dev/null 2>&1
rm -f $TEST_OUTPUT > /dev/null 2>&1

echo $(random 1342177) >> $TEST_INPUT		# 128 MB
RESULT=$(curl -XPUT --data-binary @"$TEST_INPUT" "$STORAGE_NODE"/"$TEST_REMOTE" 2>/dev/null)

if [ "$RESULT" == "HTTP/1.0 201 Created" ] # && pass "PUT" || fail "PUT" "$RESULT"
then
	echo "PUT      [OK]"
else
	echo "PUT      [FAIL] result: $RESULT"
	exit 1
fi


RESULT=$(wget -O "$TEST_OUTPUT" "$STORAGE_NODE"/"$TEST_REMOTE" > /dev/null 2>&1)

if diff "$TEST_INPUT" "$TEST_OUTPUT" >/dev/null
then
   echo "GET      [OK]"
else
   echo "GET      [FAIL]"
   exit 1
fi


echo $(random 1342177) >> $TEST_INPUT		# 256 MB
RESULT=$(curl -XPOST --data-binary @"$TEST_INPUT" "$STORAGE_NODE"/"$TEST_REMOTE" 2>/dev/null)

if [ "$RESULT" == "HTTP/1.1 202 Accepted" ]
then
	echo "POST     [OK]"
else
	echo "POST     [FAIL] result: $RESULT"
	exit 1
fi


RESULT=$(wget -O "$TEST_OUTPUT" "$STORAGE_NODE"/"$TEST_REMOTE" > /dev/null 2>&1)

if diff "$TEST_INPUT" "$TEST_OUTPUT" >/dev/null
then
   echo "GET      [OK]"
else
   echo "GET      [FAIL]"
   exit 1
fi


RESULT=$(curl -XDELETE "$STORAGE_NODE"/"$TEST_REMOTE" 2>/dev/null)

if [ "$RESULT" == "HTTP/1.1 202 Accepted" ]
then
	echo "DELETE   [OK]"
else
	echo "DELETE   [FAIL] result: $RESULT"
	exit 1
fi


# GET of empty file should be tested here
# but timeout currently is not working :)
