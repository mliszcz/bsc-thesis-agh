
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
	printf "$1\t[OK]\n"
}

function fail {
	printf "$1\t[FAIL] result: $2\n"
	exit 1
}

STORAGE_NODE="localhost:8090"

TEST_INPUT="/tmp/storage_accept_in"
TEST_OUTPUT="/tmp/storage_accept_out"
TEST_REMOTE="storage/test/accept/test_file.dat"

rm -f $TEST_INPUT > /dev/null 2>&1
rm -f $TEST_OUTPUT > /dev/null 2>&1

echo $(random 1342177) >> $TEST_INPUT		# 1.28 MB
RESULT=$(curl -XPUT --data-binary @"$TEST_INPUT" "$STORAGE_NODE"/"$TEST_REMOTE" 2>/dev/null)
[ "$RESULT" == "HTTP/1.0 201 Created" ] && pass "PUT" || fail "PUT" "$RESULT"


RESULT=$(wget -O "$TEST_OUTPUT" "$STORAGE_NODE"/"$TEST_REMOTE" > /dev/null 2>&1)
diff "$TEST_INPUT" "$TEST_OUTPUT" >/dev/null && pass "GET" || fail "GET" "diffrent files"


echo $(random 1342177) >> $TEST_INPUT		# 2.56 MB
RESULT=$(curl -XPOST --data-binary @"$TEST_INPUT" "$STORAGE_NODE"/"$TEST_REMOTE" 2>/dev/null)
[ "$RESULT" == "HTTP/1.1 202 Accepted" ] && pass "POST" || fail "POST" "$RESULT"


RESULT=$(wget -O "$TEST_OUTPUT" "$STORAGE_NODE"/"$TEST_REMOTE" > /dev/null 2>&1)
diff "$TEST_INPUT" "$TEST_OUTPUT" >/dev/null && pass "GET" || fail "GET" "diffrent files"


RESULT=$(curl -XDELETE "$STORAGE_NODE"/"$TEST_REMOTE" 2>/dev/null)
[ "$RESULT" == "HTTP/1.1 202 Accepted" ] && pass "DELETE" || fail "DELETE" "$RESULT"


RESULT=$(curl "$STORAGE_NODE"/"$TEST_REMOTE" 2>/dev/null)
[ "$RESULT" == "HTTP/1.0 404 Not Found" ] && pass "GET" || fail "GET" "$RESULT"
