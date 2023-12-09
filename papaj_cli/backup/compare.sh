#!/bin/bash

echo "Test 1"
fpclock "./rpn $1 $2" -u ms
sleep 1
echo "Test 2"
fpclock "rpn $1 $2" -u ms
