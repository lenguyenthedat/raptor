#!/usr/bin/env bash

ALL_PASS=true
for t in vtd original bayes
do
    ./dist/build/raptor/raptor sg 3 $t ./Test
    CHANGED=$(git --no-pager diff --ignore-space-change ./Test/Result/$t/Raptor_sg.csv)
    
    if [ -n "$CHANGED" ]; then
        echo "test case for $t doesn't pass"
        $ALL_PASS=false
    fi 
done

if $ALL_PASS; then
    echo "all test case pass"
fi

