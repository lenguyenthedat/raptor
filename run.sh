#!/usr/bin/env bash

ALL_PASS=true
for t in vtd original bayes
do
    .cabal-sandbox/bin/raptor sg 3 $t ./test
    CHANGED=$(git --no-pager diff --ignore-space-change ./test/Result/$t/Raptor_sg.csv)
    
    if [ -n "$CHANGED" ]; then
        echo "test case for $t doesn't pass"
        $ALL_PASS=false
    fi 
done

if $ALL_PASS; then
    echo "all test case pass"
fi

