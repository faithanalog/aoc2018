#!/bin/sh
cat input | awk '
    BEGIN {
        sum = 0
    }
    {
        sum = sum + $0
        print sum
    }
    END {
        print sum
    }
'
