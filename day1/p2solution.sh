#!/bin/sh
cat input \
    | sed -n '
        # Read in input and then print it forever
        H
        $ {
            x
            : lp
            p
            b lp
        }
    ' \
    | awk '
    BEGIN {
        sum = 0
        seen[0] = 1
    }
    /[0-9]/ {
        sum = sum + $0
        # We`ve already seen this number
        if (seen[sum] == 1) {
            print sum
            exit
        }
        seen[sum] = 1
    }' 
