#!/bin/sh

cat input \
    | awk '
        {
            delete letters
            for (i = 1; i <= length($0); i++) {
                letter = substr($0, i, 1)
                letters[letter] = letters[letter] + 1
            }
            hasTwo = 0
            hasThree = 0
            for (l in letters) {
                if (letters[l] == 2) {
                    hasTwo = 1
                }
                if (letters[l] == 3) {
                    hasThree = 1
                }
            }
            twoTimes = twoTimes + hasTwo
            threeTimes = threeTimes + hasThree
        }
        END {
            print (twoTimes * threeTimes)
        }
    '
