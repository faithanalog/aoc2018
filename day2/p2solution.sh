#!/bin/sh

cat input \
    | awk '
        BEGIN {
            n = 0
        }
        {
            lines[n] = $0
            n = n + 1
        }
        END {
            for (kln0 = 0; kln0 < n - 1; kln0++) {
                ln0 = lines[kln0]
                for (kln1 = kln0 + 1; kln1 < n; kln1++) {
                    ln1 = lines[kln1]
                    diff = 0
                    commonCharacters = ""
                    for (i = 1; i <= length(ln0); i++) {
                        c0 = substr(ln0, i, 1)
                        c1 = substr(ln1, i, 1)
                        if (c0 != c1) {
                            diff = diff + 1
                        } else {
                            commonCharacters = commonCharacters c0
                        }
                    }
                    if (diff == 1) {
                        print commonCharacters
                    }
                }
            }
        }
    '
