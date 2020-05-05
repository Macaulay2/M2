#!/bin/sh

uniq -c | \ # add repeat counter
    sed 's/\(\.\.\/\)*\(M2\/\)\?\(Macaulay2\/\)\?//g' -  | \ # trim useless directories
    sed 's/^[\t ]*\([0-9]\+\)[\t ]\+\(M2;.*\)/\2 \1/g' - | \ # correct format
    flamegraph.pl $@
