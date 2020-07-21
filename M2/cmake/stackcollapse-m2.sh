#!/bin/sh

uniq -c | \
    sed 's/\(\.\.\/\)*\(M2\/\)\?\(Macaulay2\/\)\?//g' -  | \
    sed 's/^[\t ]*\([0-9]\+\)[\t ]\+\(M2;.*\)/\2 \1/g' - | \
    flamegraph.pl --title "$TITLE" $@
