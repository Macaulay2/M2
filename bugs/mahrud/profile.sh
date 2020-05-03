cat profile.raw | sort | uniq -c | \
    sed 's/\(\.\.\/\)*\(M2\/\)\?\(Macaulay2\/\)\?//g' -  | \
    sed 's/^[\t ]*\([0-9]\+\)[\t ]\+\(M2;.*\)/\2 \1/g' - | \
    sort -n | \
    flamegraph.pl \
	--title "A Profile of \`M2 -q -e \"exit 0\"\` (debug build)" - > profile.svg
