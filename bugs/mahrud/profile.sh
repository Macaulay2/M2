# cat profile.raw | sort | \
    uniq -c | \
    sed 's/\(\.\.\/\)*\(M2\/\)\?\(Macaulay2\/\)\?//g' -  | \
    sed 's/^[\t ]*\([0-9]\+\)[\t ]\+\(M2;.*\)/\2 \1/g' - | \
    flamegraph.pl \
	--title "A Profile of \`ninja install-Macaulay2Doc\` (debug build)" - # > profile.svg
