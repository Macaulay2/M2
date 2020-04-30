cat testtimes | grep -v " - " | grep -v Start | grep -v Passed | \
    sed 's/^[^\s\t]*\s*Test\s*[^\s\t]*:\s*//' - | \
    sed 's/^\([^ ]*\)\s*\.*[^0-9]*\([0-9]*\.[0-9]*\)\s*sec/\2\t\1/g' - | \
    sort -n

#cat testtimes | grep Passed | \
#    sed 's/^[^\s\t]*\s*Test\s*[^\s\t]*:\s*//' - | \
#    sed 's/^\([^ ]*\)\s*\.*\s*Passed\s*\([0-9]*\.[0-9]*\)\s*sec/\2\t\1/g' - | \
#    sort -n
