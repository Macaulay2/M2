# it's tricky to debug, because breakpoints change the checksum of the
# memory map with the code in it!

display/i $pc
b dumpdata.c:248
# b main
# b dumpdata
b loaddata
b warning
b exit
run dump "hi there"
c
run load
