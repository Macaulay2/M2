# it's tricky to debug, because breakpoints change the checksum of the
# memory map with the code in it!

b dumpdata
b loaddata
b warning
b exit
run dump "hi there"
c
c
run load
