file ../bin/Macaulay2
set environment LD_LIBRARY_PATH /home/geometry/dan/src/M2/Macaulay2/lib
set environment LOADDATA_IGNORE_CHECKSUMS yes
dir ../e ../d ../../gc ../../factory ../../libfac ../../gmp
handle SIGSEGV nostop pass noprint

# this breakpoint is where smashed objects are detected
# b dbg_mlc.c:530

# b GB_gbprocess
# ignore 1 0d1523
# run setup.m2 bug-tony.m2

# this breakpoint is where bad handles are detected
# b x_system.cpp:59