file ../bin/Macaulay2
set environment LD_LIBRARY_PATH ../lib
dir ../e ../d ../../gc ../../factory ../../libfac ../../gmp
b exit
handle SIGSEGV nostop pass noprint
