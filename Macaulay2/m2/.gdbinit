dir ../e ../d ../../gc ../../factory ../../libfac ../../gmp
handle SIGSEGV nostop pass noprint
b main
run setup.m2 '-eGF(4)'
b ffops.cc:49
continue
