dir ../e ../d ../../gmp-2.0.2/mpn
b main
b trap
b main_inits
b actors4_setupargv
run
del 1
# set var traphandle=264801
# set var trapset=0x0016dad1
continue
p system_argv
p *system_argv
c
p system_argv
p *system_argv
c
