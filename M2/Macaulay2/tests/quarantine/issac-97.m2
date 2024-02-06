-- Moved from engine/gb-bench.m2
needs "../engine/raw-util.m2"

---------------------------------------
-- issac-97, ZZ, inhomogeneous
---------------------------------------
-- Compute a lex GB for this one.
-- FIXME: this one doesn't yet work... either memory leak, or required a ton of memory
mo = rawMonomialOrdering { Lex => 4}
R = polyring2(rawZZ(), (symbol w, symbol x, symbol y, symbol z), mo)

M = rawMatrix1(R^1, 4, (
        -2*w^2+9*w*x+8*x^2+9*w*y+9*x*y+6*y^2-7*w*z-3*x*z-7*y*z-6*z^2-4*w+8*x+4*y+8*z+2,
        3*w^2-5*w*x+4*x^2-3*w*y+2*x*y+9*y^2-6*w*z-2*x*z+6*y*z+7*z^2+9*w+7*x+5*y+7*z+5,
        7*w^2+5*w*x+2*x^2+3*w*y+9*x*y-4*y^2-5*w*z-7*x*z-5*y*z-4*z^2-5*w+4*x+6*y-9*z+2,
        8*w^2+5*w*x+5*x^2-4*w*y+2*x*y+7*y^2+2*w*z-7*x*z-8*y*z+7*z^2+3*w-7*x-7*y-8*z+8),
      0)
Gcomp = rawGB(M,false,0,{0},false,0,0,0,10)
rawStartComputation Gcomp
mgb = rawGBGetMatrix(Gcomp)
--assert(rawNumberOfColumns mgb === ???)
rawSource mgb

-*
i6 : rawStartComputation Gcomp
GC Warning: Out of Memory!  Trying to continue...
GC Warning: Out of Memory!  Trying to continue...
GC Warning: Out of Memory!  Trying to continue...
GC Warning: Out of Memory!  Trying to continue...
GC Warning: Out of Memory!  Trying to continue...
GC Warning: Out of Memory!  Trying to continue...
GC Warning: Out of Memory!  Trying to continue...
GC Warning: Out of Memory!  Trying to continue...
GC Warning: Out of Memory!  Trying to continue...
GC Warning: Out of Memory!  Trying to continue...
GC Warning: Out of Memory!  Trying to continue...
GC Warning: Out of Memory!  Trying to continue...
GC Warning: Out of Memory!  Trying to continue...
GC Warning: Out of Memory!  Trying to continue...
GC Warning: Out of Memory!  Trying to continue...
GC Warning: Out of Memory!  Trying to continue...
GC Warning: Out of Memory!  Trying to continue...
GC Warning: Out of Memory!  Trying to continue...
GC Warning: Out of Memory! Heap size: 1010 MiB. Returning NULL!
GC_debug_malloc(340424) returning NULL (../../Macaulay2/e/gmp-util.h:11)
-- SIGSEGV
-* stack trace, pid: 2537064
 0# basic_stacktrace at /usr/include/boost/stacktrace/stacktrace.hpp:126
 1# segv_handler at /home/mahrud/Projects/M2/PD/M2/BUILD/build/../../Macaulay2/bin/main.cpp:240
 2# 0x00007F16979ECA70 in /lib64/libc.so.6
 3# 0x00007F1697B15513 in /lib64/libc.so.6
 4# mpz_reallocate_limbs at /home/mahrud/Projects/M2/PD/M2/BUILD/build/../../Macaulay2/e/gmp-util.h:13
 5# RingZZ::mult(ring_elem, ring_elem) const at /home/mahrud/Projects/M2/PD/M2/BUILD/build/../../Macaulay2/e/ZZ.cpp:262
 6# Ring::mult_to(ring_elem&, ring_elem) const at /home/mahrud/Projects/M2/PD/M2/BUILD/build/../../Macaulay2/e/ring.cpp:185
 7# GBRing::gbvector_mult_by_coeff(gbvector const*, ring_elem) at /home/mahrud/Projects/M2/PD/M2/BUILD/build/../../Macaulay2/e/gbring.cpp:563
 8# GBRing::gbvector_replace_2by2_ZZ(FreeModule const*, FreeModule const*, gbvector*&, gbvector*&, gbvector*&, gbvector*&) at /home/mahrud/Projects/M2/PD/M2/BUILD/build/../../Macaulay2/e/gbring.cpp:1136
 9# gbA::reduce_ZZ(gbA::spair*) at /home/mahrud/Projects/M2/PD/M2/BUILD/build/../../Macaulay2/e/gb-default.cpp:1494
10# gbA::reduceit(gbA::spair*) at /home/mahrud/Projects/M2/PD/M2/BUILD/build/../../Macaulay2/e/gb-default.cpp:1563
11# gbA::process_spair(gbA::spair*) at /home/mahrud/Projects/M2/PD/M2/BUILD/build/../../Macaulay2/e/gb-default.cpp:2281
12# gbA::do_computation() at /home/mahrud/Projects/M2/PD/M2/BUILD/build/../../Macaulay2/e/gb-default.cpp:2500
13# gbA::start_computation() at /home/mahrud/Projects/M2/PD/M2/BUILD/build/../../Macaulay2/e/gb-default.cpp:2575
14# GBProxy::start_computation() at /home/mahrud/Projects/M2/PD/M2/BUILD/build/../../Macaulay2/e/comp-gb-proxy.hpp:55
15# rawStartComputation at /home/mahrud/Projects/M2/PD/M2/BUILD/build/../../Macaulay2/e/x-gb.cpp:607
16# interface_rawStartComputation at /home/mahrud/Projects/M2/PD/M2/Macaulay2/d/interface.dd:3428
17# evaluate_evalraw at /home/mahrud/Projects/M2/PD/M2/Macaulay2/d/evaluate.d:1293
*-
