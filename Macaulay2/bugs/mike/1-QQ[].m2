    Macaulay 2, version 0.9.20
    with packages: Classic, Elimination, LLLBases, PrimaryDecomposition, SchurRings, TangentCone

    i1 : R = QQ[]

    o1 = R

    o1 : PolynomialRing

    i2 : matrix (R,{{2/1},{1/2}})

    Program received signal SIGSEGV, Segmentation fault.
    0x40565fca in __gmpq_set () from /capybara/lib/libgmp.so.3
    (gdb) where
    #0  0x40565fca in __gmpq_set () from /capybara/lib/libgmp.so.3
    #1  0x00000001 in ?? ()
    #2  0xbffe9cf8 in ?? ()
    #3  0x081d181c in QQ::from_rational (this=0x8206042, a=0xbffe9d30)
	at ../../../../Macaulay2/e/QQ.cpp:135
    #4  0x081d1836 in QQ::from_rational () at ../../../../Macaulay2/e/QQ.cpp:137
    (gdb) 
