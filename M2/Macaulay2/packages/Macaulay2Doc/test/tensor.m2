kk=ZZ/101
R=kk[x,y,u,v,SkewCommutative=>true]
i=ideal(x+u*v)
M=coker gens i
F=res(M, LengthLimit=>5)
P = image F.dd_2
xP = ideal vars R * P
P1 = P/xP
presentation P1
prune P1
f = modulo ( gens P, gens xP)
assert( gens P * f % xP == 0 )
assert( image (gens P * f) == xP )
stderr << "rank source f = " << rank source f << endl
assert( rank source f == 21 )				    -- that's just what it happens to be...

{*

    u123$ ./Macaulay2-0.9.96/bin/M2 --no-loaddata -q ~/src/M2/Macaulay2/packages/Macaulay2Doc/test/tensor.m2 -e 'exit 0'
    Macaulay 2, version 0.9.96
    with packages: Classic, Core, Elimination, IntegralClosure, LLLBases, Parsing, PrimaryDecomposition, SchurRings, TangentCone
    rank source f = 21

    u123$ ./Macaulay2-0.9.97/bin/M2 --no-loaddata -q ~/src/M2/Macaulay2/packages/Macaulay2Doc/test/tensor.m2 -e 'exit 0'
    Macaulay 2, version 0.9.97
    with packages: Classic, Core, Elimination, IntegralClosure, LLLBases, Parsing, PrimaryDecomposition, SchurRings, TangentCone
    rank source f = 13
*}

end
-- Local Variables:
-- compile-command: "make -C $M2BUILDDIR/Macaulay2/test tensor.out"
-- End:
