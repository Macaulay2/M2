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

-- these two things check the definition of modulo
assert( image (gens P * f) == xP )
assert( isInjective inducedMap(ambient xP / xP,coker f,gens P) )

f0096 = map(target f,, {
	  {1, 0, 0, 0, 0, y, -y, x, x, -x, -x, v, 0, x, 0, 0, u, -x, 0, 0, 0}, 
	  {1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0},
	  {0, 1, -1, 0, 0, 0, 0, 0, 0, 0, 0, 1, -v, -u, -y, -x, 0, 0, 0, 0, 0},
	  {0, 0, 0, 1, -1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 1, -v, -u, -y, -x}})
f0097 = map(target f,, {
	  {1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0},
	  {1, 0, 0, -v, -u, y, -y, x, x, x, -x, -x, -x},
	  {0, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0},
	  {0, 0, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0}})
ftrimmed = map(target f,,  {
	  {1, 0, 0, v, u, y, x}, 
	  {1, 0, 0, 0, 0, 0, 0}, 
	  {0, 1, 0, 0, 0, 0, 0}, 
	  {0, 0, 1, 0, 0, 0, 0}})

assert( image f0097 == image f )
assert( image f0096 == image f )
assert( image ftrimmed == image f )


-- this used to not work because of a problem with Schreyer orders:
    R = QQ[x,y,z]
    M = R^1/ideal(vars R)
    F = res M
    S = R/(x^2-y^2)
    N = S^1 /ideal(x^3,x*y^2,y^3)
    lim = 10
    G = res (N, LengthLimit => lim)
    J=ker G.dd_lim
-- the following 2 lines are commented out, as this is not supposed to be allowed behavior!
--    G#(lim+1) = J
--    G.dd#(lim+1) = inducedMap(G_lim,G_(lim+1))
    id_(F**S) ** id_G


end

-*
    u123$ ./Macaulay2-0.9.96/bin/M2 --no-loaddata -q ~/src/M2/Macaulay2/packages/Macaulay2Doc/test/tensor.m2 -e 'exit 0'
    Macaulay 2, version 0.9.96
    with packages: Classic, Core, Elimination, IntegralClosure, LLLBases, Parsing, PrimaryDecomposition, SchurRings, TangentCone
    rank source f = 21

    u123$ ./Macaulay2-0.9.97/bin/M2 --no-loaddata -q ~/src/M2/Macaulay2/packages/Macaulay2Doc/test/tensor.m2 -e 'exit 0'
    Macaulay 2, version 0.9.97
    with packages: Classic, Core, Elimination, IntegralClosure, LLLBases, Parsing, PrimaryDecomposition, SchurRings, TangentCone
    rank source f = 13
*-

-- Local Variables:
-- compile-command: "make -C $M2BUILDDIR/Macaulay2/packages/Macaulay2Doc/test tensor.out"
-- End:
