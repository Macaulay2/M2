Q = ZZ/101[a,b]
R = Q/(f1 = a^3, f2 = b^4)
N = prune (cokernel random (Q^3, Q^{-2,-2}) ** cokernel presentation R)
E = resolution N
d = E.dd
s1 = nullhomotopy (f1*id_E)
s2 = nullhomotopy (f2*id_E)
S = ZZ/101[X1,X2,Degrees=>{2,2}]
DM = S^{ rank E_0 : 0, rank E_1 : -1, rank E_2 : -2 }
Edd = substitute(sum E.dd, S)
S1 = substitute(sum s1, S)
S2 = substitute(sum s2, S)
D = map(DM, DM, transpose( Edd - X1 * S1 - X2 * S2 ), Degree => 1)
H = prune homology(D,D)

h = relations H

ev = toList select(0 .. rank target h - 1, i -> even first (degrees target h)#i)
Hev = cokernel submatrix(h,ev,)
res Hev
betti res Hev
hilbertPolynomial Hev
hilbertSeries Hev

od = toList select(0 .. rank target h - 1, i -> odd  first (degrees target h)#i)
Hod = cokernel submatrix(h,od,)
res Hod
betti res Hod
hilbertPolynomial Hod
hilbertSeries Hod

-- check it

assert( d^2 == 0 )
assert( d*s1 + s1*d == f1 )
assert( d*s2 + s2*d == f2 )
assert( s1^2 == 0 )
assert( s2^2 == 0 )
assert( s2*s1 + s1*s2 == 0 )

assert isHomogeneous D

N' = substitute(N,R)
k = coker vars R
scan(15, i -> assert ( rank source basis Ext^i(N',k) == rank source basis(i,H) ) )
