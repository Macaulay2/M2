Q = ZZ/101[x,y,z]
f = {x^3-y*z^2,y^7,z^4}
c = #f
R = Q/f;
k = coker vars R;
N = cokernel random (R^3, R^{-3,-2})
E = resolution pushForward( map(R,Q), N )
s = f / (g -> nullhomotopy (g*id_E));
aa = nullhomotopy( s_0 * s_1 + s_1 * s_0 )

T = ZZ/101[x,y,z,X_0 .. X_(c-1),Degrees=>{3:1,c:2}];
dT = substitute(sum E.dd, T);
s'' = s / (t -> substitute(sum t, T));
aa'' = substitute(sum aa,T)
DMT = T^(apply(sort select(keys E, i -> class i === ZZ), i -> rank E_i : -i));
DT = map(DMT, DMT, transpose( - dT + sum(c, i-> X_i * s''_i ) + X_0 * X_1 * aa''), Degree => 1);

assert ( DT*DT + X_0*(x^3-y*z^2) + X_1*y^7 + X_2 * z^4 == 0 )

S = ZZ/101[X_0 .. X_(c-1),Degrees=>{c:{2}}];
D = substitute(DT,S)

--d = substitute(sum E.dd, S);
--s' = s / (t -> substitute(sum t, S));
--DM = S^(apply(sort select(keys E, i -> class i === ZZ), i -> rank E_i : -i));
--D = map(DM, DM, transpose( d - sum(c, i-> X_i * s'_i )), Degree => 1);

extNk = cokernel gens gb relations prune homology(D,D)

res extNk
betti res extNk
hilbertPolynomial extNk
hilbertSeries extNk
assert( E.dd^2 == 0 )
scan(c, i -> assert( E.dd*s_i + s_i*E.dd == f_i ))
-- scan(c, i -> assert( s_i^2 == 0 ))
-- scan(c, i -> scan(i, j -> assert( s_i*s_j + s_j*s_i == 0 )))
assert isHomogeneous D
assert( D*D == 0 )
scan(8, i -> assert ( rank source basis Ext^i(N,k) == rank source basis(i,extNk) ) )
-- exit
-- h = relations extNk
-- ev = toList select(0 .. rank target h - 1, i -> even first (degrees target h)#i)
-- Hev = cokernel gens gb submatrix(h,ev,)
-- res Hev
-- betti res Hev
-- hilbertPolynomial Hev
-- hilbertSeries Hev
-- od = toList select(0 .. rank target h - 1, i -> odd  first (degrees target h)#i)
-- Hod = cokernel gens gb submatrix(h,od,)
-- res Hod
-- betti res Hod
-- hilbertPolynomial Hod
-- hilbertSeries Hod
