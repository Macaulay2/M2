  -- taken as 'doit 1000' from roos.m2
--gbTrace = 3
kk = ZZ/31
R = kk[x,y,z,u,v]
J = ideal(x^2+y*z,y*v,z^2-x*v)
h = x*y+y^2+x*u+y*u+z*u+x*v+z*v
J1 = J + ideal(h);
A = (ring J1)/J1;
time C = res(coker vars A, LengthLimit=>6)
betti C
gbTrace = 3
time C = res(coker matrix entries vars A, LengthLimit=>5, Strategy=>1)
-- Local Variables:
-- compile-command: "make -C $M2BUILDDIR/Macaulay2/packages/Macaulay2Doc/test/slow roos2.out"
-- End:
