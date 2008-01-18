A = ZZ/103[x,y,z];
J = ideal(x^3,y^4,z^5);
B = A/J;
f = matrix {{27*x^2-19*z^2, 38*x^2*y+47*z^3},
      { -5*x^2+z^2, -37*x^2*y+51*x*y^2-36*y^3+26*x^2*z-38*x*y*z-17*y^2*z+17*x*z^2-11*y*z^2+8*z^3},
      {x^2-x*y, z^3}};
M = cokernel f;
N = B^1/(x^2 + z^2,y^3 - 2*z^3);
time E = Ext(M,N);
     -- used 3.32 seconds in version 0.9.92
t = tally degrees target presentation E
u = new Tally from {{-3, -7} => 7, {-3, -6} => 7, {0, 1} => 3, {0, 2} => 4, {-4, -9} => 5, {-4, -8} => 2, {-4, -7} => 1, {-1, -2} => 4, {-2, -5} => 2,
        {-2, -4} => 11}
assert ( t === u )
-- Local Variables:
-- compile-command: "make -C $M2BUILDDIR/Macaulay2/packages/Macaulay2Doc/test ext-total.out"
-- End:
