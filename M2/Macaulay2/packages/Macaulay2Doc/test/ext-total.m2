A = ZZ/103[x,y,z];
J = ideal(x^3,y^4,z^5)
B = A/J;
f = matrix {{27*x^2+49*x*y-14*y^2-23*x*z-6*y*z-19*z^2, 38*x^2*y-34*x*y^2+4*y^3+x^2*z+16*x*y*z-y^2*z-5*x*z^2-6*y*z^2+47*z^3},
      {-5*x^2+44*x*y+38*y^2+40*x*z+15*y*z+4*z^2, -37*x^2*y+51*x*y^2-36*y^3+26*x^2*z-38*x*y*z-17*y^2*z+17*x*z^2-11*y*z^2+8*z^3},
      {21*x^2-30*x*y+32*y^2-47*x*z+7*y*z-50*z^2, -6*x^2*y-14*x*y^2-26*y^3-7*x^2*z+41*x*y*z+50*y^2*z+26*x*z^2+46*y*z^2-44*z^3}}
M = cokernel f;
N = B^1/(x^2 + z^2,y^3 - 2*z^3)
time E = Ext(M,N);
     -- used 3.32 seconds in version 0.9.92
t = tally degrees target presentation E
u = new Tally from { {-3, -7} => 6, {-3, -6} => 6, {0, 1} => 2, {0, 2} => 7, 
     {-4, -9} => 6, {-4, -8} => 2, {-1, -2} => 4, {-2, -4} => 16 }
assert ( t === u )

