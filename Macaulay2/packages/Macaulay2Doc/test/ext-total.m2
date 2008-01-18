A = ZZ/103[x,y,z];
J = ideal(x^3,y^4,z^5)
B = A/J;
f = random (B^3, B^{-2,-3})
M = cokernel f;
N = B^1/(x^2 + z^2,y^3 - 2*z^3)
E = Ext(M,N);
tally degrees target presentation E
