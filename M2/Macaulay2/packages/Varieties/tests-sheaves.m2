TEST /// -- c.f. https://github.com/Macaulay2/M2/issues/1358
  R = QQ[x,y,z]
  I = ideal(z^2-y,y*z-x,y^2-x*z)
  M = coker (res I).dd_2
  F = sheaf(Spec R, M)
  assert(M == HH^0(F))
  assert(0 == HH^1(F))
///

TEST /// -- Hilbert polynomials, for projective and weighted projective varieties.
  R1 = ZZ/31991[u0,u1,u2,u3,u4]/(u2*u4-u3^2,u0*u3-u1*u2,u0*u4-u1*u3);
  X1 = Proj R1; -- a scroll of degree 3 in P^4, isomorphic to P^2 blown up at 1 point.
  assert(codim X1 == 2)
  P1 = hilbertPolynomial ProjectiveSpace(1);
  P2 = hilbertPolynomial ProjectiveSpace(2);
  assert(hilbertPolynomial X1 == -2*P1+3*P2)
  hilb1 = hilbertPolynomial(X1, Projective=>false); -- An element of the ring QQ[i].
  i = (class hilb1)_0;
  assert(hilb1 == (3/2)*i^2+(5/2)*i+1)
  R2 = QQ[x,y,z,Degrees=>{1,2,3}];
  X2 = Proj R2;
  hilb2 = hilbertPolynomial X2;
  i = (class hilb2)_0; -- This may not be the "same" ring QQ[i] as before.
  assert(hilb2 == (1/12)*i^2+(1/2)*i+(47/72))
///
