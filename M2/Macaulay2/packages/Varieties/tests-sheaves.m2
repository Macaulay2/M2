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

TEST /// -- degreeOnCurve, the degree of a vector bundle on a curve,
  -- in projective space or weighted projective space.
  R1 = ZZ/2[x,y,z];
  X1 = Proj R1;
  R2 = R1/(x^6*y+y^6*z+z^6*x);
  X2 = Proj R2;
  S2 = canonicalBundle X2;
  assert(degreeOnCurve S2 == 28) -- The curve X2 has genus g = 15, so deg(K_(X2)) = 2g-2 = 28.
  S3 = pullback(tangentSheaf X1, X2);
  assert(degreeOnCurve S3 == 21) -- Here det(T_(X1)) = O(3), which has degree (3)(7) = 21 on the curve X2.
  R4 = ZZ/31991[x0,x1,x2,Degrees=>{2,5,7}]/(x0^6-3*x1*x2);
  X4 = Proj R4;
  S4 = OO_X4(-3);
  assert(degreeOnCurve S4 == -18/35) -- Since the line bundle O(1) has degree 12/((2)(5)(7)) = 6/35 on the curve X4.
///

TEST /// -- The degree of a coherent sheaf on a closed subspace of projective space, or of weighted projective space.
-- Not to be confused with the degree of a line bundle on a curve.
  R = ZZ/101[x_0..x_2];
  V = Proj R;
  S1 = OO_V(-5);
  assert(degree S1 == 1)
  M2 = R^1/(x_0^7-5*x_1^7+7*x_2^7);
  S2 = sheaf M2;
  assert(degree S2 == 7)
  R3 = R/(x_0^3+x_1^3+x_2^3);
  X3 = Proj R3;
  S3 = directImage(cotangentSheaf X3,V);
  assert(degree S3 == 3)
  R4 = ZZ/2[x,y,z,w,Degrees=>{4,5,6,7}];
  X4 = Proj R4;
  S4 = OO_X4^1;
  assert(degree S4 == 1/840)
///
