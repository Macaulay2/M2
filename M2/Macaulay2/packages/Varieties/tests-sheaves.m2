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
  S2 = canonicalSheaf X2;
  assert(degreeOnCurve S2 == 28) -- The curve X2 has genus g = 15, so deg(K_(X2)) = 2g-2 = 28.
  -- S3 = pullback(tangentSheaf X1, X2);
  -- assert(degreeOnCurve S3 == 21) -- Here det(T_(X1)) = O(3), which has degree (3)(7) = 21 on the curve X2.
  R4 = ZZ/31991[x0,x1,x2,Degrees=>{2,5,7}]/(x0^6-3*x1*x2);
  X4 = Proj R4;
  S4 = OO_X4(-3);
  assert(degreeOnCurve S4 == -18/35) -- Since the line bundle O(1) has degree 12/((2)(5)(7)) = 6/35 on the curve X4.
///

TEST /// -- Check isLocallyFree, for modules or coherent sheaves. A locally free sheaf need not have constant rank.
  R1 = QQ[x,y];
  R2 = R1/(x*(x-1), x*y);
  X2 = Spec R2; -- The disjoint union of an affine line and a point.
  M1 = R2^1/(x); S1 = sheaf M1;
  M2 = R2^1/(x-1,y); S2 = sheaf M2;
  M3 = R2^1/(x,y); S3=sheaf M3;
  assert(isLocallyFree M1)
  assert(isLocallyFree M2)
  assert(not isLocallyFree M3);
  assert(isLocallyFree(M1++M2++M2))
  assert(isLocallyFree S1)
  assert(isLocallyFree S2)
  assert(not isLocallyFree S3)
  assert(isLocallyFree(S1++S2++S2))
///

TEST ///
  R4 = QQ[u,v,w];
  R5 = R4/(u*v, u*w);
  X5 = Proj R5; -- The disjoint union of a projective line and a point.
  M6 = R5^1/(u); S6 = sheaf M6;
  M7 = R5^1/(v,w); S7 = sheaf M7;
  M8 = R5^1/(u,v); S8 = sheaf M8;
  assert(isLocallyFree S6)
  assert(isLocallyFree S7)
  assert(not isLocallyFree S8)
  assert(isLocallyFree(S6++S7++S7))
  assert(not isLocallyFree(M6++M7++M7))
///

TEST ///
  R0 = QQ[x,y,z];
  R1 = R0/(x*y+z^2);
  X1 = Proj R1;
  assert(dualizingSheaf X1 == OO_X1(-1))
  use R0;
  R2 = R0/(x*y);
  X2 = Proj R2;
  assert(dualizingSheaf X2 == OO_X2(-1))
  R3 = ZZ/31991[x,y,z,w,Degrees=>{1,1,2,3}];
  R4 = R3/(x^6+y^6+z^3+w^2);
  X4 = Proj R4;
  assert(dualizingSheaf X4 == OO_X4(-1))
///

