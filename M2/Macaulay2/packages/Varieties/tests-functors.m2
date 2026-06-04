-- TODO: perhaps move these to the documentation
TEST ///
  -- The following examples appear in:
  -- Gregory G. Smith, Computing global extension modules,
  -- Journal of Symbolic Computation, 29 (2000) 729-746.

  -- Example 4.1: the bounds can be sharp.
  S = QQ[w,x,y,z];
  X = Proj S;
  I = monomialCurveIdeal(S,{1,3,4})
  N = S^1/I;
  -- Ext^i(F, G(>=b)) should return a graded module that is correct in degrees at least b.
  assert(prune truncate(-1, Ext^1(OO_X, N^~(>=-1))) == prune truncate(-1, Ext^1(truncate(3, S^1), N)))
  assert(prune truncate(0,  Ext^1(OO_X, N^~(>= 0))) == prune truncate(0,  Ext^1(truncate(2, S^1), N)))
  assert(prune truncate(0,  Ext^1(OO_X, N^~(>= 0))) != prune truncate(0,  Ext^1(truncate(1, S^1), N)))
///

TEST ///
  -- Example 4.2: locally free sheaves and global Ext.
  S = ZZ/32003[u,v,w,x,y,z];
  I = minors(2,genericSymmetricMatrix(S,u,3));
  X = variety I;
  R = ring X;
  Omega = cotangentSheaf X;
  OmegaDual = dual Omega;
  assert(prune truncate(0, Ext^1(OmegaDual, OO_X^1(>= 0))) == prune truncate (0, Ext^1(OO_X^1, Omega(>= 0))))
///

TEST ///
  -- Example 4.3: Serre-Grothendieck duality.
  S = QQ[v,w,x,y,z];
  X = variety ideal(w*x+y*z,w*y+x*z);
  R = ring X;
  omega = OO_X^{-1};
  G = sheaf cokernel genericSymmetricMatrix(R,R_0,2);
  assert(Ext^2(G,omega) == dual HH^0(G))
  assert(Ext^1(G,omega) == dual HH^1(G))
  assert(Ext^0(G,omega) == dual HH^2(G))
///

TEST ///
  -- The sheaf cohomology command hh^1(S(*)) on two weighted projective surfaces.
  -- This also tests reflexiveDifferentials X for these surfaces, which are not smooth even as stacks.
  R0 = QQ;
  R1 = R0[x,y,z,w,Degrees=>{1,1,2,3}];
  R2 = R1/(w^2+z^3+x^5*y);
  X2 = Proj R2;
  S2 = reflexiveDifferentials X2;
  S2series = (hh^1(S2(*)))_5;
  -- That should be a Divide representing sum_a h^1(S2(a)) T^a.
  S2poly = numerator S2series;
  -- That should be a Laurent polynomial in ZZ[T].
  T = (degreesRing R2)_0;
  assert(S2poly == T^(-1)+T^0+T^1)
  use R1;
  R3 = R1/(w^2+z^3+x^5*y+x^4*z);
  X3 = Proj R3;
  S3 = reflexiveDifferentials X3;
  S3series = (hh^1(S3(*)))_5;
  -- That should be a Divide representing sum_a h^1(S3(a)) T^a.
  S3poly = numerator S3series;
  assert(S3poly == T^0)
///

TEST ///
  -- The sheaf cohomology command HH^1(S(>=b)) on the same two weighted projective surfaces.
  R0 = QQ;
  R1 = R0[x,y,z,w,Degrees=>{1,1,2,3}];
  R2 = R1/(w^2+z^3+x^5*y);
  X2 = Proj R2;
  S2 = reflexiveDifferentials X2;
  T = (degreesRing R2)_0;
  M2 = HH^1(S2(>=-4));
  -- That graded R2-module is only guaranteed to be correct in degrees at least -4.
  M2trunc = truncate(-4,M2);
  assert(hilbertSeries(M2trunc, Order => 10) == T^(-1)+T^0+T^1)
  use R1;
  R3 = R1/(w^2+z^3+x^5*y+x^4*z);
  X3 = Proj R3;
  S3 = reflexiveDifferentials X3;
  M3 = HH^1(S3(>=-4));
  -- That graded R2-module is only guaranteed to be correct in degrees at least -4.
  M3trunc = truncate(-4,M3);
  assert(hilbertSeries(M3trunc, Order => 10) == T^0)
///

TEST ///
  -- hh^i of a sheaf on an AffineVariety.
  R0 = ZZ/31991[x,y];
  X0 = Spec R0;
  I1 = ideal(x^2-y^2-x-1, (x+y)*(x-2*y)-3);
  M1 = R0^1/I1;
  S1 = sheaf M1;
  assert(hh^0 S1 == 3)
  M2 = ideal(x^3+y^4-1);
  S2 = sheaf M2;
  assert(hh^0 S2 == infinity)
  assert(hh^1 S2 == 0)
///

TEST /// -- The (topological) Euler characteristic of projective varieties,
  -- Euler characteristic of coherent sheaves, and hh^i(F,a,b).
  R0 = QQ[x,y,z,w];
  I1 = ideal(x^3*y+y^3*z+z^3*w+w^3*x);
  R1 = R0/I1;
  X1 = Proj R1; -- a K3 surface
  assert(euler X1 == 24)
  R2 = degreesRing R0; -- The ring ZZ[T].
  T = R2_0;
  S3 = cotangentSheaf X1;
  assert(euler(S3,-3,3) == 16*T^(-3)-4*T^(-2)-16*T^(-1)-20*T^0-16*T^1-4*T^2+16*T^3)
  assert(hh^0(S3,-3,3) == 6*T^2+20*T^3);
  assert(hh^1(S3,-3,3) == 4*T^(-3)+10*T^(-2)+16*T^(-1)+20*T^0+16*T^1+10*T^2+4*T^3)
  assert(hh^2(S3,-3,3) == 20*T^(-3)+6*T^(-2));
///

TEST /// -- test euler and Hilbert polynomial on a non-standard graded ring
  A = QQ[a,b,c, Degrees => {1,2,3}]
  -- Note: currently Proj A must be called first.
  -- do we want euler or hilbertPolynomial to work regardless?
  X = Proj A
  assert(euler A == 1)
  assert(toString hilbertPolynomial A == "(1/12)*i^2+(1/2)*i+47/72")
///

end --


-- Tests for cohomology of projective varieties.
R0=QQ;
R3=R0[x0,x1,x2,x3,x4];
f3=x0^6+x1^6+x2^6+x3^6+x4^6; R4=R3/(ideal f3);
X3=Proj R3; X4=Proj R4;
S4=cotangentSheaf X4;
S5=OO_X4^1;
hh^2(S4(*))
hilbertSeries(HH^2(S4(>=-10)),Order=>20)
euler X4
hilbertPolynomial X4

-- Tests for cohomology of weighted projective varieties.
R5=ZZ/31991;
R6=R5[x,y,z,w,Degrees=>{1,1,2,3}];
X6=Proj R6;
degree X6
f6=w^2+z^3+x^5*y+x^4*z;
R7=R7/(ideal f1);
X7=Proj R7;
S7=OO_X7^1;
S8=reflexiveDifferentials X2;
hh^1(S8(*))
euler(S8,-10,10)
hilbertSeries(Ext^1(S7,S8(>=0)),Order=>10)
Ext^1(S7,S8)
euler X2

-- same, but one dimension higher?

-- Tests for cohomology of projective varieties.
R0=QQ;
R3=R0[x0,x1,x2,x3,x4,x5];
f3=x0^8+x1^8+x2^8+x3^8+x4^8+x5^8; R4=R3/(ideal f3);
X3=Proj R3; X4=Proj R4;
S4=cotangentSheaf X4;
S5=OO_X4^1;
hh^3(S4(*))
hilbertSeries(HH^3(S4(>=-10)),Order=>20)
euler X4
hilbertPolynomial X4

-- Tests for cohomology of weighted projective varieties.
R5=ZZ/31991;
R6=R5[x,y,z,w,Degrees=>{1,1,2,3}];
X6=Proj R6;
degree X6
f6=w^2+z^3+x^5*y+x^4*z;
R7=R7/(ideal f1);
X7=Proj R7;
S7=OO_X7^1;
S8=reflexiveDifferentials X2;
hh^1(S8(*))
euler(S8,-10,10)
hilbertSeries(Ext^1(S7,S8(>=0)),Order=>10)
Ext^1(S7,S8)
euler X2
