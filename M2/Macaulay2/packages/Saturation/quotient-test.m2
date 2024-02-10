TEST ///
  R  = ZZ[x, y];
  I  = ideal(6*x^3, 9*x*y, 8*y^2);
  J1 = ideal(-3, x^2);
  J2 = ideal(4*y);
  assert(intersect(I:J1, I:J2) == ideal(8*y^2, 3*x*y, x*y^2, 6*x^3))
  assert(I : (J1+J2) == ideal(8*y^2, 3*x*y, 6*x^3))
  for strategy in {Quotient, Iterate} do
  assert(quotient(I, J1, Strategy => strategy) == ideal(8*y^2, 3*x*y, x*y^2, 6*x^3))
///

TEST ///
  -- quotient(Ideal,Ideal)
  -- quotient(Ideal,RingElement)
  -- options to test: DegreeLimit, BasisElementLimit, PairLimit,
  --    MinimalGenerators,
  --    Strategy=>Iterate, Strategy=>Linear
  R = ZZ/101[a..d]
  I1 = monomialCurveIdeal(R, {1,3,7})
  I2 = ideal((gens I1)_{0,1})
  I3 = quotient(I2,I1)
  I4 = quotient(I2,I3)
  I5 = quotient(I2, c)

  assert(I2 == intersect(I3,I4))
  assert(ideal(c,d) == quotient(I2, I5))
  assert(I3 == I2 : I1)

--  assert(ideal(d) + I2 ==
--       quotient(I2,I1,DegreeLimit=>1)
--       )

  assert(I3 == quotient(I2,I1,Strategy=>Iterate))

  quotient(I2,I1,MinimalGenerators=>false)
--  stderr << \"  -- this fails currently\" << endl
--  assert(I5 ==
--       quotient(I2, c,Strategy=>Linear)
--       )
///

TEST ///
  -- quotient(Ideal,Ideal)
  -- quotient(Ideal,RingElement)
  -- options to test: DegreeLimit, BasisElementLimit, PairLimit,
  --    MinimalGenerators,
  --    Strategy=>Iterate, Strategy=>Linear
  R = ZZ/101[vars(0..3)]/(a*d)
  I1 = ideal(a^3, b*d)
  I2 = ideal(I1_0)

  I3 = quotient(I2,I1)
  assert(I3 == ideal(a))
  I4 = quotient(I2,I3)
  assert(I4 == ideal(a^2,d))
  I5 = quotient(I2, d)
  assert(I5 == ideal(a))
///

TEST ///
  --    quotient(Module,RingElement)
  --    quotient(Module,Ideal)

  -- This tests 'quotmod0' (default)
  R = ZZ/101[vars(0..4)]/e
  m = matrix{{a,c},{b,d}}
  M = subquotient(m_{0}, a^2**m_{0} | a*b**m_{1})
  J = ideal(a)
  Q1 = quotient(M,J)

  -- Now try the iterative version
  Q2 = quotient(M,J,Strategy=>Iterate)
  assert(Q1 == Q2)

  m = gens M
  F = target m
  mm = generators M | relations M
  j = transpose gens J
  g = (j ** F) | (target j ** mm)
  h = syz gb(g,
	  Strategy=>LongPolynomial,
	  SyzygyRows=>numgens F,
	  Syzygies=>true)
  trim subquotient(h % M.relations,
             M.relations)

///

TEST ///
  --    quotient(Module,Module)
  R = ZZ/101[a..d]
  M = image matrix{{a,b},{c,d}}
  N = super M
  I = quotient(M,N)
  assert(I == quotient(M,N,Strategy=>Iterate))
  assert(I == M : N)
  assert(I == ann(N/M))
///

TEST ///
  --    quotient(Module,Module)
  R = ZZ/101[vars(0..14)]
  N = coker genericMatrix(R,a,3,5)
  M = image N_{}
  I = quotient(M,N)
  assert(I == quotient(M,N,Strategy=>Iterate))
  assert(I == M : N)
  assert(I == ann(N/M))
///

TEST ///
  R = ZZ/101[a..d]
  M = coker matrix{{a,b},{c,d}}
  m1 = basis(2,M)
  M1 = subquotient(matrix m1, relations M)
  Q1 = M1 : a
  Q2 = quotient(M1,ideal(a,b,c,d),Strategy=>Iterate)
  assert(Q1 == Q2)
///

TEST ///
  R = ZZ/101[a..d]
  mrels = matrix{{a,b},{c,d}}
  mgens = matrix(R,{{1,0},{0,0}})
  M = trim subquotient(mgens, mrels)
  Q1 = quotient(image M_{},a*d-b*c)
  assert(Q1 == super M)  -- fails: bug in == ...
///

TEST ///
  -- Test of stopping conditions
  R = QQ[a..d]
  I = ideal(a^5,b^5,c^5,d^5)
  -- check that cached results are not inappropriately used
  debug Saturation; cacheHit QuotientComputation := C -> error 0;
  elapsedTime assert({9,4,4,5,6,7,8,8,9} == apply({0,1,2,17,18,19,26,27,28}, i -> numgens quotient(I, a^1+b^1+c^1+d^1, PairLimit         => i)))
  elapsedTime assert({21,4,4,4,21}       == apply({0,1,5,22,23},             i -> numgens quotient(I, a^2+b^2+c^2+d^2, BasisElementLimit => i)))
  elapsedTime assert({4,4,6,12,13,13}    == apply({4,5,6,7,8,{}},            i -> numgens quotient(I, a^3+b^3+c^3+d^3, DegreeLimit       => i)))
  -- check that repeating the above does not perform any more computation
  elapsedTime assert all({0,1,2,17,18,19,26,27,28},  i -> try (quotient(I, a^1+b^1+c^1+d^1, PairLimit         => i); false) else true)
  elapsedTime assert all({0,1,5,22,23},              i -> try (quotient(I, a^2+b^2+c^2+d^2, BasisElementLimit => i); false) else true)
  elapsedTime assert all({4,5,6,7,8,{}},             i -> try (quotient(I, a^3+b^3+c^3+d^3, DegreeLimit       => i); false) else true)
  --
  cacheHit QuotientComputation := lookup(cacheHit, Computation)
///

TEST ///
  -- Tests for ZZ
  assert same( ideal 4 : 6,  ideal 4 : ideal 6, ideal 2)
  assert same(4 * ZZ^2 : 6, 4 * ZZ^2 : ideal 6, 2 * ZZ^2)
  assert(4 * ZZ^2 : 6 * ZZ^2 == ideal 2)
  assert same(saturate( ideal 2, 3), saturate( ideal 2, ideal 3), ideal 2)
  assert same(saturate(2 * ZZ^2, 3), saturate(2 * ZZ^2, ideal 3), 2 * ZZ^2)
  assert(annihilator ideal 2 == ideal 0)
  -- Tests for Number
  R = QQ[x]
  ideal x : -1
  ideal x : ideal(-1_R)
  module ideal x : -1
  ideal x : ideal(-1_R)
  saturate(ideal x, -1)
  saturate(module ideal x, -1)
///

TEST ///
  R = ZZ[x, y];
  (I,  J)  = ideal \ ({x^2, x*y, y^2}, {x, y});
  (I', J') = monomialIdeal \ (I, J);
  assert(I':J == I:J')
  assert(I:J == ideal(I':J'))
  assert(monomialIdeal(I:J) == I':J')
  -- TODO: test MonomialIdeal : RingElement
///

TEST /// -- unnecessary groebner bases should not be computed
  importFrom_"FGLM" {"katsura"}
  I = katsura(8, MonomialOrder=>Lex)
  R = ring I
  J = ideal(I_0, I_1)
  elapsedTime assert(quotient(J, I) == J)
  elapsedTime assert(quotient(J, I) == J)
  elapsedTime assert(quotient(ideal J_*, I) == J)
  elapsedTime assert(quotient(module ideal J_*, I) == module J)
  elapsedTime assert(quotient(ideal I_*, I_0) == 1)
  elapsedTime assert(quotient(ideal I_*, J) == 1)
  elapsedTime assert(quotient(module ideal I_*, J) == R^1)
  elapsedTime assert(quotient(module ideal I_*, module J) == 1)
  elapsedTime assert(saturate(ideal J_*, I) == J)
  elapsedTime assert(saturate(ideal I_*, J) == 1)
  elapsedTime assert(saturate(module ideal J_*, I) == module J)
  elapsedTime assert(saturate(module ideal I_*, J) == R^1)
///

TEST ///
  -- Reported by Robert Lax, communicated by Dan Grayson
  -- cf https://github.com/Macaulay2/M2/issues/1660
  -- A = toField(QQ[w,DegreeRank=>0]/(w^3-1)); -- FIXME: this also causes a different issue
  A = QQ[w,DegreeRank=>0]/(w^3-1);
  R = A[x,y,z,Degrees=>{4,4,4}];
  S = A[s,t];
  qmap = map(S,R,{t^4-w*s^4,s*t*(t^2+s^2),s*t*(t^2-s^2)});
  I = kernel qmap;
  B = ideal gens R;
  assert(#(quotient(ideal I_*, B, Strategy => Iterate))_* == 1)
  -- assert(#(quotient(ideal I_*, B, Strategy => Quotient))_* == 1) -- FIXME
///
