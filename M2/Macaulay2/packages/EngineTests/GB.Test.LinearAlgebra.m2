-- Tests for different variants

TEST /// -- basics of Algorithm => LinearAlgebra
  I = ideal vars(QQ[x])
  assert try (elapsedTime gb(I, Algorithm => LinearAlgebra); false) else true

  R = ZZ/32003[x]
  I = ideal vars R
  elapsedTime result = gens (G = gb(I, Algorithm => LinearAlgebra));
  assert(result == vars R)

  assert try(getChangeMatrix G; false) else true
  assert try(mingens G; false) else true
  assert try(syz G; false) else true
  assert try(a // G; false) else true
  assert try(a % G; false) else true
  assert try(leadTerm G; false) else true
  leadTerm gens G
  assert(numcols gens G == 1)
///

TEST /// -- Algorithm => LinearAlgebra, over finite field.
  kk = ZZ/101;
  R1 = kk[a..f];
  J1 = ideal(a*b*c-d*e*f, a^2*c-b^2*e, b*e^4 - d*f^4 + e^5)
  elapsedTime gbC = flatten entries gens (G = gb(ideal J1_*, Algorithm => LinearAlgebra));
  elapsedTime gbB = flatten entries groebnerBasis(ideal J1_*, Strategy => "F4");
  assert(gbC == gbB)

  -- TODO: is it ok to check Msolve while we do this?
  needsPackage "Msolve"
  elapsedTime gbD = flatten entries msolveGB(ideal J1_*, Threads => 8); -- slowest, due to translation, or what?
  assert(gbD == gbB)

  assert try(getChangeMatrix G; false) else true
  assert try(mingens G; false) else true
  assert try(syz G; false) else true
  assert try(a // G; false) else true
  assert try(a % G; false) else true
  assert try(leadTerm G; false) else true
  assert(numcols gens G == 7)
///

TEST /// -- Algorithm => LinearAlgebra, over finite field.
  kk = ZZ/101;
  R1 = kk[a..g, MonomialSize=>8];
  setRandomSeed 42
  J1 = ideal random(R1^1, R1^{-2, -2, -2, -3});
  elapsedTime gbA = flatten entries gens gb(ideal J1_*);
  elapsedTime gbC = flatten entries gens (G = gb(ideal J1_*, Algorithm => LinearAlgebra));
  elapsedTime gbB = flatten entries groebnerBasis(ideal J1_*, Strategy => "F4");
  assert(gbA == gbB)
  assert(gbA == gbC)

  -- TODO: is it ok to check Msolve while we do this?
  needsPackage "Msolve"
  elapsedTime gbD = flatten entries msolveGB(ideal J1_*, Threads => 8); -- slowest, due to translation, or what?
  assert(gbD == gbB)

  assert try(getChangeMatrix G; false) else true
  assert try(mingens G; false) else true
  assert try(syz G; false) else true
  assert try(a // G; false) else true
  assert try(a % G; false) else true
  assert try(leadTerm G; false) else true
  assert(numcols gens G == 13)
///

TEST /// -- Algorithm => LinearAlgebra, over prime finite field.
  kk = ZZ/101;
  R1 = kk[a..g, MonomialSize=>8];
  setRandomSeed 42
  J1 = ideal random(R1^1, R1^{-4,-4,-5,-5});
  elapsedTime gbC = flatten entries gens (G = gb(ideal J1_*, Algorithm => LinearAlgebra));
  elapsedTime gbB = flatten entries groebnerBasis(ideal J1_*, Strategy => "F4");
  assert(gbC == gbB)

  -- TODO: is it ok to check Msolve while we do this?
  needsPackage "Msolve"
  elapsedTime gbD = flatten entries msolveGB(ideal J1_*, Threads => 8); -- slowest, due to translation, or what?
  assert(gbD == gbB)

  assert try(getChangeMatrix G; false) else true
  assert try(mingens G; false) else true
  assert try(syz G; false) else true
  assert try(a // G; false) else true
  assert try(a % G; false) else true
  assert try(leadTerm G; false) else true
  assert(numcols gens G == 78)
///

TEST /// -- Algorithm => LinearAlgebra, over non-prime finite field.
  kk = GF 125;
  R1 = kk[a..g, MonomialSize=>8];
  setRandomSeed 42
  J1 = ideal random(R1^1, R1^{-4,-4,-5,-5});

  elapsedTime gbC = flatten entries gens gb(ideal J1_*, DegreeLimit => 10);
  elapsedTime gbB = flatten entries gens (G = gb(ideal J1_*, Algorithm => LinearAlgebra, DegreeLimit => 10));
  assert(gbC == gbB)

  -- the following appears to hang. BUG!! It should say it can't do it.
  --try(groebnerBasis(ideal J1_*, Strategy => "F4"); false) else true -- BUG: this should be disallowed...!
  needsPackage "Msolve"
  elapsedTime try(msolveGB(ideal J1_*, Threads => 8); false) else true -- correctly gives error
///

-*
  restart
*-
TEST /// -- hilbert driven gb computation, for default, Algorithm => LinearAlgebra
  setRandomSeed 42
  kk = ZZ/101
  R1 = kk[a..g, MonomialSize => 8];
  K1 = ideal (a^4, b^4, c^4, d^4)
  hfJ = poincare K1
  J1 = ideal random(R1^1, R1^{-4, -4, -4, -4});
  elapsedTime gbA = flatten entries gens gb(ideal J1_*, Hilbert => hfJ);
  elapsedTime gbB = flatten entries gens gb(ideal J1_*,
                                          Algorithm => LinearAlgebra,
                                          Hilbert => hfJ);
  elapsedTime gbB2 = flatten entries gens gb(ideal J1_*,
                                          Algorithm => LinearAlgebra);
  elapsedTime gbC = flatten entries groebnerBasis(ideal J1_*, Strategy => "F4");
  assert(gbA == gbB)
  assert(gbA == gbB2)
  assert(gbA == gbC)

  needsPackage "Msolve"
  elapsedTime gbD = flatten entries msolveGB(ideal J1_*, Threads => 8);
  assert(gbA == gbD)
///


-*
  restart
*-
TEST /// -- hilbert driven gb computation, quasi-degrees
  setRandomSeed 42
  kk = ZZ/101
  R1 = kk[a..g, Degrees => {1,2,3,3,4,4,5}];
  K1 = ideal (a^8, b^4, e^2, f^2)
  hfJ = poincare K1
  J1 = ideal random(R1^1, R1^{-8, -8, -8, -8});
  --elapsedTime gbA = flatten entries gens gb(ideal J1_*, Hilbert => hfJ);
  elapsedTime gbB = flatten entries gens gb(ideal J1_*,
                                          Algorithm => LinearAlgebra,
                                          Hilbert => hfJ);
  elapsedTime gbB2 = flatten entries gens gb(ideal J1_*,
                                          Algorithm => LinearAlgebra);
  elapsedTime gbC = flatten entries groebnerBasis(ideal J1_*, Strategy => "F4");
  assert(gbB == gbB2)
  assert(gbB == gbC)
///

-*
  restart
*-
TEST /// -- GB over quotient rings
  setRandomSeed 42
  kk = ZZ/101
  R1 = kk[a,b,c,d,e,f]/ideal(a^2 - b*c)
  J1 = ideal random(R1^1, R1^{-2,-2,-3,-3});
  elapsedTime gbA = flatten entries gens gb(ideal J1_*);
  assert try(gb(ideal J1_*, Algorithm => LinearAlgebra);false) else true
  elapsedTime gbC = flatten entries groebnerBasis(ideal J1_*, Strategy => "F4");
  assert(gbA == gbC) -- because it falls back to working version

  needsPackage "Msolve"
  assert try(msolveGB(ideal J1_*, Threads => 8); false) else true -- correctly gives error
///


-*
  restart
*-
TEST /// -- exterior algebra
  kk = ZZ/101
  R1 = kk[a,b,c,d,e,f, SkewCommutative=>true]
  setRandomSeed 42
  J1 = ideal random(R1^1, R1^{-2,-2,-2,-2});
  elapsedTime gbA = flatten entries gens gb(J1);
  assert try(gb(ideal J1_*, Algorithm => LinearAlgebra); false) else true;
  elapsedTime gbC = flatten entries groebnerBasis(ideal J1_*, Strategy => "F4");
  assert(gbA == gbC)
///

-*
  restart
*-
TEST /// -- nonstandard grading
  kk = ZZ/101;
  R1 = kk[a..f, MonomialOrder => {GRevLex => {1,1,1,1,1,1}}, Degrees => {1,2,3,4,4,4}];
  setRandomSeed 42

  J1 = ideal random(R1^1, R1^{-5, -5, -5, -5});
  assert isHomogeneous J1
  elapsedTime gbA = flatten entries gens gb(J1);
  elapsedTime gbB = flatten entries gens gb(ideal J1_*, Algorithm => LinearAlgebra);
  elapsedTime gbC = flatten entries groebnerBasis(ideal J1_*, Strategy => "F4");
  needsPackage "Msolve"
  elapsedTime gbD = flatten entries msolveGB(ideal J1_*, Threads => 8);
  assert(gbA == gbB)
  assert(gbA == gbC)
  assert(gbA == gbD)
///

-*
  restart
*-
TEST /// -- GB's of modules
  -- default order: grevlex, followed by position up
  R1 = ZZ/101[vars(0..7)]
  assert(leadTerm matrix{{a},{a}} == matrix{{0},{a}})

  m1 = genericMatrix(R1, a, 2, 3)
  elapsedTime gbA = gens (gb m1);
  m1 = genericMatrix(R1, a, 2, 3)
  elapsedTime gbB = gens (G = gb(m1, Algorithm => LinearAlgebra));

  assert(gbA == gbB)

  m1 = genericMatrix(R1, a, 2, 3)
  elapsedTime gbC = groebnerBasis(m1, Strategy => "F4"); -- BUG: just wrong

  m1 = genericMatrix(R1, a, 2, 3)
  elapsedTime gbC = groebnerBasis(m1, Strategy => "MGB"); -- BUG: just wrong

  R2 = ZZ/101[vars(0..7), MonomialOrder => {Position => Up}]
  m2 = genericMatrix(R2, a, 2, 3)
  elapsedTime gbC = groebnerBasis(m2, Strategy => "MGB"); -- BUG: just wrong

  assert(leadTerm matrix{{a},{a}} == matrix{{0},{a}})

  -- note: msolveGB only works for ideals, doesn't take a matrix or Module, in any case.
///

TEST /// -- another module GB test
    R = ZZ/101[a..d]
    M = R^{0,-1,-2,-3}
    setRandomSeed 42
    mat = random(M,R^{-1,-2,-3,-4})
    gbA = gens gb mat;
    setRandomSeed 42
    mat = random(M,R^{-1,-2,-3,-4})
    gbB = gens gb(mat, Algorithm => LinearAlgebra);
    assert(gbA == gbB)
///

TEST /// -- another module GB test
    R = ZZ/101[a..d, MonomialOrder => {Position => Up, GRevLex => 2, GRevLex => 2 }]
    M = R^2
    setRandomSeed 42
    mat = matrix{{a,b},{a,c}}
    gbA = gens gb mat;
    -- to clear cache for mat
    mat = matrix{{a,b},{a,c}}
    assert try (gb(mat, Algorithm => LinearAlgebra); false) else true

    gens gb(ideal(a,b), Algorithm => LinearAlgebra)
///

TEST /// -- another module GB test
    R = ZZ/101[a..f, Degrees => {1,2,3,4,5,6}]
    M = koszul(3,vars R)
    gbA = gens gb M;
    M = koszul(3,vars R)
    gbB = gens gb(M, Algorithm => LinearAlgebra);
    assert(gbA == gbB)
///

-*
  restart
*-
TEST /// -- inhomgeneity. Algorithm => LinearAlgebra, over finite field. Need to implement still
  kk = ZZ/101;
  R1 = kk[a..g, MonomialSize=>8];
  setRandomSeed 42
  J1 = ideal (random(2, R1) + random(3, R1), random(2, R1) - 1)
  elapsedTime gbA = flatten entries gens gb(ideal J1_*);
  elapsedTime assert try(gb(ideal J1_*, Algorithm => LinearAlgebra); false) else true;
  elapsedTime gbB = flatten entries groebnerBasis(ideal J1_*, Strategy => "F4");
  assert(gbA == gbB)

  -- TODO: is it ok to check Msolve while we do this?
  needsPackage "Msolve"
  elapsedTime gbD = flatten entries msolveGB(ideal J1_*, Threads => 8); -- slowest, due to translation, or what?
  assert(gbD == gbB)
///

-*
  restart
*-
TEST /// -- different monomial orders
  kk = ZZ/101;
  R1 = kk[a..g, MonomialSize=>8];
  M = genericSymmetricMatrix(R1, 3)
  J1 = minors(2, M)

  -- grevlex
  elapsedTime gbA = flatten entries gens gb(ideal J1_*);
  elapsedTime gbC = flatten entries gens gb(ideal J1_*, Algorithm => LinearAlgebra);
  elapsedTime gbB = flatten entries groebnerBasis(ideal J1_*, Strategy => "F4");
  assert(gbA == gbB)
  assert(gbA == gbC)
  LT1 = leadTerm J1

  -- lex
  R1 = kk[a..g, MonomialOrder => Lex];
  J1 = sub(J1, R1)
  elapsedTime gbA = flatten entries gens gb(ideal J1_*);
  elapsedTime gbC = flatten entries gens gb(ideal J1_*, Algorithm => LinearAlgebra);
  elapsedTime gbB = flatten entries groebnerBasis(ideal J1_*, Strategy => "F4");
  assert(gbA == gbB)
  assert(gbA == gbC)
  LT2 = leadTerm J1
  assert(LT2 != sub(LT1, R1))

  -- product order
  R1 = kk[a..g, MonomialOrder => {3,4}];
  J1 = sub(J1, R1)
  elapsedTime gbA = flatten entries gens gb(ideal J1_*);
  elapsedTime gbC = flatten entries gens gb(ideal J1_*, Algorithm => LinearAlgebra);
  elapsedTime gbB = flatten entries groebnerBasis(ideal J1_*, Strategy => "F4");
  assert(gbA == gbB)
  assert(gbA == gbC)
  LT3 = leadTerm J1
  assert(LT3 != sub(LT1, R1))

  -- product order
  R1 = kk[a..g, MonomialOrder => {Lex => 3, GRevLex => {1,2,3,4}}];
  J1 = sub(J1, R1)
  elapsedTime gbA = flatten entries gens gb(ideal J1_*);
  elapsedTime gbC = flatten entries gens gb(ideal J1_*, Algorithm => LinearAlgebra);
  elapsedTime gbB = flatten entries groebnerBasis(ideal J1_*, Strategy => "F4");
  assert(gbA == gbB)
  assert(gbA == gbC)
  LT4 = leadTerm J1
  assert(LT4 != sub(LT1, R1))
///

-*
  restart
*-
TEST /// -- Weyl algebras
  R = QQ[x,dx, WeylAlgebra => {x=>dx}]
  I = ideal(x^3, x*dx)
  assert try (gb(I, Algorithm => LinearAlgebra); false) else true

  R = ZZ/32003[x,dx,h, WeylAlgebra => {x=>dx,h}]
  I = ideal(x^3, x*dx)
  assert try (gb(I, Algorithm => LinearAlgebra); false) else true -- this should error
///


-*
  restart
*-
TEST /// -- multigradings
  I = Grassmannian(2, 5, CoefficientRing => ZZ/32003)
  R = ring I
  es = entries id_(ZZ^6)
  degs = apply(subsets(0..5, 3), x -> sum apply(x, i -> es_i))
  S = newRing(R, Degrees => degs)
  J1 = sub(I, S)
  isHomogeneous J1
  elapsedTime gbA = flatten entries gens gb(ideal J1_*);
  elapsedTime gbC = flatten entries gens gb(ideal J1_*, Algorithm => LinearAlgebra);
  elapsedTime gbB = flatten entries groebnerBasis(ideal J1_*, Strategy => "F4");
  assert(gbA == gbB)
  assert(gbA == gbC)
///

-- todo:
--  error for quotient rings (for LinearAlgebra)
--  error for exterior algebra
--  error for Weyl algebra
--  error if module order is wrong (only if we are in a non-ideal situation).
-- check
--  Hilbert hint with quasi-gradings OK
--  multi-gradings: OK
--  check Weyl algebra (homogenized Weyl algebra): Need to error
