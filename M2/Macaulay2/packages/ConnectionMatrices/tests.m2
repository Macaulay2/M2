-------------------------------------------------------
--
-- ConnectionMatrices - Test Suite
--
-- Structure:
--   1) Worked out, complete examples
--   2) Functionality tests for (exported) methods
-------------------------------------------------------
-- Link to course notes containing a few examples below:
-- https://alsattelberger.de/wp-content/uploads/2024/12/introductionalgebraicanalysis.pdf

-------------------------------------------------------
--
-- 1) Complete Examples
--
-------------------------------------------------------

TEST /// -- course notes, Example 7.16 (1)

  D = makeWeylAlgebra(QQ[x,y], w = {1,2});
  I = ideal(x*dx^2 - y*dy^2 + dx-dy, x*dx+y*dy+1);
  assert(holonomicRank(w, comodule I) == 2);

  -- Check entries of G are sorted smallest to largest
  G = flatten entries gens gb I;
  assert(G == {y*dy+x*dx+1, x^2*dx^2-x*y*dx^2+3*x*dx-y*dx+1, x*dx*dy+x*dx^2+dy+dx });

  R = baseFractionField D
  A = pfaffianSystem I;
  assert(A_0 == map(R^2, R^2, {{0, 1}, {(-1)/(x^2-x*y), (-3*x+y)/(x^2-x*y)}}))
  assert(A_1 == map(R^2, R^2, {{(-1)/y, (-x)/y}, {1/(x*y-y^2), (x+y)/(x*y-y^2)}}))

  use D;
  SM = standardMonomials I;
  assert(SM == {1, dx});

  -- Grobner basis:
  -- | ydy+xdx+1 xdxdy+xdx^2+dy+dx x2dx^2-xydx^2+3xdx-ydx+1 |

  -- Note: this example also shows that reducing with respect to each
  -- element of G only once in order doesn't suffice, because after
  -- reducing by an element, the result may become further reducible
  -- by an earlier element in the list.
  debug needsPackage "ConnectionMatrices"
  D = makeWeylAlgebra(QQ[x,y], w = {2,1});
  I = ideal(x*dx^2 - y*dy^2 + dx-dy, x*dx+y*dy+1)
  R = rationalWeylAlgebra D
  G = flatten entries gens gb I
  f0 = dx_R*dy_R
  -- first pass
  f1 = normalForm(f0, G#0) -- does not reduce wrt G#0
  f2 = normalForm(f1, G#1) -- reduces
  f3 = normalForm(f2, G#2)
  assert same {f0, f1, dx_R*dy_R}
  assert same {f2, f3, -(y_R*x_R^-1)*dy_R^2 - (2*x_R^-1)*dy_R}
  -- second pass
  f4 = normalForm(f3, G#0) -- now it reduces wrt G#0!
  f5 = normalForm(f4, G#1)
  f6 = normalForm(f5, G#2)
  assert same {f4, f5, f6, (-x_R-y_R)*(x_R^2-x_R*y_R)^-1*dy_R - (x_R^2-x_R*y_R)^-1}
///

TEST /// -- course notes, Example 7.16 (2)

  D = makeWeylAlgebra(QQ[x,y], w = {2,1});
  I = ideal(x*dx^2 - y*dy^2 + dx-dy, x*dx+y*dy+1);
  assert(holonomicRank(w, comodule I) == 2);

  R = baseFractionField D;
  A = pfaffianSystem I;
  G = flatten entries gens gb I;

  SM = standardMonomials I;
  assert(SM == {1, dy});

  -- Check entries of GB, make sure G is sorted smallest to largest.
  use D;

  assert(G == {x*y*dy^2-y^2*dy^2+x*dy-3*y*dy-1, x*dx+y*dy+1, y*dx*dy+y*dy^2+dx+dy });

  -- Check entries of connection matrices
  use R;
  assert(A_0 == map(R^2, R^2, {{-1/x, -y/x}, {-1/(x^2-x*y), (-x-y)/(x^2-x*y)}}));
  assert(A_1 == map(R^2, R^2, {{0, 1}, {1/(x*y-y^2), (-x+3*y)/(x*y-y^2)}}));
///


TEST /// -- course notes, Example 7.16 (3)
  D = makeWeylAlgebra(QQ[x,y], w = {1,1});
  I = ideal(x*dx^2 - y*dy^2 + dx-dy, x*dx+y*dy+1);
  assert(holonomicRank(w, comodule I) == 2);

  R = baseFractionField D;
  A = pfaffianSystem I;
  G = flatten entries gens gb I;

  SM = standardMonomials I;
  assert(SM == {1, dy});

  -- Check entries of GB, make sure G is sorted smallest to largest.
  use D;

  assert(G == {x*dx+y*dy+1, x*y*dy^2-y^2*dy^2+x*dy-3*y*dy-1, y*dx*dy+y*dy^2+dx+dy });

  -- Check entries of connection matrices
  use R;
  assert(A_0 == map(R^2, R^2, {{-1/x, -y/x}, {-1/(x^2-x*y), (-x-y)/(x^2-x*y)}}));
  assert(A_1 == map(R^2, R^2, {{0, 1}, {1/(x*y-y^2), (-x+3*y)/(x*y-y^2)}}));
///


TEST /// -- Example from Overleaf

  w1 = {2,1};
  w2 = {1,2};

  D1 = makeWeylAlgebra(QQ[x,y], w1);
  D2 = makeWeylAlgebra(QQ[x,y], w2);

  -- Construct the ideal in the first Weyl algebra
  I = sub(ideal(x*dx^2-y*dy^2+2*dx-2*dy, x*dx+y*dy+1), D1);  -- Ex. 1.4
  -- Compute its holonomic rank
  assert(holonomicRank I == 2)

  -- Computing the system of connection matrices w.r.t. weight vector w1
  C1 = pfaffianSystem I;
  SM1 = standardMonomials I;

  -- Computing the system of connection matrices w.r.t. weight vector w2
  C2 = pfaffianSystem sub(I, D2);
  SM2 = standardMonomials sub(I, D2);

  R = baseFractionField D2
  -- TODO: this should work once frac acts like baseFractionField
  -- (baseFractionField is cached per-D, so two Weyl algebras differing only in their weight
  -- order currently produce ===-distinct fraction fields even though the underlying field is
  -- the same; reviving the assertion below requires Core to canonicalize these.)
  -- assert(R === baseFractionField D1)

  -- Compute Groebner Basis
  G = flatten entries gens gb I;
  changeofvar = gaugeMatrix(G, SM2);
  C1' = gaugeTransform(changeofvar, C1, D1);
  C1'' = apply(C1', p -> sub(p, R));

  -- Now transform the system of connection matrices C1 into
  -- the system of connection matrices C2 via gauge transform
  assert(C2 == C1'')
///

-------------------------------------------------------
--
-- 2) Functionality tests of (exported) methods
--
-------------------------------------------------------

TEST /// -- isEpsilonFactorized

  -- Example
  R = frac(QQ[x,y]);
  M = matrix {{y, y^2}, {(y+1)/((y-1)*(y-2)), 1/(y + y^2)}};
  assert isEpsilonFactorized(M, x);

  -- Non-Example
  R = frac(QQ[x,y]);
  M = matrix {{y, y^2}, {(y+1)/((y-1)*(y-2)), 1/(y + y^2)}};
  assert not isEpsilonFactorized(M, y);

  -- Example
  R = frac(QQ[x,y]);
  M = matrix {{x^2*y, y}, {y*x + y / (x^2 +1), 0}};
  assert isEpsilonFactorized(M, y);

  -- Matrix of zeros is factorized with respect to any variable
  R = frac(QQ[x,y]);
  M = matrix(R, {{0,0}, {0,0}});
  assert(isEpsilonFactorized(M, x) and isEpsilonFactorized(M, y));

  -- Trivial example of non-factorized (numerator not homogeneous)
  R = frac(QQ[x]);
  M = matrix {{(x+1)/x}}
  assert not isEpsilonFactorized(M, x)
///

TEST /// -- isIntegrable

  -- A connection coming from a D-ideal is integrable:
  D = makeWeylAlgebra(QQ[x,y], w = {1,2});
  I = ideal(x*dx^2 - y*dy^2 + dx-dy, x*dx+y*dy+1);
  A = pfaffianSystem I;
  assert(isIntegrable(D,A));

  -- Constant coefficient matrices that don't commute can't come from an integrable system.
  S = QQ[x,y];
  R = frac S;
  A0 = sub(matrix {{0,1}, {1,0}}, R);
  A1 = sub(matrix {{2,0}, {0,3}}, R);
  -- Since entries are constants, it will essentially check whether the matrices commute.
  -- And that is not the case.
  assert(isIntegrable({A0, A1}) == false);
///

TEST /// -- baseFractionField

  -- tests for baseFractionField
  debug needsPackage "ConnectionMatrices"
  assert(3 == numgens baseFractionField makeWeylAlgebra(QQ[x,y,z]))
  assert(4 == numgens baseFractionField makeWeylAlgebra((QQ[e, DegreeRank => 0])[x,y,z]))
  assert(7 == numgens baseFractionField makeWeylAlgebra(((QQ[a,b,c, DegreeRank => 0])[e, DegreeRank => 0])[x,y,z]))
  assert(7 == numgens baseFractionField makeWeylAlgebra((frac(QQ[a,b,c, DegreeRank => 0])[e, DegreeRank => 0])[x,y,z]))

  -- Check that inferred WeylAlgebra equals provided WeylAlgebra
  D = makeWA(frac(QQ[e, DegreeRank => 0])[x,y]);
  F = baseFractionField D;
  assert(D === inferWeylAlgebra F)
///

TEST /// -- holonomicRank

  -- Check that holonomic rank doesn't depend on the choice of positive weight.
  D = makeWeylAlgebra(QQ[x,y], w = {1,2});
  I = ideal(x*dx^2 - y*dy^2 + dx-dy, x*dx+y*dy+1);
  assert same apply({{0,0,1,2}, {0,0,5,100}, {0,0,17,3}},
      w -> holonomicRank(w, comodule I));
///

TEST /// -- Testing the elimination lex order

-- Not specifying the weight should give us the non-weighted elimination lex, i.e. the lex on:
--  dx > dy > x > y
D = makeWeylAlgebra(QQ[x,y]);

-- Monomials in elimination lex order
Plist =  {dx^2, y*dx, dy^2, x*dy, x^2, x*y, y^2, 1};
P =  sum Plist

Q = P;
for i in 0..#Plist-1 do (
  lt = leadTerm Q;
  assert(lt == Plist#i);

  Q = Q - lt;
)
assert(Q == 0);

///

TEST /// -- Testing the (1,2) weighted elimination lex order

-- Elimination lex breaking ties on {0,0,1,2} weight (w.r.t. x,y,dx,dy)
D = makeWeylAlgebra(QQ[x,y], {1,2});

-- Monomials in elimination lex order
Plist =  { dy^2,    dx^2, x*dy,    y*dx,    x^2, x*y, y^2, 1};
P =  sum Plist

Q = P;
for i in 0..#Plist-1 do (
  lt = leadTerm Q;
  assert(lt == Plist#i);

  Q = Q - lt;
)
assert(Q == 0);

///

TEST /// -- connectionMatrix (Ideal / List) is documented but had no direct assertion before
  D = makeWeylAlgebra(QQ[x,y], w = {1,2});
  I = ideal(x*dx^2 - y*dy^2 + dx-dy, x*dx+y*dy+1);

  -- The visualization-only output is a Net combining the connection matrices times their dx_i
  cm = connectionMatrix I;
  assert(class cm === Net);

  -- the (Ideal) overload agrees with the (List) overload run on the same input
  A = connectionMatrices I;
  assert(cm == connectionMatrix A);

  -- mismatched-ring list of matrices is rejected (ConnectionMatrices.m2:116)
  D' = makeWeylAlgebra(QQ[u,v], {1,1});
  F' = baseFractionField D';
  A' = sub(matrix{{0_F', 1_F'}, {0_F', 0_F'}}, F');
  assert(try (connectionMatrix {A_0, A'}; false) else true);

  -- list whose length does not match the number of differential variables is rejected
  -- (ConnectionMatrices.m2:118)
  assert(try (connectionMatrix {A_0}; false) else true);
///

TEST /// -- normalForm(P, G): pin reductions over the rational Weyl algebra (Example 1.3, w = {1,2})
  D = makeWeylAlgebra(QQ[x,y], w = {1,2});
  I = ideal(x*dx^2 - y*dy^2 + dx-dy, x*dx+y*dy+1);
  debug needsPackage "ConnectionMatrices";
  R = rationalWeylAlgebra D;
  G = flatten entries gens gb I;
  -- access the coefficient-ring generators via F_i / R_i: the bare names x,y after `use R`
  -- still refer to D's non-commutative Weyl-algebra generators, not F's commutative fractions.
  F = coefficientRing R;
  xF = F_0; yF = F_1;
  dxR = R_0; dyR = R_1;

  -- normalForm(0, G) = 0
  assert(normalForm(0_R, G) == 0_R);

  -- standard monomials reduce to themselves (here {1, dx})
  SM = standardMonomials I;
  assert all(SM, m -> normalForm(sub(m, R), G) == sub(m, R));

  -- Pin specific reductions. These mirror the post-end-- worked examples in
  -- ConnectionMatrices/normalForm.m2 and the connection matrices already asserted in
  -- "course notes, Example 7.16 (1)" above.
  assert(normalForm(dyR, G)        == -(xF/yF)*dxR - 1/yF);
  assert(normalForm(dxR*dyR, G)    == ((xF+yF)/(xF*yF - yF^2))*dxR + 1/(xF*yF - yF^2));
  assert(normalForm(dxR^2, G)      == ((-3*xF + yF)/(xF^2 - xF*yF))*dxR - 1/(xF^2 - xF*yF));
  assert(normalForm(dyR^2, G)      == ((xF^2 - 3*xF*yF)/(xF*yF^2 - yF^3))*dxR + (xF - 2*yF)/(xF*yF^2 - yF^3));
///

TEST /// -- normalForm(P, g): single-generator reductions (Example 1.3, w = {1,2})
  D = makeWeylAlgebra(QQ[x,y], w = {1,2});
  I = ideal(x*dx^2 - y*dy^2 + dx-dy, x*dx+y*dy+1);
  debug needsPackage "ConnectionMatrices";
  R = rationalWeylAlgebra D;
  G = flatten entries gens gb I;
  F = coefficientRing R;
  xF = F_0; yF = F_1;
  dxR = R_0; dyR = R_1;

  -- with the single generator G#0 = y*dy + x*dx + 1: dx is irreducible
  assert(normalForm(dxR, G#0) == dxR);

  -- dx*dy reduces (recursively, single-generator) to a polynomial in dx with rational
  -- coefficients in x, y
  assert(normalForm(dxR*dyR, G#0) == -(xF/yF)*dxR^2 - (2/yF)*dxR);

  -- dy^2 likewise reduces to a polynomial in dx alone
  assert(normalForm(dyR^2, G#0)   == (xF^2/yF^2)*dxR^2 + (4*xF/yF^2)*dxR + 2/yF^2);
///
