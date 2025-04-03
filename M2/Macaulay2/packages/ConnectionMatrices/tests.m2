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
  A = connectionMatrices I;
  assert(A_0 == map(R^2, R^2, {{0, 1}, {(-1)/(x^2-x*y), (-3*x+y)/(x^2-x*y)}}))
  assert(A_1 == map(R^2, R^2, {{(-1)/y, (-x)/y}, {1/(x*y-y^2), (x+y)/(x*y-y^2)}}))

  use D;
  SM = standardMonomials I;
  assert(SM == {1, dx});

  -- Grobner basis:
  -- | ydy+xdx+1 xdxdy+xdx^2+dy+dx x2dx^2-xydx^2+3xdx-ydx+1 |

///

TEST /// -- course notes, Example 7.16 (2)

  D = makeWeylAlgebra(QQ[x,y], w = {2,1});
  I = ideal(x*dx^2 - y*dy^2 + dx-dy, x*dx+y*dy+1);
  assert(holonomicRank(w, comodule I) == 2);

  R = baseFractionField D;
  A = connectionMatrices I;
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
  A = connectionMatrices I;
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
  C1 = connectionMatrices I;
  SM1 = standardMonomials I;

  -- Computing the system of connection matrices w.r.t. weight vector w2
  C2 = connectionMatrices sub(I, D2);
  SM2 = standardMonomials sub(I, D2);

  R = baseFractionField D2
  -- TODO: this should work once frac acts like baseFractionField
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
  A = connectionMatrices I;
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
