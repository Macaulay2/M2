-------------------------------------------------------
--
-- ConnectionMatrices - Test Suite
--
-- Structure:
--   1) Worked out, complete examples
--   2) Functionality tests for each exported method
-------------------------------------------------------

-------------------------------------------------------
--
-- 1) Complete Examples
--
-------------------------------------------------------

TEST ///-- test 0
  -- now moved to an example
///

TEST ///-- test 1
  -- ALS notes, Example 7.16
  D = makeWeylAlgebra(QQ[x,y], w = {1,2});
  I = ideal(x*dx^2 - y*dy^2 + dx-dy, x*dx+y*dy+1);
  assert(holonomicRank I == 2);

  R = baseFractionField D
  A = connectionMatrices I;
  assert(A_0 == map(R^{2:{1}},R^{{0}, {1}},{{0, 1}, {(-1)/(x^2-x*y), (-3*x+y)/(x^2-x*y)}}))
  assert(A_1 == map(R^{2:{1}},R^{{0}, {1}},{{(-1)/y, (-x)/y}, {1/(x*y-y^2), (x+y)/(x*y-y^2)}}))

  -- i2 : D = makeWeylAlgebra(QQ[x,y], w = {2,1});
  -- i3 : A = connectionMatrices(ideal (x*dx^2 - y*dy^2 + dx-dy, x*dx+y*dy+1))
  -- Grobner basis:
  -- | xdx+ydy+1 ydxdy+ydy^2+dx+dy xydy^2-y2dy^2+xdy-3ydy-1 |
  -- Standard monomials:
  -- | 1 dy |
  -- o3 = {{-1} | (-1)/x       (-y)/x         |, {-1} | 0         1               |}
  --       {-1} | (-1)/(x2-xy) (-x-y)/(x2-xy) |  {-1} | 1/(xy-y2) (-x+3y)/(xy-y2) |

  -- i4 : D = makeWeylAlgebra(QQ[x,y], w = {1,1});
  -- i5 : A = connectionMatrices(w, I = ideal (x*dx^2 - y*dy^2 + dx-dy, x*dx+y*dy+1))
  -- Grobner basis:
  -- | xdx+ydy+1 ydxdy+ydy^2+dx+dy xydy^2-y2dy^2+xdy-3ydy-1 |
  -- Standard monomials:
  -- | 1 dy |
  -- o5 = {{-1} | (-1)/x       (-y)/x         |, {-1} | 0         1               |}
  --       {-1} | (-1)/(x2-xy) (-x-y)/(x2-xy) |  {-1} | 1/(xy-y2) (-x+3y)/(xy-y2) |

  -- i6 : D = makeWeylAlgebra(QQ[x,y], w = {1,2});
  -- i7 : A = connectionMatrices(w, I = ideal (x*dx^2 - y*dy^2 + dx-dy, x*dx+y*dy+1))
  -- Grobner basis:
  -- | ydy+xdx+1 xdxdy+xdx^2+dy+dx x2dx^2-xydx^2+3xdx-ydx+1 |
  -- Standard monomials:
  -- | 1 dx |
  -- o7 = {{-1} | 0            1               |, {-1} | (-1)/y    (-x)/y        |}
  --       {-1} | (-1)/(x2-xy) (-3x+y)/(x2-xy) |  {-1} | 1/(xy-y2) (x+y)/(xy-y2) |
///


TEST /// -- test 2
  -- Example from Overleaf
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
  -- TODO: the degrees seem to differ, is this desirable or not?
  assert(matrix \ entries \ C2 == matrix \ entries \ C1'')
///

-------------------------------------------------------
--
-- 2) Functionality tests of (exported) methods
--
-------------------------------------------------------

TEST /// -- test 3: isEpsilonFactorized
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

TEST /// -- test 4: isIntegrable
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

TEST /// -- test 5: baseFractionField
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

TEST /// -- test 6: holonomicRank
  -- Check that holonomic rank doesn't depend on the choice of positive weight.
  D = makeWeylAlgebra(QQ[x,y], w = {1,2});
  I = ideal(x*dx^2 - y*dy^2 + dx-dy, x*dx+y*dy+1);
  assert same apply({{0,0,1,2}, {0,0,5,100}, {0,0,17,3}},
      w -> holonomicRank(w, comodule I));
///
