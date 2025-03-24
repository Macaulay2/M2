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

TEST ///
-- Example equation (11) from https://arxiv.org/pdf/2410.14757
w = {0,0,0,1,1,1};
D = makeWeylAlgebra(frac(QQ[e,DegreeRank=>0])[x,y,z],w);
delta1 = (x^2-z^2)*dx^2+2*(1-e)*x*dx-e*(1-e);
delta2 = (y^2-z^2)*dy^2+2*(1-e)*y*dy-e*(1-e);
delta3 = (x+z)*(y+z)*dx*dy-e*(x+z)*dx-e*(y+z)*dy+e^2;
h = x*dx+y*dy+z*dz-2*e;
I = ideal(delta1+delta3, delta2+delta3,h);

assert(holonomicRank(I) == 4);

-- Gauge transform to e-factorized form:
P = connectionMatrices I;
G = flatten entries gens gb I;

B2 = {1,dx,dy,dx*dy};
changeofvar = gaugeMatrix(G,B2);
assert(changeofvar == gaugeMatrix(I,B2));
P2 = gaugeTransform(changeofvar,P);

changeVar = transpose((1/(2*z*e^2))*matrix({{2*z*e^2, -e^2*(x-z), -e^2*(y-z), -e^2*(x+y)},{0,e*(x^2-z^2),0,e*(x+y)*(x+z)},{0,0,e*(y^2-z^2),e*(x+y)*(y+z)},{0,0,0,-(x+y)*(x+z)*(y+z)}}));
P3 = gaugeTransform(changeVar,P2);

assert(isEpsilonFactorized(P3,e));
///

TEST /// -- ALS notes, Example 7.16

  D = makeWeylAlgebra(QQ[x,y], w = {0,0,1,2});
  I = ideal(x*dx^2 - y*dy^2 + dx-dy, x*dx+y*dy+1); -- doesn't commute
  A = connectionMatrices I;
  -- TODO: add assertions

  -- i2 : D = makeWeylAlgebra(QQ[x,y], w = {0,0,2,1});
  -- i3 : A = connectionMatrices(ideal (x*dx^2 - y*dy^2 + dx-dy, x*dx+y*dy+1)) -- doesn't commute
  -- Grobner basis:
  -- | xdx+ydy+1 ydxdy+ydy^2+dx+dy xydy^2-y2dy^2+xdy-3ydy-1 |
  -- Standard monomials:
  -- | 1 dy |
  -- o3 = {{-1} | (-1)/x       (-y)/x         |, {-1} | 0         1               |}
  --       {-1} | (-1)/(x2-xy) (-x-y)/(x2-xy) |  {-1} | 1/(xy-y2) (-x+3y)/(xy-y2) |

  -- i4 : D = makeWeylAlgebra(QQ[x,y], w = {0,0,1,1});
  -- i5 : A = connectionMatrices(w, I = ideal (x*dx^2 - y*dy^2 + dx-dy, x*dx+y*dy+1)) -- doesn't commute
  -- Grobner basis:
  -- | xdx+ydy+1 ydxdy+ydy^2+dx+dy xydy^2-y2dy^2+xdy-3ydy-1 |
  -- Standard monomials:
  -- | 1 dy |
  -- o5 = {{-1} | (-1)/x       (-y)/x         |, {-1} | 0         1               |}
  --       {-1} | (-1)/(x2-xy) (-x-y)/(x2-xy) |  {-1} | 1/(xy-y2) (-x+3y)/(xy-y2) |

  -- i6 : D = makeWeylAlgebra(QQ[x,y], w = {0,0,1,2});
  -- i7 : A = connectionMatrices(w, I = ideal (x*dx^2 - y*dy^2 + dx-dy, x*dx+y*dy+1)) -- doesn't commute
  -- Grobner basis:
  -- | ydy+xdx+1 xdxdy+xdx^2+dy+dx x2dx^2-xydx^2+3xdx-ydx+1 |
  -- Standard monomials:
  -- | 1 dx |
  -- o7 = {{-1} | 0            1               |, {-1} | (-1)/y    (-x)/y        |}
  --       {-1} | (-1)/(x2-xy) (-3x+y)/(x2-xy) |  {-1} | 1/(xy-y2) (x+y)/(xy-y2) |
///



TEST ///
-- Example from Overleaf
w1 = {0,0,2,1};
w2 = {0,0,1,2};

D1 = makeWeylAlgebra(QQ[x,y],w1);
D2 = makeWeylAlgebra(QQ[x,y],w2);

-- Construct the ideal in the first Weyl algebra
I = sub(ideal(x*dx^2-y*dy^2+2*dx-2*dy,x*dx+y*dy+1),D1);  -- Ex. 1.4
-- Compute its holonomic rank
assert(holonomicRank(I) == 2)

-- Computing the system of connection matrices w.r.t. weight vector w1
C1 = connectionMatrices(I);
SM1 = standardMonomials(I);

-- Computing the system of connection matrices w.r.t. weight vector w2
C2 = connectionMatrices(sub(I,D2));
SM2 = standardMonomials(sub(I,D2));

-- Compute Groebner Basis
G = flatten entries gens gb I;
changeofvar = gaugeMatrix(G,SM2);

P' = gaugeTransform(changeofvar,C1,D1);
P'' = apply(P', p-> sub(p, ring C2#0));

-- Now transform the system of connection matrices C1 into the system of connection matrices C2 via Gauge transform
assert(C2 == P'')
///

-------------------------------------------------------
--
-- 2) Functionality tests of (exported) methods
--
-------------------------------------------------------

--
-- isEpsilonFactorized
--

TEST ///
-- Example

R = frac(QQ[x,y]);

M = matrix {{y, y^2}, {(y+1)/((y-1)*(y-2)), 1/(y + y^2)}};
assert(isEpsilonFactorized(M, x));
///

TEST ///
-- Non-Example

R = frac(QQ[x,y]);
M = matrix {{y, y^2}, {(y+1)/((y-1)*(y-2)), 1/(y + y^2)}};

assert(not isEpsilonFactorized(M, y));
///

TEST ///
-- Example
R = frac(QQ[x,y]);
M = matrix {{x^2*y, y}, {y*x + y / (x^2 +1), 0}};

assert(isEpsilonFactorized(M, y));
///

TEST ///
-- Matrix of zeros is factorized with respect to any variable
R = frac(QQ[x,y]);
M = matrix {{0,0}, {0,0}};

assert(isEpsilonFactorized(M, x) and isEpsilonFactorized(M, y));
///

TEST ///
-- Trivial example of non-factorized (numerator not homogeneous)
R = frac(QQ[x]);
M = matrix {{(x+1)/x}}
///

--
-- isIntegrable
--

TEST ///
-- A connection coming from a D-ideal is integrable:

D = makeWeylAlgebra(QQ[x,y], w = {0,0,1,2});
I = ideal(x*dx^2 - y*dy^2 + dx-dy, x*dx+y*dy+1);
A = connectionMatrices I;

assert(isIntegrable(D,A));

///

TEST ///
-- Constant coefficient matrices that don't commute can't come from an integrable system.
S = QQ[x,y];
R = frac(S);

A_0 = sub(matrix {{0,1}, {1,0}}, R);
A_1 = sub(matrix {{2,0}, {0,3}}, R);

-- Since entries are constants, it will essentially check whether the matrices commute.
-- And that is not the case.
assert(isIntegrable({A_0, A_1}) == false);
///

TEST ///
-- Check that the integrability test also works in the parametric case:

-- Based on equation (11) from https://arxiv.org/pdf/2410.14757
w = {0,0,0,1,1,1};
D = makeWeylAlgebra(frac(QQ[e,DegreeRank=>0])[x,y,z],w);
delta1 = (x^2-z^2)*dx^2+2*(1-e)*x*dx-e*(1-e);
delta2 = (y^2-z^2)*dy^2+2*(1-e)*y*dy-e*(1-e);
delta3 = (x+z)*(y+z)*dx*dy-e*(x+z)*dx-e*(y+z)*dy+e^2;
h = x*dx+y*dy+z*dz-2*e;

I = ideal(delta1+delta3, delta2+delta3,h);
A = connectionMatrices I;

assert(isIntegrable(A))
///

--
-- fractionField
--

TEST /// -- tests for fractionField
  debug needsPackage "ConnectionMatrices"
  assert(3 == numgens fractionField makeWA(QQ[x,y,z]))
  assert(4 == numgens fractionField makeWA((QQ[e, DegreeRank => 0])[x,y,z]))
  assert(7 == numgens fractionField makeWA(((QQ[a,b,c, DegreeRank => 0])[e, DegreeRank => 0])[x,y,z]))
  assert(7 == numgens fractionField makeWA((frac(QQ[a,b,c, DegreeRank => 0])[e, DegreeRank => 0])[x,y,z]))
///

--
-- inferWeylAlgbra
--
TEST /// -- Check that inferred WeylAlgebra equals provided WeylAlgebra
  debug needsPackage "ConnectionMatrices"
  D = makeWA(frac(QQ[e, DegreeRank => 0])[x,y]);
  F = fractionField(D);
  assert(D === inferWeylAlgebra(F))
///