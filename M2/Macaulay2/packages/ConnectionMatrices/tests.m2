TEST /// -- ALS notes, Example 7.16
  D = makeWeylAlgebra(QQ[x,y], w = {0,0,1,2});
  I = ideal(x*dx^2 - y*dy^2 + dx-dy, x*dx+y*dy+1); -- doesn't commute
  A = pfaffians I;
  -- TODO: add assertions

  -- i2 : D = makeWeylAlgebra(QQ[x,y], w = {0,0,2,1});
  -- i3 : A = pfaffians(w, I = ideal (x*dx^2 - y*dy^2 + dx-dy, x*dx+y*dy+1)) -- doesn't commute
  -- Grobner basis:
  -- | xdx+ydy+1 ydxdy+ydy^2+dx+dy xydy^2-y2dy^2+xdy-3ydy-1 |
  -- Standard monomials:
  -- | 1 dy |
  -- o3 = {{-1} | (-1)/x       (-y)/x         |, {-1} | 0         1               |}
  --       {-1} | (-1)/(x2-xy) (-x-y)/(x2-xy) |  {-1} | 1/(xy-y2) (-x+3y)/(xy-y2) |

  -- i4 : D = makeWeylAlgebra(QQ[x,y], w = {0,0,1,1});
  -- i5 : A = pfaffians(w, I = ideal (x*dx^2 - y*dy^2 + dx-dy, x*dx+y*dy+1)) -- doesn't commute
  -- Grobner basis:
  -- | xdx+ydy+1 ydxdy+ydy^2+dx+dy xydy^2-y2dy^2+xdy-3ydy-1 |
  -- Standard monomials:
  -- | 1 dy |
  -- o5 = {{-1} | (-1)/x       (-y)/x         |, {-1} | 0         1               |}
  --       {-1} | (-1)/(x2-xy) (-x-y)/(x2-xy) |  {-1} | 1/(xy-y2) (-x+3y)/(xy-y2) |

  -- i6 : D = makeWeylAlgebra(QQ[x,y], w = {0,0,1,2});
  -- i7 : A = pfaffians(w, I = ideal (x*dx^2 - y*dy^2 + dx-dy, x*dx+y*dy+1)) -- doesn't commute
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

-- Computing the Pfaffian system w.r.t. weight vector w1
C1 = pfaffians(I);
SM1 = stdMon(I);

-- Computing the Pfaffian system w.r.t. weight vector w2
C2 = pfaffians(sub(I,D2));
SM2 = stdMon(sub(I,D2));

-- Compute Groebner Basis
G = flatten entries gens gb I;
changeofvar = gaugeMatrix(G,SM1,SM2);

-- Now transform the Pfaffian system C1 into the Pfaffian System C2 via Gauge transform
assert(C2 == gaugeTransform(changeofvar,C1,D1)) -- TODO: Need to remove the weight information.   // Fails so far.
///