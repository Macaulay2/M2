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
