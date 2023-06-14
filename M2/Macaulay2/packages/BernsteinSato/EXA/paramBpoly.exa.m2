restart
load "Dmodules.m2"
A =  (QQ [a,b,c,d]) [x, y, Dx, Dy, WeylAlgebra => {x=>Dx, y=>Dy}]

Dtrace 1
paramBpoly(
     a*x^2 + b*x*y + c*y^2,
     "quadratic2" 
     )
paramBpoly(
     a*x^3 + b*x^2*y + c*x*y^2 + d*y^3,
     "n2d3.case1" 
     )
paramBpoly(
     x^2 + a*x^3 + b*x^2*y + c*x*y^2 + d*y^3,
     "n2d3.case2"
     )
paramBpoly(
     x*y + a*x^3 + b*x^2*y + c*x*y^2 + d*y^3,
     "n2d3.case3"
     )

-----------------------------------------------------
-- sum up times
V = loadVTree("homogeneousquadratic.v3")
total = 0
scan(V.n, u -> if u.Itype == COMPUTED then total = total + u.t)
total/60
  








