-- -*- coding: utf-8 -*-
needs "raw-util.m2"

--------------------------------
-- 4 by 4 commuting matrices.
-- Adapted from 4by4.m2
--------------------------------
mo = rawMonomialOrdering { GRevLex => 8:1, GRevLex => 12:1, GRevLex => 12:1}
R = polyring2(rawZZp 101, vars(0..31), mo)

M = rawMatrix1(R^1, 16, (-j*o+i*p-v*A+u*B-x*C+w*D,
	  -a*p+b*o+c*p-d*o+k*B-l*A+m*D-n*C,
	  -a*B+b*A+e*B-f*A+p*q-o*r-z*C+y*D,
	  -a*D+b*C+g*D-h*C+p*s-o*t+B*E-A*F,
	  a*j-b*i-c*j+d*i-q*v+r*u-s*x+t*w,
	  j*o-i*p-l*q+k*r-n*s+m*t,
	  -c*r+d*q+e*r-f*q-i*B+j*A-s*z+t*y,
	  -c*t+d*s+g*t-h*s-i*D+j*C-q*F+r*E,
	  a*v-b*u-e*v+f*u-j*k+i*l-x*E+w*F,
	  c*l-d*k-e*l+f*k+m*F-n*E+o*v-p*u,
	  l*q-k*r+v*A-u*B-z*E+y*F,
	  -e*F+f*E+g*F-h*E+l*s-k*t+v*C-u*D,
	  a*x-b*w-g*x+h*w-j*m+i*n-v*y+u*z,
	  c*n-d*m-g*n+h*m+k*z-l*y+o*x-p*w,
	  e*z-f*y-g*z+h*y+n*q-m*r+x*A-w*B,
	  n*s-m*t+x*C-w*D+z*E-y*F), 0)

Gcomp = rawGB(M,false,0,{0},false,0,0,0,10)
rawStartComputation Gcomp
mgb = rawGBGetLeadTerms(Gcomp,1)
assert(rawNumberOfColumns mgb === 294)
rawSource mgb -- BUG: these should be in ascending degree order.
  -- Still, at least the number is correct.

--------------------------------------
-- cyclic 5, homogeneous, ZZ/101
--------------------------------------
mo = rawMonomialOrdering { GRevLex => 6:1}
R = polyring2(rawZZp 101, (symbol a, symbol b, symbol c, symbol d, symbol e, symbol h), mo)

M = rawMatrix1(R^1, 5, (a+b+c+d+e,
     	  a*b+b*c+c*d+d*e+e*a,
	  a*b*c+b*c*d+c*d*e+d*e*a+e*a*b,
	  a*b*c*d+b*c*d*e+c*d*e*a+d*e*a*b+e*a*b*c,
	  a*b*c*d*e-h^5), 0)
Gcomp = rawGB(M,false,0,{0},false,0,0,0,10)
rawStartComputation Gcomp
mgb = rawGBGetMatrix(Gcomp)
assert(rawNumberOfColumns mgb === 38)
rawSource mgb

--------------------------------------
-- cyclic 5, homogeneous, QQ
--------------------------------------
mo = rawMonomialOrdering { GRevLex => 6:1}
R = polyring2(rawQQ(), (symbol a, symbol b, symbol c, symbol d, symbol e, symbol h), mo)

M = rawMatrix1(R^1, 5, (a+b+c+d+e,
     	  a*b+b*c+c*d+d*e+e*a,
	  a*b*c+b*c*d+c*d*e+d*e*a+e*a*b,
	  a*b*c*d+b*c*d*e+c*d*e*a+d*e*a*b+e*a*b*c,
	  a*b*c*d*e-h^5), 0)
Gcomp = rawGB(M,false,0,{0},false,0,0,0,10)
rawStartComputation Gcomp
mgb = rawGBGetMatrix(Gcomp)
assert(rawNumberOfColumns mgb === 38)
rawSource mgb

--------------------------------------
-- cyclic 5, inhomogeneous, ZZ/101
--------------------------------------
mo = rawMonomialOrdering { GRevLex => 5:1}
R = polyring2(rawZZp 101, (symbol a, symbol b, symbol c, symbol d, symbol e), mo)

M = rawMatrix1(R^1, 5, (a+b+c+d+e,
     	  a*b+b*c+c*d+d*e+e*a,
	  a*b*c+b*c*d+c*d*e+d*e*a+e*a*b,
	  a*b*c*d+b*c*d*e+c*d*e*a+d*e*a*b+e*a*b*c,
	  a*b*c*d*e-1), 0)
Gcomp = rawGB(M,false,0,{0},false,0,0,0,10)
rawStartComputation Gcomp
mgb = rawGBGetMatrix(Gcomp)
assert(rawNumberOfColumns mgb === 20)
rawSource mgb

--------------------------------------
-- cyclic 5, inhomogeneous, ZZ
--------------------------------------
mo = rawMonomialOrdering { GRevLex => 5:1}
R = polyring2(rawZZ(), (symbol a, symbol b, symbol c, symbol d, symbol e), mo)

M = rawMatrix1(R^1, 5, (a+b+c+d+e,
     	  a*b+b*c+c*d+d*e+e*a,
	  a*b*c+b*c*d+c*d*e+d*e*a+e*a*b,
	  a*b*c*d+b*c*d*e+c*d*e*a+d*e*a*b+e*a*b*c,
	  a*b*c*d*e-1), 0)
Gcomp = rawGB(M,false,0,{0},false,0,0,0,10)
gbTrace=3
rawStartComputation Gcomp
mgb = rawGBGetMatrix(Gcomp);
assert(rawNumberOfColumns mgb === 24)
rawSource mgb

--------------------------------------
-- cyclic 5, inhomogeneous, QQ
--------------------------------------
mo = rawMonomialOrdering { GRevLex => 5:1}
R = polyring2(rawQQ(), (symbol a, symbol b, symbol c, symbol d, symbol e), mo)

M = rawMatrix1(R^1, 5, (a+b+c+d+e,
     	  a*b+b*c+c*d+d*e+e*a,
	  a*b*c+b*c*d+c*d*e+d*e*a+e*a*b,
	  a*b*c*d+b*c*d*e+c*d*e*a+d*e*a*b+e*a*b*c,
	  a*b*c*d*e-1), 0)
Gcomp = rawGB(M,false,0,{0},false,0,0,0,10)
rawStartComputation Gcomp
mgb = rawGBGetMatrix(Gcomp)
assert(rawNumberOfColumns mgb === 20)
rawSource mgb

---------------------------------------
-- issac-97, QQ
---------------------------------------
-- Compute a lex GB for this one.
mo = rawMonomialOrdering { Lex => 4}
R = polyring2(rawQQ(), (symbol w, symbol x, symbol y, symbol z), mo)

M = rawMatrix1(R^1, 4, (
        -2*w^2+9*w*x+8*x^2+9*w*y+9*x*y+6*y^2-7*w*z-3*x*z-7*y*z-6*z^2-4*w+8*x+4*y+8*z+2,
        3*w^2-5*w*x+4*x^2-3*w*y+2*x*y+9*y^2-6*w*z-2*x*z+6*y*z+7*z^2+9*w+7*x+5*y+7*z+5,
        7*w^2+5*w*x+2*x^2+3*w*y+9*x*y-4*y^2-5*w*z-7*x*z-5*y*z-4*z^2-5*w+4*x+6*y-9*z+2,
        8*w^2+5*w*x+5*x^2-4*w*y+2*x*y+7*y^2+2*w*z-7*x*z-8*y*z+7*z^2+3*w-7*x-7*y-8*z+8),
      0)
Gcomp = rawGB(M,false,0,{0},false,0,0,0,10)
rawStartComputation Gcomp
mgb = rawGBGetMatrix(Gcomp)
assert(rawNumberOfColumns mgb === 4)
rawSource mgb

---------------------------------------
-- issac-97, ZZ, inhomogeneous
---------------------------------------
-- FIXME: moved to quarantine/issac-97.m2, either memory leak or takes >1G memory

---------------------------------------
-- issac-97, ZZ, homogeneous
---------------------------------------
mo = rawMonomialOrdering { Lex => 5}
R = polyring2(rawZZ(), (symbol w, symbol x, symbol y, symbol z, symbol t), mo)
M = rawMatrix1(R^1, 4, (
        -2*w^2+9*w*x+8*x^2+9*w*y+9*x*y+6*y^2-7*w*z-3*x*z-7*y*z-6*z^2-4*w+8*x+4*y+8*z+2,
        3*w^2-5*w*x+4*x^2-3*w*y+2*x*y+9*y^2-6*w*z-2*x*z+6*y*z+7*z^2+9*w+7*x+5*y+7*z+5,
        7*w^2+5*w*x+2*x^2+3*w*y+9*x*y-4*y^2-5*w*z-7*x*z-5*y*z-4*z^2-5*w+4*x+6*y-9*z+2,
        8*w^2+5*w*x+5*x^2-4*w*y+2*x*y+7*y^2+2*w*z-7*x*z-8*y*z+7*z^2+3*w-7*x-7*y-8*z+8),
      0)
M = rawHomogenize(M,4,(1,1,1,1,1))
Gcomp = rawGB(M,false,0,{0},false,0,0,0,10)
rawStartComputation Gcomp
mgb = rawGBGetMatrix(Gcomp)
assert(rawNumberOfColumns mgb === 62)

---------------------------------------
-- An example of Buchweitz
-- I = (x_1^q, ..., x_n^q, f), over ZZ/p, q = p^d.
-- compute a Gröbner basis, for various (random) f
-- and various q.
R = polyring(rawZZp 5, (symbol a .. symbol d))
M = rawMatrix1(R^1, 5, (a^3-2*a^2*b-a*b^2-2*b^3+a*b*c
	  -2*b^2*c+2*a*c^2-2*b*c^2-c^3+2*a*b*d-2*b^2*d
	  -a*c*d-2*b*c*d-2*c^2*d+a*d^2+c*d^2-d^3,
	  a^25, b^25, c^25, d^25), 0)
Gcomp = rawGB(M,false,0,{0},false,0,0,0,10)
rawStartComputation Gcomp
mgb = rawGBGetMatrix(Gcomp)
assert(rawNumberOfColumns mgb === 321)

---------------------------------------

-- TODO: split this up
exit 0;
