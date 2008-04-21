-----------------------
-- Testing precision --
-----------------------

-- the precision
prec = 7
Fprec = ZZ/prec

-- the rings
Rprec = ZZ[x_1,x_2,x_3,C,D]/ideal D^prec
R = ZZ[x_1,x_2,x_3,C,D]

-- functions
load "ScorzaOcta-fixed.m2"

-- random point
const = 9; 
g = apply(const,i->(1,matrix{{x_1,x_2,x_3}}*random(ZZ^3,ZZ^1)))
gprec = apply(g,i->(i#0,sub(i#1,Rprec)))

CD = C*x_1+D*x_2
CDprec = sub(CD,Rprec)
	  
time M = ScorzaR(61,CD,g,Fprec);
 -- used 7.83 seconds
time Mprec = ScorzaR(61,CDprec,gprec,Fprec);
-- used 0.71 seconds
-- the calculation with finite precision is faster
M-Mprec
assert( oo == 0 ) -- 0 (still the results are the same)
