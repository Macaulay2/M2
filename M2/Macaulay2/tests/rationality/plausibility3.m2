------------------------
-- Testing periodicity --
------------------------

-- the precision
prec = 7
Fprec = ZZ/prec

-- the ring
R = ZZ[x_1,x_2,x_3,C,D]/ideal D^prec

-- functions
load "ScorzaOcta-fixed.m2"

-- a random point
const = 9; 
g = apply(const,i->(1,matrix{{x_1,x_2,x_3}}*random(ZZ^3,ZZ^1)))

-- a generic linear form
CD = C*x_1+D*x_2

-- is the coefficient matrix of the R_i indeed prec*(prec-1) periodic?
tally apply(prec..prec^2,
     n->time ScorzaR(n,CD,g,Fprec)==ScorzaR(n+prec*(prec-1),CD,g,Fprec))
assert( keys oo === {true} ) -- Tally{true => 43} (yes)
