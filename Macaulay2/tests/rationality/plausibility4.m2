----------------------------------
-- Testing reduction mod prec^3 --
----------------------------------

-- the precision
prec = 7
Fprec = ZZ/prec

-- the ring
Rmod = ZZ[x_1,x_2,x_3,C,D]/ideal(D^prec,prec^3*D^0)
R = ZZ[x_1,x_2,x_3,C,D]/ideal(D^prec)

-- functions
load "ScorzaOcta-fixed.m2"

-- a random point
const = 9; 
g = apply(const,i->(1,matrix{{x_1,x_2,x_3}}*random(ZZ^3,ZZ^1)))
gmod = apply(g,i->(sub(i#0,Rmod),sub(i#1,Rmod)))
     
-- a generic linear form
CD = C*x_1+D*x_2
CDmod = sub(CD,Rmod)

-- do calculations over ZZ and ZZ/prec^3 agree?
tally apply(prec..prec^2,
     n->time ScorzaR(n,CD,g,Fprec)==ScorzaR(n,CDmod,gmod,Fprec))
assert( keys oo === {true} ) -- Tally{true => 43}
