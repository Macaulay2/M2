-- BAD BAD BUG!!
-- Found working with Peter Kahn, Sep 2013.
-- The third polynomial factors as two linears in x, over the field gen by b,r!!
-- But rawCharSeries misses this.
R = QQ[x,r,u,b, MonomialOrder=>Lex]
R = QQ[x,r,b,u, MonomialOrder=>Lex] -- any of these rings gives the wrong answer.
I = ideal(b^3-6*b^2+5*b-1,r^2-r*u+u^2*b+b^2-6*b+1,x^2+2*x*r*b^2-11*x*r*b+5*x*r+r*u-u^2*b)
debug Core
rawCharSeries raw gens I

