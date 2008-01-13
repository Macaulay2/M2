R = ZZ[a..d,x,y];
m = matrix{{a*x^2+b*x*y+c*y^2, a*x^3+b*x^2*y+c*x*y^2+d*y^3+a*x^2}}
monomials m
monomials(m, Variables=>{x,y})
