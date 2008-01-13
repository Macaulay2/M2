R = QQ[x,y,z,a..d,MonomialOrder=>ProductOrder{3,4}];
f = matrix{{0,x^2*(a+b)}, {a*x+2*b*y, y^2*(c+d)}}
leadTerm(1,f)
