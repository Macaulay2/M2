R = QQ[a..g]
f = a^3+b^2*c+3*f^10*d-1+e-e
weightRange({1,1,0,0,0,0,0},f)
f = a^2*b+3*a^2*c+b*c+1
sum select(terms f, t -> (weightRange({1,0},t))#0 == 2)
S = R[x,y];
weightRange({0,0,3,7},a*x^2+b*x*y)
