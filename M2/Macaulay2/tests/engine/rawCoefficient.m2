A = ZZ/101[a..d]
B = A[x,y,z]
f = (b+c)^2*x^2+(a+b)^2*x + y - 1

assert(coefficient(x,f) == (a+b)^2)
