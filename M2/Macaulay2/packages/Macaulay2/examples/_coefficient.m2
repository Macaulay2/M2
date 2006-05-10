R = GF(25,Variable=>a)[x,y,z];
f = ((a+1)*x+a*y+a^2*z)^2
coefficient(y^2,f)
S = R[r,s,t];
coefficient(r,a*x*(r+a*s))
