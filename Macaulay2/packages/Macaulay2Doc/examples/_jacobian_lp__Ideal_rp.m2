R = QQ[x,y,z];
I = ideal(y^2-x*(x-1)*(x-13))
jacobian I
R = ZZ[a,b,c][x,y,z]
jacobian ideal(a*y*z+b*x*z+c*x*y)
