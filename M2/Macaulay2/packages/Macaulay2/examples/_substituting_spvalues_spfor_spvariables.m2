R = ZZ/101[x,y,z];
f = x^3+3*y^2*z+2*z^3;
substitute(f,matrix{{-1,2,5}})
substitute(f,{x=>-1,y=>2,z=>5})
M = matrix{{x^2,x-y},{x-z,z^2},{y-z,y^2}}
substitute(M,matrix{{-1,2,x+y}})
I = ideal M
substitute(I,{x=>-1,y=>2})
