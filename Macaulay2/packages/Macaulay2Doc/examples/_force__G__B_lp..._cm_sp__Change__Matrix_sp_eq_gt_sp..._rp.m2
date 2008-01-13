gbTrace = 3
R = ZZ[x,y,z];
f = matrix{{x^2-3, y^3-1, z^4-2}};
g = forceGB(f, ChangeMatrix=>id_(source f));
x^2*y^3 // g
