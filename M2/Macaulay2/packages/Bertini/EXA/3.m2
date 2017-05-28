CC[x,y];
f = x^2-1;
g = y^2+1;
h = ii*x-y;
p = 2*f + 3*g - 5*h;
q = 7*f - 11*g + 13*h;
needsPackage "Bertini"
sols = bertiniZeroDimSolve {p,q};
VerticalList sols
