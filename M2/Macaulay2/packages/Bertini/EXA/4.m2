CC[x,y];
f = x^2-1;
g = y^2+1;
h = ii*x-y;
needsPackage "Bertini"
sols = bertiniZeroDimSolve {f,g,h};
VerticalList sols
