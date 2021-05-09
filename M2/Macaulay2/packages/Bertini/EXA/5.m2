CC[x,y];
f = x*y-1;
g = x^2+1;
needsPackage "Bertini"
sols = bertiniZeroDimSolve {f,g};
VerticalList sols
