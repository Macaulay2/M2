CC[x,y];
f = x^2-2; g = y^2-3;
needsPackage "Bertini"
F = {f,g};
sols = bertiniZeroDimSolve F; 
VerticalList sols
sols100 = bertiniRefineSols(F,sols,100)
matrix first sols100
first coordinates first sols100
