CC[a..e];
f1 = a+b+c+d+e; f2 = a*b + b*c + c*d + d*e + e*a; f3 = a*b*c + b*c*d + c*d*e + d*e*a + e*a*b; f4 = a*b*c*d + b*c*d*e + c*d*e*a + d*e*a*b + e*a*b*c; f5 = a*b*c*d*e - 1; END;
needsPackage "Bertini"
F = {f1,f2,f3,f4,f5};
sols = bertiniZeroDimSolve F; 
#sols
solsR = bertiniZeroDimSolve(F, USEREGENERATION=>1);
#solsR
areEqual(sortSolutions sols, sortSolutions solsR)
