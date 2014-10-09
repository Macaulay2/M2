needsPackage "Bertini"
-- 0-dimensional example: cyclic5
CC[x_1..x_5];
f1 = x_1+x_2+x_3+x_4+x_5; 
f2 = x_1*x_2 + x_2*x_3 + x_3*x_4 + x_4*x_5 + x_5*x_1; 
f3 = x_1*x_2*x_3 + x_2*x_3*x_4 + x_3*x_4*x_5 + x_4*x_5*x_1 + x_5*x_1*x_2; 
f4 = x_1*x_2*x_3*x_4 + x_2*x_3*x_4*x_5 + x_3*x_4*x_5*x_1 + x_4*x_5*x_1*x_2 + x_5*x_1*x_2*x_3; 
f5 = x_1*x_2*x_3*x_4*x_5 - 1; 
F = {f1,f2,f3,f4,f5};
sols = bertiniZeroDimSolve F
p := first sols
coordinates p
status p
peek p
sols2 = bertiniZeroDimSolve {f1^2,f2,f3,f4,f5};
peek first sols2
-- parameter homotopy
R = CC[x,y,z,u1,u2]
f1 = x^2+y^2-z^2
f2 = u1*x+u2*y
p0 = {{0,1}} -- parameters
p1 = {{1,0}} -- parameters
bPH = bertiniParameterHomotopy({f1,f2},{u1,u2},{p0,p1},ISPROJECTIVE=>1)
bPH#0 / clean_0.001@@matrix
bPH#1 / clean_0.001@@matrix
      
