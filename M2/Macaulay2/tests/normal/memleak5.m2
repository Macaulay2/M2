-- try with:
--  ctest -T memcheck -R memleak5

R = ZZ[x,y,z];
I = ideal(9*x^6*y^2*z^2-8*x^2*y^3*z^5,3*x^8*z^2-x^5*y^2*z^3,x^5*y^5-3*x^2*y^6*z^2);
mingens gb I;
I = null;
R = null;
collectGarbage();
collectGarbage();
collectGarbage();
collectGarbage();
