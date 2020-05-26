-- This one is too hard?
R = ZZ[symbol a .. symbol f];
I = ideal((5*a+b+3*c)^10, (3*a+17*b+4*d)^10, (9*b+13*c+12*d)^10-1);
time gens gb I;

D = 5
I = ideal((5*a+b+3*c)^D, (3*a+17*b+4*d)^D, (9*b+13*c+12*d)^D-1);
gbTrace=3
time gens gb I;

R = QQ[symbol a .. symbol f];
I = ideal((5*a+b+3*c)^12, (3*a+17*b+4*d)^12, (9*b+13*c+12*d)^12-1);
gbTrace=3
time gens gb I;
exit

R = QQ[symbol a .. symbol f];
I = ideal((5*a+b+3*c+e)^14, (3*a+17*b+4*d+e)^14, (9*b+13*c+12*d+2*e)^14-1);
gbTrace=3
time gb I;
exit

time gb I;
time leadTerm oo
time gens ooo;
