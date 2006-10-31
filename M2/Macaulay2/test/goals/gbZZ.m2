-- This one is too hard?
R = ZZ[symbol a .. symbol f];
I = ideal((5*a+b+3*c)^10, (3*a+17*b+4*d)^10, (9*b+13*c+12*d)^10-1);
time gens gbI;
