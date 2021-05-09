-- Copyright 1999-2002 by Anton Leykin and Harrison Tsai

needsPackage "Dmodules"

-- dual of an Appell F1
I = AppellF1({2,-3,-2,5});
assert(ideal relations Ddual I == 
     substitute(AppellF1({-1,4,2,-3}), vars ring I));

-- dual of gkz associated to quadratic equation
A = matrix{{1,1,1},{0,1,2}};
b = {8/3,9/17};
I = gkz(A,b);
assert(ideal relations Ddual I == substitute(gkz(A,-b-{1,1}), vars ring I));
