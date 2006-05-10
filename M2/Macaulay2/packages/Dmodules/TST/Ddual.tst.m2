-- Copyright 1999-2002 by Anton Leykin and Harrison Tsai

needs "Dmodules.m2"

-- dual of an Appell F1
I = AppellF1({2,-3,-2,5}, Vars => Local);
assert(ideal relations Ddual I == 
     substitute(AppellF1({-1,4,2,-3}, Vars=>Local), ring I));

-- dual of gkz associated to quadratic equation
A = matrix{{1,1,1},{0,1,2}};
b = {8/3,9/17};
I = gkz(A,b,Vars=>Local);
assert(ideal relations Ddual I == substitute(gkz(A,-b-{1,1},Vars=>Local), ring I));
