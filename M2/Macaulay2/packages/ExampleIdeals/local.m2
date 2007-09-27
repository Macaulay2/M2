--------------------------------------------------------
--Dan example
kk = ZZ/101
R = kk{t,x,y,z}
I = ideal"2t2z+z3t+z3+tz,x2z4t3y+t7,x4+zy2+y2"
time transpose gens gb I
I
 {* -- Singular -- doesn't seem to finish... (3-0-2)
  ring R=101,(t,x,y,z),ds;
  ideal i = 2t2z+z3t+z3+tz,x2z4t3y+t7,x4+zy2+y2;
  timer=1;
  option(prot);
  std(i);
*}
--------------------------------------------------------
--Dan2
kk = ZZ/101
R = kk{t,x,y,z}
I = ideal"2t2z+z3t+z3+tz,x2z3t3y+t7,x4+zy2+y2"
time transpose gb I
I
{* -- Singular (very fast)
  ring R=101,(t,x,y,z),ds;
  ideal i = 2t2z+z3t+z3+tz,x2z3t3y+t7,x4+zy2+y2;
  timer=1;
  option(prot);
  std(i);
*}
--------------------------------------------------------
--mike1 
kk = ZZ/101
R = kk{t,x,y,z}
F = 4*t^2*z+6*z^3*t+3*z^3+t*z
G = z^4*t^3*y + t^7
H = x^5 + y^4
I = ideal(F,G,H)
time transpose gens gb I
--------------------------------------------------------
--mike1 
kk = ZZ/101
R = kk{t,x,y,z}
M = ideal"4t2z+6z3t+3z3+tz,
          5x2z4t3y + 3t7,
          2x5 + 6z2y2 + 2y4"

gbTrace=3
time gens gb M;
{* -- Singular
  ring R=101,(t,x,y,z),ds;
  ideal i = 4t2z+6z3t+3z3+tz,
          5x2z4t3y + 3t7,
          2x5 + 6z2y2 + 2y4;
  timer=1;
  option(prot);
  std(i);
*}
