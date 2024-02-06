--------------------------------------------------------
--dan1
kk = ZZ/101
R = kk{t,x,y,z}
I = ideal"2t2z+z3t+z3+tz,x2z4t3y+t7,x4+zy2+y2"
time transpose gens gb I
I
 -* -- Singular -- doesn't seem to finish... (3-0-2)
  ring R=101,(t,x,y,z),ds;
  ideal i = 2t2z+z3t+z3+tz,x2z4t3y+t7,x4+zy2+y2;
  timer=1;
  option(prot);
  std(i);
 *-
--------------------------------------------------------
--dan2
kk = ZZ/101
R = kk{t,x,y,z}
I = ideal"2t2z+z3t+z3+tz,x2z3t3y+t7,x4+zy2+y2"
time transpose gb I
I
-* -- Singular (very fast)
  ring R=101,(t,x,y,z),ds;
  ideal i = 2t2z+z3t+z3+tz,x2z3t3y+t7,x4+zy2+y2;
  timer=1;
  option(prot);
  std(i);
*-
--------------------------------------------------------
--dan3
kk = ZZ/103
R = kk{t,x,z}
I = ideal"tz+z3+tz3,t3x+z5,t2z3-xz4-tx3z2"
transpose gens gb I
--------------------------------------------------------
--mike1 
kk = ZZ/101
R = kk{t,x,y,z}
F = 4*t^2*z+6*z^3*t+3*z^3+t*z
G = z^4*t^3*y + t^7
H = x^5 + y^4
I = ideal(F,G,H)
time transpose gens gb I
I
--------------------------------------------------------
--mike2
kk = ZZ/101
R = kk{t,x,y,z}
M = ideal"4t2z+6z3t+3z3+tz,
          5x2z4t3y + 3t7,
          2x5 + 6z2y2 + 2y4"

gbTrace=3
time gens gb M;
M
-* -- Singular
  ring R=101,(t,x,y,z),ds;
  ideal i = 4t2z+6z3t+3z3+tz,
          5x2z4t3y + 3t7,
          2x5 + 6z2y2 + 2y4;
  timer=1;
  option(prot);
  std(i);
*-
--------------------------------------------------------
--mike2-homog
--this example is much faster than mike2 above
kk = ZZ/101
kk = QQ
R = kk[h,t,x,y,z,MonomialOrder=>{1,4}]
I = ideal"4t2z+6z3t+3z3+tz,
          5x2z4t3y + 3t7,
          2x5 + 6z2y2 + 2y4"
I = homogenize(I,h)
gbTrace=3
time gens gb I;
I
--------------------------------------------------------
--mike3
kk = ZZ/101
R = kk{t,x,y,z}
F = 4*t^2*z+6*z^3*t+3*z^3+t*z
G = 5*z^4*t^3*y + 3*t^7
H = 2*x^5 + 6*z^2*y^2 + 2*y^4
I = ideal(F,G,H)
gbTrace=3
time gens gb I;
I
--------------------------------------------------------
--mike4
kk = ZZ/101
R = kk{t,x,y,z}
F = 4*t^2*z+6*z^3*t+3*z^3+t*z
G = 5*x^2*z^4*t^3*y + 3*t^7
H = 2*x^8 + 6*z^2*y^2*t + 2*y^5
I = matrix(F,G,H)
time gens gb I
I
-*
-- Singular version of the above example:
-- configure Singular this way to get detailed trace output:
--   CPPFLAGS=-DKDEBUG=1 ./configure --prefix=/usr/local/Singular-3.0.3 --disable-ntl
ring R = 101, (t,x,y,z),ds;
poly F = 4*t^2*z+6*z^3*t+3*z^3+t*z;
poly G = 5*x^2*z^4*t^3*y + 3*t^7;
poly H = 2*x^8 + 6*z^2*y^2*t + 2*y^5;
ideal I = F,G,H;
timer=1;
option(teach);
std(I);
-- option(prot) gives this:
-- [255:2]4(2)s8s10s11.12.13.14.15.16s1721-s2223...24.-.s25(3)-.26-.s27(2)28....-29-.30.31.32.33.34.35
-- Output is:
-- (here o=deg-gap=degree of lead term, e=gap, l=size)
_[1]=tz+4t2z+3z3+6tz3
_[2]=y5+3ty2z2+x8
_[3]=t7-32t3x2yz4
_[4]=x2yz11+4tz13+49t6z9-16t4z11+6tx2yz11+23t5z11+8t2x2yz11-16t3z13
_[5]=y4z15+3tyz17+50t7y4z9+5t5y4z11+35t2x2y5z11-39t6yz13+4t3x2y2z13-8t3y4z13-49x2y5z13-12t4yz15+6tx2y2z15-2ty4z15-42x10z11-26t6y4z11-8t4y4z13+3tx2y5z13-8t5yz15+12t2x2y2z15-8t2y4z15-12t3yz17
_[6]=y3z19+3tz21+47x12z11+13t6x2y4z11+22t3x4y5z11-35t4x4y2z13-7t4x2y4z13-tx4y5z13-33t5x2yz15+49t2x4y2z15+44t5y3z15+4t2x2y4z15-39t6z17+2t3x2yz17+23x4y2z17+31t3y3z17-46x2y4z17-12t4z19+3tx2yz19+2ty3z19-31t5x2y4z13-2t2x4y5z13+2t3x2y4z15+2t4x2yz17+46tx4y2z17+31t4y3z17+9tx2y4z17-8t5z19+6t2x2yz19-12t3z21
_[7]=x14z11-47t6x4y4z11+37t3x6y5z11+10t4x6y2z13+2t4x4y4z13-43tx6y5z13-5t5x4yz15-14t2x6y2z15-27t5x2y3z15-30t2x4y4z15+40t6x2z17-15t3x4yz17+14t6y2z17-21x6y2z17+20t3x2y3z17+42x4y4z17-11t4x2z19+28tx4yz19-19t4y2z19+30tx2y3z19-11t2x2z21-19t2y2z21+17x2z23+11y2z23-20t5x4y4z13+15t2x6y5z13-15t3x4y4z15-15t4x4yz17-42tx6y2z17+20t4x2y3z17-17tx4y4z17-41t5x2z19-45t2x4yz19+21t5y2z19-41t2x2y3z19-11t3x2z21-19t3y2z21+34tx2z23+22ty2z23
*-
