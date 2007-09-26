kk = ZZ/101
R = kk[t,x,y,z,MonomialOrder=>Weights=>4:-1,Global=>false]


i75 : alarm 10; time gb ideal{2*t^2*z+z^3*t+z^3+t*z,x^2*z^4*t^3*y+t^7,x^4+z*y^2+y^2}
stdio:40:16:(2):[2]: alarm occurred
     -- used 9.94462 seconds

-- Here is an example where M2 diverges from Singular, and goes slower, but still finishes.  Fix.

i77 : gbTrace=0; alarm 0; time transpose gens gb ideal{2*t^2*z+z^3*t+z^3+t*z,x^2*z^3*t^3*y+t^7,x^4+z*y^2+y^2}
     -- used 0.692043 seconds

o79 = {-17} | x6z10-t4yz11+3tx2y2z11-x2y2z12-2t2yz13-yz15+3tx6z10+4t3x2y2z10-x6z11+t5yz11-t3yz13-tyz15 |
      {-15} | x2yz10+t2z11-t5z9+3tx2yz10+t3z11+2t2x2yz10                                               |
      {-9}  | t7+t3x2yz3                                                                               |
      {-4}  | tz+2t2z+z3+tz3                                                                           |
      {-4}  | y2+y2z+x4                                                                                |

ring R = 101, (t,x,y,z),ds;
poly F = 2*t^2*z+z^3*t+z^3+t*z;
poly G = x^2*z^3*t^3*y+t^7;
poly H = x^4+z*y^2+y^2;
ideal I = F,G,H;
timer=1;
option(teach);
std(I);

