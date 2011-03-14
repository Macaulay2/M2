R = (ZZ/101){t,x,y,z}

-----------------------------------------------------------------------------
-- this example runs faster M2, but Singular takes a long time

i20 : alarm 10; gens time gb ideal{2*t^2*z+z^3*t+z^3+t*z,x^2*z^4*t^3*y+t^7,x^4+z*y^2+y^2};
     -- used 0.004001 seconds

              1       6
o21 : Matrix R  <--- R

ring R = 101, (t,x,y,z),ds;
poly F = 2*t^2*z+z^3*t+z^3+t*z;
poly G = x^2*z^4*t^3*y+t^7;
poly H = x^4+z*y^2+y^2;
ideal I = F,G,H;
timer=1;
option(teach);
std(I);
-----------------------------------------------------------------------------
-- Here is an example where M2 and Singular take the same time

i24 : gbTrace=0; alarm 0; time transpose gens gb ideal{2*t^2*z+z^3*t+z^3+t*z,x^2*z^3*t^3*y+t^7,x^4+z*y^2+y^2}
     -- used 0.256016 seconds

o26 = {-17} | x6z10+6t2x2y2z10-t4yz11+2x2y2z12-2t2yz13-yz15+4t3x2y2z10-x6z11+t5yz11+3tx2y2z12-t3yz13-tyz15 |
      {-15} | x2yz10+t2z11-t5z9+3tx2yz10+t3z11+2t2x2yz10                                                   |
      {-9}  | t7+t3x2yz3                                                                                   |
      {-4}  | tz+2t2z+z3+tz3                                                                               |
      {-4}  | y2+y2z+x4                                                                                    |

              5       1
o26 : Matrix R  <--- R

ring R = 101, (t,x,y,z),ds;
poly F = 2*t^2*z+z^3*t+z^3+t*z;
poly G = x^2*z^3*t^3*y+t^7;
poly H = x^4+z*y^2+y^2;
ideal I = F,G,H;
timer=1;
option(teach);
std(I);

0.25user 0.00system 0:08.95elapsed 2%CPU (0avgtext+0avgdata 0maxresident)k
