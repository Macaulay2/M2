-- Yanked from test/engine/raw-localgb.m2
--------------------------
-- singular example #18 --
--------------------------
kk = ZZ/101
R = kk[t,x,y,z,MonomialOrder=>Weights=>4:-1,Global=>false]
I = ideal(x-x^2)
gens gb I
x % I

-- This one is a problem
kk = ZZ/101
R = kk[t,x,y,z,MonomialOrder=>Weights=>4:-1,Global=>false]
F = 4*t^2*z+6*z^3*t+3*z^3+t*z
G = z^4*t^3*y + t^7
H = x^5 + y^4
M = matrix{{F,G,H}}
gbTrace=10
time gens(gb1 = gb M);
debug Core ; show raw gb1
G = reverse flatten entries gens gb1
netList G
(a1 = (z^10*x^5*G#0 - t*G#4)) % gb1
a1 = z^10*x^5*G#0 - t*G#4
a2 = a1 - 9*t^3*y^2*G_3 - 6*t^2*y^3*G_3 + 36*t^2*y^2*z^7*G_2 + 24*t*y^3*z^7*G_2 - 47*y^2*z^9*G_2 
a2 = -1/48_kk * a2
b1 = a2 - t^5*y^3*z^8*G_0 - 42*t^2*G_4 - 8*t^5*y*G_3 - 26*t^4*y^2*G_3 + 17*t^3*y^3*G_3 + 19*z^2*G_4 - 31*t^2*y^2*z^2*G_3 + 13*t*y^3*z^2*G_3 + 32*t^4*y*z^7*G_2
b2 = b1 + 3*t^3*y^2*z^7*G_2 + 33*t^2*y^3*z^7*G_2 + 48*t^2*y*z^9*G_2 - 23*t*y^2*z^9*G_2 - 50*y^3*z^9*G_2 - 29*y*z^11*G_2 + 32*t^6*y*G_3
b2 + 26*t^5*y^2*G_3
      +--------------------------------------------------------------+
      |        2      3       3                                      |
o19 = |t*z + 4t z + 3z  + 6t*z                                       |
      +--------------------------------------------------------------+
      | 4    5                                                       |
      |y  + x                                                        |
      +--------------------------------------------------------------+
      | 7    3   4                                                   |
      |t  + t y*z                                                    |
      +--------------------------------------------------------------+
      |   11     6 7     4 9     2 11         11      3 11     2   11|
      |y*z   + 4t z  + 6t z  + 9t z   + 4t*y*z   + 18t z   + 4t y*z  |
      +--------------------------------------------------------------+
      | 5 11     2 3 11       4 11      5 3 9      3 3 11     2 4 11 |
      |x z   - 9t y z   - 6t*y z   + 12t y z  - 18t y z   - 8t y z   |
      +--------------------------------------------------------------+

   -- ......x...........x........x.............x..............x..........x.......
   -- ............x.................x...................................x........
   -- ...................................x.......................................
   -- .......x.............................................................x.....
   -- .............................................x.............................
   -- ...........................x...............................................
   -- .......................x...................................................
   -- ..x..................................................x.....................
   -- ...........................x.......................................x.......
   -- .....................................x.....................................
   -- ....................x...........................................x..........
   -- .......................x.....................................x.............
   -- .........................x..........................x......................
   -- .........x................................x.......................x........
   -- ....................x....................x............................x....
   -- ................x..............x...........

   -- .....x..........x.........x..............x.........x...........x...........
   -- ........x..............................x...................................
   -- .....x..............................................x......................
   -- ......................................x....................................
   -- .......x.......................................................x...........
   -- ...........................x....................................x..........
   -- ..........................x..........................x.....................
   -- .......x.....................x............................x..............x.
   -- ..................x..........x.............
o10 = 0

(x^4*G#3 - y*G#4) % gb1
(t*G#3 - y*z^10*G#0) % gb1
(y^3*G#3 - z^11*G#1) % gb1

kk = ZZ/101
R = kk[t,x,y,z,MonomialOrder=>Weights=>4:-1,Global=>false]
F = 4*t^2*z+6*z^3*t+3*z^3+t*z
G = 5*z^4*t^3*y + 3*t^7
H = 2*x^5 + 6*z^2*y^2 + 2*y^4
M = matrix{{F,G,H}}
gbTrace=3
time gens gb M;

kk = ZZ/101
R = kk[t,x,y,z,MonomialOrder=>Weights=>4:-1,Global=>false]
F = 4*t^2*z+6*z^3*t+3*z^3+t*z
G = 5*x^2*z^4*t^3*y + 3*t^7
H = 2*x^5 + 6*z^2*y^2 + 2*y^4
M = matrix{{F,G,H}}
gbTrace=3
time gens gb M;

----------------------------------------------------------------------------
-- Current example Mike is workiing on (Sep 2007)
-- Mike says this one doesn't seem to finish
kk = ZZ/101
R = kk[t,x,y,z,MonomialOrder=>Weights=>4:-1,Global=>false]
F = 4*t^2*z+6*z^3*t+3*z^3+t*z
G = 5*x^2*z^4*t^3*y + 3*t^7
H = 2*x^8 + 6*z^2*y^2*t + 2*y^5
M = matrix{{F,G,H}}
gbTrace=15
time gens gb(M, DegreeLimit=>38);
time gens gb M;
J = ideal"tz+4t2z+3z3+6tz3,
  y5+3ty2z2+x8,
  t7-32t3x2yz4,
  x2yz11+4tz13+49t6z9-16t4z11+6tx2yz11+23t5z11+8t2x2yz11-16t3z13,
  y4z15+3tyz17+50t7y4z9+5t5y4z11+35t2x2y5z11-39t6yz13+4t3x2y2z13-8t3y4z13-49x2y5z13-12t4yz15+6tx2y2z15-2ty4z15-42x10z11-26t6y4z11-8t4y4z13+3tx2y5z13-8t5yz15+12t2x2y2z15-8t2y4z15-12t3yz17,
  y3z19+3tz21+47x12z11+13t6x2y4z11+22t3x4y5z11-35t4x4y2z13-7t4x2y4z13-tx4y5z13-33t5x2yz15+49t2x4y2z15+44t5y3z15+4t2x2y4z15-39t6z17+2t3x2yz17+23x4y2z17+31t3y3z17-46x2y4z17-12t4z19+3tx2yz19+2ty3z19-31t5x2y4z13-2t2x4y5z13+2t3x2y4z15+2t4x2yz17+46tx4y2z17+31t4y3z17+9tx2y4z17-8t5z19+6t2x2yz19-12t3z21,
  x14z11-47t6x4y4z11+37t3x6y5z11+10t4x6y2z13+2t4x4y4z13-43tx6y5z13-5t5x4yz15-14t2x6y2z15-27t5x2y3z15-30t2x4y4z15+40t6x2z17-15t3x4yz17+14t6y2z17-21x6y2z17+20t3x2y3z17+42x4y4z17-11t4x2z19+28tx4yz19-19t4y2z19+30tx2y3z19-11t2x2z21-19t2y2z21+17x2z23+11y2z23-20t5x4y4z13+15t2x6y5z13-15t3x4y4z15-15t4x4yz17-42tx6y2z17+20t4x2y3z17-17tx4y4z17-41t5x2z19-45t2x4yz19+21t5y2z19-41t2x2y3z19-11t3x2z21-19t3y2z21+34tx2z23+22ty2z23"
LM = reverse flatten entries gens (GM = gb(M, DegreeLimit=>27))
LS = flatten entries gens J
LM == LS
select(0..#LM-1, i -> LM#i == LS#i)

size(LM_4)
size(LS_4)
(LM_4-LS_4) % GM

LM_5
LS_5
size LM_5
size LS_5
(LM_5-LS_5)%GM

LM_6
LS_6
size LM_6
size LS_6
(LM_6-LS_6)%GM

leadTerm gens GM

-- Singular version of the above example:
-- configured this way: CPPFLAGS=-DKDEBUG=1 ./configure --prefix=/usr/local/Singular-3.0.3 --disable-ntl
ring R = 101, (t,x,y,z),ds;
poly F = 4*t^2*z+6*z^3*t+3*z^3+t*z;
poly G = 5*x^2*z^4*t^3*y + 3*t^7;
poly H = 2*x^8 + 6*z^2*y^2*t + 2*y^5;

-- Try this one instead...
poly H = 2*x^5 + 6*z^2*y^2 + 2*y^4;


ideal I = F,G,H;
timer=1;
option(teach);
std(I);
-- option(prot) gives this:
-- [255:2]4(2)s8s10s11.12.13.14.15.16s1721-s2223...24.-.s25(3)-.26-.s27(2)28....-29-.30.31.32.33.34.35
-- Output is:
-- (here o=deg-gap=degree of lead term, e=gap, l=size)
ideal/module is not homogeneous

set L
2:NULL  NULL lcm: NULL
  p : tz+4t2z+...  o:2 e:2 l:4
1:NULL  NULL lcm: NULL
  p : y5+3ty2z2+...  o:5 e:3 l:3
0:NULL  NULL lcm: NULL
  p : t7-32t3x2yz4  o:7 e:3 l:2
new s0:tz+4t16440x16438y2z256*gen(3)+...
set S
  0:tz+4t2z+...
set T
  0:tz+4t2z+... o:2 e:2 l:4
set L
1:NULL  NULL lcm: NULL
  p : y5+3ty2z2+...  o:5 e:3 l:3
0:NULL  NULL lcm: NULL
  p : t7-32t3x2yz4  o:7 e:3 l:2
new s1:y5+3yz514*gen(1)+...
set S
  0:tz+4t2z+...
  1:y5+3ty2z2+...
set T
  0:tz+4t2z+... o:2 e:2 l:4
  1:y5+3ty2z2+... o:5 e:3 l:3
set L
0:NULL  NULL lcm: NULL
  p : t7-32t3x2yz4  o:7 e:3 l:2
new s2:t7-32t16520x16438y515z1025
set S
  0:tz+4t2z+...
  1:y5+3ty2z2+...
  2:t7-32t3x2yz4
set T
  0:tz+4t2z+... o:2 e:2 l:4
  1:y5+3ty2z2+... o:5 e:3 l:3
  2:t7-32t3x2yz4 o:7 e:3 l:2
set L
0:tz+4t2z+...  t7-32t3x2yz4 lcm: 0t7z
  p : 0t8z+0  o:9 e:2 l:0
reduce -4t8z with tz+4t2z+...
to -3t6z3
reduce -3t6z3 with tz+4t2z+...
to 16t9z
reduce 16t9z with tz+4t2z+...
to 18t7z3
reduce 18t7z3 with tz+4t2z+...
to 9t5z5
reduce 9t5z5 with tz+4t2z+...
to 37t10z
reduce 37t10z with t9z+39t7z3+...
to -24t8z3
reduce -24t8z3 with tz+4t2z+...
to -36t6z5
reduce -36t6z5 with tz+4t2z+...
to -32t3x2yz5
reduce -32t3x2yz5 with tz+4t2z+...
to -27t4z7
reduce -27t4z7 with tz+4t2z+...
to -5t9z3
reduce -5t9z3 with t9z+39t7z3+...
to -29t7z5
reduce -29t7z5 with tz+4t2z+...
to 7t5z7
reduce 7t5z7 with tz+4t2z+...
to -5t2x2yz7
reduce -5t2x2yz7 with tz+4t2z+...
to -20t3z9
reduce -20t3z9 with tz+4t2z+...
to 15t8z5
reduce 15t8z5 with t8z3-49t6z5+...
to -7t6z7
reduce -7t6z7 with tz+4t2z+...
to -20t3x2yz7
reduce -20t3x2yz7 with tz+4t2z+...
to 40t4z9
reduce 40t4z9 with tz+4t2z+...
to 15tx2yz9
reduce 15tx2yz9 with tz+4t2z+...
to -41t2z11
reduce -41t2z11 with tz+4t2z+...
to 28t7z7
reduce 28t7z7 with t7z5-49t5z7+...
to 21t5z9
reduce 21t5z9 with tz+4t2z+...
to -41t2x2yz9
reduce -41t2x2yz9 with tz+4t2z+...
to -19t3z11
reduce -19t3z11 with tz+4t2z+...
to -45x2yz11
new s3:x2yz11+4t17032x16438yz3328*gen(49)+...
set S
  0:tz+4t2z+...
  1:y5+3ty2z2+...
  2:t7-32t3x2yz4
  3:x2yz11+4tz13+...
set T
  0:tz+4t2z+... o:2 e:2 l:4
  1:y5+3ty2z2+... o:5 e:3 l:3
  2:t7-32t3x2yz4 o:7 e:3 l:2
  3:t9z+39t7z3+... o:10 e:1 l:6
  4:t8z3-49t6z5+... o:11 e:1 l:7
  5:t7z5-49t5z7+... o:12 e:1 l:7
  6:-7t6z7-20t3x2yz7+... o:13 e:1 l:8
  7:21t5z9-41t2x2yz9+... o:14 e:1 l:8
  8:x2yz11+4tz13+... o:14 e:2 l:8
set L
1:tz+4t2z+...  x2yz11+4tz13+... lcm: 0tx2yz11
  p : 0t2z13+0  o:15 e:2 l:0
0:y5+3ty2z2+...  x2yz11+4tz13+... lcm: 0x2y5z11
  p : 0tx2y2z13+0  o:18 e:3 l:0
reduce 4t2z13 with tz+4t2z+...
to 49t7z9
reduce 49t7z9 with t7z5-49t5z7+...
to -39t5z11
reduce -39t5z11 with t5z9-26t2x2yz9+...
to NULL

set L
0:y5+3ty2z2+...  x2yz11+4tz13+... lcm: 0x2y5z11
  p : 0tx2y2z13+0  o:18 e:3 l:0
reduce -3tx2y2z13 with tz+4t2z+...
to 4ty4z13
reduce 4ty4z13 with tz+4t2z+...
to 49t6y4z9
reduce 49t6y4z9 with tz+4t2z+...
to -16t4y4z11
reduce -16t4y4z11 with tz+4t2z+...
to 6tx2y5z11
reduce 6tx2y5z11 with tz+4t2z+...
to 12t2x2y2z13
reduce 12t2x2y2z13 with tz+4t2z+...
to -16t2y4z13
reduce -16t2y4z13 with tz+4t2z+...
to 9x2y2z15
reduce 9x2y2z15 with x2yz11+4tz13+...
to -12y4z15
new s4:y4z15+3t17144x16438yz4353*gen(50)+...
set S
  0:tz+4t2z+...
  1:y5+3ty2z2+...
  2:t7-32t3x2yz4
  3:x2yz11+4tz13+...
  4:y4z15+3tyz17+...
set T
  0:tz+4t2z+... o:2 e:2 l:4
  1:y5+3ty2z2+... o:5 e:3 l:3
  2:t7-32t3x2yz4 o:7 e:3 l:2
  3:t9z+39t7z3+... o:10 e:1 l:6
  4:t8z3-49t6z5+... o:11 e:1 l:7
  5:t7z5-49t5z7+... o:12 e:1 l:7
  6:-7t6z7-20t3x2yz7+... o:13 e:1 l:8
  7:t5z9-26t2x2yz9+... o:14 e:1 l:8
  8:x2yz11+4tz13+... o:14 e:2 l:8
  9:y4z15+3tyz17+... o:19 e:2 l:20
set L
2:tz+4t2z+...  y4z15+3tyz17+... lcm: 0ty4z15
  p : 0t2yz17+0  o:20 e:2 l:0
1:y5+3ty2z2+...  y4z15+3tyz17+... lcm: 0y5z15
  p : 0t7y5z9+0  o:21 e:2 l:0
0:x2yz11+4tz13+...  y4z15+3tyz17+... lcm: 0x2y4z15
  p : 0tx2yz17+0  o:21 e:2 l:0
reduce 3t2yz17 with tz+4t2z+...
to 50t8y4z9
reduce 50t8y4z9 with t8z3-49t6z5+...
to 31t6y4z11
reduce 31t6y4z11 with t6z7-26t3x2yz7+...
to -39t7yz13
reduce -39t7yz13 with t7z5-49t5z7+...
to 4t4x2y2z13
reduce 4t4x2y2z13 with tz+4t2z+...
to -35t4y4z13
 degree jumped; ->L1

set T
  0:tz+4t2z+... o:2 e:2 l:4
  1:y5+3ty2z2+... o:5 e:3 l:3
  2:t7-32t3x2yz4 o:7 e:3 l:2
  3:t9z+39t7z3+... o:10 e:1 l:6
  4:t8z3-49t6z5+... o:11 e:1 l:7
  5:t7z5-49t5z7+... o:12 e:1 l:7
  6:t6z7-26t3x2yz7+... o:13 e:1 l:8
  7:t5z9-26t2x2yz9+... o:14 e:1 l:8
  8:x2yz11+4tz13+... o:14 e:2 l:8
  9:y4z15+3tyz17+... o:19 e:2 l:20
  10:4t4x2y2z13-35t4y4z13+... o:21 e:1 l:16
set L
2:y5+3ty2z2+...  y4z15+3tyz17+... lcm: 0y5z15
  p : 0t7y5z9+0  o:21 e:2 l:0
1:tz+4t2z+...  y4z15+3tyz17+... lcm: 0ty4z15
  p : -35t4y4z13  o:21 e:2 l:0
0:x2yz11+4tz13+...  y4z15+3tyz17+... lcm: 0x2y4z15
  p : 0tx2yz17+0  o:21 e:2 l:0
reduce 50t7y5z9 with tz+4t2z+...
to 5t5y5z11
reduce 5t5y5z11 with tz+4t2z+...
to 35t2x2y6z11
reduce 35t2x2y6z11 with tz+4t2z+...
to -39t6y2z13
reduce -39t6y2z13 with tz+4t2z+...
to 4t3x2y3z13
reduce 4t3x2y3z13 with tz+4t2z+...
to -8t3y5z13
reduce -8t3y5z13 with tz+4t2z+...
to -49x2y6z13
reduce -49x2y6z13 with x2yz11+4tz13+...
to -12t4y2z15
reduce -12t4y2z15 with tz+4t2z+...
to 6tx2y3z15
reduce 6tx2y3z15 with tz+4t2z+...
to -8ty5z15
reduce -8ty5z15 with tz+4t2z+...
to 2t8y5z9
reduce 2t8y5z9 with t8z3-49t6z5+...
to -42x10yz11
reduce -42x10yz11 with x2yz11+4tz13+...
to -20t6y5z11
 degree jumped; ->L0

set T
  0:tz+4t2z+... o:2 e:2 l:4
  1:y5+3ty2z2+... o:5 e:3 l:3
  2:t7-32t3x2yz4 o:7 e:3 l:2
  3:t9z+39t7z3+... o:10 e:1 l:6
  4:t8z3-49t6z5+... o:11 e:1 l:7
  5:t7z5-49t5z7+... o:12 e:1 l:7
  6:t6z7-26t3x2yz7+... o:13 e:1 l:8
  7:t5z9-26t2x2yz9+... o:14 e:1 l:8
  8:x2yz11+4tz13+... o:14 e:2 l:8
  9:y4z15+3tyz17+... o:19 e:2 l:20
  10:4t4x2y2z13-35t4y4z13+... o:21 e:1 l:16
  11:-42x10yz11-20t6y5z11+... o:22 e:1 l:23
set L
2:tz+4t2z+...  y4z15+3tyz17+... lcm: 0ty4z15
  p : -35t4y4z13  o:21 e:2 l:0
1:x2yz11+4tz13+...  y4z15+3tyz17+... lcm: 0x2y4z15
  p : 0tx2yz17+0  o:21 e:2 l:0
0:y5+3ty2z2+...  y4z15+3tyz17+... lcm: 0y5z15
  p : -20t6y5z11  o:22 e:2 l:0
reduce -35t4y4z13 with tz+4t2z+...
to 3tx2y5z13
reduce 3tx2y5z13 with tz+4t2z+...
to -4t5yz15
reduce -4t5yz15 with tz+4t2z+...
to -43t2x2y2z15
reduce -43t2x2y2z15 with tz+4t2z+...
to -6t3yz17
reduce -6t3yz17 with tz+4t2z+...
to -3y4z17
reduce -3y4z17 with y4z15+3tyz17+...
to -42tx10z11
reduce -42tx10z11 with tz+4t2z+...
to 49t7y4z11
 degree jumped; ->L1

set T
  0:tz+4t2z+... o:2 e:2 l:4
  1:y5+3ty2z2+... o:5 e:3 l:3
  2:t7-32t3x2yz4 o:7 e:3 l:2
  3:t9z+39t7z3+... o:10 e:1 l:6
  4:t8z3-49t6z5+... o:11 e:1 l:7
  5:t7z5-49t5z7+... o:12 e:1 l:7
  6:t6z7-26t3x2yz7+... o:13 e:1 l:8
  7:t5z9-26t2x2yz9+... o:14 e:1 l:8
  8:x2yz11+4tz13+... o:14 e:2 l:8
  9:y4z15+3tyz17+... o:19 e:2 l:20
  10:4t4x2y2z13-35t4y4z13+... o:21 e:1 l:16
  11:-42tx10z11+49t7y4z11+... o:22 e:1 l:18
  12:-42x10yz11-20t6y5z11+... o:22 e:1 l:23
set L
2:x2yz11+4tz13+...  y4z15+3tyz17+... lcm: 0x2y4z15
  p : 0tx2yz17+0  o:21 e:2 l:0
1:tz+4t2z+...  y4z15+3tyz17+... lcm: 0ty4z15
  p : 49t7y4z11  o:22 e:2 l:0
0:y5+3ty2z2+...  y4z15+3tyz17+... lcm: 0y5z15
  p : -20t6y5z11  o:22 e:2 l:0
reduce 3tx2yz17 with tz+4t2z+...
to -4ty3z17
reduce -4ty3z17 with tz+4t2z+...
to 50t7x2y4z9
reduce 50t7x2y4z9 with t7z5-49t5z7+...
to 31t5x2y4z11
reduce 31t5x2y4z11 with t5z9-26t2x2yz9+...
to -39t6x2yz13
reduce -39t6x2yz13 with t6z7-26t3x2yz7+...
to -49t6y3z13
reduce -49t6y3z13 with t6z7-26t3x2yz7+...
to 4t3x2y4z13
reduce 4t3x2y4z13 with tz+4t2z+...
to 3x4y5z13
 degree jumped; ->L0

set T
  0:tz+4t2z+... o:2 e:2 l:4
  1:y5+3ty2z2+... o:5 e:3 l:3
  2:t7-32t3x2yz4 o:7 e:3 l:2
  3:t9z+39t7z3+... o:10 e:1 l:6
  4:t8z3-49t6z5+... o:11 e:1 l:7
  5:t7z5-49t5z7+... o:12 e:1 l:7
  6:t6z7-26t3x2yz7+... o:13 e:1 l:8
  7:t5z9-26t2x2yz9+... o:14 e:1 l:8
  8:x2yz11+4tz13+... o:14 e:2 l:8
  9:y4z15+3tyz17+... o:19 e:2 l:20
  10:4t4x2y2z13-35t4y4z13+... o:21 e:1 l:16
  11:-42tx10z11+49t7y4z11+... o:22 e:1 l:18
  12:-42x10yz11-20t6y5z11+... o:22 e:1 l:23
  13:4t3x2y4z13+3x4y5z13+... o:22 e:1 l:21
set L
2:tz+4t2z+...  y4z15+3tyz17+... lcm: 0ty4z15
  p : 49t7y4z11  o:22 e:2 l:0
1:y5+3ty2z2+...  y4z15+3tyz17+... lcm: 0y5z15
  p : -20t6y5z11  o:22 e:2 l:0
0:x2yz11+4tz13+...  y4z15+3tyz17+... lcm: 0x2y4z15
  p : 3x4y5z13  o:22 e:2 l:0
reduce 49t7y4z11 with tz+4t2z+...
to -39t4x2y5z11
reduce -39t4x2y5z11 with tz+4t2z+...
to -16t5x2y2z13
reduce -16t5x2y2z13 with tz+4t2z+...
to -16t5y4z13
reduce -16t5y4z13 with tz+4t2z+...
to -2t2x2y5z13
reduce -2t2x2y5z13 with tz+4t2z+...
to -12t3x2y2z15
reduce -12t3x2y2z15 with tz+4t2z+...
to -16t3y4z15
reduce -16t3y4z15 with tz+4t2z+...
to 46x2y5z15
reduce 46x2y5z15 with x2yz11+4tz13+...
to 46tx2y2z17
reduce 46tx2y2z17 with tz+4t2z+...
to 6ty4z17
reduce 6ty4z17 with tz+4t2z+...
to -34t2x10z11
reduce -34t2x10z11 with tx10z11-18t7y4z11+...
to 46t6y4z13
reduce 46t6y4z13 with t6z7-26t3x2yz7+...
to 24t4x2y2z15
reduce 24t4x2y2z15 with t4x2y2z13-34t4y4z13+...
to 24t5yz17
reduce 24t5yz17 with t5z9-26t2x2yz9+...
to NULL

set L
1:y5+3ty2z2+...  y4z15+3tyz17+... lcm: 0y5z15
  p : -20t6y5z11  o:22 e:2 l:0
0:x2yz11+4tz13+...  y4z15+3tyz17+... lcm: 0x2y4z15
  p : 3x4y5z13  o:22 e:2 l:0
reduce -20t6y5z11 with tz+4t2z+...
to -8t3x2y6z11
reduce -8t3x2y6z11 with tz+4t2z+...
to -34tx8z13
reduce -34tx8z13 with tz+4t2z+...
to -46t7y2z13
reduce -46t7y2z13 with tz+4t2z+...
to -16t4x2y3z13
reduce -16t4x2y3z13 with tz+4t2z+...
to -45t4y5z13
reduce -45t4y5z13 with tz+4t2z+...
to -10tx2y6z13
reduce -10tx2y6z13 with tz+4t2z+...
to -45t5y2z15
reduce -45t5y2z15 with tz+4t2z+...
to -24t2x2y3z15
reduce -24t2x2y3z15 with tz+4t2z+...
to 48t2y5z15
reduce 48t2y5z15 with tz+4t2z+...
to 24t3y2z17
reduce 24t3y2z17 with tz+4t2z+...
to -18x2y3z17
reduce -18x2y3z17 with x2yz11+4tz13+...
to 24y5z17
reduce 24y5z17 with y4z15+3tyz17+...
to 38t6x8z9
reduce 38t6x8z9 with t6z7-26t3x2yz7+...
to -22t3x10yz9
reduce -22t3x10yz9 with tz+4t2z+...
to -22t4x8z11
 degree jumped; ->L0

set T
  0:tz+4t2z+... o:2 e:2 l:4
  1:y5+3ty2z2+... o:5 e:3 l:3
  2:t7-32t3x2yz4 o:7 e:3 l:2
  3:t9z+39t7z3+... o:10 e:1 l:6
  4:t8z3-49t6z5+... o:11 e:1 l:7
  5:t7z5-49t5z7+... o:12 e:1 l:7
  6:t6z7-26t3x2yz7+... o:13 e:1 l:8
  7:t5z9-26t2x2yz9+... o:14 e:1 l:8
  8:x2yz11+4tz13+... o:14 e:2 l:8
  9:y4z15+3tyz17+... o:19 e:2 l:20
  10:t4x2y2z13-34t4y4z13+... o:21 e:1 l:16
  11:tx10z11-18t7y4z11+... o:22 e:1 l:18
  12:-42x10yz11-20t6y5z11+... o:22 e:1 l:23
  13:4t3x2y4z13+3x4y5z13+... o:22 e:1 l:21
  14:-22t3x10yz9-22t4x8z11+... o:23 e:1 l:31
set L
1:x2yz11+4tz13+...  y4z15+3tyz17+... lcm: 0x2y4z15
  p : 3x4y5z13  o:22 e:2 l:0
0:y5+3ty2z2+...  y4z15+3tyz17+... lcm: 0y5z15
  p : -22t4x8z11  o:23 e:2 l:0
reduce 3x4y5z13 with x2yz11+4tz13+...
to -4t4x2yz15
reduce -4t4x2yz15 with tz+4t2z+...
to 9tx4y2z15
reduce 9tx4y2z15 with tz+4t2z+...
to 39t4y3z15
reduce 39t4y3z15 with tz+4t2z+...
to -18tx2y4z15
reduce -18tx2y4z15 with tz+4t2z+...
to -9x2yz19
reduce -9x2yz19 with x2yz11+4tz13+...
to 12y3z19
new s5:y3z19+3t19112x16438yz5376*gen(47)+...
set S
  0:tz+4t2z+...
  1:y5+3ty2z2+...
  2:t7-32t3x2yz4
  3:x2yz11+4tz13+...
  4:y4z15+3tyz17+...
  5:y3z19+3tz21+...
set T
  0:tz+4t2z+... o:2 e:2 l:4
  1:y5+3ty2z2+... o:5 e:3 l:3
  2:t7-32t3x2yz4 o:7 e:3 l:2
  3:t9z+39t7z3+... o:10 e:1 l:6
  4:t8z3-49t6z5+... o:11 e:1 l:7
  5:t7z5-49t5z7+... o:12 e:1 l:7
  6:t6z7-26t3x2yz7+... o:13 e:1 l:8
  7:t5z9-26t2x2yz9+... o:14 e:1 l:8
  8:x2yz11+4tz13+... o:14 e:2 l:8
  9:y4z15+3tyz17+... o:19 e:2 l:20
  10:t4x2y2z13-34t4y4z13+... o:21 e:1 l:16
  11:tx10z11-18t7y4z11+... o:22 e:1 l:18
  12:-42x10yz11-20t6y5z11+... o:22 e:1 l:23
  13:4t3x2y4z13+3x4y5z13+... o:22 e:1 l:21
  14:y3z19+3tz21+... o:22 e:2 l:30
  15:-22t3x10yz9-22t4x8z11+... o:23 e:1 l:31
set L
3:y4z15+3tyz17+...  y3z19+3tz21+... lcm: 0y4z19
  p : 0x12yz11+0  o:24 e:1 l:0
2:y5+3ty2z2+...  y4z15+3tyz17+... lcm: 0y5z15
  p : -22t4x8z11  o:23 e:2 l:0
1:tz+4t2z+...  y3z19+3tz21+... lcm: 0ty3z19
  p : 0t2z21+0  o:23 e:2 l:0
0:x2yz11+4tz13+...  y3z19+3tz21+... lcm: 0x2y3z19
  p : 0tx2z21+0  o:24 e:2 l:0
reduce 47x12yz11 with x10yz11-38t6y5z11+...
to -19t6x2y5z11
reduce -19t6x2y5z11 with t6z7-26t3x2yz7+...
to -13t7x2y2z13
reduce -13t7x2y2z13 with t7z5-49t5z7+...
to -50t7y4z13
reduce -50t7y4z13 with t7z5-49t5z7+...
to -35t5x2y2z15
reduce -35t5x2y2z15 with t5z9-26t2x2yz9+...
to 13t5y4z15
reduce 13t5y4z15 with t5z9-26t2x2yz9+...
to NULL

set L
2:y5+3ty2z2+...  y4z15+3tyz17+... lcm: 0y5z15
  p : -22t4x8z11  o:23 e:2 l:0
1:tz+4t2z+...  y3z19+3tz21+... lcm: 0ty3z19
  p : 0t2z21+0  o:23 e:2 l:0
0:x2yz11+4tz13+...  y3z19+3tz21+... lcm: 0x2y3z19
  p : 0tx2z21+0  o:24 e:2 l:0
reduce -22t4x8z11 with tz+4t2z+...
to 16tx10yz11
reduce 16tx10yz11 with tz+4t2z+...
to -9t7y5z11
reduce -9t7y5z11 with tz+4t2z+...
to -46t4x2y6z11
reduce -46t4x2y6z11 with tz+4t2z+...
to -18t8y2z13
reduce -18t8y2z13 with tz+4t2z+...
to -37t5x2y3z13
reduce -37t5x2y3z13 with tz+4t2z+...
to -50t5y5z13
reduce -50t5y5z13 with tz+4t2z+...
to 12t2x2y6z13
reduce 12t2x2y6z13 with tz+4t2z+...
to 47t6y2z15
reduce 47t6y2z15 with tz+4t2z+...
to 24t3x2y3z15
reduce 24t3x2y3z15 with tz+4t2z+...
to 5t3y5z15
reduce 5t3y5z15 with tz+4t2z+...
to -6x2y6z15
reduce -6x2y6z15 with x2yz11+4tz13+...
to 10t4y2z17
reduce 10t4y2z17 with tz+4t2z+...
to -24ty5z17
reduce -24ty5z17 with tz+4t2z+...
to 29t2y2z19
reduce 29t2y2z19 with tz+4t2z+...
to 44t5x8z11
reduce 44t5x8z11 with t5z9-26t2x2yz9+...
to 36t8y5z11
reduce 36t8y5z11 with t8z3-49t6z5+...
to -18t5x2y6z11
reduce -18t5x2y6z11 with t5z9-26t2x2yz9+...
to 37t2x4y7z11
reduce 37t2x4y7z11 with tz+4t2z+...
to -35t3x8z13
 degree jumped; ->L1

set T
  0:tz+4t2z+... o:2 e:2 l:4
  1:y5+3ty2z2+... o:5 e:3 l:3
  2:t7-32t3x2yz4 o:7 e:3 l:2
  3:t9z+39t7z3+... o:10 e:1 l:6
  4:t8z3-49t6z5+... o:11 e:1 l:7
  5:t7z5-49t5z7+... o:12 e:1 l:7
  6:t6z7-26t3x2yz7+... o:13 e:1 l:8
  7:t5z9-26t2x2yz9+... o:14 e:1 l:8
  8:x2yz11+4tz13+... o:14 e:2 l:8
  9:y4z15+3tyz17+... o:19 e:2 l:20
  10:t4x2y2z13-34t4y4z13+... o:21 e:1 l:16
  11:tx10z11-18t7y4z11+... o:22 e:1 l:18
  12:x10yz11-38t6y5z11+... o:22 e:1 l:23
  13:4t3x2y4z13+3x4y5z13+... o:22 e:1 l:21
  14:y3z19+3tz21+... o:22 e:2 l:30
  15:-22t3x10yz9-22t4x8z11+... o:23 e:1 l:31
  16:37t2x4y7z11-35t3x8z13+... o:24 e:1 l:36
set L
2:tz+4t2z+...  y3z19+3tz21+... lcm: 0ty3z19
  p : 0t2z21+0  o:23 e:2 l:0
1:y5+3ty2z2+...  y4z15+3tyz17+... lcm: 0y5z15
  p : -35t3x8z13  o:24 e:2 l:0
0:x2yz11+4tz13+...  y3z19+3tz21+... lcm: 0x2y3z19
  p : 0tx2z21+0  o:24 e:2 l:0
reduce 3t2z21 with tz+4t2z+...
to 47tx12z11
reduce 47tx12z11 with tx10z11-18t7y4z11+...
to -50t7x2y4z11
reduce -50t7x2y4z11 with t7z5-49t5z7+...
to 2t5x2y4z13
reduce 2t5x2y4z13 with t5z9-26t2x2yz9+...
to -33t6x2yz15
reduce -33t6x2yz15 with t6z7-26t3x2yz7+...
to 44t6y3z15
reduce 44t6y3z15 with t6z7-26t3x2yz7+...
to -t3x2y4z15
reduce -t3x2y4z15 with t3x2y4z13+26x4y5z13+...
to -39t7z17
reduce -39t7z17 with t7z5-49t5z7+...
to -4t5z19
reduce -4t5z19 with t5z9-26t2x2yz9+...
to NULL

set L
1:y5+3ty2z2+...  y4z15+3tyz17+... lcm: 0y5z15
  p : -35t3x8z13  o:24 e:2 l:0
0:x2yz11+4tz13+...  y3z19+3tz21+... lcm: 0x2y3z19
  p : 0tx2z21+0  o:24 e:2 l:0
reduce -35t3x8z13 with tz+4t2z+...
to x10yz13
reduce x10yz13 with x2yz11+4tz13+...
to -29t9y2z13
reduce -29t9y2z13 with tz+4t2z+...
to 47t6x2y3z13
reduce 47t6x2y3z13 with tz+4t2z+...
to -t6y5z13
reduce -t6y5z13 with tz+4t2z+...
to 16t3x2y6z13
reduce 16t3x2y6z13 with tz+4t2z+...
to 48x4y7z13
reduce 48x4y7z13 with x2yz11+4tz13+...
to 41t7y2z15
reduce 41t7y2z15 with tz+4t2z+...
to 10t4x2y3z15
reduce 10t4x2y3z15 with tz+4t2z+...
to t4y5z15
reduce t4y5z15 with tz+4t2z+...
to -12tx2y6z15
reduce -12tx2y6z15 with tz+4t2z+...
to -12t5y2z17
reduce -12t5y2z17 with tz+4t2z+...
to 29t2x2y3z17
reduce 29t2x2y3z17 with tz+4t2z+...
to -15t2y5z17
reduce -15t2y5z17 with tz+4t2z+...
to 13t3y2z19
reduce 13t3y2z19 with tz+4t2z+...
to -29y5z19
reduce -29y5z19 with y4z15+3tyz17+...
to -49t6x8z11
reduce -49t6x8z11 with t6z7-26t3x2yz7+...
to -29t6x2y6z11
reduce -29t6x2y6z11 with t6z7-26t3x2yz7+...
to -47t3x4y7z11
reduce -47t3x4y7z11 with t2x4y7z11+40t3x8z13+...
to -31t4x8z13
reduce -31t4x8z13 with tz+4t2z+...
to 36t7y5z13
 degree jumped; ->L0

set T
  0:tz+4t2z+... o:2 e:2 l:4
  1:y5+3ty2z2+... o:5 e:3 l:3
  2:t7-32t3x2yz4 o:7 e:3 l:2
  3:t9z+39t7z3+... o:10 e:1 l:6
  4:t8z3-49t6z5+... o:11 e:1 l:7
  5:t7z5-49t5z7+... o:12 e:1 l:7
  6:t6z7-26t3x2yz7+... o:13 e:1 l:8
  7:t5z9-26t2x2yz9+... o:14 e:1 l:8
  8:x2yz11+4tz13+... o:14 e:2 l:8
  9:y4z15+3tyz17+... o:19 e:2 l:20
  10:t4x2y2z13-34t4y4z13+... o:21 e:1 l:16
  11:tx10z11-18t7y4z11+... o:22 e:1 l:18
  12:x10yz11-38t6y5z11+... o:22 e:1 l:23
  13:t3x2y4z13+26x4y5z13+... o:22 e:1 l:21
  14:y3z19+3tz21+... o:22 e:2 l:30
  15:-22t3x10yz9-22t4x8z11+... o:23 e:1 l:31
  16:t2x4y7z11+40t3x8z13+... o:24 e:1 l:36
  17:-31t4x8z13+36t7y5z13+... o:25 e:1 l:29
set L
1:x2yz11+4tz13+...  y3z19+3tz21+... lcm: 0x2y3z19
  p : 0tx2z21+0  o:24 e:2 l:0
0:y5+3ty2z2+...  y4z15+3tyz17+... lcm: 0y5z15
  p : 36t7y5z13  o:25 e:2 l:0
reduce 3tx2z21 with tz+4t2z+...
to -4ty2z21
reduce -4ty2z21 with tz+4t2z+...
to 47x14z11
new s6:x14z11-47t24712x16438y1030z2820*gen(37)+...
set S
  0:tz+4t2z+...
  1:y5+3ty2z2+...
  2:t7-32t3x2yz4
  3:x2yz11+4tz13+...
  4:y4z15+3tyz17+...
  5:y3z19+3tz21+...
  6:x14z11-47t6x4y4z11+...
set T
  0:tz+4t2z+... o:2 e:2 l:4
  1:y5+3ty2z2+... o:5 e:3 l:3
  2:t7-32t3x2yz4 o:7 e:3 l:2
  3:t9z+39t7z3+... o:10 e:1 l:6
  4:t8z3-49t6z5+... o:11 e:1 l:7
  5:t7z5-49t5z7+... o:12 e:1 l:7
  6:t6z7-26t3x2yz7+... o:13 e:1 l:8
  7:t5z9-26t2x2yz9+... o:14 e:1 l:8
  8:x2yz11+4tz13+... o:14 e:2 l:8
  9:y4z15+3tyz17+... o:19 e:2 l:20
  10:t4x2y2z13-34t4y4z13+... o:21 e:1 l:16
  11:tx10z11-18t7y4z11+... o:22 e:1 l:18
  12:x10yz11-38t6y5z11+... o:22 e:1 l:23
  13:t3x2y4z13+26x4y5z13+... o:22 e:1 l:21
  14:y3z19+3tz21+... o:22 e:2 l:30
  15:-22t3x10yz9-22t4x8z11+... o:23 e:1 l:31
  16:t2x4y7z11+40t3x8z13+... o:24 e:1 l:36
  17:x14z11-47t6x4y4z11+... o:25 e:1 l:39
  18:-31t4x8z13+36t7y5z13+... o:25 e:1 l:29
set L
2:y5+3ty2z2+...  y4z15+3tyz17+... lcm: 0y5z15
  p : 36t7y5z13  o:25 e:2 l:0
1:tz+4t2z+...  x14z11-47t6x4y4z11+... lcm: 0tx14z11
  p : 0t7x4y4z11+0  o:26 e:2 l:0
0:x2yz11+4tz13+...  x14z11-47t6x4y4z11+... lcm: 0x14yz11
  p : 0t6x4y5z11+0  o:26 e:2 l:0
reduce 36t7y5z13 with tz+4t2z+...
to 37t4x2y6z13
reduce 37t4x2y6z13 with tz+4t2z+...
to 8t2x8z15
reduce 8t2x8z15 with tz+4t2z+...
to -7t8y2z15
reduce -7t8y2z15 with tz+4t2z+...
to -20t5x2y3z15
reduce -20t5x2y3z15 with tz+4t2z+...
to 14t2x2y6z15
reduce 14t2x2y6z15 with tz+4t2z+...
to -21t6y2z17
reduce -21t6y2z17 with tz+4t2z+...
to 43t3x2y3z17
reduce 43t3x2y3z17 with tz+4t2z+...
to 43t3y5z17
reduce 43t3y5z17 with tz+4t2z+...
to 29x2y6z17
reduce 29x2y6z17 with x2yz11+4tz13+...
to 32t4y2z19
reduce 32t4y2z19 with tz+4t2z+...
to -14tx2y3z19
reduce -14tx2y3z19 with tz+4t2z+...
to 45t2y2z21
reduce 45t2y2z21 with tz+4t2z+...
to 23t5x8z13
reduce 23t5x8z13 with t5z9-26t2x2yz9+...
to -43t8y5z13
reduce -43t8y5z13 with t8z3-49t6z5+...
to -7t5x2y6z13
reduce -7t5x2y6z13 with t5z9-26t2x2yz9+...
to 20t2x4y7z13
reduce 20t2x4y7z13 with t2x4y7z11+40t3x8z13+...
to -8t3x8z15
reduce -8t3x8z15 with tz+4t2z+...
to -6x10yz15
 degree jumped; ->L0

set T
  0:tz+4t2z+... o:2 e:2 l:4
  1:y5+3ty2z2+... o:5 e:3 l:3
  2:t7-32t3x2yz4 o:7 e:3 l:2
  3:t9z+39t7z3+... o:10 e:1 l:6
  4:t8z3-49t6z5+... o:11 e:1 l:7
  5:t7z5-49t5z7+... o:12 e:1 l:7
  6:t6z7-26t3x2yz7+... o:13 e:1 l:8
  7:t5z9-26t2x2yz9+... o:14 e:1 l:8
  8:x2yz11+4tz13+... o:14 e:2 l:8
  9:y4z15+3tyz17+... o:19 e:2 l:20
  10:t4x2y2z13-34t4y4z13+... o:21 e:1 l:16
  11:tx10z11-18t7y4z11+... o:22 e:1 l:18
  12:x10yz11-38t6y5z11+... o:22 e:1 l:23
  13:t3x2y4z13+26x4y5z13+... o:22 e:1 l:21
  14:y3z19+3tz21+... o:22 e:2 l:30
  15:-22t3x10yz9-22t4x8z11+... o:23 e:1 l:31
  16:t2x4y7z11+40t3x8z13+... o:24 e:1 l:36
  17:x14z11-47t6x4y4z11+... o:25 e:1 l:39
  18:-31t4x8z13+36t7y5z13+... o:25 e:1 l:29
  19:-8t3x8z15-6x10yz15+... o:26 e:1 l:24
set L
2:tz+4t2z+...  x14z11-47t6x4y4z11+... lcm: 0tx14z11
  p : 0t7x4y4z11+0  o:26 e:2 l:0
1:x2yz11+4tz13+...  x14z11-47t6x4y4z11+... lcm: 0x14yz11
  p : 0t6x4y5z11+0  o:26 e:2 l:0
0:y5+3ty2z2+...  y4z15+3tyz17+... lcm: 0y5z15
  p : -6x10yz15  o:26 e:2 l:0
reduce -47t7x4y4z11 with tz+4t2z+...
to 37t4x6y5z11
reduce 37t4x6y5z11 with tz+4t2z+...
to 10t5x6y2z13
reduce 10t5x6y2z13 with tz+4t2z+...
to 2t5x4y4z13
reduce 2t5x4y4z13 with tz+4t2z+...
to -43t2x6y5z13
reduce -43t2x6y5z13 with tz+4t2z+...
to -5t6x4yz15
reduce -5t6x4yz15 with tz+4t2z+...
to -14t3x6y2z15
reduce -14t3x6y2z15 with tz+4t2z+...
to -27t6x2y3z15
reduce -27t6x2y3z15 with tz+4t2z+...
to -30t3x4y4z15
reduce -30t3x4y4z15 with tz+4t2z+...
to 40t7x2z17
reduce 40t7x2z17 with tz+4t2z+...
to -15t4x4yz17
reduce -15t4x4yz17 with tz+4t2z+...
to 14t7y2z17
reduce 14t7y2z17 with tz+4t2z+...
to -21tx6y2z17
reduce -21tx6y2z17 with tz+4t2z+...
to 20t4x2y3z17
reduce 20t4x2y3z17 with tz+4t2z+...
to 42tx4y4z17
reduce 42tx4y4z17 with tz+4t2z+...
to -11t5x2z19
reduce -11t5x2z19 with tz+4t2z+...
to 28t2x4yz19
reduce 28t2x4yz19 with tz+4t2z+...
to -19t5y2z19
reduce -19t5y2z19 with tz+4t2z+...
to 30t2x2y3z19
reduce 30t2x2y3z19 with tz+4t2z+...
to -11t3x2z21
reduce -11t3x2z21 with tz+4t2z+...
to -19t3y2z21
reduce -19t3y2z21 with tz+4t2z+...
to 17tx2z23
 degree jumped; ->L0

set L
2:x2yz11+4tz13+...  x14z11-47t6x4y4z11+... lcm: 0x14yz11
  p : 0t6x4y5z11+0  o:26 e:2 l:0
1:y5+3ty2z2+...  y4z15+3tyz17+... lcm: 0y5z15
  p : -6x10yz15  o:26 e:2 l:0
0:tz+4t2z+...  x14z11-47t6x4y4z11+... lcm: 0tx14z11
  p : 17tx2z23  o:26 e:2 l:0
reduce -47t6x4y5z11 with tz+4t2z+...
to 37t3x6y6z11
reduce 37t3x6y6z11 with tz+4t2z+...
to -4tx12z13
reduce -4tx12z13 with tz+4t2z+...
to 10t4x6y3z13
reduce 10t4x6y3z13 with tz+4t2z+...
to 2t4x4y5z13
reduce 2t4x4y5z13 with tz+4t2z+...
to -43tx6y6z13
reduce -43tx6y6z13 with tz+4t2z+...
to -5t5x4y2z15
reduce -5t5x4y2z15 with tz+4t2z+...
to -14t2x6y3z15
reduce -14t2x6y3z15 with tz+4t2z+...
to -27t5x2y4z15
reduce -27t5x2y4z15 with tz+4t2z+...
to -30t2x4y5z15
reduce -30t2x4y5z15 with tz+4t2z+...
to 40t6x2yz17
reduce 40t6x2yz17 with tz+4t2z+...
to -15t3x4y2z17
reduce -15t3x4y2z17 with tz+4t2z+...
to 14t6y3z17
reduce 14t6y3z17 with tz+4t2z+...
to -21x6y3z17
reduce -21x6y3z17 with x2yz11+4tz13+...
to 20t3x2y4z17
reduce 20t3x2y4z17 with tz+4t2z+...
to 42x4y5z17
reduce 42x4y5z17 with x2yz11+4tz13+...
to -11t4x2yz19
reduce -11t4x2yz19 with tz+4t2z+...
to 11tx4y2z19
reduce 11tx4y2z19 with tz+4t2z+...
to -19t4y3z19
reduce -19t4y3z19 with tz+4t2z+...
to -37tx2y4z19
reduce -37tx2y4z19 with tz+4t2z+...
to -11t2x2yz21
reduce -11t2x2yz21 with tz+4t2z+...
to -19t2y3z21
 degree jumped; ->L1

set L
2:y5+3ty2z2+...  y4z15+3tyz17+... lcm: 0y5z15
  p : -6x10yz15  o:26 e:2 l:0
1:x2yz11+4tz13+...  x14z11-47t6x4y4z11+... lcm: 0x14yz11
  p : -19t2y3z21  o:26 e:2 l:0
0:tz+4t2z+...  x14z11-47t6x4y4z11+... lcm: 0tx14z11
  p : 17tx2z23  o:26 e:2 l:0
reduce -6x10yz15 with x2yz11+4tz13+...
to -21t6y5z15
reduce -21t6y5z15 with tz+4t2z+...
to -40t3x2y6z15
reduce -40t3x2y6z15 with tz+4t2z+...
to 9t4y5z17
reduce 9t4y5z17 with tz+4t2z+...
to 15tx2y6z17
reduce 15tx2y6z17 with tz+4t2z+...
to 11t5y2z19
reduce 11t5y2z19 with tz+4t2z+...
to 28t2x2y3z19
reduce 28t2x2y3z19 with tz+4t2z+...
to -9t2y5z19
reduce -9t2y5z19 with tz+4t2z+...
to 39t3y2z21
reduce 39t3y2z21 with tz+4t2z+...
to 42x2y3z21
reduce 42x2y3z21 with x2yz11+4tz13+...
to -28y5z21
reduce -28y5z21 with y4z15+3tyz17+...
to -9t6x8z13
reduce -9t6x8z13 with t6z7-26t3x2yz7+...
to -32t3x10yz13
reduce -32t3x10yz13 with tx10z11-18t7y4z11+...
to 30t9y5z13
reduce 30t9y5z13 with t9z+39t7z3+...
to -28t6x2y6z13
reduce -28t6x2y6z13 with t6z7-26t3x2yz7+...
to -21t3x4y7z13
reduce -21t3x4y7z13 with t3x2y4z13+26x4y5z13+...
to 41x6y8z13
reduce 41x6y8z13 with x2yz11+4tz13+...
to 36tx10yz15
 degree jumped; ->L0

set T
  0:tz+4t2z+... o:2 e:2 l:4
  1:y5+3ty2z2+... o:5 e:3 l:3
  2:t7-32t3x2yz4 o:7 e:3 l:2
  3:t9z+39t7z3+... o:10 e:1 l:6
  4:t8z3-49t6z5+... o:11 e:1 l:7
  5:t7z5-49t5z7+... o:12 e:1 l:7
  6:t6z7-26t3x2yz7+... o:13 e:1 l:8
  7:t5z9-26t2x2yz9+... o:14 e:1 l:8
  8:x2yz11+4tz13+... o:14 e:2 l:8
  9:y4z15+3tyz17+... o:19 e:2 l:20
  10:t4x2y2z13-34t4y4z13+... o:21 e:1 l:16
  11:tx10z11-18t7y4z11+... o:22 e:1 l:18
  12:x10yz11-38t6y5z11+... o:22 e:1 l:23
  13:t3x2y4z13+26x4y5z13+... o:22 e:1 l:21
  14:y3z19+3tz21+... o:22 e:2 l:30
  15:-22t3x10yz9-22t4x8z11+... o:23 e:1 l:31
  16:t2x4y7z11+40t3x8z13+... o:24 e:1 l:36
  17:x14z11-47t6x4y4z11+... o:25 e:1 l:39
  18:-31t4x8z13+36t7y5z13+... o:25 e:1 l:29
  19:-8t3x8z15-6x10yz15+... o:26 e:1 l:24
  20:41x6y8z13+36tx10yz15+... o:27 e:1 l:46
set L
2:x2yz11+4tz13+...  x14z11-47t6x4y4z11+... lcm: 0x14yz11
  p : -19t2y3z21  o:26 e:2 l:0
1:tz+4t2z+...  x14z11-47t6x4y4z11+... lcm: 0tx14z11
  p : 17tx2z23  o:26 e:2 l:0
0:y5+3ty2z2+...  y4z15+3tyz17+... lcm: 0y5z15
  p : 36tx10yz15  o:27 e:2 l:0
reduce -19t2y3z21 with tz+4t2z+...
to 17x2yz23
reduce 17x2yz23 with x2yz11+4tz13+...
to 11y3z23
reduce 11y3z23 with y3z19+3tz21+...
to -49t6x12z9
reduce -49t6x12z9 with t6z7-26t3x2yz7+...
to 39t3x14yz9
reduce 39t3x14yz9 with t3x10yz9+t4x8z11+...
to -7t7x4y5z11
reduce -7t7x4y5z11 with t7z5-49t5z7+...
to 14t8x4y2z13
reduce 14t8x4y2z13 with t8z3-49t6z5+...
to -34t5x4y5z13
reduce -34t5x4y5z13 with t5z9-26t2x2yz9+...
to -41t6x4y2z15
reduce -41t6x4y2z15 with t6z7-26t3x2yz7+...
to 28t6x2y4z15
reduce 28t6x2y4z15 with t6z7-26t3x2yz7+...
to 41t3x4y5z15
reduce 41t3x4y5z15 with t3x2y4z13+26x4y5z13+...
to 42t7x2yz17
reduce 42t7x2yz17 with t7z5-49t5z7+...
to 45t7y3z17
reduce 45t7y3z17 with t7z5-49t5z7+...
to -19t5x2yz19
reduce -19t5x2yz19 with t5z9-26t2x2yz9+...
to -42t5y3z19
reduce -42t5y3z19 with t5z9-26t2x2yz9+...
to NULL

set L
1:tz+4t2z+...  x14z11-47t6x4y4z11+... lcm: 0tx14z11
  p : 17tx2z23  o:26 e:2 l:0
0:y5+3ty2z2+...  y4z15+3tyz17+... lcm: 0y5z15
  p : 36tx10yz15  o:27 e:2 l:0
reduce 17tx2z23 with tz+4t2z+...
to 11ty2z23
reduce 11ty2z23 with tz+4t2z+...
to -4t2x14z11
reduce -4t2x14z11 with tx10z11-18t7y4z11+...
to 15t8x4y4z11
reduce 15t8x4y4z11 with t8z3-49t6z5+...
to -3x14z13
reduce -3x14z13 with x14z11-47t6x4y4z11+...
to 40t6x4y4z13
reduce 40t6x4y4z13 with t6z7-26t3x2yz7+...
to 20t7x4yz15
reduce 20t7x4yz15 with t7z5-49t5z7+...
to -15t4x6y2z15
reduce -15t4x6y2z15 with t4x2y2z13-34t4y4z13+...
to 7t7x2y3z15
reduce 7t7x2y3z15 with t7z5-49t5z7+...
to 42t8x2z17
reduce 42t8x2z17 with t8z3-49t6z5+...
to 45t8y2z17
reduce 45t8y2z17 with t8z3-49t6z5+...
to -20t5x2y3z17
reduce -20t5x2y3z17 with t5z9-26t2x2yz9+...
to 41t6x2z19
reduce 41t6x2z19 with t6z7-26t3x2yz7+...
to -21t6y2z19
reduce -21t6y2z19 with t6z7-26t3x2yz7+...
to NULL

set L
0:y5+3ty2z2+...  y4z15+3tyz17+... lcm: 0y5z15
  p : 36tx10yz15  o:27 e:2 l:0
reduce 36tx10yz15 with tz+4t2z+...
to 17t7x2y3z15
reduce 17t7x2y3z15 with tz+4t2z+...
to -21t4x4y4z15
reduce -21t4x4y4z15 with tz+4t2z+...
to 28t7y5z15
reduce 28t7y5z15 with tz+4t2z+...
to 22tx6y5z15
reduce 22tx6y5z15 with tz+4t2z+...
to 9t4x2y6z15
reduce 9t4x2y6z15 with tz+4t2z+...
to -3tx4y7z15
reduce -3tx4y7z15 with tz+4t2z+...
to -5t2x8z17
reduce -5t2x8z17 with tz+4t2z+...
to 38t5x2y3z17
reduce 38t5x2y3z17 with tz+4t2z+...
to 43t5y5z17
reduce 43t5y5z17 with tz+4t2z+...
to 11t2x2y6z17
reduce 11t2x2y6z17 with tz+4t2z+...
to 38t6y2z19
reduce 38t6y2z19 with tz+4t2z+...
to -11t3x2y3z19
reduce -11t3x2y3z19 with tz+4t2z+...
to -22x4y4z19
reduce -22x4y4z19 with x2yz11+4tz13+...
to -23t3y5z19
reduce -23t3y5z19 with tz+4t2z+...
to -41x2y6z19
reduce -41x2y6z19 with x2yz11+4tz13+...
to 12t4y2z21
reduce 12t4y2z21 with tz+4t2z+...
to 4tx2y3z21
reduce 4tx2y3z21 with tz+4t2z+...
to -22ty5z21
reduce -22ty5z21 with tz+4t2z+...
to -50t2y2z23
reduce -50t2y2z23 with tz+4t2z+...
to 32x14y3z11
reduce 32x14y3z11 with x10yz11-38t6y5z11+...
to 15t6x4y7z11
reduce 15t6x4y7z11 with t6z7-26t3x2yz7+...
to -27t4x10yz13
reduce -27t4x10yz13 with tx10z11-18t7y4z11+...
to -11t7x4y4z13
reduce -11t7x4y4z13 with t7z5-49t5z7+...
to 19t10y5z13
reduce 19t10y5z13 with t9z+39t7z3+...
to -11t7x2y6z13
reduce -11t7x2y6z13 with t7z5-49t5z7+...
to 37t5x8z15
reduce 37t5x8z15 with t5z9-26t2x2yz9+...
to 17t5x4y4z15
reduce 17t5x4y4z15 with t5z9-26t2x2yz9+...
to 23t8y5z15
reduce 23t8y5z15 with t8z3-49t6z5+...
to 45t5x2y6z15
reduce 45t5x2y6z15 with t5z9-26t2x2yz9+...
to -38t2x4y7z15
reduce -38t2x4y7z15 with t2x4y7z11+40t3x8z13+...
to -38t3x8z17
reduce -38t3x8z17 with t3x8z15+26x10yz15+...
to 14x10yz17
reduce 14x10yz17 with x10yz11-38t6y5z11+...
to 33t9y2z17
reduce 33t9y2z17 with t9z+39t7z3+...
to 43t6x2y3z17
reduce 43t6x2y3z17 with t6z7-26t3x2yz7+...
to -44t3x4y4z17
reduce -44t3x4y4z17 with t3x2y4z13+26x4y5z13+...
to 44t6y5z17
reduce 44t6y5z17 with t6z7-26t3x2yz7+...
to 33x6y5z17
reduce 33x6y5z17 with x2yz11+4tz13+...
to 6t3x2y6z17
reduce 6t3x2y6z17 with tz+4t2z+...
to -13x4y7z17
reduce -13x4y7z17 with x2yz11+4tz13+...
to -45tx8z19
reduce -45tx8z19 with tz+4t2z+...
to -44t4x4yz19
reduce -44t4x4yz19 with tz+4t2z+...
to 50t7y2z19
reduce 50t7y2z19 with tz+4t2z+...
to -2tx6y2z19
reduce -2tx6y2z19 with tz+4t2z+...
to 2t4x2y3z19
reduce 2t4x2y3z19 with tz+4t2z+...
to -2tx4y4z19
reduce -2tx4y4z19 with tz+4t2z+...
to 6tx2y6z19
reduce 6tx2y6z19 with tz+4t2z+...
to 12t5y2z21
reduce 12t5y2z21 with tz+4t2z+...
to -43t2x2y3z21
reduce -43t2x2y3z21 with tz+4t2z+...
to 3t2y5z21
reduce 3t2y5z21 with tz+4t2z+...
to 2x4yz23
reduce 2x4yz23 with x2yz11+4tz13+...
to 40t3y2z23
reduce 40t3y2z23 with tz+4t2z+...
to -35x2y3z23
reduce -35x2y3z23 with x2yz11+4tz13+...
to 38y5z23
reduce 38y5z23 with y4z15+3tyz17+...
to -8tx2z25
reduce -8tx2z25 with tz+4t2z+...
to -23ty2z25
reduce -23ty2z25 with tz+4t2z+...
to 43x14z15
reduce 43x14z15 with x14z11-47t6x4y4z11+...
to 10t3x10yz15
reduce 10t3x10yz15 with tx10z11-18t7y4z11+...
to -20x12y2z15
reduce -20x12y2z15 with x10yz11-38t6y5z11+...
to -22t9y5z15
reduce -22t9y5z15 with t9z+39t7z3+...
to -22t6x2y6z15
reduce -22t6x2y6z15 with t6z7-26t3x2yz7+...
to 10t4x8z17
reduce 10t4x8z17 with t4x8z13-37t7y5z13+...
to -2tx10yz17
reduce -2tx10yz17 with tx10z11-18t7y4z11+...
to -30t7x2y3z17
reduce -30t7x2y3z17 with t7z5-49t5z7+...
to -17t7y5z17
reduce -17t7y5z17 with t7z5-49t5z7+...
to -38t4x2y6z17
reduce -38t4x2y6z17 with t4x2y2z13-34t4y4z13+...
to 21t4y8z17
reduce 21t4y8z17 with tz+4t2z+...
to -22tx2y9z17
reduce -22tx2y9z17 with tz+4t2z+...
to -39t2x8z19
reduce -39t2x8z19 with tz+4t2z+...
to 3t8y2z19
reduce 3t8y2z19 with tz+4t2z+...
to -24t5x2y3z19
reduce -24t5x2y3z19 with tz+4t2z+...
to -21t2x4y4z19
reduce -21t2x4y4z19 with tz+4t2z+...
to 37t5y5z19
reduce 37t5y5z19 with tz+4t2z+...
to -25t2x2y6z19
reduce -25t2x2y6z19 with tz+4t2z+...
to -50t6y2z21
reduce -50t6y2z21 with tz+4t2z+...
to 32t3x2y3z21
reduce 32t3x2y3z21 with tz+4t2z+...
to 41x4y4z21
reduce 41x4y4z21 with x2yz11+4tz13+...
to -42t3y5z21
reduce -42t3y5z21 with tz+4t2z+...
to -11x2y6z21
reduce -11x2y6z21 with x2yz11+4tz13+...
to 22y8z21
reduce 22y8z21 with y4z15+3tyz17+...
to -22t4y2z23
reduce -22t4y2z23 with tz+4t2z+...
to -4tx2y3z23
reduce -4tx2y3z23 with tz+4t2z+...
to -31ty5z23
reduce -31ty5z23 with tz+4t2z+...
to -8t2y2z25
reduce -8t2y2z25 with tz+4t2z+...
to 5tx10y4z15
reduce 5tx10y4z15 with tx10z11-18t7y4z11+...
to 41t2x10yz17
reduce 41t2x10yz17 with tx10z11-18t7y4z11+...
to 31t8y5z17
reduce 31t8y5z17 with t8z3-49t6z5+...
to 25t3x8z19
reduce 25t3x8z19 with t3x8z15+26x10yz15+...
to -23x10yz19
reduce -23x10yz19 with x10yz11-38t6y5z11+...
to -12t9y2z19
reduce -12t9y2z19 with t9z+39t7z3+...
to 43t6y5z19
reduce 43t6y5z19 with t6z7-26t3x2yz7+...
to 9tx8z21
reduce 9tx8z21 with tz+4t2z+...
to 37t7y2z21
reduce 37t7y2z21 with tz+4t2z+...
to -10t4x2y3z21
reduce -10t4x2y3z21 with tz+4t2z+...
to -15t4y5z21
reduce -15t4y5z21 with tz+4t2z+...
to 43tx2y6z21
reduce 43tx2y6z21 with tz+4t2z+...
to 35t5y2z23
reduce 35t5y2z23 with tz+4t2z+...
to 8t2x2y3z23
reduce 8t2x2y3z23 with tz+4t2z+...
to 6t2y5z23
reduce 6t2y5z23 with tz+4t2z+...
to 23t3y2z25
reduce 23t3y2z25 with tz+4t2z+...
to -2x2y3z25
reduce -2x2y3z25 with x2yz11+4tz13+...
to 14y5z25
reduce 14y5z25 with y4z15+3tyz17+...
to 4tx10yz19
reduce 4tx10yz19 with tx10z11-18t7y4z11+...
to -22t7y5z19
reduce -22t7y5z19 with t7z5-49t5z7+...
to 22t2x8z21
reduce 22t2x8z21 with tx8z21+49t7y2z21+...
to -14t8y2z21
reduce -14t8y2z21 with t8z3-49t6z5+...
to 2t5x2y3z21
reduce 2t5x2y3z21 with t5z9-26t2x2yz9+...
to -49t2x4y4z21
reduce -49t2x4y4z21 with tz+4t2z+...
to -41t5y5z21
reduce -41t5y5z21 with tz+4t2z+...
to 39t2x2y6z21
reduce 39t2x2y6z21 with tz+4t2z+...
to 8t6y2z23
reduce 8t6y2z23 with tz+4t2z+...
to 15t3x2y3z23
reduce 15t3x2y3z23 with tz+4t2z+...
to -39x4y4z23
reduce -39x4y4z23 with x2yz11+4tz13+...
to -27t3y5z23
reduce -27t3y5z23 with tz+4t2z+...
to -35x2y6z23
reduce -35x2y6z23 with x2yz11+4tz13+...
to -39t4y2z25
reduce -39t4y2z25 with tz+4t2z+...
to -25tx2y3z25
reduce -25tx2y3z25 with tz+4t2z+...
to 24ty5z25
reduce 24ty5z25 with tz+4t2z+...
to -28t2y2z27
reduce -28t2y2z27 with tz+4t2z+...
to -21t2x10yz19
reduce -21t2x10yz19 with tx10z11-18t7y4z11+...
to 26t8y5z19
reduce 26t8y5z19 with t8z3-49t6z5+...
to 49t3x8z21
reduce 49t3x8z21 with t3x8z15+26x10yz15+...
to 9x10yz21
reduce 9x10yz21 with x10yz11-38t6y5z11+...
to 19t6y5z21
reduce 19t6y5z21 with t6z7-26t3x2yz7+...
to 36tx8z23
reduce 36tx8z23 with tx8z21+49t7y2z21+...
to 41t7y2z23
reduce 41t7y2z23 with t7z5-49t5z7+...
to 48t4x2y3z23
reduce 48t4x2y3z23 with t4x2y2z13-34t4y4z13+...
to -4t4y5z23
reduce -4t4y5z23 with tz+4t2z+...
to -36t5y2z25
reduce -36t5y2z25 with tz+4t2z+...
to 18t2x2y3z25
reduce 18t2x2y3z25 with tz+4t2z+...
to 46t2y5z25
reduce 46t2y5z25 with tz+4t2z+...
to t3y2z27
reduce t3y2z27 with tz+4t2z+...
to 37x2y3z27
reduce 37x2y3z27 with x2yz11+4tz13+...
to 23y5z27
reduce 23y5z27 with y4z15+3tyz17+...
to -39t7y5z21
reduce -39t7y5z21 with t7z5-49t5z7+...
to 25t2x8z23
reduce 25t2x8z23 with tx8z21+49t7y2z21+...
to -13t8y2z23
reduce -13t8y2z23 with t8z3-49t6z5+...
to 39t5x2y3z23
reduce 39t5x2y3z23 with t5z9-26t2x2yz9+...
to 4t2x4y4z23
reduce 4t2x4y4z23 with t2x4y4z21+40t5y5z21+...
to -41t5y5z23
reduce -41t5y5z23 with t5z9-26t2x2yz9+...
to -35t6y2z25
reduce -35t6y2z25 with t6z7-26t3x2yz7+...
to 11t3y5z25
reduce 11t3y5z25 with tz+4t2z+...
to -17x2y6z25
reduce -17x2y6z25 with x2yz11+4tz13+...
to 33t4y2z27
reduce 33t4y2z27 with tz+4t2z+...
to 50tx2y3z27
reduce 50tx2y3z27 with tz+4t2z+...
to 34ty5z27
reduce 34ty5z27 with tz+4t2z+...
to -t2y2z29
reduce -t2y2z29 with tz+4t2z+...
to 36x10yz23
reduce 36x10yz23 with x10yz11-38t6y5z11+...
to -21t6y5z23
reduce -21t6y5z23 with t6z7-26t3x2yz7+...
to 43tx8z25
reduce 43tx8z25 with tx8z21+49t7y2z21+...
to -11t7y2z25
reduce -11t7y2z25 with t7z5-49t5z7+...
to -42t4x2y3z25
reduce -42t4x2y3z25 with t4x2y2z13-34t4y4z13+...
to -t4y5z25
reduce -t4y5z25 with t4y5z23+9t5y2z25+...
to -26t5y2z27
reduce -26t5y2z27 with t5z9-26t2x2yz9+...
to NULL
_[1]=tz+4t2z+3z3+6tz3
_[2]=y5+3ty2z2+x8
_[3]=t7-32t3x2yz4
_[4]=x2yz11+4tz13+49t6z9-16t4z11+6tx2yz11+23t5z11+8t2x2yz11-16t3z13
_[5]=y4z15+3tyz17+50t7y4z9+5t5y4z11+35t2x2y5z11-39t6yz13+4t3x2y2z13-8t3y4z13-49x2y5z13-12t4yz15+6tx2y2z15-2ty4z15-42x10z11-26t6y4z11-8t4y4z13+3tx2y5z13-8t5yz15+12t2x2y2z15-8t2y4z15-12t3yz17
_[6]=y3z19+3tz21+47x12z11+13t6x2y4z11+22t3x4y5z11-35t4x4y2z13-7t4x2y4z13-tx4y5z13-33t5x2yz15+49t2x4y2z15+44t5y3z15+4t2x2y4z15-39t6z17+2t3x2yz17+23x4y2z17+31t3y3z17-46x2y4z17-12t4z19+3tx2yz19+2ty3z19-31t5x2y4z13-2t2x4y5z13+2t3x2y4z15+2t4x2yz17+46tx4y2z17+31t4y3z17+9tx2y4z17-8t5z19+6t2x2yz19-12t3z21
_[7]=x14z11-47t6x4y4z11+37t3x6y5z11+10t4x6y2z13+2t4x4y4z13-43tx6y5z13-5t5x4yz15-14t2x6y2z15-27t5x2y3z15-30t2x4y4z15+40t6x2z17-15t3x4yz17+14t6y2z17-21x6y2z17+20t3x2y3z17+42x4y4z17-11t4x2z19+28tx4yz19-19t4y2z19+30tx2y3z19-11t2x2z21-19t2y2z21+17x2z23+11y2z23-20t5x4y4z13+15t2x6y5z13-15t3x4y4z15-15t4x4yz17-42tx6y2z17+20t4x2y3z17-17tx4y4z17-41t5x2z19-45t2x4yz19+21t5y2z19-41t2x2y3z19-11t3x2z21-19t3y2z21+34tx2z23+22ty2z23

-- and now playing around with the polynomials for insight...
g1 = F
g2 = H//2
g3 = G//3
netList{g1,g2,g3}
g4 = (t^6*g1-z*g3 - 4*t*z*g3 -3*t^5*z^2*g1)//(-6) -- not minimal
t^6*g1-z*g3 -4*t^7*g1 - 3*t^5*z^2*g1 + 16*t^8*g1 + 18*t^6*z^2*g1 +9*t^4*z^4*g1
 + 6*t^6*z^2*g1)
---------------------------------------------------------------

restart

ring R = 101, (t,x,y,z),ds;
poly F = 4*t^2*z+6*z^3*t+3*z^3+t*z;
poly G = 5*t^2*z^7*y^3*x + 5*x^2*z^4*t^3*y + 3*t^7;
poly H = 6*z*t^2*y + 2*x^8 + 6*z^2*y^2*t + 2*y^5;
ideal I = F,G,H;
timer=1;
option(prot);
option(teach);
std(I);
-- 
-- [255:2]4(2)s8s13s14.15.16s1721-s2223...24...s(4)25...-.26...-....s27(3).....-28-.....-.29.30.31.32.33.34.35.36.37.38.39.40.41.42.43.44.45.46.47.48.49.50.51.52.53.54.55
-- product criterion:7 chain criterion:3
-- _[1]=tz+4t2z+3z3+6tz3
-- _[2]=y5-12t3yz+3ty2z2-9tyz3-18t2yz3+x8
-- _[3]=t7-32t3x2yz4-32t2xy3z7
-- _[4]=x2yz11+4tz13+49t12z3+46t10z5-33t8z7-26t5x2yz7-33t6z9+27t3x2yz9+45txy3z10+28t4z11-2tx2yz11-32t2z13+23t11z5-28t9z7+13t7z9-39t4x2yz9-11t2xy3z10+19t5z11-8t2x2yz11+21t3z13
-- _[5]=y4z15-23x2yz16+3tyz17+50t13y4z3-8t11y4z5-39t12yz7-14t9y4z7+25t6x2y5z7-16t10yz9+47t7y4z9+31t4x2y5z9-43t2xy7z10-50t8yz11+31t5x2y2z11+50t5y4z11+32t2x2y5z11-16t5x2yz12-14xy7z12-50t6yz13-t3x2y2z13-36t3y4z13+50x2y5z13-24t3x2yz14-42txy4z14+21t4yz15-10ty4z15-9tx2yz16-24t2yz17-26t12y4z5+23t10y4z7-8t11yz9+34t8y4z9-13t5x2y5z9-42x10z11-21t9yz11+34t6y4z11-37t3x2y5z11-28txy7z12+35t7yz13-4t4x2y2z13+14t4y4z13-tx2y5z13-24t4x2yz14+17t2xy4z14-11t5yz15-24t2y4z15-27t2x2yz16+41t3yz17
-- _[6]=y3z19-23x2z20+3tz21-27t12xy6z4+38t12x2y4z5+20t10xy6z6+2t10x2y4z7-39t13x2z8+12t8xy6z8+37t5x3y7z8+35t11x2yz9-13t11y3z9-47t8x2y4z9+19t5x4y5z9-18t11x2z10+12t6xy6z10-19t3x3y7z10-39t12z11+47x12z11+4t9x2yz11-39t9y3z11-37t6x2y4z11-11t3x4y5z11+2tx2y9z11+19t9x2z12-31t6x4yz12-t4xy6z12+29tx3y7z12-16t10z13-38t7x2yz13+33t4x4y2z13+17t7y3z13-19t4x2y4z13-42tx4y5z13+30t7x2z14+4t4x4yz14+45t2xy6z14-50t8z15-7t5x2yz15-26t2x4y2z15+17t5y3z15-21t2x3y3z15+41t2x2y4z15-39t5x2z16+3t2x4yz16-40x3y4z16-28xy6z16-50t6z17+15t3x2yz17-13x4y2z17+7t3y3z17+19x3y3z17+2x2y4z17+20t3x2z18+39x4yz18-42txy3z18+21t4z19-46tx2yz19-8ty3z19+28tx2z20-24t2z21+10t11xy6z6-44t11x2y4z7+t9xy6z8-31t9x2y4z9-8t12x2z10+32t7xy6z10+5t4x3y7z10+2t10x2yz11+31t10y3z11+42t7x2y4z11-22t4x4y5z11+4t2x2y9z11-24t10x2z12+39t5xy6z12-43t2x3y7z12-8t11z13-20t8x2yz13-7t8y3z13+42t5x2y4z13+17t2x4y5z13+26t8x2z14+4t5x4yz14-26t3xy6z14-21t9z15-34t6x2yz15-t3x4y2z15-22t6y3z15+12t3x2y4z15+26t6x2z16-46t3x4yz16+21tx3y4z16+45txy6z16+35t7z17+24t4x2yz17-26tx4y2z17+30t4y3z17+38tx3y3z17+4tx2y4z17-19t4x2z18-23tx4yz18+17t2xy3z18-11t5z19+9t2x2yz19-20t2y3z19+47t2x2z20+41t3z21
-- _[7]=x4z20+35tx2z21-13ty2z21-12t12x3y6z4-28t12x4y4z5-36t10x3y6z6-44t10x4y4z7+50t13x4z8+39t8x3y6z8-6t5x5y7z8+38t11x4yz9-17t11x2y3z9+24t8x4y4z9-14t5x6y5z9-8t11x4z10+39t6x3y6z10+14t3x5y7z10+50t12x2z11-24x14z11+13t9x4yz11-33t12y2z11+50t9x2y3z11+6t6x4y4z11+40t3x6y5z11-44tx4y9z11-14t9x4z12-25t6x6yz12+22t4x3y6z12-32tx5y7z12+49t10x2z13+28t7x4yz13+2t10y2z13-19t4x6y2z13+30t7x2y3z13+14t4x4y4z13+15tx6y5z13+47t7x4z14+13t4x6yz14+20t2x3y6z14-11t8x2z15-48t5x4yz15-19t8y2z15-34t2x6y2z15-37t5x2y3z15-43t2x5y3z15+7t2x4y4z15+50t5x4z16+35t2x6yz16-29x5y4z16+10x3y6z16-11t6x2z17-27t3x4yz17-19t6y2z17-17x6y2z17+36t3x2y3z17-14x5y3z17-44x4y4z17-36t3x4z18-50x6yz18+15tx3y3z18-20txy5z18+43t4x2z19+2tx4yz19+10t4y2z19+31tx2y3z19-10tx4z20+23t2x2z21+3t2y2z21-18t11x3y6z6-42t11x4y4z7-22t9x3y6z8-25t9x4y4z9-26t12x4z10+3t7x3y6z10-9t4x5y7z10-44t10x4yz11+25t10x2y3z11-15t7x4y4z11-21t4x6y5z11+13t2x4y9z11+23t10x4z12-50t5x3y6z12+37t2x5y7z12-26t11x2z13+36t8x4yz13+t11y2z13-48t8x2y3z13-15t5x4y4z13+30t2x6y5z13+34t8x4z14+13t5x6yz14-34t3x3y6z14-43t9x2z15+41t6x4yz15-10t9y2z15+22t3x6y2z15-21t6x2y3z15+39t3x4y4z15+34t6x4z16+2t3x6yz16+43tx5y4z16+20tx3y6z16+38t7x2z17-23t4x4yz17-17t7y2z17-34tx6y2z17-3t4x2y3z17-28tx5y3z17+13tx4y4z17+14t4x4z18+tx6yz18+30t2x3y3z18-40t2xy5z18+40t5x2z19+4t2x4yz19+14t5y2z19-39t2x2y3z19-24t2x4z20+7t3x2z21-43t3y2z21
kk = ZZ/101
R = kk[t,x,y,z,MonomialOrder=>Weights=>4:-1,Global=>false]
F = 4*t^2*z+6*z^3*t+3*z^3+t*z
G = 5*t^2*z^7*y^3*x + 5*x^2*z^4*t^3*y + 3*t^7
H = 6*z*t^2*y + 2*x^8 + 6*z^2*y^2*t + 2*y^5
M = matrix{{F,G,H}}
gbTrace=3
time gens gb(M);
gb1 = gb(M,DegreeLimit=>27);
G = reverse flatten entries gens gb1
netList G
(t*G#6 - x^4*z^19*G#0) % gb1

-- spair [6,1] is 
gbs#1
gbs#4
sp = y*gbs#4-z^15*gbs#1 -- this one should reduce to 0, but the following line doesn't seem to finish..
sp % gb1

time gens gb(M,BasisElementLimit =>20);
leadTerm gbSnapshot M

F1 = F
F2 = 1/3_kk * G
F3 = 1/2_kk * (H - 6*t*y*F1)

F1
F2
F3

s1 = y^5*F1-t*z*F3 - 12*t^3*y*z * F1
 + 3*t*y^2*z^2 * F1 - 9*t*y*z^3*F1 - 4*t*y^5*F1

kk = ZZ/101
R = kk[h,t,x,y,z,MonomialOrder=>Eliminate 1]
F = homogenize(4*t^2*z+6*z^3*t+3*z^3+t*z,h)
G = homogenize(5*t^2*z^7*y^3*x + 5*x^2*z^4*t^3*y + 3*t^7,h)
H = homogenize(6*z*t^2*y + 2*x^8 + 6*z^2*y^2*t + 2*y^5,h)

F1 = F
F2 = 1/3_kk * G
F3 = 1/2_kk * (H - 6*t*y*
     
L = new MutableHashTable from {" 1 " => F1, " 2 " => F2}
see L
see = method()
see HashTable := see MutableHashTable := (H) -> netList apply(pairs H, toList)

addto = (L, F) -> (i := 1+#values L; L#(" "|i|" ") = F; L)

L = new MutableHashTable 
see addto(L, F1)
see addto(L,F2)
F3 = 1/2_kk*(H - 6*h^2*t*y*F1)
see addto(L,F3)     

-- spair(F1,F3) is it automatic that this will reduce to zero?
F4' = t*z*F3-h*y^5*F1 + 12*h*t^3*y*z*F1 - 3*h*t*y^2*z^2*F1 + 9*h*t*y*z^3*F1 + 4*t*y^5*F1 - 48*t^4*y*z*F1 + 12*t^2*y^2*z^2*F1
F4' = -1/3_kk * F4'
  -- at this point, it needs to be deferred:
F4'' = -1/34_kk * (h*F4' - z^3*F3 + 6*h*t^2*y*z^3*F1 + 39*t^2*y^5*F1 + 37*t^5*y*z*F1  + 16*t^3*y^2*z^2*F1 + 2*t*F4' - 12*t^3*y*z^3*F1 )
h*F4' - x^8*F1 - h*F4' + x^8*F1 == 0
-- spair(F1,F2)
p1 = z*F2-h^4*t^6*F1 + 4*h^3*t^7*F1 + 3*h^3*t^5*z^2*F1 - 16*h^2*t^8*F1 - 18*h^2*t^6*z^2*F1 -9*h^2*t^4*z^4*F1 - 37*h*t^9*F1 - 5*h*t^7*z^2*F1 - 29*h*t^5*z^4*F1 + 32*h*t^2*x^2*y*z^4*F1 + 27*h*t^3*z^6*F1 + 47*t^10*F1
p1 = -1/14_kk *( p1 + 25*t^8*z^2*F1 +37*t^6*z^4*F1 - 27*t^3*x^2*y*z^4*F1 + 33*t^4*z^6*F1 + 5*t*x^2*y*z^6*F1 + 20*t^2*z^8*F1 )
p2 = h*p1-t^11*F1 + 23*t^9*z^2*F1 + 21*t^7*z^4*F1 + 50*t^4*x^2*y*z^4*F1 + 41*t^5*z^6*F1 + 31*t^2*x^2*y*z^6*F1 + 46*t^3*z^8*F1 - 35*x^2*y*z^8*F1 - 39*t*z^10*F1 + 4*t*p1
p2 = 1/49_kk * p2

- 21*h*t^4*z^4*F1 + 21*t^7*z^4*F1 + 50*t^4*x^2*y*z^4*F1 + 41*t^5*z^6*F1 + 31*t^2*x^2*y*z^6*F1 + 46*t^3*z^8*F1 - 35*x^2*y*z^8*F1 - 39*t*z^11*F1 + 39*t*z^11*F1

-- homogeneous version
R = ZZ/101[h,t,x,y,z]
F = 4*t^2*z+6*z^3*t+3*z^3+t*z
G = 5*t^2*z^7*y^3*x + 5*x^2*z^4*t^3*y + 3*t^7
H = 6*z*t^2*y + 2*x^8 + 6*z^2*y^2*t + 2*y^5
M = matrix{{F,G,H}}
M = homogenize(M,h)
hf = poincare coker M
R = ZZ/101[h,t,x,y,z,MonomialOrder=>Eliminate 1]
M = substitute(M,R)
installHilbertFunction(M,hf)
gbTrace=3
time gens gb M;
mingens ideal substitute(leadTerm gens gb M, h=>1)
transpose gens gb M

-- in singular.  This is very fast.
ring R = 101, (t,x,y,z),ds;
poly F = 4*t^2*z+6*z^3*t+3*z^3+t*z;
poly G = 5*t^2*z^7*y^3*x + 5*x^2*z^4*t^3*y + 3*t^7;
poly H = 6*z*t^2*y + 2*x^8 + 6*z^2*y^2*t + 2*y^5;
ideal I = F,G,H;
timer=1;
option(prot);
std(I);

poly F = 4*t^2*z+6*z^3*t+3*z^3+t*z;
poly G = z^4*t^3*y + t^7;
poly H = x^5 + y^4;

--------------------------------------------------
A = QQ[x,y]
I = ideal"x10+x9y2,y8-x2y7"
gens gb I

A = QQ[x,y,MonomialOrder=>Weights=>2:-1,Global=>false]
I = ideal"x10+x9y2,y8-x2y7"
gens gb I

end

-- dan's simple one:
gbTrace=3
debug Core
R = ZZ/103[t,x,z,MonomialOrder=>Weights=>3:-1,Global=>false]
show raw(G=gb(M=ideal"tz+z3+tz3,t3x+z5,t2z3-xz4-tx3z2",BasisElementLimit=>60))
transpose gens G

show raw(G=gb(M=ideal"tz+z3+tz3,t3x+z5,t2z3-xz4-tx3z2"))
