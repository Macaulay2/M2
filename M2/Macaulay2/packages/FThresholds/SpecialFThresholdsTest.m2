-- tests for fpt computations that call special algorithms

--diagonalFPT test 1
TEST ///
ZZ/5[x,y,z];
f = x^3+y^2;
g = x^3+y^4+z^5;
assert(fpt(f)==4/5)
assert(fpt(g)==3/5)
///

--diagonalFPT test 2
TEST ///
ZZ/11[x,y,z];
f = x^3+y^2;
g = x^3+y^4+z^5;
assert(fpt(f)==9/11)
assert(fpt(g)==8/11)
///

--diagonalFPT test 3
TEST ///
ZZ/13[x,y,z];
f = x^3+y^2;
g = x^3+y^4+z^5;
assert(fpt(f)==5/6)
assert(fpt(g)==10/13)
///

--Below, we verify computations for a binomial from Example 4.3-4.5 in
--"F-pure thresholds of binomial hypersurfaces" by D. Hernandez.
--binomialFPT test 1
TEST ///
ZZ/47[x,y];
f= x^7*y^2 + x^5*y^6;
assert(fpt(f)==3/16)
///

--binomialFPT test 2
TEST ///
ZZ/43[x,y];
f= x^7*y^2 + x^5*y^6;
assert(fpt(f)==8/43)
///

--binomialFPT test 3
TEST ///
ZZ/37[x,y];
f= x^7*y^2 + x^5*y^6;
assert(fpt(f)==1283/6845)
///

-- binaryFormFPT test 1
-- These values were tested with isFPT
TEST ///
R = ZZ/2[x,y]
assert( fpt( x ) == 1 )
assert( fpt( x^10*y^15 ) == 1/15 )
assert( fpt( x^10*y^3+x^9*y^4+x^6*y^7+x^4*y^9+x^3*y^10+x*y^12+y^13 ) == 315/2048 )
assert( fpt( x^10*y+x^9*y^2+x^8*y^3+x^7*y^4+x^4*y^7+x^3*y^8+x*y^10 ) == 93/512 )
assert( fpt( x^10+x^8*y^2+x^5*y^5+x^4*y^6+x^3*y^7 ) == 1/5 )
///

-- binaryFormFPT test 2
-- These values were tested with isFPT
TEST ///
R = ZZ/31[x,y]
L = { x, y, x+3*y, x+10*y }
assert( fpt( L, { 3, 6, 6, 10 } ) == 221645/2770563 )
assert( fpt( L, { 5, 17, 20, 11 } ) == 160922837253/4264455187205 )
assert( fpt( L, { 5, 12, 8, 11 } ) == 1/18 )
assert( fpt( L, { 19, 14, 17, 18 } ) == 14198854736226229/482761061031691789 )
assert( fpt( L, { 17, 10, 17, 20 } ) == 1/32 )
///

-- binaryFormFPT test 3
-- These values were tested by running the exact algorithm from "F-threshold
-- functions" semi-manually
TEST ///
kk = GF( ZZ/5[a]/ideal( a^3+a+1 ) )
R = kk[x,y]
L = { x, y, x+y, x-y, x+a*y, x+a^2*y }
assert( fpt( L, { 4, 19, 2, 11, 5, 20 } ) == 30535166380835361168/931322574615478515625 )
assert( fpt( L, { 22, 76, 46, 30, 92, 88 } ) == 18466082398285089576311704129149/3268496584496460855007171630859375 )
assert( fpt( L, {420, 419, 417, 390, 402, 438} ) == 46636216675556057485911762783799675605705641779512143/57968817327716179454988321140262996777892112731933593750 )
assert( fpt( L, { 3, 32, 2, 32, 73, 84 } ) == 18765116046319289672094923747611764472226361925425255193119555448/2120458113234079732946726383480129385361578897573053836822509765625 )
///
