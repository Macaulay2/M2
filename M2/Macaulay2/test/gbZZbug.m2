-- Date: Mon, 30 Oct 2000 17:34:09 -0700 (MST)
-- From: Anurag Singh <singh@math.utah.edu>
-- To: Macaulay2@math.uiuc.edu
-- Subject: Macaulay2 bug?

-- Dear Professor Grayson and Professor Stillman,

-- I am running version 0.8.60 of Macaulay2. There appears to be a problem
-- when I check for ideal membership in homogeneous ideals in a polynomial
-- ring over ZZ.

-- Thanks for looking into this. Regards, Anurag Singh

-- >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>

 S=ZZ[x,y,z];
 f1=x^2+y^2+z^2;
 f2=x^2+x*y+y^2+x*z;
 f3=x^2+x*y+z^2+y*z;
 g=f1^3*f2^3*f3;

 ideal(f1^3, f2^3, f3^3) : ideal(g)

-- >>> gives answer "ideal(5, z, x+y, 2*y^6)" implying that "g" is NOT an 
-- >>> element of the ideal(f1^3, f2^3, f3^3). On the other hand, 

 ideal(f1^3, f2^3) : ideal(g)

-- >>> gives answer "ideal 1" meaning that g is an element of the 
-- >>> SMALLER ideal(f1^3, f2^3). Alternately:

 g % ideal(f1^3, f2^3, f3^3) == 0

-- >>> gives answer: "false"

 g % ideal(f1^3, f2^3) == 0

-- >>> gives answer "true" indicating the same problem. 

if g % ideal(f1^3, f2^3) == 0 then assert( g % ideal(f1^3, f2^3, f3^3) == 0 )
