-- if getenv "USER" == "dan" then exit 0

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


----
 S=ZZ[x,y,z];
 f1=x^2+y^2+z^2;
 f2=x^2+x*y+y^2+x*z;
 f3=x^2+x*y+z^2+y*z;
 g=f1^3*f2;
 
 I = ideal(f1^3,f2^3,f3^3)
 g % I
 J = ideal(f1^3,f2^3)
 g % J
transpose gens gb J
transpose gens gb I
(gens gb J) % I
getChangeMatrix gb J
G = gb(J,ChangeMatrix=>true)
getChangeMatrix G

f2^3-f1^3 == (gens gb J)_(0,1)
transpose gens gb(J,DegreeLimit=>8)
transpose gens gb(I,DegreeLimit=>8)
h0 = f2^3
h1 = h0 - f1^3
h0 = -h1 + h0 

h0 == f1^3
h1 == -f1^3 + f2^3
h2 = -((x*h1-3*y*h0) - y*h1 - 3*z*h0 - z*h1)
h3 = -5*y^2*h1 + 3*x*h2 + y*h2 - 4*y*z*h1 - 9*z^2*h0 - 9*z^2*h1 + z*h2
h4 = -(-2*y*h2 + 5*h3) - 2*z*h2 - 2*z^2*h1
gbJ = first entries gens gb(J,DegreeLimit=>8)
gbJ == {h0,h1,h2,h3,h4}
gbJ_0 - h0
gbJ_1 - h1
gbJ_2 - h2
gbJ_3 - h3
gbJ_4 - h4

-- Now do gbI by hand...
gbI = first entries gens gb(I,DegreeLimit=>8)
g0 = f1^3 + f3^3 - f2^3
g1 = f3^3 - f1^3
g2 = -f3^3 + f2^3
gbI_0 == g0
gbI_1 == g1
gbI_2 == g2

G = gb(I,DegreeLimit=>8,ChangeMatrix=>true);
transpose gens G
h3 % G
h3 - first first entries (matrix{{f1^3,f2^3,f3^3}} * (h3 // G))
getChangeMatrix G
transpose gens G - transpose((gens I) * getChangeMatrix G )
