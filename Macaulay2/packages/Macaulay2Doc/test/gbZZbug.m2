-- warm up:

 S=QQ[x,y,z];
 f1=x^2+y^2+z^2;
 f2=x^2+x*y+y^2+x*z;
 f3=x^2+x*y+z^2+y*z;
 I=ideal(f1^3, f2^3, f3^3);
 g=I_0*f2^2*f3;
 assert( g % I == 0 )
 h = gens gb I;
 stack ((f -> wrap(160,"",toString leadTerm f)) \ flatten entries h) 

	-- o9 = 3*x^4*y^2
	--      3*x^5*y
	--      x^6
	--      87*x^2*y^5
	--      29*x^3*y^4
	--      493*y^8
	--      493*x*y^7
	--      347318*y^7*z^2
	--      173659*x*y^6*z^2
	--      190447851234*y^6*z^4
	--      285671776851*x*y^5*z^4
	--      571343553702*x^2*y^4*z^4
	--      31741308539*x^3*y^3*z^4
	--      799261798324784872*y^5*z^6
	--      2397785394974354616*x*y^4*z^6
	--      2397785394974354616*x^2*y^3*z^6
	--      799261798324784872*x^3*y^2*z^6
	--      799261798324784872*x^4*y*z^6
	--      799261798324784872*x^5*z^6
	--      235154884556722216539670*y^4*z^8
	--      235154884556722216539670*x*y^3*z^8
	--      23129988644923496708820*x^2*y^2*z^8
	--      23515488455672221653967*x^3*y*z^8
	--      235154884556722216539670*x^4*z^8
	--      2340804247195214168393759*y^3*z^10
	--      7022412741585642505181277*x*y^2*z^10
	--      7022412741585642505181277*x^2*y*z^10
	--      2340804247195214168393759*x^3*z^10
	--      7347797475404821*y^2*z^12
	--      7347797475404821*x*y*z^12
	--      7347797475404821*x^2*z^12
	--      3530901*y*z^14
	--      3530901*x*z^14
	--      z^16

 
-- now the bug:

 S=ZZ[x,y,z];
 f1=x^2+y^2+z^2;
 f2=x^2+x*y+y^2+x*z;
 f3=x^2+x*y+z^2+y*z;
 I = ideal(f1^3, f2^3, f3^3)
 g=I_0*f2^2*f3;
 assert( g % I == 0 )
 h = gens gb I;
 stack ((f -> wrap(160,"",toString leadTerm f)) \ flatten entries h) 

	-- o18 = 3*x^4*y^2
	--       3*x^5*y
	--       x^6
	--       87*x^2*y^5
	--       x^3*y^4
	--       x^4*y^3
	--       17*y^8
	--       x*y^7
	--       x^2*y^6
	--       1389272*y^7*z^2
	--       x*y^6*z^2
	--       3*x^2*y^5*z^2
	--       y^8*z
	--       y^9
	--       761791404936*y^6*z^4
	--       12*x*y^5*z^4
	--       12*x^2*y^4*z^4
	--       2*x^3*y^3*z^4
	--       4*y^7*z^3
	--       12788188773196557952*y^5*z^6
	--       24*x*y^4*z^6
	--       4*x^2*y^3*z^6
	--       4*x^3*y^2*z^6
	--       4*x^4*y*z^6
	--       2*x^5*z^6
	--       4*y^6*z^5
	--       6*x*y^5*z^5
	--       3762478152907555464634720*y^4*z^8
	--       32*x*y^3*z^8
	--       96*x^2*y^2*z^8
	--       8*x^3*y*z^8
	--       8*x^4*z^8
	--       32*y^5*z^7
	--       8*x*y^4*z^7
	--       2*x*y^5*z^6
	--       x^4*y^2*z^6
	--       37452867955123426694300144*y^3*z^10
	--       336*x*y^2*z^10
	--       48*x^2*y*z^10
	--       16*x^3*z^10
	--       16*y^4*z^9
	--       16*x*y^3*z^9
	--       48*x^2*y^2*z^9
	--       16*y^5*z^8
	--       x^5*y*z^7
	--       40324712545021657648*y^2*z^12
	--       5488*x*y*z^12
	--       5488*x^2*z^12
	--       16*y^3*z^11
	--       949501649712*y*z^14
	--       268912*x*z^14
	--       5488*y^2*z^13
	--       112*x*y^2*z^12
	--       16*x^2*y*z^12
	--       26353376*z^16
	--       268912*y*z^15

/// -- Attempt to reduce this element by hand
g % I
G = first entries gens gb I;
inG = apply(G, f -> leadMonomial f)
finddivs = (h,inG) -> (inh := leadMonomial h; positions(inG, g -> inh % g  == 0))
getdivs = (h) -> (p := finddivs(h,inG); G_p)
seedivs = (g) ->  (p := finddivs(g,inG); print p; transpose matrix{G_p})
seedivs g

finddivs(x^5*y*z^6,inG)
seedivs(x^5*y*z^6)
seedivs h
 
h = g
h = h - x^6 * G_2
h = h - x^6 * G_1
h = h - 8*x^4*y^2* G_2
h = h - 19*x^3*y^3 * G_2

J = ideal(3*x^5*y, 4*x^4*y*z^6,2*x^5*z^6)
gens gb J

loadPackage "LLLBases"
S1 = ideal vars S
S2 = ideal basis(2,S)
S3 = ideal basis(3,S)
S4 = ideal basis(4,S)
S5 = ideal basis(5,S)
S6 = ideal basis(6,S)
getleads = (Sd) -> (
     (m,c) = coefficients gens (Sd * I);
     M = transpose substitute(c,ZZ);
     M = transpose matrix apply(entries M,reverse);
     m = matrix { reverse flatten entries m };
     L := flatten entries(m * (leadTerm gens gb M));
     apply(L, m -> if leadCoefficient m < 0 then -m else m))
removeredundants = (Gi,G) -> (
     
G6 = getleads ideal(1_S)
H6 = G6
G7 = getleads S1
H7 = set G7 - (set{x,y,z} ** set G6)/times
G8 = getleads S2
H8 = set G8 - (set{x,y,z} ** set G7)/times
G9 = getleads S3
H9 = set G9 - (set{x,y,z} ** set G8)/times
G10 = getleads S4
H10 = set G10 - (set{x,y,z} ** set G9)/times
G11 = getleads S5
H11 = set G11 - (set{x,y,z} ** set G10)/times
G12 = getleads S6
H12 = set G12 - (set{x,y,z} ** set G11)/times


hermiteLLL M
gens gb(S1 * I, DegreeLimit=>7)
gens gb(S2 * I, DegreeLimit=>8)
gens gb(S3 * I, DegreeLimit=>10)
///

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
end
-- Local Variables:
-- compile-command: "make -C $M2BUILDDIR/Macaulay2/packages/Macaulay2Doc/test gbZZbug.out"
-- End:
