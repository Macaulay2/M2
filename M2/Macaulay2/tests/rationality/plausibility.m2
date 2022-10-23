-- Some plausibility-checks for the
-- functions of ScorzaOcta.m2
--
-- Christian Boehning & Hans-Christian Graf v. Bothmer

-------------
-- Testing --
-------------

restart

-- the Ring
R = ZZ[x_1,x_2,x_3,C,D]

-- functions
load"ScorzaOcta.m2"

---------------------
-- Testing Clebsch --
---------------------

-- a random example
Clebsch apply(4,i->random(1,R))
-- two times the same linear form should give zero
Clebsch(apply(2,i->random(1,R))|{x_1,x_1})
-- 0 (OK)

-- is Clebsch covariant?
M = random(ZZ^3,ZZ^3)
BaseChange = map(R,R,matrix{{x_1,x_2,x_3}}*M|matrix{{C,D}})
L4 = apply(4,i->random(1,R))
(det M)^4*Clebsch L4-Clebsch apply(L4,l->BaseChange(l))
-- 0 (yes)

--------------------
-- Testing Scorza --
--------------------

-- sum of 3 powers (should lie in the base locus)
f = Scorza(3,{(1,x_1),(1,x_2),(1,x_3)})
-- 0 (OK)

-- sum of 4 powers (image should be reducible)
f = Scorza(3,{(1,x_1),(1,x_2),(1,x_3),(1,x_1+x_2+x_3)})
factor f
-- reducible (OK)

-- sum of 5 powers (image should be a Lueroth quartic)
g5 = {(1,x_1),(1,x_2),(1,x_3),(1,x_1+x_2+x_3),(1,2*x_1-2*x_2+3*x_3)}
f = Scorza(1,g5)
factor f
-- irreducible

-- is Scorza really dependent on n?
Scorza(1,g5)-Scorza(2,g5)
-- not zero (yes)

-- is Scorza covariant?
M = random(ZZ^3,ZZ^3)
BaseChange = map(R,R,matrix{{x_1,x_2,x_3}}*M|matrix{{C,D}})
(det M)^12*BaseChange(Scorza(3,g5))-Scorza(3,apply(g5,i->(i#0,BaseChange(i#1))))
-- 0 (yes)

-- sum of 10 powers vanishing at (0,0,1)
-- (should lie in the base locus)
Bo = apply(10,i->(1,x_1*random(10)+x_2*random(10)))
Scorza(3,Bo)
-- 0 (in the base locus)

------------------
-- Testing Octa --
------------------

-- sum of 3 powers (should lie in the base locus)
f = Octa(3,{(1,x_1),(1,x_2),(1,x_3)})
-- 0 (OK)

-- sum of 4 powers (image should be reducible)
f = Octa(3,{(1,x_1),(1,x_2),(1,x_3),(1,x_1+x_2+x_3)})
factor f
-- reducible (OK)

-- sum of 5 powers
g5 = {(1,x_1),(1,x_2),(1,x_3),(1,x_1+x_2+x_3),(1,2*x_1-2*x_2+3*x_3)}
f = Octa(1,g5)
factor f
-- irreducible

-- is Octa covariant?
M = random(ZZ^3,ZZ^3)
BaseChange = map(R,R,matrix{{x_1,x_2,x_3}}*M|matrix{{C,D}})
(det M)^12*BaseChange(Octa(3,g5))-Octa(3,apply(g5,i->(i#0,BaseChange(i#1))))
-- 0 (yes)

-- sum of 10 powers vanishing at (0,0,1)
-- (should lie in the base locus)
Bo = apply(10,i->(1,x_1*random(10)+x_2*random(10)))
Octa(3,Bo)
-- 0 (in the base locus)

--------------------------
-- Testing Divisibility --
--------------------------

-- a random point
const = 9; g = apply(const,i->(1,matrix{{x_1,x_2,x_3}}*random(ZZ^3,ZZ^1)))

-- a generic linear form
CD = C*x_1+D*x_2

-- Scorza

-- check if Q_i are divisible as predicted by Prop 3.5
MQ = coeffMatrixQ(PolyScorza(7,CD,g),4);
sub(MQ,ZZ/7)
-- indeed Q_(d-2)...Q_(d-7+1) are divisible by 7

MQ = coeffMatrixQ(PolyScorza(8,CD,g),4);
sub(MQ_{0..6},ZZ/7)
-- indeed Q_(d-5)...Q_(d-7+1) are divisible by 7

-- the coefficients of R_i*6!
ScorzaR(8,CD,g,ZZ/7)
-- the first 2 columns are multiplied by 6!/(8 choose 0)=-1 in F_7
-- the next 3 columns are multiplied by 6!/(8 choose 1)=-1 in F_7
-- the last 2 columns are multiplied by 6!/(8 choose 2) and therefore
-- divided by 7. Consequently the reduction mod 7 changes

-- Octa

MQ = coeffMatrixQ(PolyOcta(7,CD,g),8);
sub(MQ_{0..6},ZZ/7)
-- indeed Q_(d-3)...Q_(d-7+1) are divisible by 7

MQ = coeffMatrixQ(PolyOcta(8,CD,g),8);
sub(MQ_{0..6},ZZ/7)
-- indeed Q_(d-6)...Q_(d-7+1) are divisible by 7

-- the coefficients of R_i*6!
OctaR(8,CD,g,ZZ/7)
-- the first 6 columns are multiplied by -1 the last one is new

-----------------------
-- Testing precision --
-----------------------

restart

-- the precision
prec = 7
Fprec = ZZ/prec

-- the rings
Rprec = ZZ[x_1,x_2,x_3,C,D]/ideal D^prec
R = ZZ[x_1,x_2,x_3,C,D]

-- functions
load"ScorzaOcta.m2"

-- random point
const = 9; 
g = apply(const,i->(1,matrix{{x_1,x_2,x_3}}*random(ZZ^3,ZZ^1)))
gprec = apply(g,i->(i#0,sub(i#1,Rprec)))

CD = C*x_1+D*x_2
CDprec = sub(CD,Rprec)
	  
time M = ScorzaR(61,CD,g,Fprec);
 -- used 7.83 seconds
time Mprec = ScorzaR(61,CDprec,gprec,Fprec);
-- used 0.71 seconds
-- the calculation with finite precision is faster
M-Mprec
-- 0 (still the results are the same)

------------------------
-- Testing periodicity --
------------------------

restart

-- the precision
prec = 7
Fprec = ZZ/prec

-- the ring
R = ZZ[x_1,x_2,x_3,C,D]/ideal D^prec

-- functions
load"ScorzaOcta.m2"

-- a random point
const = 9; 
g = apply(const,i->(1,matrix{{x_1,x_2,x_3}}*random(ZZ^3,ZZ^1)))

-- a generic linear form
CD = C*x_1+D*x_2

-- is the coefficient matrix of the R_i indeed prec*(prec-1) periodic?
tally apply(prec..prec^2,
     n->time ScorzaR(n,CD,g,Fprec)==ScorzaR(n+prec*(prec-1),CD,g,Fprec))
-- Tally{true => 43} (yes)


----------------------------------
-- Testing reduction mod prec^3 --
----------------------------------

restart

-- the precision
prec = 7
Fprec = ZZ/prec

-- the ring
Rmod = ZZ[x_1,x_2,x_3,C,D]/ideal(D^prec,prec^3*D^0)
R = ZZ[x_1,x_2,x_3,C,D]/ideal(D^prec)

-- functions
load"ScorzaOcta.m2"

-- a random point
const = 9; 
g = apply(const,i->(1,matrix{{x_1,x_2,x_3}}*random(ZZ^3,ZZ^1)))
gmod = apply(g,i->(sub(i#0,Rmod),sub(i#1,Rmod)))
     
-- a generic linear form
CD = C*x_1+D*x_2
CDmod = sub(CD,Rmod)

-- do calculations over ZZ and ZZ/prec^3 agree?
tally apply(prec..prec^2,
     n->time ScorzaR(n,CD,g,Fprec)==ScorzaR(n,CDmod,gmod,Fprec))
-- Tally{true => 43}
