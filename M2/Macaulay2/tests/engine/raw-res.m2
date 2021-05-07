--status: this old test depends on internal things and probably should be deleted


-- Test of the raw resolution routines

needs "raw-util.m2"

----------------------------------
-- Koszul complex on 3 elements --
-- Algorithm 1                  --
----------------------------------
R = polyring(rawZZp(101), (symbol a, symbol b, symbol c))
m = mat{{a,b,c}}
gbTrace=3
C = rawResolution(m,true,5,false,0,1,0)

rawGBSetStop(C,false,{},0,0,0,0,0,false,{})

rawStartComputation C
rawGBBetti(C,0)
m1 = rawResolutionGetMatrix(C,1)
m2 = rawResolutionGetMatrix(C,2)
m3 = rawResolutionGetMatrix(C,3)
m4 = rawResolutionGetMatrix(C,4)
assert(m1*m2 == 0)
assert(m2*m3 == 0)
assert(m3*m4 == 0)

R = ZZ/101[symbol a,symbol b, symbol c]
m = matrix{{a^2,b^2,c^2*b-a^3}}
M = coker m
gbTrace = 3
C = res(M, Strategy => 2)
C.dd
C.dd^2
betti C
res M
----------------------------------
-- Koszul complex on 3 elements --
-- Algorithm 0                  --
----------------------------------
needs "raw-util.m2"
R = polyring(rawZZp(101), (symbol a, symbol b, symbol c))
m = mat{{a,b,c}}
gbTrace=3
C = rawResolution(m,true,5,false,0,0,0)

rawGBSetStop(C,false,{},0,0,0,0,0,false,{})

rawStartComputation C
rawGBBetti(C,0)
m1 = rawResolutionGetMatrix(C,1)
m2 = rawResolutionGetMatrix(C,2)
m3 = rawResolutionGetMatrix(C,3)
m4 = rawResolutionGetMatrix(C,4)
assert(m1*m2 == 0)
assert(m2*m3 == 0)
assert(m3*m4 == 0)
----------------------------------
-- Koszul complex on 3 elements --
-- Algorithm 2                  --
----------------------------------
needs "raw-util.m2"
R = polyring(rawZZp(101), (symbol a, symbol b, symbol c))
m = mat{{a,b,c}}
gbTrace=10
C = rawResolution(m,true,5,false,0,2,0)

rawGBSetStop(C,false,{},0,0,0,0,0,false,{})

rawStartComputation C
rawGBBetti(C,0)
m1 = rawResolutionGetMatrix(C,1)
m2 = rawResolutionGetMatrix(C,2)
m3 = rawResolutionGetMatrix(C,3)
m4 = rawResolutionGetMatrix(C,4)
assert(m1*m2 == 0)
assert(m2*m3 == 0)
assert(m3*m4 == 0)
F = rawSource m1
F2 = rawTarget m2
F3 = rawSource m2
Ffour = rawSource m3
F3b = rawTarget m3
assert(F3 == F3b)
----------------------------------
-- Twisted cubic                --
-- Algorithm 2                  --
----------------------------------
needs "raw-util.m2"
R = polyring(rawZZp(101), (symbol a, symbol b, symbol c, symbol d))
m = mat{{b^2-a*c,a*d-b*c,c^2-b*d}}
gbTrace=3
C = rawResolution(m,true,5,false,0,2,0)

rawGBSetStop(C,false,{},0,0,0,0,0,false,{})

rawStartComputation C
rawGBBetti(C,0)
m1 = rawResolutionGetMatrix(C,1)
m2 = rawResolutionGetMatrix(C,2)
m3 = rawResolutionGetMatrix(C,3)
m4 = rawResolutionGetMatrix(C,4)
assert(m1*m2 == 0)
assert(m2*m3 == 0)
assert(m3*m4 == 0)
F = rawSource m1
F2 = rawTarget m2
F3 = rawSource m2
F3b = rawTarget m3
assert(F3 == F3b)

----------------------------------
-- real projective plane        --
-- Algorithm 0                  --
----------------------------------
-- WARNING: algorithm 0 requires a GB!!
needs "raw-util.m2"
R = QQ[symbol a .. symbol f]
I = ideal(a*b*c,a*b*f,a*c*e,a*d*e,a*d*f, b*c*d,b*d*e,b*e*f,c*d*f,c*e*f)
M = module I
m = raw gens gb syz gens I
C = rawResolution(m,true,6,false,0,0,0)
rawGBSetStop(C,false,{},0,0,0,0,0,false,{})
rawStartComputation C
assert(rawbettimat(C,0) == matrix{{10,15,6}})
m1 = rawResolutionGetMatrix(C,1)
m2 = rawResolutionGetMatrix(C,2)
m3 = rawResolutionGetMatrix(C,3)
assert(m1*m2 == 0)
assert(m3 == 0)
----------------------------------
-- real projective plane        --
-- Algorithm 1                  --
----------------------------------
-- WARNING: algorithm 0 requires a GB!!
needs "raw-util.m2"
R = QQ[symbol a .. symbol f]
I = ideal(a*b*c,a*b*f,a*c*e,a*d*e,a*d*f, b*c*d,b*d*e,b*e*f,c*d*f,c*e*f)
M = module I
m = raw gens gb syz gens I
C = rawResolution(m,true,6,false,0,1,0)
rawGBSetStop(C,false,{},0,0,0,0,0,false,{})
rawStartComputation C
assert(rawbettimat(C,0) == matrix{{10,15,6}})
m1 = rawResolutionGetMatrix(C,1)
m2 = rawResolutionGetMatrix(C,2)
m3 = rawResolutionGetMatrix(C,3)
assert(m1*m2 == 0)
assert(m3 == 0)
----------------------------------
-- real projective plane        --
-- Algorithm 2                  --
----------------------------------
-- WARNING: algorithm 0 requires a GB!!
needs "raw-util.m2"
R = QQ[symbol a .. symbol f]
I = ideal(a*b*c,a*b*f,a*c*e,a*d*e,a*d*f, b*c*d,b*d*e,b*e*f,c*d*f,c*e*f)
M = module I
m = raw gens gb syz gens I
C = rawResolution(m,true,6,false,0,2,0)
rawGBSetStop(C,false,{},0,0,0,0,0,false,{})
rawStartComputation C
assert(rawbettimat(C,0) == matrix {{10, 15, 7}, {0, 1, 0}})
m1 = rawResolutionGetMatrix(C,1)
m2 = rawResolutionGetMatrix(C,2)
m3 = rawResolutionGetMatrix(C,3)
assert(m1*m2 == 0)
assert(m3 == 0)
----------------------------------
-- 3 by 3 commuting matrices    --
-- Algorithm 0                  --
----------------------------------
-- WARNING: algorithm 0 requires a GB!!
needs "raw-util.m2"
R = QQ[vars(0..17)]
m1 = genericMatrix(R,a,3,3)
m2 = genericMatrix(R,j,3,3)
J = flatten(m1*m2-m2*m1)
J = ideal J
m = raw gens gb J
time C = rawResolution(m,true,10,false,0,0,0)
rawGBSetStop(C,false,{},0,0,0,0,0,false,{})
time rawStartComputation C
bettimat = matrix {{1, 0, 0, 0, 0, 0, 0}, 
        {0, 8, 2, 0, 0, 0, 0}, 
	{0, 0, 31, 32, 3, 0, 0}, 
	{0, 0, 0, 28, 58, 32, 4}, 
	{0, 0, 0, 0, 0, 0, 1}}
assert(bettimat == rawbettimat(C,0))
   
m1 = rawResolutionGetMatrix(C,1);
m2 = rawResolutionGetMatrix(C,2);
m3 = rawResolutionGetMatrix(C,3);
m4 = rawResolutionGetMatrix(C,4);
m5 = rawResolutionGetMatrix(C,5);
m6 = rawResolutionGetMatrix(C,6);
m7 = rawResolutionGetMatrix(C,7)
assert(m1*m2 == 0)
assert(m2*m3 == 0)
assert(m3*m4 == 0)
assert(m4*m5 == 0)
assert(m5*m6 == 0)
assert(m7 == 0)
----------------------------------
-- 3 by 3 commuting matrices    --
-- Algorithm 1                  --
----------------------------------
needs "raw-util.m2"
R = ZZ/32003[vars(0..17)]
m1 = genericMatrix(R,a,3,3)
m2 = genericMatrix(R,j,3,3)
J = flatten(m1*m2-m2*m1)
J = ideal J
m = raw gens J
time C = rawResolution(m,true,10,false,0,1,0)
rawGBSetStop(C,false,{},0,0,0,0,0,false,{})
gbTrace = 0
time rawStartComputation C
bettimat = matrix {{1, 0, 0, 0, 0, 0, 0}, 
        {0, 8, 2, 0, 0, 0, 0}, 
	{0, 0, 31, 32, 3, 0, 0}, 
	{0, 0, 0, 28, 58, 32, 4}, 
	{0, 0, 0, 0, 0, 0, 1}}
assert(bettimat == rawbettimat(C,0))
   
m1 = rawResolutionGetMatrix(C,1);
m2 = rawResolutionGetMatrix(C,2);
m3 = rawResolutionGetMatrix(C,3);
m4 = rawResolutionGetMatrix(C,4);
m5 = rawResolutionGetMatrix(C,5);
m6 = rawResolutionGetMatrix(C,6);
m7 = rawResolutionGetMatrix(C,7)
assert(m1*m2 == 0)
assert(m2*m3 == 0)
assert(m3*m4 == 0)
assert(m4*m5 == 0)
assert(m5*m6 == 0)
assert(m7 == 0)
----------------------------------
-- 3 by 3 commuting matrices    --
-- Algorithm 2                  --
----------------------------------
needs "raw-util.m2"
R = QQ[vars(0..17)]
m1 = genericMatrix(R,a,3,3)
m2 = genericMatrix(R,j,3,3)
J = flatten(m1*m2-m2*m1)
J = ideal J
m = raw gens J
time C = rawResolution(m,true,10,false,0,2,0)
rawGBSetStop(C,false,{},0,0,0,0,0,false,{})
time rawStartComputation C
bettimat = matrix {
     {1, 0, 1, 0, 0, 0, 0}, 
     {0, 9, 2, 0, 0, 0, 0}, 
     {0, 0, 31, 32, 3, 0, 0}, 
     {0, 0, 0, 28, 58, 32, 4}, 
     {0, 0, 0, 0, 0, 0, 1}}
assert(bettimat == rawbettimat(C,0))
   
m1 = rawResolutionGetMatrix(C,1);
m2 = rawResolutionGetMatrix(C,2);
m3 = rawResolutionGetMatrix(C,3);
m4 = rawResolutionGetMatrix(C,4);
m5 = rawResolutionGetMatrix(C,5);
m6 = rawResolutionGetMatrix(C,6);
m7 = rawResolutionGetMatrix(C,7)
assert(m1*m2 == 0)
assert(m2*m3 == 0)
assert(m3*m4 == 0)
assert(m4*m5 == 0)
assert(m5*m6 == 0)
assert(m7 == 0)
assert(m1 == m)
----------------------------------
-- Quotient over ZZ/101         --
-- the max ideal
-- Algorithm 0                  --
----------------------------------
needs "raw-util.m2"
R = polyring(rawZZp(101), (symbol a, symbol b, symbol c))
A = rawQuotientRing(R,mat{{a^2+b^2+c^2}})
a = rawPromote(A,a)
b = rawPromote(A,b)
c = rawPromote(A,c)
a^2+b^2+c^2
m = mat{{a,b,c}}
gbTrace=3
algorithm = 0
C = rawResolution(m,true,5,false,0,algorithm,0)

rawGBSetStop(C,false,{},0,0,0,0,0,false,{})

rawStartComputation C
rawGBBetti(C,0)
m1 = rawResolutionGetMatrix(C,1)
m2 = rawResolutionGetMatrix(C,2)
m3 = rawResolutionGetMatrix(C,3)
m4 = rawResolutionGetMatrix(C,4)
assert(rawNumberOfRows m4 == 4)
assert(m1*m2 == 0)
assert(m2*m3 == 0)
assert(m3*m4 == 0)
----------------------------------
-- Quotient over ZZ/101         --
-- the max ideal
-- Algorithm 1                  --
----------------------------------
needs "raw-util.m2"
R = polyring(rawZZp(101), (symbol a, symbol b, symbol c))
A = rawQuotientRing(R,mat{{a^2+b^2+c^2}})
m = mat{{a,b,c}}
<< "rawPromote needs to be able to accept matrices" << endl;
--m = rawPromote(A,m) -- THIS SHOULD BE MADE TO WORK...
a = rawPromote(A,a)
b = rawPromote(A,b)
c = rawPromote(A,c)
a^2+b^2+c^2
m = mat{{a,b,c}}
gbTrace=3
algorithm = 1
C = rawResolution(m,true,5,false,0,algorithm,0)

rawGBSetStop(C,false,{},0,0,0,0,0,false,{})

rawStartComputation C
rawGBBetti(C,0)
m1 = rawResolutionGetMatrix(C,1)
m2 = rawResolutionGetMatrix(C,2)
m3 = rawResolutionGetMatrix(C,3)
m4 = rawResolutionGetMatrix(C,4)
assert(rawNumberOfRows m4 == 4)
assert(m1*m2 == 0)
assert(m2*m3 == 0)
assert(m3*m4 == 0)
----------------------------------
-- Quotient over ZZ/101         --
-- the max ideal
-- Algorithm 2                  --
----------------------------------
needs "raw-util.m2"
R = polyring(rawZZp(101), (symbol a, symbol b, symbol c))
A = rawQuotientRing(R,mat{{a^2+b^2+c^2}})
m = mat{{a,b,c}}
a = rawPromote(A,a)
b = rawPromote(A,b)
c = rawPromote(A,c)
a^2+b^2+c^2
m = mat{{a,b,c}}
gbTrace=3
algorithm = 2
C = rawResolution(m,true,5,false,0,algorithm,0)

rawGBSetStop(C,false,{},0,0,0,0,0,false,{})

rawStartComputation C
rawGBBetti(C,0)
m1 = rawResolutionGetMatrix(C,1)
m2 = rawResolutionGetMatrix(C,2)
m3 = rawResolutionGetMatrix(C,3)
m4 = rawResolutionGetMatrix(C,4)
assert(rawNumberOfRows m4 == 4)
assert(m1*m2 == 0)
assert(m2*m3 == 0)
assert(m3*m4 == 0)
----------------------------------
R = polyring(rawZZp(101), (symbol a, symbol b, symbol c, symbol d))
m = mat{{b^2-a*c,a*d-b*c,c^2-b*d}}
C = rawResolution(m,true,5,false,0,1,0)
rawStartComputation C
rawbettimat(C,0)
m1 = rawResolutionGetMatrix(C,1)
m2 = rawResolutionGetMatrix(C,2)
m3 = rawResolutionGetMatrix(C,3)
assert(m1*m2 == 0)
assert(m3 == 0)

R = polyring(rawZZp(101), (symbol a, symbol b, symbol c, symbol d))
m = mat{{b^2-a*c,a*d-b*c,c^3-b*d^2}}
C = rawResolution(m,true,5,false,0,1,0)
rawStartComputation C
rawGBBetti(C,0)
m1 = rawResolutionGetMatrix(C,1)
m2 = rawResolutionGetMatrix(C,2)
m3 = rawResolutionGetMatrix(C,3)
m4 = rawResolutionGetMatrix(C,4)
assert(m1*m2 == 0)
assert(m2*m3 == 0)
assert(rawStatus1 C == 6)

R = polyring(rawQQ(), (symbol a, symbol b, symbol c, symbol d))
m = mat{{2*b^2-a*c,a*d-3*b*c,c^3-7*b*d^2}}
C = rawResolution(m,true,5,false,0,1,0)
rawStartComputation C
rawGBBetti(C,0)
m1 = rawResolutionGetMatrix(C,1)
m2 = rawResolutionGetMatrix(C,2)
m3 = rawResolutionGetMatrix(C,3)
m4 = rawResolutionGetMatrix(C,4)
assert(m1*m2 == 0)
assert(m2*m3 == 0)
assert(rawStatus1 C == 6)

