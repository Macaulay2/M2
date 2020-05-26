--status: this old test depends on internal things and probably should be deleted
end

-- this test takes 40 seconds and 220MB with my debug version

----------------------------------
-- Koszul complex on 3 elements --
-- Algorithm 3                  --
----------------------------------
needs "Macaulay2Doc/test/engine/raw-util.m2"
R = polyring(rawZZp(101), (symbol a, symbol b, symbol c))
m = mat{{a,b,c}}
--gbTrace=10
C = rawResolution(m,true,5,false,0,3,0)

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
-- Koszul complex on 4 elements --
-- Algorithm 3                  --
----------------------------------
needs "Macaulay2Doc/test/engine/raw-util.m2"
R = polyring(rawZZp(101), (symbol a, symbol b, symbol c, symbol d))
m = mat{{a,b,c^2,d^2}}
--gbTrace=10
C = rawResolution(m,true,5,false,0,3,0)

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
Ffour = rawSource m3
F5 = rawSource m4
assert(F3 == F3b)

----------------------------------
-- real projective plane        --
-- Algorithm 3                  --
----------------------------------
-- WARNING: algorithm 0 requires a GB!!
needs "Macaulay2Doc/test/engine/raw-util.m2"
R = QQ[symbol a .. symbol f]
I = ideal(a*b*c,a*b*f,a*c*e,a*d*e,a*d*f, b*c*d,b*d*e,b*e*f,c*d*f,c*e*f)
M = module I
m = raw gens gb syz gens I
C = rawResolution(m,true,6,false,0,3,0)
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
-- Algorithm 3                  --
----------------------------------
needs "Macaulay2Doc/test/engine/raw-util.m2"
R = ZZ/32003[vars(0..17)]
m1 = genericMatrix(R,a,3,3)
m2 = genericMatrix(R,j,3,3)
J = flatten(m1*m2-m2*m1)
J = ideal J
m = raw gens J
time C = rawResolution(m,true,10,false,0,3,0)
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

--------------------------------
-- non raw versions
--------------------------------
needs "Macaulay2Doc/test/engine/raw-util.m2"

bettiMatrix = (C) -> rawbettimat(raw C.Resolution, 0)

R = ZZ/101[a,b,c]
M = coker vars R
C = res(M, Strategy => 2)
assert(C.dd^2 == 0)
bettiMatrix C == matrix"1,3,3,1"

M = coker matrix{{a,b,c}}
C = res(M, Strategy => 3)
assert(C.dd^2 == 0)
bettiMatrix C == matrix"1,3,3,1"

R = ZZ/101[a,b,c,d]
M = coker vars R
C = res(M, Strategy => 2)
assert(C.dd^2 == 0)
bettiMatrix C == matrix"1,4,6,4,1"

M = coker matrix{{a,b,c,d}}
C = res(M, Strategy => 3)
assert(C.dd^2 == 0)
bettiMatrix C == matrix"1,4,6,4,1"

R = QQ[symbol a .. symbol f]
I = ideal(a*b*c,a*b*f,a*c*e,a*d*e,a*d*f, b*c*d,b*d*e,b*e*f,c*d*f,c*e*f)
M = coker syz gens I

C = res(M, Strategy => 2)
assert(C.dd^2 == 0)
bettiMatrix C == matrix"10,15,6"

I = ideal(a*b*c,a*b*f,a*c*e,a*d*e,a*d*f, b*c*d,b*d*e,b*e*f,c*d*f,c*e*f)
M = coker syz gens I
C = res(M, Strategy => 3)
assert(C.dd^2 == 0)
bettiMatrix C == matrix"10,15,6"

R = ZZ/32003[vars(0..17)]
m1 = genericMatrix(R,a,3,3)
m2 = genericMatrix(R,j,3,3)
J = ideal flatten(m1*m2-m2*m1)
time C = res(J, Strategy => 2)
assert(C.dd^2 == 0)
bettiMatrix C == matrix"1,0,1,0,0,0,0;
                        0,9,2,0,0,0,0;
			0,0,31,32,3,0,0;
			0,0,0,28,58,32,4;
			0,0,0,0,0,0,1"
J = ideal flatten(m1*m2-m2*m1)
time C = res(J, Strategy => 1)
assert(C.dd^2 == 0)
bettiMatrix C == matrix"1,0,0,0,0,0,0;
                        0,8,2,0,0,0,0;
			0,0,31,32,3,0,0;
			0,0,0,28,58,32,4;
			0,0,0,0,0,0,1"

J = ideal flatten(m1*m2-m2*m1)
time time C = res(J, Strategy => 0)
assert(C.dd^2 == 0)
bettiMatrix C == matrix"1,0,0,0,0,0,0;
                        0,8,2,0,0,0,0;
			0,0,31,32,3,0,0;
			0,0,0,28,58,32,4;
			0,0,0,0,0,0,1"

J = ideal flatten(m1*m2-m2*m1)
time C = res(J, Strategy => 3)
assert(C.dd^2 == 0)
bettiMatrix C == matrix"1,0,1,0,0,0,0;
                        0,9,2,0,0,0,0;
			0,0,31,32,3,0,0;
			0,0,0,28,58,32,4;
			0,0,0,0,0,0,1"

J = ideal flatten(m1*m2-m2*m1)
R = ring J
time C = res(J, Strategy => 3, SortStrategy=>2^10)
assert(C.dd^2 == 0)
bettiMatrix C == matrix"1,0,1,0,0,0,0;
                        0,9,2,0,0,0,0;
			0,0,31,32,3,0,0;
			0,0,0,28,58,32,4;
			0,0,0,0,0,0,1"

J = ideal flatten(m1*m2-m2*m1)
R = ring J
time C = res(J, Strategy => 2, SortStrategy=>1+2+4+2^11)
assert(C.dd^2 == 0)
bettiMatrix C == matrix"1,0,1,0,0,0,0;
                        0,9,2,0,0,0,0;
			0,0,31,32,3,0,0;
			0,0,0,28,58,32,4;
			0,0,0,0,0,0,1"

R = ZZ/31991 [a, b, c, d, e, f, g, h, i, j, k, l, m, n, o, p, q, r, s, t, MonomialSize=>8]
I = ideal" pr-os+mt,          
     jr-is+gt,
     pq-ns+lt,
     oq-nr+kt,
     mq-lr+ks,
     jq-hs+ft,
     iq-hr+et,
     gq-fr+es,
     dq-cr+bs-at,
     jo-ip+dt,
     mn-lo+kp,
     jn-hp+ct,
     in-ho+bt,
     gn-fo+ep+at,
     dn-co+bp,
     jm-gp+ds,
     im-go+dr,
     hm-fo+ep+cr-bs+at,
     jl-fp+cs,
     il-fo+cr+at,
     hl-fn+cq,
     gl-fm+as,
     dl-cm+ap,
     jk-ep+bs-at,
     ik-eo+br,
     hk-en+bq,
     gk-em+ar,
     fk-el+aq,
     dk-bm+ao,
     ck-bl+an,
     gh-fi+ej,
     dh-ci+bj,
     df-cg+aj,
     de-bg+ai,
     ce-bf+ah"
time C = res(I, Strategy=>1);
betti C == new BettiTally from {
     (0,{0}) => 1, 
     (1,{2}) => 35, 
     (2,{3}) => 140, 
     (3,{4}) => 189, 
     (3,{5}) => 112, 
     (4,{6}) => 735, 
     (5,{7}) => 1080, 
     (6,{8}) => 735, 
     (7,{9}) => 112, 
     (7,{10}) => 189,
     (8,{11}) => 140, 
     (9,{12}) => 35, 
     (10,{14}) => 1
     }

I = ideal flatten entries gens I;
-- Now for a quotient
R = ZZ/101[a..d]/(a*d,b*c)
Q = cokernel matrix{{a,b,c,d}}
C = res(Q, Strategy=>3, LengthLimit => 6)
toExternalString toString net betti C  ==
toExternalString "total: 1 4 8 12 16 20 24
    0: 1 4 8 12 16 20 24"

-- Now for a skew commutative ring
gbTrace=3
kk=ZZ/101;
E=kk[y_0..y_6,SkewCommutative=>true]/(y_0*y_1-y_2*y_3)
n=matrix{{y_1*y_2*y_4,y_2*y_3*y_5,y_3*y_4*y_6,y_4*y_5*y_0,y_5*y_6*y_1,y_6*y_0*y_2,y_0*y_1*y_3},
         {y_3*y_5*y_6,y_4*y_6*y_0,y_5*y_0*y_1,y_6*y_1*y_2,y_0*y_2*y_3,y_1*y_3*y_4,y_2*y_4*y_5}}
M = image n
betti(fn=res(M,LengthLimit=>4))
n=matrix{{y_1*y_2*y_4,y_2*y_3*y_5,y_3*y_4*y_6,y_4*y_5*y_0,y_5*y_6*y_1,y_6*y_0*y_2,y_0*y_1*y_3},
         {y_3*y_5*y_6,y_4*y_6*y_0,y_5*y_0*y_1,y_6*y_1*y_2,y_0*y_2*y_3,y_1*y_3*y_4,y_2*y_4*y_5}}
M = image n
betti(fn=res(M,Strategy=>3,LengthLimit=>4))
betti fn == new BettiTally from {
     (0,{3}) => 7, 
     (1,{4}) => 18, 
     (1,{5}) => 37, 
     (2,{5}) => 33, 
     (2,{6}) => 248, 
     (3,{6}) => 56, 
     (3,{7}) => 983, 
     (4,{7}) => 90, 
     (4,{8}) => 2968, 
     (4,{9}) => 185}
