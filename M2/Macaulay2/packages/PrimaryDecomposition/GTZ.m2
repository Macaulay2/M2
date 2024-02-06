-------------------------------------------------------
-- GTZ algorithm for computing primary decomposition --
-------------------------------------------------------

-- saturation
-- 

protect AnswerToDate					    -- unexported ??

primaryDecompositionGTZ = method()
primaryDecompositionGTZ Ideal := (I) -> (
     -- if homogeneous, find the Hilbert function
     -- compute codimension
     -- handle some specific cases
     -- switch to lex order
     -- compute independent sets, maybe a lot of them
     primarycomps := {};
     primecomps := {};
     -- loop thru these sets, until ...
     --    R1 := product order ring
     --    R2 := k(u)[x\u];
     --    move I to R1, compute a gb
     --    h := compute a flattener element
     --    J := move gb(I) to R2
     --    call zeroDimensionalPD(J,...)
     --    for each (P,Q), 
     --     intersect back with the polynomial ring
     )

protect ans							    -- see below, this can't be right

PD = method(Options=>{AnswerToDate => null})
PD Ideal := opt -> (I) -> (
     result := {}; -- a list of pairs (Q,P).
     indep := independentSets I; -- a list
     -- Now we loop through each one, or as many as we wish
     --  once the codimension of I increases, then we are done with
     --  this part
     c := codim I;
     while #indep > 0 and c === codim I do (
	  );
     -- at this point
     join(result, PD(I, AnswerToDate => ans))    
     )

toFractionField = (I,u) -> (
     R := ring u;
     n := numgens R;
     vars1 := support u;
     d := #vars1;
     vars2 := support ((product gens R)//u);
     K := frac((coefficientRing R) (monoid[vars1]));
     S := K (monoid [vars2,MonomialSize=>8]);
     substitute(I,S)     
     )

gbdim0 = (I,u) -> (
     -- compute a gb of IK[x\u], but with no fractions
     K := frac ring I;
     IK := toFractionField(I,u);
     gbIK := ideal apply(flatten entries substitute(gens gb IK, K), numerator);
     substitute(gbIK,ring IK)
     )

PDstep = (I,u) -> (
     -- returns: I : h, h
     H := time flatt(I,u);
     h := time intersect values H;
     h = h_0;
     X := time minSat(I,h);
     (X#0, X#2)
     )

PD1 = (I) -> (
     time gb I;
     time u := independentSets(I, Limit=>1);
     (J,h) := PDstep(I,u#0);
     (J, time trim(I + ideal(h)))
     )
-------------------------------------------------------

makeProductRing = (basevars) -> (
     -- basevars should be a product of variables
     -- in its ring R
     -- return value is the ring with a
     -- product order where the basevars are
     -- considered after the non-basevars.
     R := ring basevars;
     n := numgens R;
     vars1 := variables basevars;
     d := #vars1;
     vars2 := variables ((product gens R)//basevars);
     (coefficientRing R) monoid([vars2,vars1,
	  MonomialOrder=>ProductOrder{n-d,d},
	  MonomialSize=>16]))

flatt = (I, m) -> (
     -- First create a new ring with correct order
     local ones;
     local mm;
     local F;
     R := ring I;
     n := numgens R;
     vars1 := variables m;
     d := #vars1;
     vars2 := variables ((product gens R)//m);
     RU := (coefficientRing R) monoid([vars2,vars1,
	  MonomialOrder=>ProductOrder{n-d,d},
	  MonomialSize=>16]);
     J := substitute(I,RU);
     -- Collect lead coefficients of GB
     JG := J;
     leads := leadTerm(1,gens gb J);
     --(mons,cs) = coefficients(toList(0..n-d-1),leads);
     (mons,cs) := coefficients(leads, Variables => toList(0..n-d-1));
     monsid := trim ideal select(flatten entries mons, f -> f != 1);
     monsid = substitute(monsid,R);
     --monset = set flatten entries gens monsid;
     monset := new MutableHashTable;
     scan(flatten entries gens monsid, m -> monset#m = {});
     monslist := flatten entries substitute(mons,R);
     p := positions(monslist, f -> monset#?f);
     cs = transpose cs;
     scan(p, i -> monset#(monslist#i) = substitute(ideal(compress transpose (cs_{i})),R));
     monset)

sortPolys = (facs) -> (
     x := apply(facs, f -> (degree f, size f, f));
     x = sort x;
     apply(x, i -> i#2))

greedySat = method()
greedySat(Ideal,RingElement) := (I,F) -> (
     R := ring I;
     facs := factors F;
     facs = sortPolys facs;
     G := 1_R;
     satI := I; -- will be the saturation
     i := 0;
     while i < #facs do (
	  g := facs_i;
	  newI := satI : g;
	  if satI != newI then (
	       G = g * G;
	       satI = newI;
	  ) else (
	     i = i+1;
	  ));
     (satI, G))   
     
-- saturation = method()
-- saturation(Ideal,RingElement) := (I,F) -> (
--      facs := factors F;
--      ret := minSatPPD(I,facs);
--      (ret#0, ret#2))

saturation = method()
saturation(Ideal,RingElement) := (I,F) -> greedySat(I,F)

GTZ0 = method()
GTZ0 Ideal := (I) -> (
     ms := independentSets(I, Limit=>1);
     if #ms == 0 then (I, ideal(0_(ring I)))
     else (
	  F := flattener(I,ms_0);
     	  (J,G) := saturation(I,F);
     	  (J, I + ideal(G))))

GTZ1 = method()
GTZ1 Ideal := (I) -> (
     R := ring I;
     L := ideal(1_R); -- intersection of all components found so far
     comps := {};
     while I != 0 do (
	  (I1,J1) := GTZ0 I;
	  if gens L % I1 != 0 then (
	       print("new component is " | toString I1);
	       L = intersect(L,I1);
	       comps = append(comps, I1);
	       );
	  I = J1;
	  );
     comps
     )

end
-- Bayes example #138
loadPackage "ExampleIdeals"
time H = examplesBayes();
I = example(H,138);
R = (coefficientRing ring I)[gens ring I, MonomialSize=>8]
K = frac R
I = substitute(I,R)
load "toric.m2"

time Isat = sat I;
Irest = I : Isat;
Irest = trim Irest;
betti Irest
time intersect(Irest,Isat);
oo == I -- true
betti Irest
codim Irest
degree Irest
Z1 = PD1 Irest;
time Za = Irest : Z1#0;
intersect(Za,Z1#0) == Irest

Iwork = Irest;
Qs = {}
time (Q,Iwork) = PD1 Iwork; Qs = append(Qs,Q);
time scan(30, i -> (time (Q,Iwork) = PD1 Iwork; Qs = append(Qs,Q)))
Qs/codim
Qs/degree
degree Iwork
codim Iwork
time (Q,Iwork) = PD1 Iwork;
time (Q,Iwork) = PD1 Iwork;
time (Q,Iwork) = PD1 Iwork;
time (Q,Iwork) = PD1 Iwork;

I14 = intersect select(Qs, I -> codim I === 14);
I15g = Irest : I14;
intersect(I14,I15g);
oo == Irest -- false
gbTrace=0

indep = independentSets Irest;
(Q1, h1) = PDstep(Irest,indep#0)
h1
Iwork = Irest
Iwork = trim(Iwork + ideal(h1))
(Q2, h2) = PDstep(Iwork,indep#1)
transpose mingens Q2
degree Q1
degree Q2
Iwork = trim(Iwork + ideal(h2))
time indep = independentSets Iwork;
(Q3, h3) = PDstep(Iwork,indep#0)
Iwork = trim(Iwork + ideal(h3))
(Q4, h) = PDstep(Iwork,indep#1)
codim Q4
codim Iwork
(I1,h1) = PDstep(I,indep#0)
degree I1
I2 = I + ideal(h)
(I11, h2) = PDstep(I2,indep#1)
-time indep = independentSets I;
time flatt(I,indep#0);
hi = intersect values oo
h = hi_0
saturate(I,h)
factors oo_0

S = makeProductRing indep#0
IS = substitute(I,S)
time gens gb IS;
indep#0
debug PrimaryDecomposition
H = flatt(I,indep#0)
peek H
intersect values H
apply(indep, u -> (
  IK = toFractionField(I,u);
  gbIK = ideal apply(flatten entries substitute(gens gb IK, K), numerator);
  print toString gbIK;
  gbIK))
time gens gb IK
///
restart
load "PrimaryDecomposition/GTZ.m2"
kk = ZZ/32003
R = kk[x,y,z,t,u,v]
I = ideal( 
    2*x^2+2*y^2+2*z^2+2*t^2+2*u^2+v^2-v, 
    x*y+y*z+2*z*t+2*t*u+2*u*v-u, 
    2*x*z+2*y*t+2*z*u+u^2+2*t*v-t, 
    2*x*t+2*y*u+2*t*u+2*z*v-z, 
    t^2+2*x*v+2*y*v+2*z*v-y, 
    2*x+2*y+2*z+2*t+2*u+v-1)
load "Elimination.m2"
Iv = eliminate(I,{x,y,z,t,u})
product factors (Iv_0) == Iv_0
Is = apply(factors Iv_0, g -> I + ideal(g))
Is/degree
transpose gens gb Is_0
transpose gens gb Is_1
transpose gens gb Is_2
transpose gens gb Is_3
factor (gens gb Is_3)_(0,5)
transpose gens gb Is_4
trim Is_4
J = Is_4 -- is this prime?
eliminate(J,{y,z,t,u,v})
degree J
S = makeProductRing x
J = substitute(J,S)
transpose gens gb J
I5 = substitute(Is_5, S)
transpose gens gb I5

-- In dimension 0 situation:
-- If I, Ix have the same degree, then I is radical iff Ix is, and
-- components match.  So for this ideal, we could do:
codim I -- codim 6, i.e. 0-dimensional
degree I -- degree 32
Iv = eliminate(I,{x,y,z,t,u}) -- also degree 32, so factors will match components of I.
product factors (Iv_0) == Iv_0 -- true, so there are 
# factors(Iv_0) -- 6 components, all prime
-- now get each component:
-- The best way is to do a lex GB for each, with v last
-------------------------------------
load "Elimination.m2"
R = ZZ/32003[a,b,c,d,e,h]
I = ideal(
    a+b+c+d+e,
    d*e+c*d+b*c+a*e+a*b,
    c*d*e+b*c*d+a*d*e+a*b*e+a*b*c,
    b*c*d*e+a*c*d*e+a*b*d*e+a*b*c*e+a*b*c*d,
    a*b*c*d*e-h^5)
codim I -- therefore a CI, therefore equidimensional
independentSets I
I1 = eliminate(I, {a,b,c,d})
F = oo_0
factor F 
Ih = saturate(I,h)
--------------------------------------
restart
load "Elimination.m2"
load "PrimaryDecomposition/GTZ.m2"
R = ZZ/32003[x,y,z]
I = ideal(
    x^2+x*y^2*z-2*x*y+y^4+y^2+z^2,
    -x^3*y^2+x*y^2*z+x*y*z^3-2*x*y+y^4,
    -2*x^2*y+x*y^4+y*z^4-3)
codim I
degree I
time J = eliminate(I,{x,y}) -- degree 54
factor J_0
factors J_0 -- 2 factors, therefore 2 components, both prime
--------------------------------------
R = ZZ/32003[x,y,z,t]
I = ideal(
    y^2*z+2*x*y*t-2*x-z,
    -x^3*z+4*x*y^2*z+4*x^2*y*t+2*y^3*t+4*x^2-10*y^2+4*x*z-10*y*t+2,
    2*y*z*t+x*t^2-x-2*z,
    -x*z^3+4*y*z^2*t+4*x*z*t^2+2*y*t^3+4*x*z+4*z^2-10*y*t-10*t^2+2)
codim I
degree I
radical I
g1 = product factors (eliminate(I,{x,y,z}))_0 -- 
g2 = product factors (eliminate(I,{x,y,t}))_0 -- 
g3 = product factors (eliminate(I,{x,z,t}))_0 -- 
g4 = product factors (eliminate(I,{y,z,t}))_0 -- 
radI = I + ideal(g1,g2,g3,g4)
transpose gens gb radI
degree radI -- 32
J1 = radI : (g1//((factors g1)_0))
degree J1
J2 = radI : J1
degree J2
----------------------------------------
restart
load "Elimination.m2"
load "PrimaryDecomposition/GTZ.m2"
R = ZZ/32003[b,s,t,u,v,w,x,y,z]
I = ideal(
    b*v+s*u,
    b*w+t*u,
    s*w+t*v,
    b*y+s*x,
    b*z+t*x,
    s*z+t*y,
    u*y+v*x,
    u*z+w*x,
    v*z+w*y)
independentSets(I,Limit=>1)
flatt(I,b*s*t)
peek oo
(J1, F1) = GTZ0 I
(J2, F2) = GTZ0 F1
(J3, F3) = GTZ0 F2
(J4, F4) = GTZ0 F3
(J5, F5) = GTZ0 F4
(J6, F6) = GTZ0 F5
(J7, F7) = GTZ0 F6
(J8, F8) = GTZ0 F7
(J9, F9) = GTZ0 F8
(J10, F10) = GTZ0 F9
(J11, F11) = GTZ0 F10
(J12, F12) = GTZ0 F11
(J13, F13) = GTZ0 F12
(J14, F14) = GTZ0 F13
(J15, F15) = GTZ0 F14
(J16, F16) = GTZ0 F15
L = intersect(J1,J2,J3,J4,J5,J6,J7,J8,J9,J10,J11,J12,J13,J14,J15)
oo == I
(gens L) % J16 == 0
(J16, F17) = GTZ0 F16
(gens L) % J16 == 0
(J16, F18) = GTZ0 F17
(gens L) % J16 == 0
(J16, F19) = GTZ0 F18
(gens L) % J16 == 0
(J16, F20) = GTZ0 F19
(gens L) % J16 == 0
(J16, F21) = GTZ0 F20
(gens L) % J16 == 0
(J16, F22) = GTZ0 F21
(gens L) % J16 == 0
(J16, F23) = GTZ0 F22
(gens L) % J16 == 0
(J16, F24) = GTZ0 F23
(gens L) % J16 == 0
(J16, F25) = GTZ0 F24
(gens L) % J16 == 0
(J16, F26) = GTZ0 F25
(gens L) % J16 == 0
(J16, F27) = GTZ0 F26
(gens L) % J16 == 0
(J16, F28) = GTZ0 F27
(gens L) % J16 == 0
(J16, F29) = GTZ0 F28
(gens L) % J16 == 0
(J16, F30) = GTZ0 F29
(gens L) % J16 == 0
(J16, F31) = GTZ0 F30
(gens L) % J16 == 0
(J16, F32) = GTZ0 F31
(gens L) % J16 == 0
(J16, F33) = GTZ0 F32
(gens L) % J16 == 0
(J16, F34) = GTZ0 F33
(gens L) % J16 == 0
(J16, F35) = GTZ0 F34
(gens L) % J16 == 0
radI = intersect(L,F35)
intersect minimalPrimes I
(J16, F36) = GTZ0 F35
(gens L) % J16 == 0

ms = independentSets I
C = flatt(I, ms_0)
peek oo
F = (intersect values C)_0
(Isat,G) = saturation(I, F)
I1 = I + ideal(G);
ms = independentSets I1
C = flatt(I1,ms_0)
peek oo
F = (intersect values C)_0
(J2, G) = saturation(I1,F)
----------------------------------------
restart
load "Elimination.m2"
load "PrimaryDecomposition/GTZ.m2"
R = ZZ/3[x,y,u,s,t]
I = ideal(
    x^27,
    y^27,
    u^27,
    u^5-x*y*(x-y)*(s*x-t*y))
independentSets(I,Limit=>1)
lt = flatt(I, s*t)
peek lt
ltideal = intersect values lt
lts = factors ltideal_0
I1 = saturate(I,s)
I1 == (I2 = I1 : lts#2)
I2 = saturate(I1,lts#2)
I2 == I1
I3 = saturate(I2,lts#3)
I3 == I2
I4 = saturate(I3,lts#4)
I4 == I3
I5 = saturate(I4,lts#5)
I5 == I4
I6 = saturate(I5,lts#6)
I6 == I5
I7 = saturate(I6,lts#7)
I7 == I6
I8 = saturate(I7,lts#8)
I8 == I7
I9 = saturate(I8,lts#9)
I9 == I8 -- true!
I10 = saturate(I9,lts#10)
I10 == I9 -- true
Isat = saturate(I,product lts)
I2 = saturate(I1,t)
I1 == I2 -- true
I1 : 

Isat = I
scan(lts, f -> time Isat = saturate(Isat,f))
degree Isat -- but this is primary

lts = (sort(lts/(f -> (degree f, f))))/(g -> g#1)
Isat = I
scan(lts, f -> time (I2 = saturate(Isat,f);
	       print (I2 == Isat);
	       Isat = I2))


P1 = quotMin(I, lts, product lts);
P2 = quotMin toSequence P1;
P3 = quotMin toSequence P2;
P4 = quotMin toSequence P3;
P5 = quotMin toSequence P4;
P6 = quotMin toSequence P5;
P7 = quotMin toSequence P6;
P8 = quotMin toSequence P7;
P9 = quotMin toSequence P8;
P10 = quotMin toSequence P9;
P11 = quotMin toSequence P10;
P12 = quotMin toSequence P11;
P13 = quotMin toSequence P12; -- the final one
P14 = quotMin toSequence P13; -- P14 is P13.

P15 = quotMin toSequence P14;  


P15_0 == P13_0

time (I1 = I : (s^8-t^8))
time I2 = I1 : (s^8-t^8)
I2 == I1 -- false
time I3 = I2 : (s^8-t^8)
I3 == I2
time I4 = I3 : (s^8-t^8)
I4 == I3
time I5 = I4 : (s^8-t^8)
I5 == I4
time I6 = I5 : (s^8-t^8)
I6 == I5
time I7 = I6 : (s^8-t^8)
I7 == I6
time I8 = I7 : (s^8-t^8)
I8 == I7
gens gb I8
gens gb I7
time I9 = I8 : (s^8-t^8)
I9 == I8
time I10 = I9 : (s^8-t^8)
I10 == I9
time I11 = I10 : (s^8-t^8)
I11 == I10
time I12 = I11 : (s^8-t^8)
I12 == I11
gens gb I12;
time I13 = I12 : (s^8-t^8)
I13 == I12
time I14 = I13 : (s^8-t^8)
I14 == I13 -- TRUE finally!!
gens gb I14

gens gb Isat
J = trim Isat;
gens J;

Irest = I : Isat;

codim Irest
gens Irest;
Irest = trim Irest;
transpose gens Irest

J = ideal mingens(Irest + ideal(x,y,u))
J_3
factor oo

satI = I : ideal(x,y,u,s,t);
L = satI : ideal(x,y,u,s,t);
satI == L

--------------------------
R = ZZ/3[x,y,u,s,t]
I = ideal(
    x^9,
    y^9,
    u^9,
    u^5-x*y*(x-y)*(s*x-t*y))
time GTZ1 I;


restart
load "Elimination.m2"
load "PrimaryDecomposition/GTZ.m2"
R = ZZ/3[x,y,u,s,t]
I = ideal(
    x^27,
    y^27,
    u^27,
    u^5-x*y*(x-y)*(s*x-t*y))
CoeffList = {t, s, s-t, s+t, s^2+t^2, s^2+s*t-t^2, s^2-s*t-t^2,
       s^3-s^2*t+t^3, s^3+s^2*t+s*t^2-t^3, s^3+s^2*t-s*t^2+t^3,
       s^4+s^3*t-t^4, s^4-s^3*t-s^2*t^2+s*t^3-t^4,
       s^4+s^3*t+s^2*t^2-s*t^3-t^4, s^4-s^3*t+s^2*t^2+s*t^3-t^4,
       s^4-s^3*t+s^2*t^2-s*t^3+t^4, s^4-s^3*t+s^2*t^2+t^4,
       s^4-s^2*t^2-t^4, s^4+s^2*t^2-s*t^3+t^4, s^4+s^2*t^2-t^4,
       s^6+s^5*t-s^4*t^2-s^3*t^3+t^6, s^6-s^5*t+s^3*t^3-s^2*t^4+t^6,
       s^6-s^5*t+s^3*t^3+s^2*t^4+s*t^5+t^6, s^6+s^5*t+s^3*t^3+s*t^5+t^6,
       s^6-s^5*t-s^4*t^2-s^3*t^3+s^2*t^4-t^6,
       s^6-s^5*t-s^4*t^2+s^3*t^3-t^6,
       s^6-s^5*t+s^4*t^2-s^3*t^3+s^2*t^4-s*t^5+t^6,
       s^6-s^4*t^2+s^3*t^3+s^2*t^4+s*t^5-t^6,
       s^6-s^3*t^3-s^2*t^4+s*t^5+t^6,
       s^6+s^5*t+s^4*t^2+s^3*t^3-s*t^5+t^6,
       s^8-s^7*t-s^6*t^2+s^5*t^3-s^4*t^4-t^8,
       s^9+s^8*t-s^7*t^2-s^6*t^3+s^5*t^4-s^4*t^5+t^9,
       s^14-s^13*t+s^12*t^2+s^11*t^3+s^9*t^5-s^8*t^6-s^5*t^9-s^2*t^12-t^14,
       s^16+s^15*t+s^14*t^2+s^13*t^3+s^11*t^5+s^9*t^7-s^8*t^8+s^7*t^9+s^5*t^11+s^3*t^13+s^2*t^14+s*t^15+t^16}

gbTrace = 1  
time greedySat(I,product CoeffList);
use ring J
time greedySat(J,t);

time C = GTZ1 I;




///
