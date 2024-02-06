--needsPackage "LocalRings"


localTrim = I -> (
    S := ring I;
    if not S.?maxIdeal then setMaxIdeal ideal vars S;
    ideal localMingens gens I)

generalLink = J -> (
    J = if class J === Ideal then J else ideal J;
    if J == ideal(1_(ring J)) then return J;
    S:= ring J;
    J0 := ideal (gens(J)*random(S^(rank source gens J), S^(codim J)));
    elapsedTime K := J0:J;
    elapsedTime J' := if isHomogeneous K then K else localTrim K
)
pregeneralLink = J -> (
    J = if class J === Ideal then J else ideal J;
    if J == ideal(1_(ring J)) then return J;
    S:= ring J;
    J0 := ideal (gens(J)*random(S^(rank source gens J), S^(codim J)));
    (J0,J)
)

S = ZZ/32003[x,y,z]
M0 = monomialIdeal(x^3,x^2*y^2,x*y^3,y^4,x^2*y*z,x*y^2*z,z^5)
M0 = monomialIdeal(x^3,x^2*y^2,x*y^3,y^4,x^2*y*z,x*y^2*z,z^5)
end--

uninstallPackage "LocalRings"
restart
needsPackage ("LocalRings", FileName => "../LocalRings.m2")
load "mike-linkage.m2"

setRandomSeed 0
I = generalLink M0;
I2 = generalLink I;
res I2
R = localRing(S,ideal vars S)
J = sub(I2,R)
debugLevel = 2
Jmin = mingens J
syz Jmin;
--elapsedTime F = res J

f = J_0
phi= map(R^1, R^1,f)
debugLevel = 3
s0 = quotient(phi, F.dd_1, Strategy => Local)
--s0 = phi//F.dd_1
f1 = map(F_1,F_1, f*id_(F_1))
s1 = quotient(f1 - s0*F.dd_1, F.dd_2,  Strategy => Local)
f1 == F.dd_2*s1 + s0*F.dd_1
f1 -  (F.dd_2*s1 + s0*F.dd_1)
f1 -  (s0*F.dd_1)

image f1 == image(F.dd_2*s1 + s0*F.dd_1)

errorDepth = 0
a = matrix{{x+x^2+x^2*y^3}}
b = matrix{{x}}
quotient(b,a)

----simpler failing example 2/26/2021
restart
needsPackage ("LocalRings", FileName => "../LocalRings.m2")
load "mike-linkage.m2"
peek loadedFiles
S = ZZ/32003[x,y,z]
M0 = ideal ((gens ideal(z^2, x^3, y^4, x*y^2,x^2*y))*random(S^5,S^5))
--M0 = ideal (random(S^1, S^-{3,5})*random(S^3,S^3))
isHomogeneous M0
use S
I = generalLink M0;
res I
R = localRing(S,ideal vars S)
J = sub(I,R)
F = res J
F.dd
f = J_0
phi= map(R^1, R^1,f)
s0 = phi//F.dd_1
f1 = map(F_1,F_1, f*id_(F_1))
s1 = (f1 - s0*F.dd_1)//F.dd_2
f1 == F.dd_2*s1 + s0*F.dd_1
f1 -  (F.dd_2*s1 + s0*F.dd_1)




elapsedTime I = generalLink I;
elapsedTime (J0,J) = pregeneralLink ideal(I_*);
numgens J
numgens J0
J
elapsedTime K=(J0:J);
numgens K
res K

S' = localRing(S, ideal vars S)
K' = sub(K,S');
numgens K'
--elapsedTime trim K'; doesn't seem to complete
J0' = sub(J0,S');
J' = sub(J,S');
elapsedTime K' = J0':J';
trim J'
numgens J'

loc = (ZZ/32003){x,y,z}
L0 = sub(J0,loc);
L = sub(J,loc);
M = sub(K,loc);
elapsedTime (L0:L);


-- simplify an ideal I in the local ring.  Perhaps we are assuming it is finite length.
simplifyIdeal = (I, loc) -> (
    vs := for x in gens ring I list (
        elim := eliminate(I, toList(set gens ring I - set{x}));
        if numgens elim == 0 then continue;
        x^((terms elim_0)/degree/first//min)
        );
    J := trim(I + ideal vs);
    Jloc := sub(J, loc);
    ideal gens gb Jloc)

L = simplifyIdeal (J,loc);
--elapsedTime(L0:L); not good: > 123 sec
see L
see trim L
numgens J1
netList J1_*
codim J1
gens(J1)%sub(J0,loc)

P = simplifyIdeal(I, loc)
P_0
(P_0 - 9943*z*P_1 + 11263*z^2*P_1 + 10170*z^3*P_1)  -- this gives y*z in the ideal, therefore x*z, therefore x*y
use ring P
(x*y) % P
(x*z) % P
(y*z) % P
(z^2) % P
P
elapsedTime gens gb J0;
elapsedTime gens gb ideal(J_*);
J0 = ideal J0_*;
J = ideal J_*;
elapsedTime G = groebnerBasis (ideal(J0_*), Strategy => "F4");
elapsedTime groebnerBasis (ideal(J_*), Strategy => "F4");
leadTerm gens gb J0;
fz = eliminate({x,y},J0);
fy = eliminate({x,z},J0);
fx = eliminate({y,z},J0);
use ring J0
J0 = trim(ideal(x*z^2, x^4, y^4, z^7) + J0);
see ideal gens gb J0

T = ZZ/32003[e,x,y,z,MonomialOrder => Eliminate 1]
JT0 = sub(J0, T);
hJT0 = ideal gens gb homogenize(JT0,e);
--eJT0 = (flatten entries hJT0);
L = (leadTerm gens gb hJT0);
L1 = flatten entries gens ideal sub(L, e=>1)
realinit = flatten entries gens trim ideal sub(L, e=>1)
realpositions = flatten apply(realinit, m -> positions (L1, ell-> ell == m))
localgb = ideal (gens hJT0)_realpositions;
see localgb

(e^100*x^2) % hJT0;
localgb_2
use ring J0
eliminate(J0, {x,y}); -- gives z^7 in the ideal (as it is the lead term, in antidegree ordering)
eliminate(J0, {x,z}); -- y^4 is in the ideal
eliminate(J0, {y,z}); -- x^4 is in the ideal
trim(ideal(x^4, y^4, z^7) + J0);


loc = (ZZ/32003){x,y,z}

phi = map(loc,T,{1,x,y,z})
locJ0 = phi localgb;
gens gb oo

use ring I
eliminate({x,y}, I)
eliminate({x,z}, I)
eliminate({y,z}, I)
I = trim(ideal(x*y*z,z^5,y^2,x^2) + I)
hI = homogenize(sub(I, T), e)
IT = ideal gens gb 
Iloc = sub(I, loc)
gens gb Iloc;
see ideal oo

leadTerm locJ0
fJ0 = forceGB locJ0;
R = loc/(ideal locJ0)
f = locJ_0
sub(f, R) -- bug
locJ = sub(J, loc);
(locJ)_0 % fJ0


J_0
f = sub(J_0,T) -e
JT = ideal (e*f-1) + JT0
elapsedTime groebnerBasis (ideal(JT_*), Strategy => "F4");
leadTerm oo

T = (ZZ/32003){x,y,z}
JT0 = sub(J0, T);
f = sub(J_0,T)
JT = ideal (e*f-1) + JT0
gens gb JT0;
netList JT0_*
elapsedTime JT0:ideal f;
elapsedTime groebnerBasis (ideal(JT_*), Strategy => "F4");
leadTerm oo

J0 = ideal J0_*;
J = ideal J_*;
elapsedTime groebnerBasis (ideal(J_*), Strategy => "MGB"); -- slower than F4
elapsedTime G = groebnerBasis (ideal(J0_*), Strategy => "MGB"); -- slower than F4.

numgens J0
numgens J
see J0
elapsedTime quotient(J0,J,Strategy => Iterate); -- 9 - 10 sec -- the winner! and the default.
elapsedTime quotient(J0, J_0);

m = matrix{{J_0}} | (gens J0);
elapsedTime syz(gb (m, Syzygies => true, SyzygyRows => 1, Strategy => LongPolynomial));


S' = localRing(S, ideal vars S)
elapsedTime quotient(J0,J,Strategy => Quotient); -- 15 sec

T = ZZ/32003[t,x,y,z]
J0h = ideal apply(J0_*, f-> homogenize(sub(f,T),t));
Jh = ideal apply(J_*, f-> homogenize(sub(f,T),t));
elapsedTime  J0h: Jh;
mingens oo

elapsedTime gb J0;
elapsedTime gb ideal(J_*);
for f in J_* list elapsedTime J0:f;
elapsedTime intersect oo;
for i from 0 to numgens J -1 list 
elapsedTime syz (matrix{J_*}|matrix{J0_*});
elapsedTime groebnerBasis (ideal(J_*), Strategy => "F4");
elapsedTime G = groebnerBasis (ideal(J0_*), Strategy => "F4");
m = transpose gens J | (target transpose gens J)**G;
elapsedTime syz(gb (m, Syzygies => true, SyzygyRows => 1, Strategy => LongPolynomial));
T = ZZ/32003[t, x,y,z]--, MonomialOrder => Eliminate 1]
elapsedTime groebnerBasis (t*sub(J0, T)+(1-t)*ideal(sub(J_0,T)) , Strategy => "F4")


--------------------
LIB "random.lib";
LIB "modules.lib";
ring r=32003,(x,y,z),ds;
ideal m = (x^3,x^2*y^2,x*y^3,y^4,x^2*y*z,x*y^2*z,z^5);
m;
ideal j = randomid(m, 3);
ideal m1 = quotient(j,m);
def F = res(m,0);
print(betti (F),"betti");
def s1 = std(m1);
def F1 = fres(s1,0);
print(betti (F1),"betti");




ideal id1=maxideal(3);
ideal id2=x2+xyz,y2-z3y,z3+y5xz;
ideal id6=quotient(id1,id2);
id6;
quotient(id2,id1);
==> _[1]=z2
==> _[2]=yz
==> _[3]=y2
==> _[4]=xz
==> _[5]=xy
==> _[6]=x2
module m=x*freemodule(3),y*freemodule(2);
ideal id3=x,y;
quotient(m,id3);
==> _[1]=[1]
==> _[2]=[0,1]
==> _[3]=[0,0,x]
 
-- consider minbase

