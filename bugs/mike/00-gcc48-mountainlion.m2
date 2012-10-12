
#/var/folders/_s/927kdr250gjf5p9f3x78_dgh0000gp/T/M2-84774-0/0___Hom_lp__Module_cm__Module_rp.m2
#/var/folders/_s/927kdr250gjf5p9f3x78_dgh0000gp/T/M2-84774-0/0___Ext^__Z__Z_lp__Coherent__Sheaf_cm__Sum__Of__Twists_rp.m2
#/var/folders/_s/927kdr250gjf5p9f3x78_dgh0000gp/T/M2-84774-0/0___Ext_lp__Module_cm__Module_rp.m2
#/var/folders/_s/927kdr250gjf5p9f3x78_dgh0000gp/T/M2-84774-0/0_module_sphomomorphisms.m2
#/var/folders/_s/927kdr250gjf5p9f3x78_dgh0000gp/T/M2-84774-0/0_exp_lp__Ring__Element_rp.m2
#/var/folders/_s/927kdr250gjf5p9f3x78_dgh0000gp/T/M2-84774-0/0___Chain__Complex_sp_us_sp__Z__Z.m2
#/var/folders/_s/927kdr250gjf5p9f3x78_dgh0000gp/T/M2-84774-0/0_get__Non__Unit.m2
#/var/folders/_s/927kdr250gjf5p9f3x78_dgh0000gp/T/M2-84774-0/0___H__H_sp__Chain__Complex.m2
#/var/folders/_s/927kdr250gjf5p9f3x78_dgh0000gp/T/M2-84774-0/0_fraction_spfields.m2
#/var/folders/_s/927kdr250gjf5p9f3x78_dgh0000gp/T/M2-84774-0/0_resolution_lp__Module_rp.m2
/var/folders/_s/927kdr250gjf5p9f3x78_dgh0000gp/T/M2-84774-0/0___Ext^__Z__Z_lp__Coherent__Sheaf_cm__Coherent__Sheaf_rp.m2

R = QQ[x,y]/(y^2-x^3);
1_R//(1_R) -- this appears to be the problem with 'prune'
M = image matrix{{x,y}}
H = Hom(M,M)
H1 = prune H -- crashes
f1 = homomorphism H_{0}
f2 = homomorphism H_{1}
f3 = homomorphism H_{2}

S = QQ[a..d];
I = monomialCurveIdeal(S,{1,3,4})
R = S/I
X = Proj R
IX = sheaf (module I ** R)
Ext^1(IX,OO_X(>=-3)) -- crashes
Ext^0(IX,OO_X(>=-10))

R = QQ[x,y]/(x^3,y^2);
N = cokernel matrix {{x^2, x*y}}
H = Ext(N,N);
ring H
S = ring H;
H
isHomogeneous H
rank source basis( { -2,-3 }, H)
rank source basis( { -3 }, Ext^2(N,N) ) -- crashes
rank source basis( { -4,-5 }, H)
rank source basis( { -5 }, Ext^4(N,N) )
hilbertSeries H
hilbertSeries(H,Order=>11)

R = QQ[x,y]/(y^2-x^3);
M = module ideal(x,y)
F = map(R^1,M,matrix{{y,x^2}})
source F
target F == R^1
matrix F
isWellDefined F
isIsomorphism F
inc = inducedMap(R^1, M)
G = F // inc
target G == M and source G == M
inc * G == F
isWellDefined G
isIsomorphism G
prune coker G -- crashes
kernel G == 0

R = ZZ/11[x]/x^9
exp x -- crashes

R = QQ[x,y,z]/(x^3,y^3,z^3,x*y*z);
C = res(coker vars R, LengthLimit=>8) -- crashes

A = ZZ/101[a]/(a^2-1);
toField A
1//(a-1) -- crashes
getNonUnit A

R = QQ[x]/x^5;
C = res coker vars R -- crashes

frac ZZ
R = ZZ/101[x,y]/(x^3 + 1 + y^3)
frac R
x
1/x
x/1 -- crashes
use frac R
x
f = (x-y)/(x^6-y^6)
(x^3 - y^3) * f
numerator f
denominator f
liftable(1/f,R)
liftable(f,R)
lift(1/f,R)

k = ZZ/101; T = k[v..z];
m = matrix {{x,y,z,x^2*v,x*y*v,y^2*v,z*v,x*w,y^3*w,z*w}}
n = rank source m
R = k[u_1 .. u_n]
S = k[u_1 .. u_n,Degrees => degrees source m]
f = map(T,R,m)
g = map(T,S,m)
res ker f
res ker g
isHomogeneous f
isHomogeneous g
R = ZZ/32003[a..d]/(a^2+b^2+c^2+d^2);
M = coker vars R
C = resolution(M, LengthLimit=>6) -- crashes
A = QQ[x,y]
C = chainComplex(
               map(A^1,A^{3:-2},{{x^2,x*y,y^2}}),
               map(A^{3:-2},A^{2:-3},{{y,0},{ -x,y},{0,-x}}),
               map(A^{2:-3},0,0))
M = HH_0 C
res M = C;
res M

S = QQ[a..d];
I = monomialCurveIdeal(S,{1,3,4})
R = S/I
X = Proj R
IX = sheaf (module I ** R)
Ext^1(IX,OO_X) -- crashes
Hom(IX,OO_X)
