kk = ZZ/32749
ringP3 = kk[x_0..x_3]
ringP1 = kk[s,t]
cubicMap = map(ringP1,ringP3,{s^3, s^2*t, s*t^2, t^3})
idealCubic = kernel cubicMap
idealCubic2 = monomialCurveIdeal(ringP3,{1,2,3})
M = matrix{{x_0,x_1,x_2},{x_1,x_2,x_3}}
idealCubic3 = minors(2, M)
codim idealCubic
degree idealCubic
dim idealCubic
gens idealCubic
0 == (gens idealCubic)%(gens idealCubic3)
idealCubic == idealCubic3
f = vars ringP3
OmegaP3 = kernel f
g=generators OmegaP3
OmegaP3=image g
presentation OmegaP3
G = res coker f
G.dd
G.dd_2
degrees source G.dd_2
degrees target G.dd_2
betti G
m = matrix{{x_0^3, x_1^2, x_2,x_3},{x_1^3,x_2^2,x_3,0}}
I = minors(2,m)
F = res(ringP3^1/I)
betti F
betti G
OmegaP3res = kernel (f ** (ringP3^1/idealCubic))
delta1 = jacobian idealCubic
delta2 = delta1 // (gens OmegaP3res)
delta = map(OmegaP3res, module idealCubic, delta2)
OmegaCubic = prune coker delta
prune HH^0((sheaf OmegaCubic)(>=0))
Cubic = Proj(ringP3/idealCubic)
cotangentSheaf Cubic
ringP4 = kk[x_0..x_4]
idealX = ideal(x_1+x_3, x_2+x_4)
idealL1 = ideal(x_1,x_2)
idealL2 = ideal(x_3,x_4)
idealY = intersect(idealL1,idealL2)
degree(idealX+idealY)
degree Tor_0(ringP4^1/idealX, ringP4^1/idealY)
degree Tor_1(ringP4^1/idealX, ringP4^1/idealY)
degree Tor_2(ringP4^1/idealX, ringP4^1/idealY)
res (ringP4^1/idealX)
ringP3 = kk[x_0..x_3];
load "mystery.m2"
idealX = mystery ringP3
prettyPrint gens idealX
X = variety idealX
idealX == saturate idealX
dim X
idealXtop = topComponents idealX
(gens idealXtop)%(gens idealX) == 0
codim singularLocus idealX
# decompose idealX
HH^0 OO_X
rank oo
HH^1 OO_X
degree idealX
P3 = Proj ringP3
HH^1((OO_P3(1)/idealX)(>=0))
degrees oo
omegaX = prune Ext^(codim idealX)(ringP3^1/idealX, ringP3^{-4})
dualModule = Hom(omegaX, ringP3^1/idealX)
betti prune dualModule
f = homomorphism dualModule_{0}
canGens = f*basis(0,omegaX)
ringX = ringP3/idealX
ringP5 = kk[x_0..x_5]
idealXcan = trim kernel map(ringX, ringP5, 
                               substitute(matrix canGens,ringX),
                               DegreeMap => i -> 5*i)
betti res idealXcan
deg2places = positions(degrees idealXcan, i->i=={2})
idealS= ideal (gens idealXcan)_deg2places
codim singularLocus idealS
omegaS = prune Ext^(codim idealS)(ringP5^1/idealS, ringP5^{-6})
OS = ringP5^1/idealS
omegaS**omegaS
omega2S = Hom(Hom(omegaS**omegaS, OS),OS)
L = Hom(omegaS, OS**(ringP5^{-1}))
dualModule = Hom(L, OS)
betti generators dualModule
g = homomorphism dualModule_{0}
toP2 = g*basis(0,L)
ringXcan = ringP5/idealXcan
ringP2 = kk[x_0..x_2]
idealXplane = trim kernel map(ringXcan, ringP2, 
                                  substitute(matrix toP2,ringXcan))
ringP2 = kk[x_0..x_2]
idealC2 = ideal(x_0^5+x_1^5+x_2^5)
ringC2 = ringP2/idealC2
ringP5 = kk[x_0..x_5]
idealC5 = trim kernel map(ringC2, ringP5, 
        gens (ideal vars ringC2)^2)
ringC5 = ringP5/idealC5
use ringC5
idealC = trim kernel map(ringC5, ringP3,
        matrix{{x_0+x_1,x_2,x_3,x_5}})
idealC == idealX
code mystery
code prettyPrint
