uninstallPackage "SurfacesInP4"
restart
path = append(path, homeDirectory | "M2/msri2010/")
installPackage("SurfacesInP4", UserMode => true) 

-- See also Decker/Ein/Schreyer: Construction of Surfaces in PP^4
KK = ZZ/32003;
S = KK[x_0..x_4];
E = KK[e_0..e_4, SkewCommutative=>true];

-- B1. Rational Surfaces

-- B1.4 (cubic scroll)
H = hilbertPolynomialFromInvariants(3,0,0,0);
M = guessCohomologyTable(H,-2,2);
betti res constructSurface(M,E,S)

--B1.6 (Veronese)
H = hilbertPolynomialFromInvariants(4,0,0,0);
M = guessCohomologyTable(H,-2,2);
betti res constructSurface(M,E,S)

--B1.7 (Castelnuovo)
H = hilbertPolynomialFromInvariants(5,2,0,0);
M = guessCohomologyTable(H,-1,3)
betti res constructSurface(M,E,S)

-- alternatively:
M = guessCohomologyTable(H,-2,2)
betti res constructSurface(M,E,S)


--B1.8 (Bordiga)
H = hilbertPolynomialFromInvariants(6,3,0,0);
M = guessCohomologyTable(H,-1,3)
betti res constructSurface(M,E,S)

--B1.9 
H = hilbertPolynomialFromInvariants(7,4,0,0);
M = guessCohomologyTable(H,-1,3)
betti res constructSurface(M,E,S)

--B1.10 
H = hilbertPolynomialFromInvariants(8,5,0,0);
M = guessCohomologyTable(H,0,4)
betti res constructSurface(M,E,S)

--B1.11
H = hilbertPolynomialFromInvariants(8,6,0,0);
M = guessCohomologyTable(H,0,4)
betti res constructSurface(M,E,S)

--B1.12
-- generic construction actually gives the following Enriques-surface
--B3.1 
H = hilbertPolynomialFromInvariants(9,6,0,0);
M = guessCohomologyTable(H,0,4)
betti res constructSurface(M,E,S)

--B1.13 
H = hilbertPolynomialFromInvariants(9,7,0,0);
M = guessCohomologyTable(H,0,4)
betti res constructSurface(M,E,S)
monadE = guessDifferentials(M,E)
monadEt = guessDifferentialsTranspose(M,E)
betti res constructSurface(monadEt,S)

--B1.14
--doesn't work
H = hilbertPolynomialFromInvariants(10,8,0,0);
M = guessCohomologyTable(H,0,4)
betti res constructSurface(M,E,S)


alpha1 = ideal (x_0..x_4,0,0)
ran = random(S^1,S^1) ++ random(S^1,S^1) ++ random(S^1,S^1) ++ random(S^1,S^1) ++ random(S^1,S^1)
alpha21 = vars S * ran
j22 = ideal (x_0^2,x_1^2,x_2^2)
j23 = ideal (x_2^2,x_3^2,x_4^2)
ran22 = random(S^3,S^1)
ran23 = random(S^3,S^1)
alpha22 = (gens j22) * ran22
alpha23 = (gens j23) * ran23
alpha21 = map(S^{0},,alpha21 | alpha22 | alpha23)
alpha  = (gens alpha1) || alpha21
falpha = res coker alpha
betti falpha
kos = res coker map(S^{1:1},,vars S)
tkos5 = transpose kos.dd_5
ran = random(S^{1:4},S^{12:4})
aaa = submatrix(falpha.dd_3,,{0..11})
taaa = transpose aaa
bbb = ran*taaa
tphi = bbb//tkos5
tpresg = transpose falpha.dd_3
dir = tpresg || tphi
fdir = res coker dir
betti fdir 
I = trim ideal fdir.dd_2;


--B1.15
H = hilbertPolynomialFromInvariants(10,9,0,0);
M = guessCohomologyTable(H,0,4)
betti res constructSurface(M,E,S)

--B1.16
H = hilbertPolynomialFromInvariants(10,9,0,0);
-- non minimal cohomology table
M = matrix{{0,0,0,0,0},{0,0,0,0,0},{0,2,0,0,0},{0,0,0,2,1},{0,0,0,0,2}}
betti res constructSurface(M,E,S)



--B1.17
H = hilbertPolynomialFromInvariants(11,11,0,0);
M = guessCohomologyTable(H,0,4)
betti res constructSurface(M,E,S)

--B1.18 and B1.19: matrices should not be chosen generically, cohomology table the same as B1.17

--B2 ruled surfaces
--B2.1 quintic elliptic scroll

H = hilbertPolynomialFromInvariants(5,1,1,0);
M = guessCohomologyTable(H,-2,2)
betti res constructSurface(M,E,S)

-- B2.2 Elliptic conic bundle

H = hilbertPolynomialFromInvariants(8,5,1,0);
M = guessCohomologyTable(H,-1,3)
betti res constructSurface(M,E,S)

-- B3 Enriques Surfaces

--B3.1
H = hilbertPolynomialFromInvariants(9,6,0,0);
M = guessCohomologyTable(H,0,4)
betti res constructSurface(M,E,S)

--B3.2
H = hilbertPolynomialFromInvariants(10,8,0,0);
M = guessCohomologyTable(H,0,4)
betti res constructSurface(M,E,S)
-- does not work


--B3.3
H = hilbertPolynomialFromInvariants(11,10,0,0);
M = guessCohomologyTable(H,0,4)
betti res constructSurface(M,E,S)
-- does not work

--B3.4
H = hilbertPolynomialFromInvariants(13,16,0,0);
M = guessCohomologyTable(H,0,4)
betti res constructSurface(M,E,S)
-- does not work

--B4 K3-Surfaces

--B4.3
H = hilbertPolynomialFromInvariants(7,5,0,1);
M = guessCohomologyTable(H,-1,3)
betti res constructSurface(M,E,S)
--B4.4
H = hilbertPolynomialFromInvariants(8,6,0,1);
M = guessCohomologyTable(H,-1,3)
betti res constructSurface(M,E,S)
--B4.5
H = hilbertPolynomialFromInvariants(9,8,0,1);
M = guessCohomologyTable(H,0,4)
betti res constructSurface(M,E,S)
--B4.6
H = hilbertPolynomialFromInvariants(10,9,0,1);
-- non minimal coohomology table

rd' = random(S^{1:5,1:4},S^{7:3}) | (random(S^{1:5},S^{3:4}) || map(S^{1:4},S^{3:4},0))
frd' = res coker rd' 
betti frd'
frd = res coker transpose frd'.dd_5
betti frd
kos = res coker map(S^{1:3},S^{5:2},vars S)
betti kos
tkos5 = transpose kos.dd_5
trd = transpose frd.dd_3;
betti trd
betti tkos5
ran = random(target tkos5,target trd)
f = ((ran*trd)//tkos5)||transpose frd.dd_3;
tphi = f || random(S^{1:1},source f);
ftphi = res prune coker tphi
I = ideal ftphi.dd_2;
hilbertPolynomial I

M = matrix{{0,0,0,0,0},{1,0,0,0,0},{0,1,0,0,0},{0,0,1,3,1},{0,0,0,0,1}}
betti res (I = constructSurface(M,E,S))
-- gives different syzygies. This is actually the elliptic surface B7.4
-- does not work.

-- B4.7.
-- non minimal coohomology table
M = matrix{{0,0,0,0,0},{1,0,0,0,0},{0,1,0,0,0},{0,0,1,3,2},{0,0,0,0,2}}
betti res (I = constructSurface(M,E,S))
-- does not work
-- alternative construction 

rd = random(S^{1:2},S^{2:1,4:0})
frd = res coker rd
betti frd
tphi11 = random(S^{1:1},S^{1:0})
tphi12 = random(S^{1:1},S^{10:1})
tphi13 = map(S^{1:1},S^{3:2},0)
tphi14 = random(S^{1:1},S^{1:0})
tphi11 = tphi11 | tphi12 | tphi13 | tphi14
ran = random(S^1,S^8)
aaa = submatrix(frd.dd_3,,{0..7})
taaa = transpose aaa
bbb' = ran*taaa
bbb = map(S^{rank target bbb':2},,bbb')
tphi21 = bbb//tkos5
kos = res coker map(S^{1:3},S^{5:2},vars S)
betti kos
tkos5 = transpose kos.dd_5
tkos4 = transpose kos.dd_4
tphi22 = random(S^{10:1},S^{1:1})
tphi22 = tkos4*tphi22
tphi21 = tphi21 | tphi22
tphi = tphi11 || tphi21
tpresg = transpose frd.dd_3
nul = map(S^{16:2},S^{1:0},0)
tpresg = tpresg | nul
dir = tphi || tpresg 
fdir = res prune coker dir
betti fdir 
I = ideal fdir.dd_2;
codim singularLocus I == 5 


--B4.8
H = hilbertPolynomialFromInvariants(11,11,0,1);
M = guessCohomologyTable(H,0,4)
betti res constructSurface(M,E,S)

--B4.9
-- same cohomology table, but one 6-secant line




--B4.10
-- same cohomology table, but two 6-secant lines
--B4.11
-- same cohomology table, but three 6-secant lines
--B4.12
H = hilbertPolynomialFromInvariants(11,12,0,1);
M = guessCohomologyTable(H,0,4)
betti res constructSurface(M,E,S)
--B4.13
H = hilbertPolynomialFromInvariants(12,14,0,1);
M = guessCohomologyTable(H,0,4)
betti res constructSurface(M,E,S)
--B4.14
-- doesn't work
H = hilbertPolynomialFromInvariants(13,16,0,1);
M = guessCohomologyTable(H,0,4)
betti res constructSurface(M,E,S)

--B4.15
-- doesn't work
H = hilbertPolynomialFromInvariants(14,19,0,1);
M = guessCohomologyTable(H,0,4)
betti res constructSurface(M,E,S)

--B5 Bielliptic surfaces
-- doesn't work
H = hilbertPolynomialFromInvariants(10,6,1,0);
M = guessCohomologyTable(H,-1,3)
betti res constructSurface(M,E,S)

-- B5 no surface constructable by general method

-- B6 Abelian surfaces

--B6.1
-- non minimal cohomology table
M = matrix{{0,0,0,0,0},{5,1,0,0,0},{0,2,0,0,0},{0,0,0,5,10},{0,0,0,0,0}}
betti res constructSurface(M,E,S)
-- does not work
--betti guessDifferentials(M,E)

alphad  = map(E^{5:0},E^{2:-2},{{e_4*e_1,e_2*e_3},{e_0*e_2,e_3*e_4},{e_1*e_3,e_4*e_0},{e_2*e_4,e_0*e_1},{e_3*e_0,e_1*e_2}})
betad = syz alphad
betti alphad
betti betad
--constructSurface(chainComplex{alphad,betad},S)

loadPackage "BGG"
alpha = beilinson(alphad,S)
beta = beilinson(betad,S)
F = prune homology(alpha,beta);
betti F
fF = res F
betti fF
rd = random(target presentation F,S^{1:-3})
betti (tphi = transpose (presentation F | rd))
ftphi = res prune coker tphi
betti ftphi     
I = ideal ftphi.dd_2;
betti I
codim singularLocus I === 5

-- B6.2
-- non minimal cohomology table
M = matrix{{0,0,0,0,0},{1,0,0,0,0},{2,10,10,5,0},{0,0,0,0,0},{0,0,0,0,0}}
betti res constructSurface(M,E,S)
-- does not give a monad

-- Use the surface obtained in B6.1. Then construct the surface via linkage
V = ideal (gens I*random(source gens I,S^{2:-5}));
J = V : I;
betti J
codim singularLocus J == 5

-- B7 Elliptic surfaces
--B7.1
-- rootfinder issue
H = hilbertPolynomialFromInvariants(7,6,0,2);
M = guessCohomologyTable(H,-1,3)
betti res constructSurface(M,E,S)

--B7.2
H = hilbertPolynomialFromInvariants(8,7,0,2);
M = guessCohomologyTable(H,0,4)
betti res constructSurface(M,E,S)

--B7.3
H = hilbertPolynomialFromInvariants(9,7,0,1);
M = guessCohomologyTable(H,0,4)
betti res constructSurface(M,E,S)

--B7.4
-- doesn't work since table is not minimal
H = hilbertPolynomialFromInvariants(10,9,0,1);
M = guessCohomologyTable(H,0,4)
betti res constructSurface(M,E,S)

--B7.5
H = hilbertPolynomialFromInvariants(10,10,0,2);
M = guessCohomologyTable(H,0,4)
betti res constructSurface(M,E,S)

--B7.6
H = hilbertPolynomialFromInvariants(11,12,0,2);
M = guessCohomologyTable(H,0,4)
betti res constructSurface(M,E,S)

--B7.7
-- doesn't work
H = hilbertPolynomialFromInvariants(12,13,0,2);
M = guessCohomologyTable(H,0,4)
betti res constructSurface(M,E,S)

--B7.8
H = hilbertPolynomialFromInvariants(12,14,0,2);
M = guessCohomologyTable(H,0,4)
betti res constructSurface(M,E,S)

--B7.9
-- doesn't work since constructed by special syzygy
H = hilbertPolynomialFromInvariants(12,14,0,1);
M = guessCohomologyTable(H,0,4)
betti res constructSurface(M,E,S)