restart
uninstallPackage "SurfacesInP4"
restart
loadPackage "SurfacesInP4"
installPackage "SurfacesInP4"
restart;
loadPackage "SurfacesInP4";
-- See also Decker/Ein/Schreyer: Construction of Surfaces in PP^4
kk = ZZ/32003;
S = kk[x_0..x_4];
E = kk[e_0..e_4, SkewCommutative=>true];

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

--B1.14
--doesn't work
H = hilbertPolynomialFromInvariants(10,8,0,0);
M = guessCohomologyTable(H,0,4)
betti res constructSurface(M,E,S)

--B1.15
H = hilbertPolynomialFromInvariants(10,9,0,0);
M = guessCohomologyTable(H,0,4)
betti res constructSurface(M,E,S)

--B1.16
-- doesn't work: constant entries of beta are non-zero but should be zero
H = hilbertPolynomialFromInvariants(10,9,0,0);
M = guessCohomologyTable(H,0,4)
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
M = guessCohomologyTable(H,0,4)
betti res constructSurface(M,E,S)
-- does not work, since Cohomology Tabel non minimal
--B4.6
-- does not work, since Cohomology Tabel non minimal
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
-- doesn't work
H = hilbertPolynomialFromInvariants(10,6,2,1);
M = guessCohomologyTable(H,-1,3)
betti res constructSurface(M,E,S)


-- B6.2
-- doesn't work
H = hilbertPolynomialFromInvariants(15,21,2,1);
M = guessCohomologyTable(H,0,4)
betti res constructSurface(M,E,S)


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