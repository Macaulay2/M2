kk = ZZ/101
R = kk[a..c]
I = ideal random(R^1, R^{-2,-2})
time gens gb(I, Algorithm=>LinearAlgebra, GBDegrees=>{1,1,1}, Strategy=>UseSyzygies)
collectGarbage()

-----------------------------------
R = ZZ/101[a..e]
I = ideal random(R^1, R^{-2,-2,-2})
time gens gb(I, Algorithm=>LinearAlgebra)
gbTrace=3
gbRemove I
time I = gens gb I

------------------------------
R = ZZ/101[a,b,c,d]
I = ideal(a^2, a*b, a*c, b^2,a*d^3, b^4)
gens gb(I, Algorithm=>LinearAlgebra, GBDegrees=>{1,1,1,1})

R = ZZ/101[a,b,c,d]
I = ideal(a^2-b*c, a*b)
gens gb(I, Algorithm=>LinearAlgebra, GBDegrees=>{1,1,1,1})

------------------------------
R = ZZ/101[a,b,c,d,e,f]
I = ideal(a*b*c,b*c*d,c*d*e)
gens gb(I, Algorithm=>LinearAlgebra)

------------------------------
R = ZZ/101[a,b,c,d,e,f]
I = ideal basis(3,R)
gens gb(I, Algorithm=>LinearAlgebra)
------------------------------
R = ZZ/101[a..h]
I = ideal random(R^1, R^{-3,-3,-3})
time I = gens gb I;
gbTrace=3
time gens gb(I, Algorithm=>LinearAlgebra, GBDegrees=>{1,1,1,1,1,1,1,1});
------------------------------
R = ZZ/101[a..h,MonomialSize=>8]
I = ideal random(R^1, R^{-3,-4,-4,-5});
J = ideal"a3,b4,c4,d5"
gbTrace=3
installHilbertFunction(I, poincare J)
time I1 = gens gb(I);
I = ideal flatten entries gens I
time I1 = gens gb(I, Algorithm=>LinearAlgebra);

gbRemove I
I = matrix entries I;
time gens gb(I, Algorithm=>LinearAlgebra, GBDegrees=>{1,1,1,1,1,1,1,1});
I1 = ideal  flatten entries gens I;
time gens gb(I1, Algorithm=>LinearAlgebra, GBDegrees=>{1,1,1,1,1,1,1,1});
time gens gb(I1, Algorithm=>LinearAlgebra)
------------------------------
R = ZZ/101[vars(0..17)]
m1 = genericMatrix(R,a,3,3)
m2 = genericMatrix(R,j,3,3)
J = ideal flatten(m1*m2-m2*m1)
J1 = ideal flatten(m1*m2-m2*m1)
time gens gb J
time gens gb(J1, Algorithm=>LinearAlgebra, GBDegrees=>toList(18:1))

time gens gb ideal(m1^4);
time gens gb(ideal(m1^4), Algorithm=>LinearAlgebra, GBDegrees=>toList(18:1));
------------------------------
R = ZZ/5[a..d,MonomialSize=>16];
I = ideal{a^3-2*a^2*b-a*b^2-2*b^3+a*b*c-2*b^2*c+2*a*c^2-2*b*c^2-c^3+2*a*b*d
	  -2*b^2*d-a*c*d-2*b*c*d-2*c^2*d+a*d^2+c*d^2-d^3, a^25, b^25, c^25, d^25}

time gens gb(I, Algorithm=>LinearAlgebra, GBDegrees=>toList(4:1));
time gens gb(I, Algorithm=>LinearAlgebra);
I = ideal flatten entries gens I;
gbTrace=3
time gens gb(I);


R = ZZ/5[a..d,MonomialSize=>16];
I = ideal{a^3-2*a^2*b-a*b^2-2*b^3+a*b*c-2*b^2*c+2*a*c^2-2*b*c^2-c^3+2*a*b*d
	  -2*b^2*d-a*c*d-2*b*c*d-2*c^2*d+a*d^2+c*d^2-d^3, a^125, b^125, c^125, d^125}
time gens gb(I, Algorithm=>LinearAlgebra, GBDegrees=>toList(4:1));
time gens gb(I, Algorithm=>LinearAlgebra, GBDegrees=>toList(4:1), DegreeLimit=>180);
time gens gb(I, Algorithm=>LinearAlgebra, GBDegrees=>toList(4:1), DegreeLimit=>191);

time gens gb(I, Algorithm=>LinearAlgebra, DegreeLimit=>191);
time gens gb(I, Algorithm=>LinearAlgebra);

-- interrupted in degree 191, used 58.43 seconds, space: 1.07 GB real, 1.16 GB virtual


gbTrace=3
time gb I;
time gb(I, Algorithm=>Homogeneous2, Strategy=>LongPolynomial);


-------------------------------
restart
loadPackage "ExampleIdeals"
I = cyclicRootsHomogeneous(8,ZZ/23)
time gens gb(I, Algorithm=>LinearAlgebra, GBDegrees=>toList(numgens ring I:1));
time gens gb(I, Algorithm=>LinearAlgebra, GBDegrees=>toList(numgens ring I:1), Strategy=>15);
describe R
R1 = (coefficientRing R)[i, a..h]
I = substitute(I,R1)
R1 = (coefficientRing R)[reverse gens R]
I = substitute(I,R1)
R1 = (coefficientRing R)[i,reverse(a..h)]
I = substitute(I,R1)
R1 = (coefficientRing R)[reverse(a..h),i]
I = substitute(I,R1)

R = (coefficientRing R)[gens R, MonomialSize=>8]
I = substitute(I,R)

I = cyclicRootsHomogeneous(8,ZZ/32003)
time gens gb(I, Algorithm=>LinearAlgebra, GBDegrees=>toList(numgens ring I:1));
gbTrace=3
time gens gb I;
time gens gb(I, Algorithm=>Homogeneous2, Strategy=>LongPolynomial);

I = cyclicRootsHomogeneous(9,ZZ/23)
R1 = (coefficientRing R)[j, a..i]
I = substitute(I,R1)
time gens gb(I, Algorithm=>LinearAlgebra, GBDegrees=>toList(numgens ring I:1));

I = katsura(8,ZZ/23)
time gens gb(I, Algorithm=>LinearAlgebra, GBDegrees=>toList(numgens ring I:1));

R = ZZ/101[a..e];
I = ideal random(R^1, R^{-2,-2,-2})
L = flatten entries gens I;
scan(1000, i -> time gens gb(ideal L, Algorithm=>LinearAlgebra, GBDegrees=>{1,1,1,1,1}))


restart
loadPackage "ExampleIdeals"
I = commuting4by4grevlex(ZZ/101)
time gens gb(I, Algorithm=>LinearAlgebra, GBDegrees=>toList(numgens ring I:1));

-- Inhomogeneous GB's don't work yet:
R = ZZ/101[a..d]
I = ideal"ab-4,ac-3c-d,d3-5"
gbI = matrix"bc+34bd-35c,ac-3c-d,ab-4,d3-5"
gens gb I  == gbI
gbTrace = 10
time gens gb(I, Algorithm=>LinearAlgebra, GBDegrees=>toList(numgens ring I:1));

-- GB over modules working? NO!!
R = ZZ/101[a..d]
M = matrix"a,b,c,d;b,c,d,a"
gbTrace=10
time gens gb(M, Algorithm=>LinearAlgebra, GBDegrees=>toList(numgens R:1));

R = ZZ/101[a..d, MonomialOrder=>{4,Position=>Up}]
R = ZZ/101[a..d, MonomialOrder=>{4,Position=>Down}] -- currently this does not work
M = matrix"a,b,c,d;b,c,d,a"
M = matrix"a,b,c,d;a,c,d,a"
gbTrace=10
time gens gb(M, Algorithm=>LinearAlgebra, GBDegrees=>toList(numgens R:1));
time gens gb(M, Algorithm=>LinearAlgebra);

-------------------------
R = ZZ/23[x,y,z,h];
I = ideal{x^2*y-z^2*h, x*z^2-y^2*h, y*z^3-x^2*h}
gbTrace = 3
time G =  gens gb(I, Algorithm=>LinearAlgebra, GBDegrees=>toList(numgens ring I:1)
    , Strategy=>UseSyzygies
    )

R = ZZ/23[x,y,z,h];
I = ideal{x^2*y-z^2*h, x*z^2-y^2*h, y*z^3-x^2*h}
gbTrace = 3
time G =  gens gb(I, Algorithm=>LinearAlgebra, GBDegrees=>toList(numgens ring I:1)
     , DegreeLimit => {5}
     , Strategy=>UseSyzygies
     )

-- To work on:
-- (a) Inhomogeneous GB (are they working yet?)
-- (b) Over modules: problem seems to be a sorting issue
-- (c) Different monomial orders
-- (d) Further: Quotient rings, over QQ, over fraction fields.  HF use, syzygies.
-- (e) Think through the memory allocation a bit better
-- (f) Pack the monomials better
-- (g) weights other than 1,1,1...,1.  
--     These could be handled by changing the exponents at the start, at end.
-- (h) Skew commuting variables, and Weyl algebra arithmetic
-- (i) want to allow matrix size to be limited, but allow easy continuation of the computation.

-- check out the Groebner walk
