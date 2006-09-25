 --  From: http://www.symbolicdata.org/doc/Data/INTPS/Issac-97.html
 --
 --
 --  Type:    Key:Issac-97
 --  INTPS
 --
 --  basis:   -2*w^2+9*w*x+8*x^2+9*w*y+9*x*y+6*y^2-7*w*z-3*x*z-7*y*z-6*z^2-4*w+8*x+4*y+8*z+2,
 --           3*w^2-5*w*x+4*x^2-3*w*y+2*x*y+9*y^2-6*w*z-2*x*z+6*y*z+7*z^2+9*w+7*x+5*y+7*z+5,
 --           7*w^2+5*w*x+2*x^2+3*w*y+9*x*y-4*y^2-5*w*z-7*x*z-5*y*z-4*z^2-5*w+4*x+6*y-9*z+2,
 --           8*w^2+5*w*x+5*x^2-4*w*y+2*x*y+7*y^2+2*w*z-7*x*z-8*y*z+7*z^2+3*w-7*x-7*y-8*z+8
 --
 --  vars:    w, x, y, z
 --
 --  dlist:   2, 2, 2, 2
 --
 --  isHomog: 0
 --
 --  llist:   15, 15, 15, 15
 --
 --  Comment: The ISSAC'97 system challenge: Compute lex Groebner basis
 --           (over the rationals).
 --
 --  Record created by obachman on Dec 10 1999.       Latest change on Mar 20 2000.
 --
 --

error "after re-implementation these computations consume a gigantic amount of memory!!!  Still true Apr  7, 2006."

R = QQ[w,x,y,z,MonomialOrder => Lex]
gbTrace = 3

-- I had to comment this out to get the tests to run, but to reinstate this test later.

time betti gens gb ideal (
        -2*w^2+9*w*x+8*x^2+9*w*y+9*x*y+6*y^2-7*w*z-3*x*z-7*y*z-6*z^2-4*w+8*x+4*y+8*z+2,
        3*w^2-5*w*x+4*x^2-3*w*y+2*x*y+9*y^2-6*w*z-2*x*z+6*y*z+7*z^2+9*w+7*x+5*y+7*z+5,
        7*w^2+5*w*x+2*x^2+3*w*y+9*x*y-4*y^2-5*w*z-7*x*z-5*y*z-4*z^2-5*w+4*x+6*y-9*z+2,
        8*w^2+5*w*x+5*x^2-4*w*y+2*x*y+7*y^2+2*w*z-7*x*z-8*y*z+7*z^2+3*w-7*x-7*y-8*z+8)

R = ZZ[w,x,y,z,t,MonomialOrder => Lex]
time betti gb homogenize(ideal (
        -2*w^2+9*w*x+8*x^2+9*w*y+9*x*y+6*y^2-7*w*z-3*x*z-7*y*z-6*z^2-4*w+8*x+4*y+8*z+2,
        3*w^2-5*w*x+4*x^2-3*w*y+2*x*y+9*y^2-6*w*z-2*x*z+6*y*z+7*z^2+9*w+7*x+5*y+7*z+5,
        7*w^2+5*w*x+2*x^2+3*w*y+9*x*y-4*y^2-5*w*z-7*x*z-5*y*z-4*z^2-5*w+4*x+6*y-9*z+2,
        8*w^2+5*w*x+5*x^2-4*w*y+2*x*y+7*y^2+2*w*z-7*x*z-8*y*z+7*z^2+3*w-7*x-7*y-8*z+8), t);

end
R = QQ[w,x,y,z,MonomialOrder => Lex]
gbTrace = 3
time gens gb (I=ideal (
        -2*w^2+9*w*x+8*x^2+9*w*y+9*x*y+6*y^2-7*w*z-3*x*z-7*y*z-6*z^2-4*w+8*x+4*y+8*z+2,
        3*w^2-5*w*x+4*x^2-3*w*y+2*x*y+9*y^2-6*w*z-2*x*z+6*y*z+7*z^2+9*w+7*x+5*y+7*z+5,
        7*w^2+5*w*x+2*x^2+3*w*y+9*x*y-4*y^2-5*w*z-7*x*z-5*y*z-4*z^2-5*w+4*x+6*y-9*z+2,
        8*w^2+5*w*x+5*x^2-4*w*y+2*x*y+7*y^2+2*w*z-7*x*z-8*y*z+7*z^2+3*w-7*x-7*y-8*z+8));
transpose gens I

R = ZZ[w,x,y,z,t,MonomialOrder => Lex]
time betti gb (I=homogenize(ideal (
        -2*w^2+9*w*x+8*x^2+9*w*y+9*x*y+6*y^2-7*w*z-3*x*z-7*y*z-6*z^2-4*w+8*x+4*y+8*z+2,
        3*w^2-5*w*x+4*x^2-3*w*y+2*x*y+9*y^2-6*w*z-2*x*z+6*y*z+7*z^2+9*w+7*x+5*y+7*z+5,
        7*w^2+5*w*x+2*x^2+3*w*y+9*x*y-4*y^2-5*w*z-7*x*z-5*y*z-4*z^2-5*w+4*x+6*y-9*z+2,
        8*w^2+5*w*x+5*x^2-4*w*y+2*x*y+7*y^2+2*w*z-7*x*z-8*y*z+7*z^2+3*w-7*x-7*y-8*z+8), t));
///
-- Let's try to compute this degree by degree
S = ZZ[w,x,y,z,t,MonomialOrder => Lex]
I = homogenize(ideal (
        -2*w^2+9*w*x+8*x^2+9*w*y+9*x*y+6*y^2-7*w*z-3*x*z-7*y*z-6*z^2-4*w+8*x+4*y+8*z+2,
        3*w^2-5*w*x+4*x^2-3*w*y+2*x*y+9*y^2-6*w*z-2*x*z+6*y*z+7*z^2+9*w+7*x+5*y+7*z+5,
        7*w^2+5*w*x+2*x^2+3*w*y+9*x*y-4*y^2-5*w*z-7*x*z-5*y*z-4*z^2-5*w+4*x+6*y-9*z+2,
        8*w^2+5*w*x+5*x^2-4*w*y+2*x*y+7*y^2+2*w*z-7*x*z-8*y*z+7*z^2+3*w-7*x-7*y-8*z+8), t)

S0 = ideal (1_S)
S1 = ideal vars S
S2 = ideal basis(2,S)
S3 = ideal basis(3,S)
S4 = ideal basis(4,S)
S5 = ideal basis(5,S)
S6 = ideal basis(6,S)
getleads = (Sd) -> (
     (m,c) = coefficients gens (Sd * I);
     M = transpose substitute(c,ZZ);
     M = transpose matrix apply(entries M,reverse);
     m = matrix { reverse flatten entries m };
     L := flatten entries(m * (leadTerm gens gb M));
     apply(L, m -> if leadCoefficient m < 0 then -m else m))

getGB = (Sd,Gmin,) -> (
     (m,c) = coefficients gens (Sd * I);
     M = transpose substitute(c,ZZ);
     M = transpose matrix apply(entries M,reverse);
     m = matrix { reverse flatten entries m };
     L := flatten entries(m * (leadTerm gens gb M));
     apply(L, m -> if leadCoefficient m < 0 then -m else m))

G2 = getleads ideal(1_S)
H2 = G2

G3 = getleads S1
H3 = set G3 - (set{x,y,z} ** set G2)/times

G4 = getleads S2
H4 = set G4 - (set{x,y,z} ** set G3)/times


///
end
-- Local Variables:
-- compile-command: "make -C $M2BUILDDIR/Macaulay2/test ISSAC-97.out"
-- End:
