-- doc-solve.m2
-- binomialSolve.  Documented: for a zero-dimensional unital binomial ideal,
-- "the list of points in the zero locus of I", with coordinates in a
-- cyclotomic field containing the entire variety.
-- Oracles: every returned point is a list of numgens R field elements and
-- satisfies all generators of I; the number of DISTINCT returned points equals
-- dim_QQ R/radical(I), the number of points of V(I) over the algebraic closure
-- (QQ is perfect, so radical I stays radical after extension); hence the
-- returned points cover V(I).
-- Multiplicities/duplicates and the shape of the output when the origin is a
-- solution are tested in doc-FINDING-solve-*.m2.

-- number of points of V(I) over the algebraic closure: dim_QQ of R/rad(I) (independent of the grading)
nPoints = I -> (J := radical I; numcols basis((ring J)/J));
satisfies = (I, p) -> (K := ring first p; f := map(K, ring I, p); all(flatten entries gens I, g -> f g == 0));
checkSolve = I -> (
    assert(dim I <= 0);
    sols := binomialSolve I;
    assert(instance(sols, List));
    if I == 1 then (assert(#sols == 0); return);
    assert(all(sols, p -> instance(p, List) and #p == numgens ring I));
    K := ring first first sols;
    assert(all(sols, p -> all(p, c -> ring c === K)));
    assert(all(sols, p -> satisfies(I, p)));
    assert(#unique sols == nPoints I);
    );

R = QQ[x,y,z,w];
checkSolve ideal(x-y, y-z, z*w-w, w^2-x);
checkSolve ideal(x^3-1, y-x, z-1, w-1);
checkSolve ideal(x^2-1, y^2-1, z^2-1, w^2-1);
checkSolve ideal(x^2-1, y^2-1, z^2-1, x*y*z-1, w-x);
checkSolve ideal(x^5-1, y-x^2, z-x^3, w-x^4);
checkSolve ideal(x*y-1, x^2-y, z^2, w-1);
checkSolve ideal(x^2, y^2-1, z-1, w-y);
checkSolve ideal(x^3-y^2, x^2-1, z-1, w^2-x);
checkSolve ideal(x-1, y-1, z-1, w-1);
checkSolve ideal(1_R);
R = QQ[x,y];
checkSolve ideal(x^2-y, y^2-x);
checkSolve ideal(x^6-1, y-x^2);
checkSolve ideal(x^12-1, y^2-x^3);
checkSolve ideal(x^4-1, y^3-1);
checkSolve ideal(x^2, y^2-1);
checkSolve ideal(x^3-1, y^2);
checkSolve ideal(x^8-1, x*y-1);
-- explicit counts
assert(#unique binomialSolve ideal(x^4-1, y^3-1) == 12);
assert(#unique binomialSolve ideal(x^12-1, y^2-x^3) == 24);
-- rational points come out rational
sols = binomialSolve ideal(x^2-1, y^2-1);
assert(set apply(sols, p -> apply(p, c -> toString c)) === set {{"1","1"},{"1","-1"},{"-1","1"},{"-1","-1"}});
-- one variable
R = QQ[t];
for n in {1,2,3,4,5,6,8,12} do (
    sols := binomialSolve ideal(t^n - 1);
    assert(#unique sols == n);
    assert(all(sols, p -> #p == 1 and (p#0)^n == 1)));
-- other term orders / gradings
for R in {QQ[x,y, MonomialOrder=>Lex], QQ[x,y, Degrees=>{2,3}], QQ[x,y, Weights=>{5,1}]} do (
    x := R_0; y := R_1;
    checkSolve ideal(x^3-1, y^2-x);
    checkSolve ideal(x^4-y^2, y^4-1));
