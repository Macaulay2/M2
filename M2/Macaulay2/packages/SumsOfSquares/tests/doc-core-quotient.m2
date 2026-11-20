-- doc-core-quotient.m2 -- independent tests written from the documentation
-- (node "sum-of-squares problem in a quotient ring", SumsOfSquares node,
-- [P05] Example 5, [JSAG] sec. 1).
-- Self-contained; assumes SumsOfSquares is loaded.
setRandomSeed 0;  -- fixed seed for anything random in the solver path

isPSDQQ = Q -> (
    n := numrows Q;
    if n != numcols Q or Q != transpose Q then return false;
    A := mutableMatrix Q;
    for k from 0 to n-1 do (
        p := A_(k,k);
        if p < 0 then return false;
        if p == 0 then (
            for j from k+1 to n-1 do if A_(k,j) != 0 then return false;
            ) else (
            for i from k+1 to n-1 do for j from k+1 to n-1 do
                A_(i,j) = A_(i,j) - A_(i,k)*A_(k,j)/p;
            );
        );
    true)
okStatus = "SDP solved, primal-dual feasible";
-- exact certificate in the quotient ring: equality is taken modulo the ideal
exactCert = (f, sol) -> (
    Q := sol#GramMatrix; mon := sol#Monomials;
    Q =!= null and mon =!= null and ring Q === QQ and isPSDQQ Q
      and (transpose(mon)*Q*mon - f == 0)
      and (s := sosPoly sol; instance(s, SOSPoly) and value s == f
           and all(coefficients s, c -> c > 0)));

-------------------------------------------------------------------------
-- [P05] Example 5: f = 10 - x^2 - y is nonnegative on the unit circle
R = QQ[x,y]/ideal(x^2 + y^2 - 1);
f = 10-x^2-y;
mon = matrix {{1}, {x}, {y}};
sol = solveSOS (f, mon);
assert instance(sol, SDPResult);
assert(sol#Status == okStatus);
assert exactCert(f, sol);
-- the sum of squares lives in the quotient ring
s = sosPoly sol;
assert(ring s === R);
assert(value s == f);
-- equality modulo the ideal, checked in the polynomial ring
P = ambient R;
I = ideal R;
assert((lift(value s, P) - lift(f, P)) % I == 0);

-- degree bound D: monomials of degree at most D/2
for D in {2, 4, 6} do (
    sl := solveSOS (f, D);
    assert(sl#Status == okStatus);
    assert exactCert(f, sl);
    assert all(flatten entries sl#Monomials, m -> first degree lift(m, P) <= D//2);
    );

-- SumsOfSquares node: "The previous computation gives decomposition with
-- three sums of squares" (solveSOS(f,2))
-- (at most three: the number of squares may change, LOG.md 1.7 decision 1;
-- lead, after the test audit)
assert(length sosPoly solveSOS (f, 2) <= 3);

-- TraceObj: [P05] obtains the two-square decomposition
-- 10 - x^2 - y = (3 - y/6)^2 + 35/36 y^2  mod ideal
sT = sosPoly solveSOS (f, 2, TraceObj=>true);
assert(length sT == 2);
assert(value sT == f);
assert all(coefficients sT, c -> c > 0);
assert((3 - y/6)^2 + 35/36*y^2 == f);

-------------------------------------------------------------------------
-- parametrized problem in the quotient ring (doc example):
-- max t s.t. f - t is SOS mod the ideal; the minimum of f on the circle is
-- 9 + y^2 - y at y = 1/2, i.e. 35/4
S = R[t];
for sl in {solveSOS(f-t, -t, mon, RoundTol=>12), solveSOS(f-t, -t, 2, RoundTol=>12),
           solveSOS(f-t, -t, mon), solveSOS(f-t, -t, 2)} do (
    assert(sl#Status == okStatus);
    tv := (sl#Parameters)_(0,0);
    assert(ring tv === QQ);
    fs := lift(sub(f - t, {t => tv}), R);
    assert exactCert(fs, sl);
    -- an exact certificate proves tv <= 35/4
    assert(tv <= 35/4);
    assert(tv > 35/4 - 1/1000);
    -- moment matrix: the minimizers are (+-sqrt(3)/2, 1/2); the moments of
    -- 1, x, y are [[1,0,1/2],[0,3/4,0],[1/2,0,1/4]] up to the order of mon
    MM := sl#MomentMatrix;
    assert(MM =!= null and numrows MM == 3);
    );
sl = solveSOS(f-t, -t, mon, RoundTol=>12);
assert(flatten entries sl#Monomials == {1_R, x, y});
-- the optimal moment matrices form a segment (mixtures of the two minimizers),
-- so only the moments shared by all of them are checked: X_11 = 1,
-- X_xx + X_yy = 1, X_1y = 1/2, X_yy = 1/4 (lead, after the test audit; the
-- original lands on the symmetric mixture with X_1x = 0)
MM = sl#MomentMatrix;
assert(abs(MM_(0,0) - 1) < 1e-4 and abs(MM_(1,1) + MM_(2,2) - 1) < 1e-4)
assert(abs(MM_(0,2) - .5) < 1e-4 and abs(MM_(2,2) - .25) < 1e-4)
assert(min eigenvalues(MM, Hermitian => true) > -1e-6)

-------------------------------------------------------------------------
-- Caveat: without rounding the result is real and the SOSPoly lives in a
-- newly created ring which is not a quotient ring (doc)
use R;
s5 = solveSOS (f, mon, RoundTol=>infinity);
assert(s5#GramMatrix =!= null);
assert(ring s5#GramMatrix =!= QQ);
R' = ring sosPoly s5;
assert(not instance(R', QuotientRing));
assert instance(R', PolynomialRing);
assert(instance(coefficientRing R', RealField));
-- constructing the quotient again: the decomposition agrees with f up to
-- rounding errors
S' = R'/(sub (ideal (x^2 + y^2 - 1), R'));
-- (clean does not act on quotient ring elements: lift first)
assert(clean(1e-6, lift(sub (f, S') - sub (value sosPoly s5, S'), R')) == 0);

-------------------------------------------------------------------------
-- a quotient by an ideal with several generators: the Boolean square
-- {0,1}^2; f = 1 + x - y >= 0 there, and 1 + x - y = (1-y)^2 + x^2 mod I
B = QQ[symbol x, symbol y]/ideal(x^2 - x, y^2 - y);
fb = 1 + x - y;
sb = solveSOS(fb, 2);
assert(sb#Status == okStatus);
assert exactCert(fb, sb);
assert((1-y)^2 + x^2 == fb);
-- min of x - y on {0,1}^2 is -1
SB = B[t];
sl = solveSOS(x - y - t, -t, 2);
tv = (sl#Parameters)_(0,0);
assert(ring tv === QQ);
assert(tv <= -1 and tv > -1 - 1/1000);
assert exactCert(lift(sub(x - y - t, {t => tv}), B), sl);
-- x*y takes the values 0 and 1 on {0,1}^2, and x*y = (x*y)^2 mod I
assert exactCert(x*y, solveSOS(x*y, 4));
-- x - y is negative at (0,1): not SOS mod I
sn = solveSOS(x - y, 2);
assert(not (sn#GramMatrix =!= null and ring sn#GramMatrix === QQ and isPSDQQ sn#GramMatrix
            and transpose(sn#Monomials)*sn#GramMatrix*sn#Monomials - (x - y) == 0));

-------------------------------------------------------------------------
-- sphere intersected with a plane (two generators): 1 - x >= 0 there and
-- 1 - x = ((1-x)^2 + y^2 + z^2)/2 modulo x^2+y^2+z^2-1
Sp = QQ[symbol x, symbol y, symbol z]/ideal(x^2+y^2+z^2-1, x+y+z);
g = 1 - x;
sg = solveSOS(g, 2);
assert(sg#Status == okStatus);
assert exactCert(g, sg);
-- on the sphere alone
Sph = QQ[symbol x, symbol y, symbol z]/ideal(x^2+y^2+z^2-1);
assert exactCert(1 - x, solveSOS(1 - x, 2));
assert exactCert(3 - x - y - z, solveSOS(3 - x - y - z, 2));
assert(((1-x)^2 + y^2 + z^2)/2 == 1 - x);
