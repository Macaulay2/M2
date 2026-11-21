-- Tests written from the documentation alone (independent tester, applications).
-- Topic: lowerBound on varieties: lowerBound(f,D) in a quotient ring and
-- lowerBound(f,h,D) with equations h (doc node lowerBound, "Quotient rings"
-- and "Avoiding quotient rings"; [JSAG] section 5; [BPT13] section 3.3.5;
-- [P05] Example 5).
-- Checked: the documented identities f - bound == sosPoly sol (in the
-- quotient) and f - bound + h*mult == sosPoly sol, positive weights, bounds
-- below the true minimum over the real variety and close to the relaxation
-- value, monotonicity in D, the h-relaxation is not stronger than the
-- quotient one ("alternative (though weaker) relaxation"), shapes.

conCertQ = (f, b, sol) -> (
    instance(b, QQ) and sol#GramMatrix =!= null and (
        s := sosPoly sol;
        f - b == s and all(coefficients s, c -> c > 0)));
conCertH = (f, h, r) -> (
    (b, sol, mult) := r;
    instance(b, QQ) and sol#GramMatrix =!= null and instance(mult, Matrix)
    and numRows mult == numColumns h and numColumns mult == 1 and (
        s := sosPoly sol;
        f - b + h*mult == s and all(coefficients s, c -> c > 0)));
conVal = (pt, name) -> (
    sel := select(pt, o -> toString o#0 == name);
    assert(#sel == 1);
    sel#0#1);

-- doc example (quotient ring): f = x - y on {0,1}^2, minimum -1 at (0,1) ----
S = QQ[x,y]/ideal(x^2 - x, y^2 - y);
f = x - y;
r = lowerBound(f, 2);
assert(instance(r, Sequence) and #r == 2);
(bound, sol) = r;
assert(instance(sol, SDPResult));
assert(f - bound == sosPoly sol);                          -- documented identity
assert(conCertQ(f, bound, sol));
assert(bound == -1);                                       -- tight, rational
pt = recoverSolution sol;
assert(abs(conVal(pt, "x") - 0) < 1e-3 and abs(conVal(pt, "y") - 1) < 1e-3);

-- doc example (equations): same problem with h = (x^2-x, y^2-y) --------------
R = QQ[x,y];
f = x - y;
h = matrix{{x^2 - x, y^2 - y}};
r = lowerBound(f, h, 2);
assert(instance(r, Sequence) and #r == 3);
(bound, sol, mult) = r;
assert(instance(sol, SDPResult));
assert(f - bound + h*mult == sosPoly sol);                 -- documented identity
assert(conCertH(f, h, r));
assert(bound == -1);
pt = recoverSolution sol;                                  -- [JSAG] section 5: {x => 0, y => 1}
assert(abs(conVal(pt, "x") - 0) < 1e-3 and abs(conVal(pt, "y") - 1) < 1e-3);

-- [P05] Example 5 / [BPT13] Example 3.99: 10 - x^2 - y on the unit circle.
-- On the circle f = 9 + y^2 - y, minimum 35/4 at y = 1/2; already degree 2
-- gives 35/4 since f - 35/4 = (y - 1/2)^2 modulo the circle.
S = QQ[x,y]/ideal(x^2 + y^2 - 1);
f = 10 - x^2 - y;
for D in {2, 4, 6} do (
    (b, s) = lowerBound(f, D);
    print("circle in quotient ring, D = " | toString D);
    assert(conCertQ(f, b, s));
    assert(b <= 35/4 and b >= 35/4 - 1e-2));
R = QQ[x,y];
f = 10 - x^2 - y;
hc = matrix{{x^2 + y^2 - 1}};
for D in {2, 4} do (
    rr = lowerBound(f, hc, D);
    print("circle with equation, D = " | toString D);
    assert(conCertH(f, hc, rr));
    assert(rr#0 <= 35/4 and rr#0 >= 35/4 - 1e-2));
-- RR coefficients
S = RR[x,y]/ideal(x^2 + y^2 - 1);
(bRR, sRR) = lowerBound(10 - x^2 - y, 2);
assert(instance(bRR, RR) and abs(bRR - 35/4) < 1e-4);

-- quotient ring with three generators: f = xy + yz + xz on {-1,1}^3.
-- The minimum is -1; degree 2 gives the first relaxation, value -3/2
-- (f + 3/2 = (x+y+z)^2/2 modulo the ideal, and the MAXCUT SDP of a triangle
-- has value -3/2); degree 4 is exact for three variables.
S = QQ[x,y,z]/ideal(x^2 - 1, y^2 - 1, z^2 - 1);
f = x*y + y*z + x*z;
(bq2, sq2) = lowerBound(f, 2);
assert(conCertQ(f, bq2, sq2));
assert(bq2 <= -3/2 + 1e-6 and bq2 >= -3/2 - 1e-2);
(bq4, sq4) = lowerBound(f, 4);
assert(conCertQ(f, bq4, sq4));
assert(bq4 <= -1 and bq4 >= -1 - 1e-2);
assert(bq4 >= bq2);                                        -- higher degree, better bound
-- the same with equations
R = QQ[x,y,z];
f = x*y + y*z + x*z;
h3 = matrix{{x^2 - 1, y^2 - 1, z^2 - 1}};
rh2 = lowerBound(f, h3, 2);
assert(conCertH(f, h3, rh2));
assert(rh2#0 <= -3/2 + 1e-6 and rh2#0 >= -3/2 - 1e-2);
rh4 = lowerBound(f, h3, 4);
assert(conCertH(f, h3, rh4));
assert(rh4#0 <= -1 and rh4#0 >= -1 - 1e-2);
-- the relaxation with multipliers is not stronger than the quotient one
-- (compared without rounding: two rounded bounds of equal relaxations may
-- land on different grid points; lead, after the test audit)
bq2inf = first lowerBound(sub(f, S), 2, RoundTol => infinity);
bq4inf = first lowerBound(sub(f, S), 4, RoundTol => infinity);
assert(first lowerBound(f, h3, 2, RoundTol => infinity) <= bq2inf + 1e-6);
assert(first lowerBound(f, h3, 4, RoundTol => infinity) <= bq4inf + 1e-6);
-- every minimizer of the variety gives f >= bound: check at all 8 points
pts8 = flatten flatten apply({-1,1}, a -> apply({-1,1}, b -> apply({-1,1}, c -> {a,b,c})));
assert(all(pts8, v -> sub(f, {x => v#0, y => v#1, z => v#2}) >= rh4#0));
assert(min apply(pts8, v -> sub(f, {x => v#0, y => v#1, z => v#2})) == -1);

-- a polynomial with constraints in more variables, several equations:
-- minimize x + y + z on {x^2 = 1, y^2 = 4, z = 3}: minimum -1 - 2 + 3 = 0
R = QQ[x,y,z];
hm = matrix{{x^2 - 1, y^2 - 4, z - 3}};
rm = lowerBound(x + y + z, hm, 2);
assert(conCertH(x + y + z, hm, rm));
assert(rm#0 <= 0 and rm#0 >= -1e-2);
S = QQ[x,y,z]/ideal(x^2 - 1, y^2 - 4, z - 3);
(bm, sm) = lowerBound(x + y + z, 2);
assert(conCertQ(x + y + z, bm, sm));
assert(bm <= 0 and bm >= -1e-2);

-- RR coefficients with equations: the documented identity holds numerically
R = RR[x,y];
f = x - y;
h = matrix{{x^2 - x, y^2 - y}};
(bR, solR, multR) = lowerBound(f, h, 2);
assert(instance(bR, RR) and abs(bR + 1) < 1e-4);
dR = value sosPoly solR - sub((f - bR + h*multR)_(0,0), ring value sosPoly solR);
assert(max(apply(terms dR, t -> abs leadCoefficient t) | {0}) < 1e-6);
