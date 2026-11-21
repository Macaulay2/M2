-- doc-input-functions.m2
-- makeBinomial (R, m, c) and latticeBasisIdeal (R, L).
-- Convention taken from the documented example makeBinomial(R,[1,-1,-2],10)
-- (which is x - 10*y*z^2): the binomial is x^(m+) - c*x^(m-).
-- latticeBasisIdeal is documented as the unital lattice basis ideal: one
-- binomial x^(l+) - x^(l-) per column l of L.  Oracle for lattice ideals: the
-- saturation of the lattice basis ideal at the product of all variables is the
-- lattice ideal; for a saturated lattice this is the toric ideal, computed
-- independently as a kernel of a monomial map.

oracleBinomial = (R, m, c) -> (
    m = toList m;
    product(#m, i -> R_i^(max(m#i,0))) - c * product(#m, i -> R_i^(max(-m#i,0))));

R = QQ[x,y,z];
assert(makeBinomial(R, [1,-1,-2], 10) == x - 10*y*z^2);
assert(makeBinomial(R, [2,1,0], 1/3) == x^2*y - 1/3);
assert(makeBinomial(R, [-2,-1,0], 1) == 1 - x^2*y);
assert(makeBinomial(R, [3,0,-1], -1) == x^3 + z);
assert(makeBinomial(R, [0,0,0], 10) == 1 - 10);
assert(ring makeBinomial(R, [1,-1,-2], 10) === R);
setRandomSeed 4711;
for i from 1 to 30 do (
    m := apply(3, j -> random(-4, 4));
    c := random(-5, 5) / random(1, 4);
    assert(makeBinomial(R, new Array from m, c) == oracleBinomial(R, m, c)));

-- coefficient ring other than QQ: GF(101) and a cyclotomic-like number field
F = GF 101; S = F[a,b];
assert(makeBinomial(S, [1,-2], 3_F) == a - 3*b^2);
K = toField(QQ[t]/(t^2+t+1)); T = K[u,v];
assert(makeBinomial(T, [2,-1], t) == u^2 - t*v);

-- latticeBasisIdeal
assert(latticeBasisIdeal(R, matrix {{1,1},{-3,0},{0,1}}) == ideal(x - y^3, x*z - 1));
assert(latticeBasisIdeal(R, matrix{{1},{-1},{0}}) == ideal(x - y));
assert(latticeBasisIdeal(R, matrix{{2,0},{-2,1},{0,-1}}) == ideal(x^2-y^2, y-z));
for L in {matrix{{1,0},{-2,1},{1,-2}}, matrix{{3},{-1},{-2}}, matrix{{1,2},{2,-1},{-3,-1}}} do (
    lbi := latticeBasisIdeal(R, L);
    assert(isBinomial lbi and isUnital lbi);
    assert(lbi == ideal apply(numcols L, i -> oracleBinomial(R, flatten entries L_{i}, 1))));

-- twisted cubic: saturation of the lattice basis ideal is the toric ideal
S = QQ[a,b,c,d];
L = matrix{{1,0},{-2,1},{1,-2},{0,1}};
lbi = latticeBasisIdeal(S, L);
assert(lbi == ideal(a*c-b^2, b*d-c^2));
toric = ker map(QQ[s,t], S, {s^3, s^2*t, s*t^2, t^3});
assert(saturate(lbi, a*b*c*d) == toric);
assert(lbi != toric); -- the lattice basis ideal misses a*d-b*c
