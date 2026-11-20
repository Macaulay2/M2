-- Tests written from the documentation alone (independent tester, applications).
-- Topic: sosInIdeal(h,D) and sosInIdeal(S,D) (doc node sosInIdeal; [JSAG]
-- section 2: "if f = sum lambda_i f_i^2 is in I then each f_i lies in the
-- real radical of I"; [BPT13] Definition A.51).
-- Checked: h*mult == sosPoly sol exactly over QQ, mult a column vector,
-- positive weights, a nonzero SOS; in the quotient ring sosPoly sol is zero
-- but not all its generators are; every generator lies in the real radical
-- (membership in a known real radical ideal, and vanishing at the real
-- points); exact characterisations of all SOS of low degree in the ideal;
-- no nonzero SOS returned where none exists.  setRandomSeed before each call.

siPos = s -> all(coefficients s, c -> c > 0);
siInIdeal = (gs, J) -> all(gs, g -> g % J == 0);
siEval = (g, P) -> (
    -- value of the polynomial g at the real point P (list of numbers)
    v := sub(g, apply(#P, i -> (ring g)_i => 1.0 * P#i));
    if instance(v, RingElement) then lift(v, RR) else v);
siCheckH = (h, D) -> (
    -- runs sosInIdeal(h,D) and checks the documented identity and shapes
    setRandomSeed 0;
    r := sosInIdeal(h, D);
    assert(instance(r, Sequence) and #r == 2);
    (sol, mult) := r;
    assert(instance(sol, SDPResult));
    assert(instance(mult, Matrix) and numRows mult == numColumns h and numColumns mult == 1);
    s := sosPoly sol;
    assert(h*mult == s);
    assert(value s != 0);
    assert(siPos s);
    s);

-- doc / [JSAG] section 2 example ----------------------------------------------
R = QQ[x,y,z];
h = matrix {{x^2 - 4*x + 2*y^2, 2*z^2 - y^2 + 2}};
s = siCheckH(h, 2);
-- The SOS of degree <= 2 in I are exactly the c*((x/2-1)^2 + z^2), c > 0:
-- a h1 + b h2 has Gram matrix (in 1,x,y,z) with entries forcing b = 2a.
w = (x/2 - 1)^2 + z^2;
c = leadCoefficient(value s) / leadCoefficient(w);
assert(c > 0 and value s == c * w);
-- real radical of I: the real points are (2, +-sqrt 2, 0), so it is
-- J = (x - 2, z, y^2 - 2) ([JSAG]); every generator lies in J
J = ideal(x - 2, z, y^2 - 2);
assert(siInIdeal(gens s, J));
-- and vanishes at the real points
realPts = {{2, sqrt 2, 0}, {2, -sqrt 2, 0}};
assert(all(gens s, g -> all(realPts, P -> abs siEval(g, P) < 1e-9)));
-- the real points are indeed on the variety, so J is the real radical
assert(all(realPts, P -> all(flatten entries h, g -> abs siEval(g, P) < 1e-9)));

-- the quotient ring version ("decompose the zero of the quotient ring")
use R;
S = R/ideal h;
setRandomSeed 0;
solS = sosInIdeal(S, 2);
assert(instance(solS, SDPResult));
sS = sosPoly solS;
assert(ring value sS === S);
assert(value sS == 0);                                    -- zero in S
assert(any(gens sS, g -> g != 0));                        -- but not trivially
assert(siPos sS);
-- lifted to R: in the ideal, a positive multiple of (x/2-1)^2 + z^2, and the
-- generators lie in the real radical
use R;
liftS = sum(#gens sS, i -> (coefficients sS)#i * (lift((gens sS)#i, R))^2);
assert(liftS % ideal h == 0);
cS = leadCoefficient(liftS) / leadCoefficient(w);
assert(cS > 0 and liftS == cS * w);
assert(siInIdeal(apply(gens sS, g -> lift(g, R)), J));

-- more ideals ----------------------------------------------------------------
R = QQ[x,y];
-- x^2 + y^2: real radical (x, y)
s = siCheckH(matrix{{x^2 + y^2}}, 2);
assert(siInIdeal(gens s, ideal(x, y)));
-- a generator of degree 1 (the trivial case of [JSAG] section 2): (x-1)^2
s = siCheckH(matrix{{x - 1}}, 2);
assert(siInIdeal(gens s, ideal(x - 1)));
-- empty real variety: x^2 + y^2 + 1 (real radical = whole ring); any
-- nonzero SOS in the ideal is fine
s = siCheckH(matrix{{x^2 + y^2 + 1}}, 2);
-- the circle: I = (x^2+y^2-1) is real radical.  In degree <= 2 the ideal
-- contains no nonzero SOS (c(x^2+y^2-1) takes both signs), so no nonzero
-- SOS may be returned
h = matrix{{x^2 + y^2 - 1}};
setRandomSeed 0;
(sol, mult) = sosInIdeal(h, 2);
assert(sol#GramMatrix === null or value sosPoly sol == 0);
-- in degree 4 there is (x^2+y^2-1)^2
s = siCheckH(h, 4);
assert(siInIdeal(gens s, ideal h));
-- in the quotient ring by a real radical ideal, every SOS representation of
-- zero is trivial (all generators zero in the quotient)
S = QQ[x,y]/ideal(x^2 + y^2 - 1);
for D in {2, 4} do (
    setRandomSeed 0;
    solc := sosInIdeal(S, D);
    print("circle quotient, D = " | toString D);
    assert(solc#GramMatrix === null or all(gens sosPoly solc, g -> g == 0)));
-- the quotient by x^2 + y^2: x and y are nonzero there but x^2 + y^2 = 0
S = QQ[x,y]/ideal(x^2 + y^2);
setRandomSeed 0;
sol2 = sosInIdeal(S, 2);
s2 = sosPoly sol2;
assert(value s2 == 0 and any(gens s2, g -> g != 0) and siPos s2);
assert(siInIdeal(apply(gens s2, g -> lift(g, ambient S)), ideal((ambient S)_0, (ambient S)_1)));

-- three variables, two generators, two real points:
-- h = (x^2 + y^2 + z^2 - 2x, y^2 + z^2); real points (0,0,0), (2,0,0);
-- real radical (y, z, x^2 - 2x)
R = QQ[x,y,z];
h = matrix{{x^2 + y^2 + z^2 - 2*x, y^2 + z^2}};
s = siCheckH(h, 2);
J = ideal(y, z, x^2 - 2*x);
assert(siInIdeal(gens s, J));
assert(all(gens s, g -> sub(g, {x => 0, y => 0, z => 0}) == 0 and sub(g, {x => 2, y => 0, z => 0}) == 0));
S = R/ideal h;
setRandomSeed 0;
s3 = sosPoly sosInIdeal(S, 2);
assert(value s3 == 0 and any(gens s3, g -> g != 0) and siPos s3);
use R;
assert(siInIdeal(apply(gens s3, g -> lift(g, R)), J));
-- a quotient ring with two generators and empty real variety:
-- (x^2 + y^2, yz - 1) has no real points
S = QQ[x,y,z]/ideal(x^2 + y^2, y*z - 1);
setRandomSeed 0;
s4 = sosPoly sosInIdeal(S, 2);
assert(value s4 == 0 and any(gens s4, g -> g != 0) and siPos s4);

-- results do not depend on the seed in the sense that the documented
-- properties hold for other seeds as well
R = QQ[x,y,z];
h = matrix {{x^2 - 4*x + 2*y^2, 2*z^2 - y^2 + 2}};
w = (x/2 - 1)^2 + z^2;
for sd in {1, 7, 12345} do (
    setRandomSeed sd;
    (solr, multr) = sosInIdeal(h, 2);
    print("doc example, seed " | toString sd);
    sr := sosPoly solr;
    assert(h*multr == sr and siPos sr);
    assert(value sr == (leadCoefficient(value sr)/leadCoefficient w) * w));
