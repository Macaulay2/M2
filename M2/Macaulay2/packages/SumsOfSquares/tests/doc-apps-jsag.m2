-- Tests written from the documentation alone (independent tester, applications).
-- Topic: the examples of [JSAG] (Cifuentes, Kahle, Parrilo, Sums of squares
-- in Macaulay2, JSAG 10 (2020)) in the area of the applications: section 2
-- (sosInIdeal, real radical), section 3 (sosdecTernary of the Motzkin
-- form), section 4 (lowerBound of the dehomogenized Motzkin polynomial),
-- section 5 (lowerBound on a variety, both ways, and recoverSolution).
-- The checks the paper itself performs are repeated (o5 = true, o6 = true),
-- plus the mathematical claims made in the text.  Printed fractions are not
-- pinned here (see doc-apps-printed-*.m2).

-- section 2 ------------------------------------------------------------------
R = QQ[x,y,z]; d = 1;
h = matrix {{x^2 - 4*x + 2*y^2, 2*z^2 - y^2 + 2}};
setRandomSeed 0;
(sol, mult) = sosInIdeal(h, 2*d);
s = sosPoly sol;
assert(h * mult == s);                                     -- i5/o5 of the paper
-- "In both cases we obtained a multiple of the sum-of-squares polynomial
-- (x/2 - 1)^2 + z^2"
w = (x/2 - 1)^2 + z^2;
c = leadCoefficient(value s)/leadCoefficient(w);
assert(c > 0 and value s == c * w);
S = R/ideal h;
setRandomSeed 0;
sS = sosPoly sosInIdeal(S, 2*d);
use R;
liftS = sum(#gens sS, i -> (coefficients sS)#i * (lift((gens sS)#i, R))^2);
cS = leadCoefficient(liftS)/leadCoefficient(w);
assert(cS > 0 and liftS == cS * w);
-- "This computation reveals that x - 2 and z lie in the real radical of I.
-- Indeed, we have realRadical(I) = (x - 2, z, y^2 - 2)."
J = ideal(x - 2, z, y^2 - 2);
assert(isSubset(ideal h, J));                              -- J contains I
assert(all(gens s, g -> g % J == 0));
-- the SOS c*((x/2-1)^2 + z^2) vanishes exactly on the line {x = 2, z = 0},
-- so its generators (of degree <= 1) lie in the span of x - 2 and z, and
-- span it because the quadratic form has rank 2
assert(ideal(gens s) == ideal(x - 2, z));
-- J is radical and all its complex zeros are real (y = +-sqrt 2): real radical
assert(radical J == J and dim J == 0 and degree J == 2);

-- section 3 ------------------------------------------------------------------
R = QQ[x,y,z];
f = library("Motzkin", {x,y,z});
assert(f == x^4*y^2 + x^2*y^4 - 3*x^2*y^2*z^2 + z^6);      -- o2 of the paper
setRandomSeed 0;
(Nums, Dens) = sosdecTernary f;
num = first Nums;
den = first Dens;
assert(instance(num, SOSPoly) and instance(den, SOSPoly));
assert(f * value(den) == value(num));                      -- i6/o6 of the paper
assert(all(coefficients num | coefficients den, c -> c > 0));

-- section 4, lowerBound part -------------------------------------------------
R = QQ[x,z];
f = library("Motzkin", {x, 1_R, z});
(t, sol) = lowerBound(f, RoundTol => 12);
assert(instance(t, QQ));
assert(f - t == sosPoly sol and all(coefficients sosPoly sol, c -> c > 0));
assert(t <= 0);                                            -- f(1,1) = 0
assert(t >= -729/4096 - 1e-6);                             -- o4 of the paper: -729/4096
-- the parametric formulation of the same section
Rt = QQ[x,z][tt];
ft = library("Motzkin", {x, 1_Rt, z});
solt = solveSOS(ft - tt, -tt, RoundTol => 12);
assert(abs((solt#Parameters)_(0,0) - t) < 1e-6);

-- section 5 ------------------------------------------------------------------
-- quotient ring
R = QQ[x,y]/ideal(x^2 - x, y^2 - y);
f = x - y; d = 1;
(t, sol) = lowerBound(f, 2*d);
assert(t == -1);                                           -- o4
assert(f - t == sosPoly sol);                              -- o5
-- equations
R = QQ[x,y];
f = x - y; d = 1;
h = matrix{{x^2 - x, y^2 - y}};
(t, sol, mult) = lowerBound(f, h, 2*d);
assert(t == -1);                                           -- o5
assert(f - t + h*mult == sosPoly sol);                     -- o6
-- recoverSolution: o7 = {x => 1.77345e-9, y => 1}
pt = recoverSolution sol;
assert(#pt == 2);
xv = (select(pt, o -> toString o#0 == "x"))#0#1;
yv = (select(pt, o -> toString o#0 == "y"))#0#1;
assert(abs xv < 1e-4 and abs(yv - 1) < 1e-4);
