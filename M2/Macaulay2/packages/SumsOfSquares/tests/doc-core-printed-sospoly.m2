-- doc-core-printed-sospoly.m2 -- output fidelity: the printed values shown in
-- the documentation of SOSPoly, sosPoly, value(SOSPoly), clean(RR,SOSPoly)
-- (example outputs of the Macaulay2 1.26.06 build).  These may legitimately
-- change in a renewed version (printed forms are not part of the semantics).
-- Self-contained; assumes SumsOfSquares is loaded.
setRandomSeed 0;  -- fixed seed for anything random in the solver path

R = QQ[x,y];
s = sosPoly(R, {x+1,y}, {2,3} );
-- o6/o7 of SOSPoly and sosPoly: 2*(x + 1)^2 + 3*(y)^2
assert(unstack net s == {"         2        2", "2*(x + 1)  + 3*(y)"});
-- peek s
assert(unstack net peek s == {"SOSPoly{coefficients => {2, 3}  }",
                              "        generators => {x + 1, y}",
                              "        ring => R"});
-- gens, ring, coefficients, length
assert(toString gens s == "{x+1, y}");
assert(toString ring s == "R");
assert(toString coefficients s == "{2, 3}");
assert(length s == 2);
-- 2 * s
assert(coefficients(2*s) == {4, 6});
assert(gens(2*s) == {x+1, y});
-- s + s: the four squares of both summands
assert(coefficients(s+s) == {2, 3, 2, 3});
assert(gens(s+s) == {x+1, y, x+1, y});
-- s * s: products of the generators
assert(coefficients(s*s) == {4, 6, 6, 9});
assert(gens(s*s) == {x^2+2*x+1, x*y+y, x*y+y, y^2});
assert(unstack net (s*s) == {"    2          2              2              2       2 2",
                             "4*(x  + 2x + 1)  + 6*(x*y + y)  + 6*(x*y + y)  + 9*(y )"});
-- s == s
assert((s == s) === true);
-- value s = 2x^2 + 3y^2 + 4x + 2
assert(toString value s == "2*x^2+3*y^2+4*x+2");
-- expression s: an Expression of class Sum
assert(class expression s === Sum);
assert(toString expression s == "2*(x+1)^2+3*(y)^2");
-- sub (s, S)
S = QQ[x,y,z];
assert(unstack net sub(s, S) == {"         2        2", "2*(x + 1)  + 3*(y)"});

-- sosPoly(mon,Q) with Q all ones: (1)(x + y + 1)^2
use R;
Q = matrix(QQ,{{1,1,1},{1,1,1},{1,1,1}});
mon = matrix{{1},{x},{y}};
sq = sosPoly(mon,Q);
assert(coefficients sq == {1});
assert(gens sq == {x + y + 1});
assert(unstack net sq == {"              2", "(1)(x + y + 1)"});

-- clean(RR,SOSPoly): 2*(x^2 + .0001x + 1)^2 + .0001*(y)^2 -> 2*(x^2 + 1)^2
T = RR[x,y];
cs = sosPoly(T, {x^2+.0001*x+1, y}, {2, .0001});
assert(unstack net cs == {"    2              2            2", "2*(x  + .0001x + 1)  + .0001*(y)"});
cc = clean( .001, cs );
assert(unstack net cc == {"    2     2", "2*(x  + 1)"});
assert(gens cc == {x^2 + 1});
assert(coefficients cc == {2});
