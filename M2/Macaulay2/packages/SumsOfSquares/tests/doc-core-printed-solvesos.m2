-- doc-core-printed-solvesos.m2 -- output fidelity: the exact values shown in
-- the documentation (nodes SumsOfSquares, solveSOS, SDPResult, solveSOS in a
-- quotient ring, TraceObj; Macaulay2 1.26.06 example outputs) and in [JSAG]
-- sections 1 and 4.  These are particular solutions picked by the numerical
-- solver and the rounding; they may legitimately change in a renewed
-- version (the mathematical properties are tested elsewhere).
-- Self-contained; assumes SumsOfSquares is loaded.
setRandomSeed 0;  -- fixed seed for anything random in the solver path

-------------------------------------------------------------------------
-- SumsOfSquares node, solveSOS node, [JSAG] sec. 1
R = QQ[x,y];
f = 2*x^4+5*y^4-2*x^2*y^2+2*x^3*y;
sol = solveSOS f;
-- (lead, step 5: the documented Gram matrix | 2 1 -83/40 ; 1 43/20 0 ; -83/40 0 5 |
-- and its decomposition came from the rounding of the original; since the
-- rounding in image coordinates, LOG.md 5.1 decision 3, the printed fractions
-- change (decision 1 of 1.7); pinned now: an exact Gram matrix of f)
assert(ring sol#GramMatrix === QQ and (transpose sol#Monomials * sol#GramMatrix * sol#Monomials)_(0,0) == f);
assert(sol#Monomials == matrix{{x^2}, {x*y}, {y^2}});
s = sosPoly sol;
-- documented: (5)(-83/200 x^2 + y^2)^2 + (43/20)(20/43 x^2 + xy)^2 + (231773/344000)(x^2)^2
-- (lead, step 5: as above; pinned now: three squares with positive weights)
assert(#coefficients s == 3 and all(coefficients s, c -> c > 0));
assert(toString value s == "2*x^4+2*x^3*y-2*x^2*y^2+5*y^4");

-- SumsOfSquares node and TraceObj node: the circle example
Rq = QQ[x,y]/ideal(x^2 + y^2 - 1);
fq = 10-x^2-y;
s3 = sosPoly solveSOS (fq, 2);
-- (79/8)(y - 4/79)^2 + (71/8)(x)^2 + (63/632)(1)^2
assert(coefficients s3 == {79/8, 71/8, 63/632});
assert(gens s3 == {y - 4/79, x, 1_Rq});
-- TraceObj: (9)(-1/18 y + 1)^2 + (35/36)(y)^2  ([JSAG] sec. 1, o3)
sT = sosPoly solveSOS (fq, 2, TraceObj=>true);
assert(coefficients sT == {9, 35/36});
assert(gens sT == {-1/18*y + 1, y});

-------------------------------------------------------------------------
-- solveSOS node: parameters
R1 = QQ[symbol x][symbol t];
f1 = (t-1)*x^4+1/2*t*x+1;
sol1 = solveSOS f1;
assert(sol1#Parameters == matrix(QQ, {{29/8}}));
s1 = sosPoly sol1;
-- documented: (21/8)(x^2 - 43/105)^2 + (43/20)(x + 145/344)^2 + (1027351/5779200)(1)^2
-- (lead, step 5: the parameter is unchanged, the decomposition changes with
-- the rounding, LOG.md 5.1 decision 3; pinned now: three squares, positive
-- weights, and the decomposition of f1 at t = 29/8)
assert(#coefficients s1 == 3 and all(coefficients s1, c -> c > 0));
assert(value s1 == lift(sub(f1, (gens ring f1)#0 => 29/8), coefficientRing ring f1));
-- Robinson with two parameters: | 125/4 | 125/4 | in the documentation
-- (the [JSAG] paper, version 2.1, printed | 34 | 34 |)
R2 = QQ[symbol x, symbol y, symbol z][symbol s, symbol t];
g = library("Robinson", {x,y,z}) + s*x^6 + t*y^6;
sol2 = solveSOS g;
-- (lead, step 6: a feasibility problem with many solutions; in the kernel
-- form of LOG.md 6.6 the solver returns s = t = 2283/4; pinned now: rational
-- parameters for which g is exactly SOS by the returned Gram matrix)
P2 = sol2#Parameters;
assert(ring P2 === QQ and ring sol2#GramMatrix === QQ);
assert(lift(sub(g, {s => P2_(0,0), t => P2_(1,0)}), coefficientRing R2) == (transpose sol2#Monomials * sol2#GramMatrix * sol2#Monomials)_(0,0));
-- optimization: | -9/4 |
R3 = QQ[symbol x][symbol t];
f3 = x^2 - 3*x - t;
sol3 = solveSOS (f3, -t, RoundTol=>12);
assert(sol3#Parameters == matrix(QQ, {{-9/4}}));

-------------------------------------------------------------------------
-- SDPResult node
assert(unstack net sol3 == {
        "MomentMatrix   2x2 matrix over RR_53           ",
        "GramMatrix     2x2 matrix over QQ              ",
        "Monomials      2x1 matrix over QQ[x]           ",
        "Parameters     1x1 matrix over QQ              ",
        "Status         SDP solved, primal-dual feasible"});
assert(sol3#GramMatrix == matrix(QQ, {{9/4, -3/2}, {-3/2, 1}}));
assert(entries sol3#Monomials == {{1}, {x}});
assert(sol3#Status == "SDP solved, primal-dual feasible");
-- printed as matrix {{1, 1.5}, {1.5, 2.25}}; the digits depend on the solver
-- and the machine (1.50012 on an Apple M5, CSDP 6.2.0)
assert(max apply(flatten entries(sol3#MomentMatrix - matrix(RR, {{1, 1.5}, {1.5, 2.25}})), abs) < 1e-3);
-- sosPoly sol: (9/4)(-2/3 x + 1)^2
s3p = sosPoly sol3;
assert(coefficients s3p == {9/4});
assert(gens s3p == {-2/3*x + 1});

-------------------------------------------------------------------------
-- solveSOS(RingElement,Matrix) node (quotient ring)
R4 = QQ[symbol x, symbol y]/ideal(x^2 + y^2 - 1);
f4 = 10-x^2-y;
mon4 = matrix {{1}, {x}, {y}};
expected = {
        "MomentMatrix   3x3 matrix over RR_53           ",
        "GramMatrix     3x3 matrix over QQ              ",
        "Monomials      3x1 matrix over R4              ",
        "Status         SDP solved, primal-dual feasible"};
assert(unstack net solveSOS (f4, mon4) == expected);
assert(unstack net solveSOS (f4, 2) == expected);
S4 = R4[symbol t];
assert(unstack net solveSOS(f4-t,-t,mon4,RoundTol=>12) == {
        "MomentMatrix   3x3 matrix over RR_53           ",
        "GramMatrix     3x3 matrix over QQ              ",
        "Monomials      3x1 matrix over R4              ",
        "Parameters     1x1 matrix over QQ              ",
        "Status         SDP solved, primal-dual feasible"});
-- the caveat: the SOSPoly lives in RR_53[x..y]; the final comparison of the
-- documented example prints false (floating point: exact == of reals)
use R4;
s5 = solveSOS (f4, mon4, RoundTol=>infinity);
assert(unstack net ring sosPoly s5 == {"RR  [x..y]", "  53"});
R' = ring sosPoly s5;
S' = R'/(sub (ideal (x^2 + y^2 - 1), R'));
assert((sub (f4, S') == sub (sosPoly s5, S')) === false);
