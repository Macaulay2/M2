-- doc-core-printed-roundtol.m2 -- output fidelity: the values shown in the
-- documentation nodes RoundTol and "coefficient field" (Macaulay2 1.26.06
-- example outputs) and in [JSAG] sec. 4.  These may legitimately change in a
-- renewed version.  Self-contained; assumes SumsOfSquares is loaded.
setRandomSeed 0;  -- fixed seed for anything random in the solver path

-- RoundTol node (with lowerBound, as in the documentation)
R = QQ[x,z];
f = x^4+x^2+z^6-3*x^2*z^2;
(bound,sol) = lowerBound (f,RoundTol=>4);
-- (lead, step 5: -23/128 now, better than the documented -3/16, LOG.md 5.1
-- decision 3; pinned now: exact, at least -3/16, at most -729/4096)
assert(ring bound === QQ and bound >= -3/16 and bound <= -729/4096);
(bound,sol) = lowerBound (f,RoundTol=>12);
assert(bound == -729/4096);
(bound,sol) = lowerBound (f,RoundTol=>infinity);
assert(instance(bound, RR));
-- (lead, step 5: the printed -.1779785174727856 came from the floating point
-- model of RoundTol => infinity; since the fix of BUGS.md 30 the solver gets the
-- exact model, and the value moves within the solver's accuracy; pinned now:
-- the distance to the optimum -729/4096 of the relaxation, LOG.md 5.2)
assert(abs(bound - (-729/4096)) < 1e-7);

-- [JSAG] sec. 4: solveSOS (f-t, -t, RoundTol=>12) gives | -729/4096 |
R2 = QQ[symbol x, symbol z][symbol t];
f2 = library ("Motzkin", {x,1,z});
sol2 = solveSOS (f2-t, -t, RoundTol=>12);
assert(sol2#Parameters == matrix(QQ, {{-729/4096}}));

-- coefficient field node: Scheiderer's polynomial, six real squares, two of
-- them with weights of order 1e-9; clean(0.001, .) keeps four
fS = library("Scheiderer", QQ[symbol x, symbol y, symbol z]);
solS = solveSOS (fS);
sS = sosPoly solS;
assert(length sS == 6);
cS = clean (0.001, sS);
assert(length cS == 4);
expectedW = {.612611, .88919, 1.85689, 3.9924};
assert(all(4, i -> abs((sort coefficients cS)#i - expectedW#i) < 1e-4));
assert(all(take(sort coefficients sS, 2), w -> w < 1e-8));
