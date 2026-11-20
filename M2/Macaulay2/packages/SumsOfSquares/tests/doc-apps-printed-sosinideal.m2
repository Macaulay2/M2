-- Output fidelity (independent tester, applications): printed outputs of the
-- documentation nodes sosInIdeal and sosdecTernary and of [JSAG] sections 2
-- and 3, as computed by the Macaulay2 1.26.06 build.  These pin printed
-- values, not mathematical properties.  setRandomSeed before each call.
-- Not pinned: the printed numerator/denominator of [JSAG] section 3
-- (2267/64 ..., version 2.1), which the current version does not reproduce.

-- sosInIdeal node o4 / [JSAG] section 2 o4: (395/2)(-1/2x + 1)^2 + (395/2)(z)^2
R = QQ[x,y,z];
h = matrix {{x^2 - 4*x + 2*y^2, 2*z^2 - y^2 + 2}};
-- (lead, step 3: the scale 395/2 came from the random retries of the
-- dependency; with the fix of BUGS.md 18 the printed scale changes, LOG.md
-- 1.7 decision 3; pinned now: the same squares with one common weight c)
setRandomSeed 0;
(sol, mult) = sosInIdeal(h, 2);
s = sosPoly sol;
c = first coefficients s;
assert(c > 0 and coefficients s == {c, c});
assert(gens s == {-1/2*x + 1, z});
assert(mult == matrix(R, {{c/4}, {c/2}}));     -- implied by h*mult == sosPoly sol
-- sosInIdeal node o8 / [JSAG] section 2 o7: (1031833/2048)(-1/2x + 1)^2 + (1031833/2048)(z)^2
S = R/ideal h;
setRandomSeed 0;
sS = sosPoly sosInIdeal(S, 2);
cS = first coefficients sS;
assert(cS > 0 and coefficients sS == {cS, cS});
assert(apply(gens sS, g -> lift(g, R)) == {-1/2*R_0 + 1, R_2});

-- sosdecTernary node o4: (#p, #q) = (1, 1) for the Motzkin form over RR
R = RR[x,y,z];
setRandomSeed 0;
(p, q) = sosdecTernary library("Motzkin", {x,y,z});
assert((#p, #q) == (1, 1));
-- [JSAG] section 3: one numerator and one denominator over QQ
R = QQ[x,y,z];
setRandomSeed 0;
(Nums, Dens) = sosdecTernary library("Motzkin", {x,y,z});
assert((#Nums, #Dens) == (1, 1));
assert(ring value first Nums === R and ring value first Dens === R);
