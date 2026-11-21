-- doc-FINDING-core-roundtol-large-no-rounding.m2 -- open finding
-- (independent tester, core).  Expected to FAIL on the original package.
-- Doc of RoundTol: "The rounding strategy is guaranteed to work whenever the
-- space of Gram matrices is full dimensional ... Higher values of RoundTol lead
-- to better solutions"; [JSAG] Appendix: the rounding works when the
-- numerical Gram matrix is in the interior and the rounding tolerance is small
-- enough, i.e. for large RoundTol.  Only RoundTol => infinity is documented to
-- skip the rounding.
-- On the original, every RoundTol >= 34 (34, 40, 52, 53, 60, 100, 1000 tried)
-- returns a real (RR) Gram matrix even for strictly feasible problems with a
-- positive definite Gram matrix (x^2+y^2+1: the identity), where RoundTol 32
-- gives an exact rational certificate.  (33 is an error, see
-- doc-FINDING-core-roundtol-33-error.m2.)
-- Asserted here: rational exact certificates for RoundTol 40 and 52 (within
-- double precision).
setRandomSeed 0;  -- fixed seed for anything random in the solver path
R = QQ[x,y];
for f in {x^2 + y^2 + 1, 2*x^4+5*y^4-2*x^2*y^2+2*x^3*y, (x^2+1)*(y^2+1) + x*y} do
    for rt in {40, 52} do (
        sol := solveSOS(f, RoundTol=>rt);
        assert(sol#Status == "SDP solved, primal-dual feasible");
        assert(ring sol#GramMatrix === QQ);
        mon := sol#Monomials;
        assert(transpose(mon)*sol#GramMatrix*mon - f == 0);
        assert(value sosPoly sol == f);
        assert all(coefficients sosPoly sol, c -> c > 0);
        );

-- (lead, after the decision on BUGS.md 26, LOG.md 2.10: (c))
-- Above 52 bits the rounding is skipped with a warning at every Verbosity
-- (Core's one-time warning about inexact fields is not output of the package).
out = cmd -> (
    (err, o) := capture cmd;
    assert(not err);
    concatenate select(lines o, l -> not match("experimental computation over inexact field|results not reliable|^ *$|^i[0-9]+ :|^o[0-9]+ ", l)))
sol = solveSOS(x^2 + y^2 + 1, RoundTol => 60);
assert(instance(ring sol#GramMatrix, RealField));
assert match("[Ww]arning", out "solveSOS(x^2 + y^2 + 1, RoundTol => 60);")
assert(not match("[Ww]arning", out "solveSOS(x^2 + y^2 + 1, RoundTol => infinity);"))
-- Tiny polynomials are scaled before the solver and the rounding: exact
-- certificates at the default RoundTol.
g = 2*x^4 + 5*y^4 - 2*x^2*y^2 + 2*x^3*y;
for e in {9, 12, 15} do (
    f := 10^-e * g;
    sol := solveSOS f;
    assert(ring sol#GramMatrix === QQ);
    mon := sol#Monomials;
    assert(transpose(mon)*sol#GramMatrix*mon - f == 0);
    assert(value sosPoly sol == f);
    assert all(coefficients sosPoly sol, c -> c > 0);
    );
-- and a certified bound of the same relative quality as for x^2 - 3x itself
-- (the default RoundTol 3 rounds the bound to the grid 2^-3 of the scaled problem;
-- the original gives -2^-27, a valid bound 3300 times below the minimum)
U = QQ[symbol u];
(b, sol) = lowerBound(10^-12 * (u^2 - 3*u));
assert(b =!= null and ring b === QQ);
assert(b <= -9/4 * 10^-12 and b >= (-9/4 - 1/4) * 10^-12);
