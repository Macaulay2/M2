-- Verbosity (documentation, conventions D8): 0 prints nothing; 1 minimal
-- information (the solver used, the status of the solver); 2 more.  The
-- output of a call is captured as a string.  (checkSolver's own Verbosity is
-- BUGS.md 7, tests/open/.)  Core's one-time warning about computations over
-- inexact fields is not output of the package and is ignored.
out = cmd -> (
    (err, o) := capture cmd;
    assert(not err);
    concatenate select(lines o, l -> not match("experimental computation over inexact field|results not reliable|^ *$|^i[0-9]+ :|^o[0-9]+ ", l)))
R = QQ[x,y];
f = 2*x^4 + 2*x^3*y - 2*x^2*y^2 + 5*y^4;
-- each call with its last argument followed by OPTS
for c in {"solveSOS(f OPTS)", "lowerBound(x^2 + 1 OPTS)", "lowerBound((x^2 - x)/(x^2 + 1) OPTS)",
        "sosInIdeal(matrix{{x - y}}, 2 OPTS)", "sosdecTernary(library(\"Motzkin\", QQ[x,y,z]) OPTS)"} do (
    o0 := out(replace("OPTS", "", c) | ";");
    if o0 != "" then error("output at Verbosity 0: " | c | ": " | o0);
    o1 := out(replace("OPTS", ", Verbosity => 1", c) | ";");
    if not (match("CSDP", o1) and match("Status", o1)) then error("Verbosity 1: " | c | ": " | o1);
    o2 := out(replace("OPTS", ", Verbosity => 2", c) | ";");
    assert(#o2 >= #o1))
-- the failures say something at Verbosity => 1 (the wording is not pinned)
assert(out "solveSOS(x^3 + 1, Verbosity => 1);" != "")
assert(out "solveSOS(x^2 + 1, matrix{{x}}, Verbosity => 1);" != "")

-- recoverSolution warns when the moment matrix is not rank one (documentation:
-- it works "by checking if the moment matrix is rank one"): the minimum of
-- 10 - x^2 - y on the circle is attained at two points (added after the
-- mutation analysis, LOG.md 2.6; the one-variable case is BUGS.md 24)
S = QQ[x,y]/ideal(x^2 + y^2 - 1);
(b, sol) = lowerBound(10 - x^2 - y, 2, RoundTol => infinity);
assert match("rank", out "recoverSolution sol;")
