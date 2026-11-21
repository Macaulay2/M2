-- BUGS.md 7: checkSolver(solver, fun) ignores Verbosity.  At Verbosity => 1
-- the functions print the solver used ("Executing CSDP").
(err, out) = capture "checkSolver(\"CSDP\", \"lowerBound\", Verbosity => 1)";
assert(not err and match("Executing CSDP", out))
