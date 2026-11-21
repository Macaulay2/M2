-- Tests written from the documentation alone (independent tester, applications).
-- Topic: checkSolver (doc node checkSolver: "tests whether various
-- functions work properly using a specified solver ... The most basic
-- invocation uses only the name of the solver.  This runs all tests with
-- that solver and prints a summary.  All tests should pass for CSDP and
-- MOSEK.")  fun: one of optimize, solveSOS, sosdecTernary, sosInIdeal,
-- lowerBound.  Key (checkSolver,String,String): the function given by name.
-- The return value is not documented; by analogy with checkOptimize
-- ("List indicating whether the solver passed each of the tests") the
-- tests below accept a list of booleans and require all of them to be true.

csAllPassed = r -> instance(r, List) and #r > 0 and all(r, b -> b === true);

-- the basic invocation runs everything and prints a summary (no error)
setRandomSeed 0;
checkSolver "CSDP";

-- one function at a time: all tests pass with CSDP
for fn in {lowerBound, sosInIdeal, sosdecTernary, solveSOS, optimize} do (
    setRandomSeed 0;
    r := checkSolver("CSDP", fn);
    print("checkSolver CSDP " | toString fn | ": " | toString r);
    assert(csAllPassed r));
-- the function given by its name (checkSolver,String,String)
for fn in {"lowerBound", "sosInIdeal", "sosdecTernary", "solveSOS", "optimize"} do (
    setRandomSeed 0;
    r := checkSolver("CSDP", fn);
    print("checkSolver CSDP \"" | fn | "\": " | toString r);
    assert(csAllPassed r));
-- the option Verbosity is accepted and does not change the outcome
for v in {0, 1} do (
    setRandomSeed 0;
    r := checkSolver("CSDP", lowerBound, Verbosity => v);
    assert(csAllPassed r));
