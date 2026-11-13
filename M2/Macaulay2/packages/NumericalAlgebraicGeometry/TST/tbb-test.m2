-- Multi-threaded vs single-threaded path tracking.
-- Tracks each system with numTBBThreads = 1 and with several threads, then
-- checks that the two runs agree path by path: same number of solutions,
-- same statuses, same coordinates (up to tolerance), and small residuals
-- at the target for the regular solutions.
-- Usage: load "NumericalAlgebraicGeometry/TST/tbb-test.m2"
restart
needsPackage "NumericalAlgebraicGeometry"
needsPackage "ExampleSystems"
NAGtrace 0

manyThreads = 4
coordTol = 1e-6
residualTol = 1e-8

trackWith = (H, solsS, k) -> (
    numTBBThreads = k;
    t := elapsedTiming trackHomotopy(H, solsS);
    numTBBThreads = 0;
    (t#0, t#1)
    )

compareThreads = (name, T) -> (
    (S, solsS) := totalDegreeStartSystem T;
    H := segmentHomotopy(ii*S, T);
    (t1, sols1) := trackWith(H, solsS, 1);
    (tk, solsk) := trackWith(H, solsS, manyThreads);
    << "-- " << name << ": " << #solsS << " paths; "
      << "1 thread: " << t1 << "s, " << manyThreads << " threads: " << tk
      << "s, speedup " << t1/tk << endl;
    assert(#sols1 == #solsS);
    assert(#solsk == #solsS);
    -- statuses agree path by path
    assert(apply(sols1, status) == apply(solsk, status));
    -- coordinates agree path by path for the regular solutions
    reg := positions(sols1, s -> status s === Regular);
    assert all(reg, i -> areEqual(coordinates sols1#i, coordinates solsk#i,
	    Tolerance => coordTol));
    -- regular solutions solve the target system
    F := polySystem T;
    assert all(reg, i -> norm evaluate(F, solsk#i) < residualTol);
    << "   " << #reg << " regular solutions agree" << endl;
    )

compareThreads("katsura 10", katsura(10, CC_53))
compareThreads("cyclic 6", cyclic(6, CC_53))        -- many paths diverge
compareThreads("noon5", noon5 CC_53)
compareThreads("katsura 6 over CC_100", katsura(6, CC_100)) -- multiprecision path
