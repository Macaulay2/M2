-- Benchmark: parallel homotopy tracking speedup over TBB thread counts.
-- Usage: M2 -q --script parallel_track_bench.m2
-- Records wall-clock time for trackHomotopy on curated high-path-count systems
-- at each thread count in threadCounts, then prints speedup vs 1-thread baseline.
-- Note: meaningful speedup is limited by available physical cores.
needsPackage "NumericalAlgebraicGeometry"
needsPackage "ExampleSystems"

NAGtrace 0   -- suppress per-step trace and progress bar for clean timing
threadCounts := {1, 2, 4, 8, 16, 32}

benchSystem = (name, F) -> (
    (S, solsS) := totalDegreeStartSystem F;
    H := segmentHomotopy(ii*S, F);
    nPaths := #solsS;
    << "-- " << name << " (" << nPaths << " paths)" << endl;
    times := new MutableList from apply(#threadCounts, i -> -1.);
    scan(#threadCounts, i -> (
        k := threadCounts#i;
        numTBBThreads = k;
        tr := elapsedTiming (try trackHomotopy(H, solsS) else null);
        times#i = tr#0;
        << "   threads=" << k << "  t=" << times#i << "s" << endl;
    ));
    numTBBThreads = 0;   -- restore automatic
    t1 := times#0;
    speedups := apply(toList times, t -> if t > 0 and t1 > 0 then t1/t else 0.);
    << "   speedup: " << speedups << endl;
);

-- Curated systems with >=200 homotopy paths (Bezout bound).
try benchSystem("katsura 9",        katsura(9,  CC_53));   -- 512 paths
try benchSystem("katsura 10",       katsura(10, CC_53));   -- 1024 paths
try benchSystem("katsura 11",       katsura(11, CC_53));   -- 2048 paths
try benchSystem("cyclic 6",         cyclic(6,   CC_53));   -- 720 paths
try benchSystem("cyclic 7",         cyclic(7,   CC_53));   -- 5040 paths
try benchSystem("randomSys(5,3)",   randomSystem(5,3,CC_53));  -- 243 paths
try benchSystem("randomSys(5,4)",   randomSystem(5,4,CC_53));  -- 1024 paths
try benchSystem("reimer5",          reimer5 CC_53);         -- 720 paths
try benchSystem("noon5",            noon5 CC_53);           -- 243 paths
try benchSystem("butcher",        butcher CC_53);        -- 4608 paths, ~82s each

end--
restart
load "parallel_track_bench.m2"
