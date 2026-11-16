-- Adaptive precision (Precision => infinity) on ExampleSystems and on a few
-- systems with singular solutions: does precision get raised needlessly near
-- t = 1 on paths that diverge or end at singular solutions?
-- Three runs per system: adaptive (default EndZoneFactor 0.05: no precision
-- change past t = 0.95), adaptive with EndZoneFactor => 0, fixed double.
--
-- Usage: M2 --script adaptive_precision_examples.m2 NAME
-- Outcome (default EndZoneFactor): no path changes precision; same endpoints,
-- statuses and time as fixed double, except a few diverging paths end with
-- IncreasePrecision where fixed double ends with MinStepFailure a step later
-- (cyclic5: 5 of 50, proddeco: 169 of 188, reimer5: 204 of 555).
-- With EndZoneFactor => 0 every MinStepFailure path is resumed at 128 bits
-- (some at 256): diverging paths then end as Infinity and singular endpoints
-- are reached, at 30-1000 times the double-precision time.
needsPackage "ExampleSystems"
NAGtrace 0
numTBBThreads = 0
name = last commandLine
K = CC_53
systems = hashTable {
    "wood" => () -> wood K,
    "trinks" => () -> trinks K,
    "lorentz" => () -> lorentz K,
    "lumped" => () -> lumped K,
    "wright" => () -> wright K,
    "sendra" => () -> sendra K,
    "caprasse" => () -> caprasse K,
    "cyclic4" => () -> cyclic(4,K),     -- positive-dimensional
    "cyclic5" => () -> cyclic(5,K),
    "cyclic6" => () -> cyclic(6,K),     -- positive-dimensional
    "puma" => () -> puma K,
    "chemequ" => () -> chemequ K,
    "quadgrid" => () -> quadgrid K,     -- ill-conditioned
    "camera1s" => () -> camera1s K,
    "comb3000s" => () -> comb3000s K,
    "ipp" => () -> ipp K,
    "noon5" => () -> noon5 K,
    "rose" => () -> rose K,
    "proddeco" => () -> proddeco K,
    "virasoro" => () -> virasoro K,
    "geneig" => () -> geneig K,
    "reimer5" => () -> reimer5 K,
    "eco8" => () -> eco8 K,
    -- singular solutions
    "doubleroot" => () -> (R := K[x,y]; {y - x^2, y - x + 1/4}),                        -- double root
    "tripleroot" => () -> (R := K[x,y]; {y - (x-1)^3, y}),                              -- triple root
    "tangentcircles" => () -> (R := K[x,y]; {x^2 + y^2 - 1, (x-2)^2 + y^2 - 1}),        -- double root, 2 paths to infinity
    "multroot" => () -> (R := K[x,y]; {(x-1/5)*(x-1/2)^2, (y+1/5)*(y+1/3)^3}),         -- multiplicities 1,2,3,6
    "cusp" => () -> (R := K[x,y]; {y^2 - x^3, y - x^2 + x})                              -- cusp meets parabola at origin
    }
F = (systems#name)()
(S,solsS) = totalDegreeStartSystem F
H = segmentHomotopy(S, F, gamma=>0.6+0.8*ii)
<< "=== " << name << ": " << #solsS << " paths, " << numgens ring F#0 << " variables" << endl << flush

dedupe = sols -> (
    reps := {};
    for s in sols do if not any(reps, r -> areEqual(r, s, Tolerance=>1e-5)) then reps = append(reps, s);
    #reps)
summary = (label, sols, tm) -> (
    regs := select(sols, s -> status s === Regular);
    << "  " << label << ": " << toString tally apply(sols, status)
      << (if (first sols).cache#?"MaxPrecision" then ", max precision " | toString tally apply(sols, s -> s.cache#"MaxPrecision") else "")
      << ", " << dedupe regs << " distinct regular endpoints, " << tm << "s" << endl << flush)

tA  = elapsedTiming trackHomotopy(H, solsS, Precision=>infinity);
tA0 = elapsedTiming trackHomotopy(H, solsS, Precision=>infinity, EndZoneFactor=>0);
tD  = elapsedTiming trackHomotopy(H, solsS, Precision=>53);
summary("adaptive        ", tA#1, tA#0)
summary("adaptive EZF=0  ", tA0#1, tA0#0)
summary("fixed 53        ", tD#1, tD#0)

-- paths that changed precision: when, and how fixed double fared on them
bucket = t -> if t < 0.9 then "t<0.9" else if t < 0.99 then "0.9<=t<0.99" else "t>=0.99"
analyze = (label, A, D) -> (
    changed := select(#A, i -> #((A#i).cache#"PrecisionHistory") > 0);
    << "  " << label << ": " << #changed << " paths changed precision" << endl;
    if #changed == 0 then return;
    firstInc := i -> first select((A#i).cache#"PrecisionHistory", h -> h#0 =!= DecreasePrecision);
    << "    by t of first increase and fixed-53 outcome: " 
      << toString tally apply(changed, i -> (bucket realPart (firstInc i)#1, status D#i)) << endl;
    << "    final adaptive status of those paths: " << toString tally apply(changed, i -> status A#i) << endl;
    both := select(changed, i -> status D#i === Regular and status A#i === Regular);
    if #both > 0 then << "    both regular: " << #select(both, i -> areEqual(A#i, D#i, Tolerance=>1e-6)) << " of " << #both << " agree to 1e-6" << endl;
    for i in take(changed, min(12, #changed)) do (
	s := A#i; d := D#i;
	<< "    path " << i << ": " << toString apply(s.cache#"PrecisionHistory", h -> (h#0, round(4, realPart h#1), h#2))
	  << " -> " << status s << " @" << round(4, realPart s.cache.LastT) << " |x|=" << round(2, norm matrix s) << " maxprec " << s.cache#"MaxPrecision"
	  << " | fixed53: " << status d << " @" << round(4, realPart d.cache.LastT) << " |x|=" << round(2, norm matrix d) << endl;
	);
    << flush)
crosstab = (label, A, D) -> (
    << "  " << label << " vs fixed 53, (adaptive status, fixed status): " << toString tally apply(#A, i -> (status A#i, status D#i)) << endl;
    ip := select(#A, i -> status A#i === IncreasePrecision);
    for i in take(ip, min(8, #ip)) do (
	s := A#i; d := D#i;
	<< "    path " << i << " IncreasePrecision @" << round(5, realPart s.cache.LastT) << " |x|=" << round(2, norm matrix s) 
	  << " | fixed53: " << status d << " @" << round(5, realPart d.cache.LastT) << " |x|=" << round(2, norm matrix d) << endl);
    << flush)
crosstab("adaptive        ", tA#1, tD#1)
analyze("adaptive        ", tA#1, tD#1)
analyze("adaptive EZF=0  ", tA0#1, tD#1)
