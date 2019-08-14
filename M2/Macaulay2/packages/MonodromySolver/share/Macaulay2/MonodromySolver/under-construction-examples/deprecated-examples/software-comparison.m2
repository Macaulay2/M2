-- Don't run this file directly to test monodromySolve. Due to the garbage collection
-- issues, we need to restart M2 after each run of monodromySolve. Run
-- python M2-test-wrapper.py and it will run numTrials runs of the monodromy
-- solver where numTrials is defined in M2-test-wrapper.py

load "../paper-examples/cyclic.m2" 
needsPackage "NumericalAlgebraicGeometry"
randomSeedValue = value (get ("sentinelFile")); --File created by python process. It just contains an int for the random seed.
setRandomSeed randomSeedValue;

n = 10;
nnodes = 2;
nedges = 4;
init = completeGraphInit;
polys = parametrizedCyclic(n);
(p0,x0) := createSeedPair polySystem polys;
mixedVol = (computeMixedVolume specializeSystem (p0,polys));

monodromyTime = (elapsedTiming monodromyResult = monodromySolve(polys,p0,{x0},
  GraphInitFunction=>init,
  NumberOfNodes=>nnodes,
  NumberOfEdges=>nedges,
  SelectEdgeAndDirection => selectBestEdgeAndDirection,
  Potential=> potentialE,
  TargetSolutionCount=>mixedVol))#0;

openOutAppend "MonodromyCyclicBigExample.txt" << "Cyclic-" << n << 
". Random seed: " << randomSeedValue << ", Time: " << monodromyTime << 
" , Monodromy Result: " << monodromyResult << endl << close;
end ---------------------------

load "../paper-examples/cyclic.m2" 
needsPackage "NumericalAlgebraicGeometry"
n = 10;
numTrials = 3;
polys = (cyclicRoots(n, CC))_*;
for software in {PHCPACK, BERTINI, M2engine -*, HOM4PS2*-} do (
  softwareTime = (elapsedTiming (apply(numTrials, temp -> solveSystem (polys, Software=> software))))#0;
  << software << " time: " << softwareTime/numTrials << endl;
);
