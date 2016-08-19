load "../examples/cyclic.m2" 
needsPackage "NumericalAlgebraicGeometry"

n = 6;
numTrials = 3;
<< endl << endl << "------Timings for cyclic-" << n << ": average of " << numTrials << " trials------" << endl;

-- Not sure what our best option is for the below three. This configuration is a placeholder.
nnodes = 3;
nedges = 3;
init = flowerGraphInit;

polys = parametrizedCyclic(n);
(p0,x0) := createSeedPair polySystem polys;

monodromyTime = (elapsedTiming (apply(numTrials, temp -> monodromySolve(polys,p0,{x0},
  GraphInitFunction=>init,
  NumberOfNodes=>nnodes,
  NumberOfEdges=>nedges,
  SelectEdgeAndDirection => selectBestEdgeAndDirection, --Obviously the best
  Potential=> potentialE, -- also obviously the best
  TargetSolutionCount=>(computeMixedVolume specializeSystem (p0,polys))))))#0;
<< "Monodromy time: " << monodromyTime/numTrials << endl;

polys = (cyclicRoots(n, CC))_*;
for software in {PHCPACK, BERTINI, M2engine {*, HOM4PS2*}} do (
  softwareTime = (elapsedTiming (apply(numTrials, temp -> solveSystem (polys, Software=> software))))#0;
  << software << " time: " << softwareTime/numTrials << endl;
);

end ------------------------------
restart
elapsedTime load "comparison-template.m2"