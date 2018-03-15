restart ---------------------------------------------------------------------
load "../cyclic.m2"
nedges = 3
setRandomSeed 0
polys = parametrizedCyclic 10 
(p0,x0) = createSeedPair polySystem polys
elapsedTime mixedVolume = computeMixedVolume specializeSystem (p0,polys)
elapsedTime (V,npaths) = monodromySolve(polys,p0,{x0},NumberOfEdges=>nedges,
    NumberOfNodes=>2, TargetSolutionCount=>mixedVolume, Verbose=>true)
getTrackTime V.Graph
-* -- Anton's office machine:
mixedVolume = 35940
-- 610.357 seconds elapsed
o7 = (HomotopyNode{...5...}, 107820)
08 = 579.812715933
*-

restart ---------------------------------------------------------------------
load "../cyclic.m2"
nedges = 4
setRandomSeed 0
polys = parametrizedCyclic 10 
(p0,x0) = createSeedPair polySystem polys
elapsedTime mixedVolume = computeMixedVolume specializeSystem (p0,polys)
elapsedTime (V,npaths) = monodromySolve(polys,p0,{x0},NumberOfEdges=>nedges,
    NumberOfNodes=>2, TargetSolutionCount=>mixedVolume, Verbose=>true)
getTrackTime V.Graph
-* -- Anton's office machine:

     -- 610.323 seconds elapsed (track, 3.9G)
     -- 740.461 seconds elapsed

o7 = (HomotopyNode{...5...}, 129910)
o8 = 704.35711509
*-

restart ---------------------------------------------------------------------
load "../cyclic.m2"
nedges = 4
setRandomSeed 0
polys = parametrizedCyclic 10 
(p0,x0) = createSeedPair polySystem polys
elapsedTime mixedVolume = computeMixedVolume specializeSystem (p0,polys)
elapsedTime (V,npaths) = monodromySolve(polys,p0,{x0},NumberOfEdges=>nedges,TargetSolutionCount=>mixedVolume,
    NumberOfNodes=>2, SelectEdgeAndDirection=>selectBestEdgeAndDirection, Verbose=>true, 
    Potential=>potentialE)
getTrackTime V.Graph
-*
     -- 617.643 seconds elapsed

o9 = (HomotopyNode{...5...}, 107640)

trackHomotopy:
     -- 1083.45 seconds elapsed
     o7 = (HomotopyNode{...5...}, 107700)
     o8 = 585.668276680999
*-

restart --- PotentialE, BatchSize => 1 ----------------------------------------------------
load "../cyclic.m2"
nedges = 4
setRandomSeed 0
polys = parametrizedCyclic 10 
(p0,x0) = createSeedPair polySystem polys
elapsedTime mixedVolume = computeMixedVolume specializeSystem (p0,polys)
elapsedTime (V,npaths) = monodromySolve(polys,p0,{x0},NumberOfEdges=>nedges,TargetSolutionCount=>mixedVolume,
    NumberOfNodes=>2, SelectEdgeAndDirection=>selectBestEdgeAndDirection, -- Verbose=>true, 
    Potential=>potentialE, BatchSize=>1)
getTrackTime V.Graph
-*
trackHomotopy:
     -- 3784.71 seconds elapsed
o7 = (HomotopyNode{...5...}, 104739)
o8 = 576.537621946002
*-

restart ---------------------------------------------------------------------
load "../cyclic.m2"
nedges = 4
setRandomSeed 0
polys = parametrizedCyclic 10 
(p0,x0) = createSeedPair polySystem polys
elapsedTime mixedVolume = computeMixedVolume specializeSystem (p0,polys)
elapsedTime  monodromySolve(polys,p0,{x0},NumberOfEdges=>nedges,TargetSolutionCount=>mixedVolume,
    NumberOfNodes=>2, SelectEdgeAndDirection=>selectBestEdgeAndDirection, 
    Potential=>makeBatchPotential 100, BatchSize=>100, Verbose=>true)
-*
trackHomotopy:
     -- 700.332 seconds elapsed

o21 = (HomotopyNode{...5...}, 94889)
4.3G
*-

restart ---------------------------------------------------------------------
load "../cyclic.m2"
nedges = 4
setRandomSeed 0
polys = parametrizedCyclic 10 
(p0,x0) = createSeedPair polySystem polys
elapsedTime mixedVolume = computeMixedVolume specializeSystem (p0,polys)
elapsedTime  monodromySolve(polys,p0,{x0},NumberOfEdges=>nedges,TargetSolutionCount=>mixedVolume,
    NumberOfNodes=>2, SelectEdgeAndDirection=>makeRandomizedSelect 0.1, 
    Potential=>potentialE, BatchSize=>100, Verbose=>true)

-*
trackHomotopy:
     -- 656.717 seconds elapsed

o7 = (HomotopyNode{...5...}, 110192)
4.1G
*-

restart ------make failure rate high ---------------------------------------------------------------
load "../cyclic.m2"
nedges = 4
setRandomSeed 0
polys = parametrizedCyclic 10 
(p0,x0) = createSeedPair polySystem polys
elapsedTime mixedVolume = computeMixedVolume specializeSystem (p0,polys)
setDefault(tStepMin=>0.005)
elapsedTime monodromySolve(polys,p0,{x0},NumberOfEdges=>nedges,TargetSolutionCount=>mixedVolume,
     NumberOfNodes=>2, Verbose=>true)

-*
     -- 802.092 seconds elapsed

o8 = (HomotopyNode{...5...}, 162038)
*-

restart --- naive ------------------------------------------------------------------
load "../cyclic.m2"
setRandomSeed 0
polys = parametrizedCyclic 10 
(p0,x0) = createSeedPair polySystem polys
elapsedTime mixedVolume = computeMixedVolume specializeSystem (p0,polys)
elapsedTime (V,npaths) = dynamicFlowerSolve(polys.PolyMap,p0,{x0},TargetSolutionCount=>mixedVolume);
npaths
-* -- Anton's office machine:
-- 2376.16 seconds elapsed
i11 : npaths
o11 = 299873
*-


restart ---PHCpack------------------------------------------------------------------
load "../cyclic.m2"
needsPackage "PHCpack"
polys = parametrizedCyclic 10 
(p0,x0) = createSeedPair polySystem polys
specPolys = specializeSystem (p0,polys)
R = CC[x_1..x_(numgens ring first specPolys)]
toR = map(R,ring first specPolys,vars R)
elapsedTime (mv,q,qsols) = mixedVolume(specPolys/toR,StartSystem => true);

-*
i8 : elapsedTime (mv,q,qsols) = mixedVolume(specPolys/toR,StartSystem => true);
     -- 538.198 seconds elapsed
*-

restart ---linear tracker in PHCpack------------------------------------------------------------------
load "../cyclic.m2"
-- setDefault(Software=>PHCPACK) -- if this is HERE we get SIGSEGV
nedges = 3
setRandomSeed 0
polys = parametrizedCyclic 10
(p0,x0) = createSeedPair polySystem polys
elapsedTime mixedVolume = computeMixedVolume specializeSystem (p0,polys)
debug Core
setDefault(Software=>PHCPACK)
elapsedTime (G,npaths) = monodromySolve(polys,p0,{x0},NumberOfEdges=>nedges,TargetSolutionCount=>mixedVolume, Verbose=>true, NumberOfNodes=>2)
