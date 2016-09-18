restart ---------------------------------------------------------------------
load "cyclic.m2"
nedges = 3
setRandomSeed 0
polys = parametrizedCyclic 10 
(p0,x0) = createSeedPair polySystem polys
elapsedTime mixedVolume = computeMixedVolume specializeSystem (p0,polys)
elapsedTime (G,npaths) = monodromySolve(polys,p0,{x0},NumberOfEdges=>nedges,TargetSolutionCount=>mixedVolume, Verbose=>true)
{* -- Anton's office machine:
mixedVolume = 35940
-- 618.102 seconds elapsed
o7 = (HomotopyNode{...5...}, 107820)
*}

restart ---------------------------------------------------------------------
load "cyclic.m2"
nedges = 4
setRandomSeed 0
polys = parametrizedCyclic 10 
(p0,x0) = createSeedPair polySystem polys
elapsedTime mixedVolume = computeMixedVolume specializeSystem (p0,polys)
elapsedTime (G,npaths) = monodromySolve(polys,p0,{x0},NumberOfEdges=>nedges,TargetSolutionCount=>mixedVolume, Verbose=>true)
{* -- Anton's office machine:

     -- 610.323 seconds elapsed (track, 3.9G)
     -- 745.344 seconds elapsed (trackHomotopy, 1.9G)
      
o17 = (HomotopyNode{...5...}, 129910)
*}

restart ---------------------------------------------------------------------
load "cyclic.m2"
nedges = 4
setRandomSeed 0
polys = parametrizedCyclic 10 
(p0,x0) = createSeedPair polySystem polys
elapsedTime mixedVolume = computeMixedVolume specializeSystem (p0,polys)
elapsedTime (G,npaths) = monodromySolve(polys,p0,{x0},NumberOfEdges=>nedges,TargetSolutionCount=>mixedVolume,
    SelectEdgeAndDirection=>selectBestEdgeAndDirection, Potential=>potentialE)
{*
     -- 617.643 seconds elapsed

o9 = (HomotopyNode{...5...}, 107640)

trackHomotopy:
     -- 1265.95 seconds elapsed, 4.2G

o7 = (HomotopyNode{...5...}, 107700)


*}

restart ---------------------------------------------------------------------
load "cyclic.m2"
nedges = 4
setRandomSeed 0
polys = parametrizedCyclic 10 
(p0,x0) = createSeedPair polySystem polys
elapsedTime mixedVolume = computeMixedVolume specializeSystem (p0,polys)
elapsedTime  monodromySolve(polys,p0,{x0},NumberOfEdges=>nedges,TargetSolutionCount=>mixedVolume,
    SelectEdgeAndDirection=>selectBestEdgeAndDirection, 
    Potential=>makeBatchPotential 100, BatchSize=>100, Verbose=>true)
{*
trackHomotopy:
     -- 700.332 seconds elapsed

o21 = (HomotopyNode{...5...}, 94889)
4.3G
*}

restart ---------------------------------------------------------------------
load "cyclic.m2"
nedges = 4
setRandomSeed 0
polys = parametrizedCyclic 10 
(p0,x0) = createSeedPair polySystem polys
elapsedTime mixedVolume = computeMixedVolume specializeSystem (p0,polys)
elapsedTime  monodromySolve(polys,p0,{x0},NumberOfEdges=>nedges,TargetSolutionCount=>mixedVolume,
    SelectEdgeAndDirection=>makeRandomizedSelect 0.1, 
    Potential=>potentialE, BatchSize=>100, Verbose=>true)

{*
trackHomotopy:
     -- 656.717 seconds elapsed

o7 = (HomotopyNode{...5...}, 110192)
4.1G
*}

restart ---PHCpack------------------------------------------------------------------
load "cyclic.m2"
needsPackage "PHCpack"
polys = parametrizedCyclic 10 
(p0,x0) = createSeedPair polySystem polys
specPolys = specializeSystem (p0,polys)
R = CC[x_1..x_(numgens ring first specPolys)]
toR = map(R,ring first specPolys,vars R)
elapsedTime (mv,q,qsols) = mixedVolume(specPolys/toR,StartSystem => true);

{*
i8 : elapsedTime (mv,q,qsols) = mixedVolume(specPolys/toR,StartSystem => true);
     -- 538.198 seconds elapsed
*}

restart ---linear tracker in PHCpack------------------------------------------------------------------
load "cyclic.m2"
-- setDefault(Software=>PHCPACK) -- if this is HERE we get SIGSEGV
nedges = 3
setRandomSeed 0
polys = parametrizedCyclic 10
(p0,x0) = createSeedPair polySystem polys
elapsedTime mixedVolume = computeMixedVolume specializeSystem (p0,polys)
debug Core
setDefault(Software=>PHCPACK)
elapsedTime (G,npaths) = monodromySolve(polys,p0,{x0},NumberOfEdges=>nedges,TargetSolutionCount=>mixedVolume, Verbose=>true)
