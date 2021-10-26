restart ---------------------------------------------------
load "../cyclic.m2"
nedges = 3
setRandomSeed 0
polys = parametrizedCyclic 11
(p0,x0) = createSeedPair polySystem polys
elapsedTime mixedVolume = computeMixedVolume specializeSystem (p0,polys)
print mixedVolume
elapsedTime (V,npaths) = monodromySolve(polys,p0,{x0},NumberOfEdges=>nedges,NumberOfNodes=>2,TargetSolutionCount=>mixedVolume, Verbose=>true)
getTrackTime V.Graph
-* -- Anton's office machine:

     mixedVolume = 184756
-- 6514.17 seconds elapsed
(tracking = 6295.760326436)

o8 = (HomotopyNode{...5...}, 540155)
*-

restart ---------------------------------------------------
load "../cyclic.m2"
nedges = 4
setRandomSeed 0
polys = parametrizedCyclic 11
(p0,x0) = createSeedPair polySystem polys
elapsedTime mixedVolume = computeMixedVolume specializeSystem (p0,polys)
print mixedVolume
elapsedTime (G,npaths) = monodromySolve(polys,p0,{x0},NumberOfEdges=>nedges,NumberOfNodes=>2,TargetSolutionCount=>mixedVolume, Verbose=>true)
-* -- Anton's office machine:
     -- 8450.44 seconds elapsed
     mixedVolume = 184756
     npaths = 737432
     6.3G
*-

restart --------------------------------------------------
load "../cyclic.m2"
nedges = 4
setRandomSeed 0
polys = parametrizedCyclic 11 
(p0,x0) = createSeedPair polySystem polys
elapsedTime mixedVolume = computeMixedVolume specializeSystem (p0,polys)
elapsedTime (G,npaths) = monodromySolve(polys,p0,{x0},NumberOfEdges=>nedges,NumberOfNodes=>2,TargetSolutionCount=>mixedVolume,
    SelectEdgeAndDirection=>selectBestEdgeAndDirection, Potential=>potentialE, Verbose=>true)
-*
     trackedPaths 159599
     -- 11752.8 seconds elapsed
     npaths =  553355
     6.7G
*-

restart ------make failure rate high ---------------------------------------------------------------
load "../cyclic.m2"
nedges = 4
setRandomSeed 0
polys = parametrizedCyclic 11
(p0,x0) = createSeedPair polySystem polys
elapsedTime mixedVolume = computeMixedVolume specializeSystem (p0,polys)
setDefault(tStepMin=>0.001)
elapsedTime monodromySolve(polys,p0,{x0},NumberOfEdges=>nedges,NumberOfNodes=>2,TargetSolutionCount=>mixedVolume,
         Verbose=>true)
-*    
-- 9327.69 seconds elapsed
o11 = (HomotopyNode{...5...}, 759189)
*-

restart --- naive ------------------------------------------------------------------
load "../cyclic.m2"
setRandomSeed 0
polys = parametrizedCyclic 11
(p0,x0) = createSeedPair polySystem polys
elapsedTime mixedVolume = computeMixedVolume specializeSystem (p0,polys)
elapsedTime (V,npaths) = dynamicFlowerSolve(polys.PolyMap,p0,{x0},TargetSolutionCount=>mixedVolume);
npaths
-* -- Anton's office machine:
*-

restart ---PHCpack------------------------------------------------------------------
load "../cyclic.m2"
needsPackage "PHCpack"
polys = parametrizedCyclic 11 
(p0,x0) = createSeedPair polySystem polys
specPolys = specializeSystem (p0,polys)
R = CC[x_1..x_(numgens ring first specPolys)]
toR = map(R,ring first specPolys,vars R)
elapsedTime (mv,q,qsols) = mixedVolume(specPolys/toR,StartSystem => true);

-*
-- 4256.73 seconds elapsed
*-

restart ---linear tracker in PHCpack------------------------------------------------------------------
load "../cyclic.m2"
-- setDefault(Software=>PHCPACK) -- if this is HERE we get SIGSEGV
nedges = 4
setRandomSeed 0
polys = parametrizedCyclic 11
(p0,x0) = createSeedPair polySystem polys
elapsedTime mixedVolume = computeMixedVolume specializeSystem (p0,polys)
debug Core
setDefault(Software=>PHCPACK)
elapsedTime (V,npaths) = monodromySolve(polys,p0,{x0},NumberOfEdges=>nedges,NumberOfNodes=>2,TargetSolutionCount=>mixedVolume, Verbose=>true)
getTrackTime V.Graph
