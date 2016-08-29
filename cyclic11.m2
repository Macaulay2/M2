load "cyclic.m2"
nedges = 4
setRandomSeed 0
polys = parametrizedCyclic 11
(p0,x0) = createSeedPair polySystem polys
elapsedTime mixedVolume = computeMixedVolume specializeSystem (p0,polys)
print mixedVolume
elapsedTime (G,npaths) = monodromySolve(polys,p0,{x0},NumberOfEdges=>nedges,TargetSolutionCount=>mixedVolume, Verbose=>true)
{* -- Anton's office machine:
     -- 8450.44 seconds elapsed
     mixedVolume = 184756
     npaths = 737432
     6.3G
*}
end

restart
load "cyclic.m2"
nedges = 4
setRandomSeed 0
polys = parametrizedCyclic 11 
(p0,x0) = createSeedPair polySystem polys
elapsedTime mixedVolume = computeMixedVolume specializeSystem (p0,polys)
elapsedTime (G,npaths) = monodromySolve(polys,p0,{x0},NumberOfEdges=>nedges,TargetSolutionCount=>mixedVolume,
    SelectEdgeAndDirection=>selectBestEdgeAndDirection, Potential=>potentialE, Verbose=>true)
{*
     trackedPaths 159599
     -- 11752.8 seconds elapsed
     npaths =  553355
     6.7G
*}

restart
needsPackage "NumericalAlgebraicGeometry"
getDefault Software
{*
setDefault(Software=>PHCPACK)
*}
load "cyclic11.m2"
