load "cyclic.m2"
getDefault Software
{*
setDefault(Software=>PHCPACK)
*}
nedges = 4

setRandomSeed 0
polys = parametrizedCyclic 10 
(p0,x0) = createSeedPair polySystem polys
elapsedTime mixedVolume = computeMixedVolume specializeSystem (p0,polys)
elapsedTime monodromySolve(polys,p0,{x0},NumberOfEdges=>nedges,TargetSolutionCount=>mixedVolume)
{* -- Anton's office machine:

     -- 610.323 seconds elapsed

o17 = (HomotopyNode{...5...}, 129910)

-- eats 3.9G 

*}
end

setRandomSeed 1
elapsedTime  monodromySolve(polys,p0,{x0},NumberOfEdges=>nedges,TargetSolutionCount=>mixedVolume,
    SelectEdgeAndDirection=>selectBestEdgeAndDirection, Potential=>potentialE)
{*
     -- 617.643 seconds elapsed

o9 = (HomotopyNode{...5...}, 107640)
*}
restart
load "cyclic10.m2"
