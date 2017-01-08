needsPackage "MonodromySolver"
setRandomSeed 0
needsPackage "ExampleIdeals"

parametrizedCyclic = n -> (
    S := gens cyclicRoots(n,CC);
    R := ring S;
    polys := flatten entries S;
    ind := flatten apply(#polys,i-> -- indices for parameters
    	apply(exponents polys#i, t->(i,t))
    	);
    AR := CC[apply(ind,i->A_i)][gens R];
    polysP := for i to #polys-1 list -- system with parameteric coefficients and same support 
    sum(exponents polys#i, t->A_(i,t)*AR_(t));
    polySystem transpose matrix {polysP}
    )
end ------------------------------------------------

restart
load "MonodromySolver/examples/cyclic.m2"
nedges = 4
setRandomSeed 0
polys = parametrizedCyclic 5
(p0,x0) = createSeedPair polySystem polys
mixedVolume = computeMixedVolume specializeSystem (p0,polys)
elapsedTime  monodromySolve(polys,p0,{x0},NumberOfEdges=>nedges,TargetSolutionCount=>mixedVolume)
elapsedTime  monodromySolve(polys,p0,{x0},NumberOfEdges=>nedges,TargetSolutionCount=>mixedVolume,SelectEdgeAndDirection=>selectBestEdgeAndDirection, Potential=>potentialE)
elapsedTime  monodromySolve(polys,p0,{x0},NumberOfEdges=>nedges,TargetSolutionCount=>mixedVolume,
    SelectEdgeAndDirection=>selectBestEdgeAndDirection, 
    Potential=>makeBatchPotential 10, BatchSize=>10, Verbose=>true)
elapsedTime  monodromySolve(polys,p0,{x0},NumberOfEdges=>nedges,TargetSolutionCount=>mixedVolume,
    SelectEdgeAndDirection=>makeRandomizedSelect 0.1, 
    Potential=>potentialE, BatchSize=>10, Verbose=>true)

setRandomSeed 0
polys = parametrizedCyclic 7
(p0,x0) = createSeedPair polySystem polys
mixedVolume = computeMixedVolume specializeSystem (p0,polys)
elapsedTime  monodromySolve(polys,p0,{x0},NumberOfEdges=>nedges,TargetSolutionCount=>mixedVolume)


setRandomSeed 0
polys = parametrizedCyclic 8
(p0,x0) = createSeedPair polySystem polys
mixedVolume = computeMixedVolume specializeSystem (p0,polys)
elapsedTime  monodromySolve(polys,p0,{x0},NumberOfEdges=>nedges,TargetSolutionCount=>mixedVolume)

setRandomSeed 0
polys = parametrizedCyclic 9
(p0,x0) = createSeedPair polySystem polys
mixedVolume = computeMixedVolume specializeSystem (p0,polys)
elapsedTime  monodromySolve(polys,p0,{x0},NumberOfEdges=>nedges,TargetSolutionCount=>mixedVolume)
{*
     -- 140.501 seconds elapsed

o16 = (HomotopyNode{...5...}, 42898)
*}


----------------------------------------------------------------
-- this is the old naive solver ("dynamic flower")
setRandomSeed 0
polys = parametrizedCyclic 9
(p0,x0) = createSeedPair polySystem polys
mdebug MonodromySolver
stop = (n,L)->n>1
elapsedTime sols = solveViaMonodromy(transpose polys.PolyMap,c0,{pre0});
{*
number of paths tracked: 151542
found 11016 points in the fiber so far
     -- 688.773 seconds elapsed
*}

