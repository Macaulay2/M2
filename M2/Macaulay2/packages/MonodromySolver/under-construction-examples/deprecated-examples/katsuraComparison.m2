load "../paper-examples/katsura.m2"

n = value (get ("sentinelFile")); --File created by python process. It just contains an int for the n value.
randomSeedValue = value (get ("sentinelFile2")); --File created by python process. It just contains an int for the random seed.

polys = parametrizedKatsura n;

testTriples = {
    (completeGraphInit, 2, 3), 
    (completeGraphInit, 2, 4), 
    (completeGraphInit, 2, 5), 
    (completeGraphInit, 3, 2), 
    (completeGraphInit, 4, 1), 
    (flowerGraphInit, 3, 2), -- two petals
    (flowerGraphInit, 4, 2), 
    (flowerGraphInit, 5, 2), 
    (flowerGraphInit, 3, 3),
    (flowerGraphInit, 4, 3)
    }


(p0,x0) = createSeedPair polySystem polys;
mixedVolume = computeMixedVolume specializeSystem (p0,polys)

file = openOutAppend (currentFileDirectory | "katsuraComparison.output")
    for triple in testTriples do (
	(init,nnodes,nedges) := triple;
    	pairs := apply(1, seed -> (
		setRandomSeed randomSeedValue;
    		(p0,x0) := createSeedPair polySystem polys;
    		(node,numPaths) := monodromySolve(polys,p0,{x0},
		    GraphInitFunction=>init,
		    NumberOfNodes=>nnodes,
		    NumberOfEdges=>nedges,
		    SelectEdgeAndDirection=>selectBestEdgeAndDirection,
		    Potential=>potentialE,
		    TargetSolutionCount=>mixedVolume);
		(length node.PartialSols == mixedVolume, numPaths)
		));
    	success := select(pairs,p->first p);
    	(file << " , " << #success)
    	);
    	file << endl;
close file 

end ------------------------------
