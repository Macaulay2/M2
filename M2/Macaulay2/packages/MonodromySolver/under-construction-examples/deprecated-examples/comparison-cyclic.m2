load "../paper-examples/cyclic.m2" 
-* each case is a quadruple (
        type of graph, 
      	#nodes,
	#edges,
	"edge selection strategy" => "potential" -- "potential" = null if potential is not used
	)
*-
testTriples = {
    (completeGraphInit, 2, 3), 
    (completeGraphInit, 2, 4), 
    (completeGraphInit, 2, 5), 
    (completeGraphInit, 3, 2), 
    (completeGraphInit, 4, 1), 
    (completeGraphInit, 5, 1), 
    (flowerGraphInit, 3, 2), -- two petals
    (flowerGraphInit, 4, 2), 
    (flowerGraphInit, 5, 2), 
    (flowerGraphInit, 3, 3),
    (flowerGraphInit, 4, 3)
    }
testOptions = {
    selectRandomEdgeAndDirection => null,
    selectBestEdgeAndDirection => potentialLowerBound,
    selectBestEdgeAndDirection => potentialE
    }

n = 5
polys = parametrizedCyclic n;
(p0,x0) = createSeedPair polySystem polys;
mixedVolume = computeMixedVolume specializeSystem (p0,polys)
numSeeds = 10

file = openOut (currentFileDirectory | "cyclic-" | n | ".output")         
file << "\\begin{array}{|c||" << concatenate(#testTriples:"c|") << "} " << endl;
for opt in testOptions do (
    file << opt;
    for triple in testTriples do (
	print triple;
	(init,nnodes,nedges) := triple;
    	pairs := apply(numSeeds, seed -> (
		setRandomSeed seed;
    		(p0,x0) := createSeedPair polySystem polys;
    		(node,numPaths) := monodromySolve(polys,p0,{x0},
		    GraphInitFunction=>init,
		    NumberOfNodes=>nnodes,
		    NumberOfEdges=>nedges,
		    SelectEdgeAndDirection=>first opt,
		    Potential=>last opt,
		    TargetSolutionCount=>mixedVolume);
		(length node.PartialSols == mixedVolume, numPaths)
		));
    	success := select(pairs,p->first p);
    	file << " & " << 100.*#success/#pairs << "\\%" << ",\\ " << toRR sum(success,p->last p)/#success 
    	);
    file << "\\\\" << endl;
    )
file << "\\end{array}" << endl;
close file 

end ------------------------------
restart
elapsedTime load "comparison-cyclic.m2"
