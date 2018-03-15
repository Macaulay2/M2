load "../paper-examples/example-CRN.m2"
-* each case is a quadruple (
        type of graph, 
      	#nodes,
	#edges,
	"edge selection strategy" => "potential" -- "potential" = null if potential is not used
	)
*-
testTriples = {
    (completeGraphInit, 2, 4), 
    (completeGraphInit, 2, 5), 
    (completeGraphInit, 2, 7), 
    (completeGraphInit, 3, 2), 
    (completeGraphInit, 3, 7), 
    (completeGraphInit, 5, 2),
    (completeGraphInit, 6, 1), 
    (flowerGraphInit, 3, 2), -- two petals
    (flowerGraphInit, 4, 5), 
    (flowerGraphInit, 5, 4), 
    (flowerGraphInit, 3, 6),
    (flowerGraphInit, 4, 5)
    }
testOptions = {
    selectRandomEdgeAndDirection => null,
    selectBestEdgeAndDirection => potentialLowerBound,
    selectBestEdgeAndDirection => potentialE
    }

n = 1
R = wnt()
polys = createPolySystem(R, CC);
(p0,x0) = createSeedPair (polySystem polys,"initial parameters" => "random");
solsCount = 9
numSeeds = 3

file = openOut (currentFileDirectory | "CRN-" | n | ".output")
file << "\\begin{array}{|c||" << concatenate(#testTriples:"c|") << "} " << endl;
for opt in testOptions do (
    file << opt;
    for triple in testTriples do (
	(init,nnodes,nedges) := triple;
    	pairs := apply(numSeeds, seed -> (
		setRandomSeed seed;
    		(p0,x0) := createSeedPair (polySystem polys,"initial parameters" => "random");
    		(node,numPaths) := monodromySolve(polys,p0,{x0},
		    GraphInitFunction=>init,
		    NumberOfNodes=>nnodes,
		    NumberOfEdges=>nedges,
		    SelectEdgeAndDirection=>first opt,
		    Potential=>last opt,
		    TargetSolutionCount=>solsCount,
		    Verbose=>true);
		(length node.PartialSols == solsCount, numPaths)
		));
    	success := select(pairs,p->first p);
    	if #success > 0 then
    	(file << " & " << 100.*#success/#pairs << "%" << ",\\ " << toRR sum(success,p->last p)/#success) else
    	(file << " & " << 100.*#success/#pairs << "%" << ",\\ - ")
    	);
    file << "\\\\" << endl;
    )
file << "\\end{array}" << endl;
close file 

end ------------------------------
restart
elapsedTime load "comparison-CRN.m2"
