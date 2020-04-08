load "../paper-examples/cyclic.m2" 
n = 7
polys = parametrizedCyclic n


-* each case is a quadruple (
        type of graph, 
      	#nodes,
	#edges,
	"edge selection strategy" => "potential" -- "potential" = null if potential is not used
	)
*-
testTriples = {
    (completeGraphInit, 3, 2), 
    (flowerGraphInit, 3, 2)
    }
testOptions = {
1,
10,
50,
100
    }

(p0,x0) = createSeedPair polySystem polys;
mixedVolume = computeMixedVolume specializeSystem (p0,polys)
numSeeds = 20

file = openOut (currentFileDirectory | "cyclic-batchsize-" | n | ".output")

file << "\\begin{array}{|c||" << concatenate(#testTriples:"c|") << "} " << endl;
for opt in testOptions do (
    file << opt;
    for triple in testTriples do (
	(init,nnodes,nedges) := triple;
    	pairs := apply(numSeeds, seed -> (
		setRandomSeed seed;
    		(p0,x0) := createSeedPair polySystem polys;
    		(node,numPaths) := monodromySolve(polys,p0,{x0},
		    GraphInitFunction=>init,
		    NumberOfNodes=>nnodes,
		    NumberOfEdges=>nedges,
		    SelectEdgeAndDirection=>selectBestEdgeAndDirection,
		    Potential=>potentialE,
		    BatchSize=>opt,
		    TargetSolutionCount=>mixedVolume);
		(length node.PartialSols == mixedVolume, numPaths)
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
