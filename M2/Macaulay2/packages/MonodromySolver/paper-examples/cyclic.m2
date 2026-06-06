needsPackage "MonodromySolver"

-- copied from ExampleIdeals.m2
cyclicRoots = (n,kk) -> (
     R := kk[vars(0..n-1)];
     ideal apply(1..n-1, d-> sum(0..n-1, i -> product(d, k -> R_((i+k)%n)))) 
       + ideal(product gens R - 1))

parametrizedCyclic = n -> (
	S := gens cyclicRoots(n,CC);
	R := ring S;
	polys := flatten entries S;
	ind := flatten apply(#polys,i-> -- indices for parameters
		apply(exponents polys#i, t->(i,t))
		);
	AR := CC[apply(ind,i->A_i)][gens R];
	polysP := for i to #polys-1 list -- system with parametric coefficients and same support 
	sum(exponents polys#i, t->A_(i,t)*AR_(t));
	polySystem transpose matrix {polysP}
);

end 
--------------------------------------------------------------
-- push F11 starting on the line below
restart
load "cyclic.m2"

polys = parametrizedCyclic 7;


-* each case is a quadruple (
        type of graph, 
      	#nodes,
	#edges,
	"edge selection strategy" => "potential" -- "potential" = null if potential is not used
	)
*-

setRandomSeed 0

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

(p0,x0) = createSeedPair polySystem polys;
mixedVolume = computeMixedVolume specializeSystem (p0,polys)
numSeeds = 10

file = openOut (currentFileDirectory | "cyclic-7" | ".output")

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
		    						SelectEdgeAndDirection=>first opt,
		    						Potential=>last opt,
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

end
