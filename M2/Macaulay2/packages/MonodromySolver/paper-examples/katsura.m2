needsPackage "MonodromySolver"

-- copied from ExampleIdeals.m2
katsura = (n,kk) -> (
     -- This is written to match the Singular version, which seems to differ
     -- from the POSSO version
     n = n-1;
     R := kk[vars(0..n)];
     L := gens R;
     u := (i) -> (
	  if i < 0 then i = -i;
	  if i <= n then L_i else 0_R);
     f1 := -1 + sum for i from -n to n list u i;
     I := ideal prepend(f1,
	  apply(0..n-1, i -> (
	       - u i + sum(-n..n, j -> (u j) * (u (i-j)))
	       )))
     )

parametrizedKatsura = n -> (
	S := gens katsura(n,CC);
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


numSeeds = 20;

for n from 5 to 10 do (
	setRandomSeed 0;
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
  };

	(p0,x0) = createSeedPair polySystem polys;
	mixedVolume = computeMixedVolume specializeSystem (p0,polys);
	file = openOut (currentFileDirectory | "katsura-" | n | ".output");

	file << "\\begin{array}{|c||" << concatenate(#testTriples:"c|") << "} " << endl;
  for triple in testTriples do (
		(init,nnodes,nedges) := triple;
  	pairs := apply(numSeeds, seed -> (
			print("n"=>n, "seed"=>seed);
			setRandomSeed seed;
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
		if #success > 0 then
			(file << " & " << 100.*#success/#pairs << "%" << ",\\ " << toRR sum(success,p->last p)/#success)
		else
			(file << " & " << 100.*#success/#pairs << "%" << ",\\ - ")
	);
  	
  file << "\\\\" << endl;
	file << "\\end{array}" << endl;
	close file ;
);

end
