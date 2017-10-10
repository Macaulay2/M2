needsPackage "MonodromySolver"
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
);

runNaiveDynamicMonodromy = polys -> (
	(p0,x0) := createSeedPair polySystem polys;

	targetSystem := x0#SolutionSystem;

	MV := computeMixedVolume (ideal(targetSystem))_*;

	nextP := ((p0)->point {apply(#coordinates p0, i->exp(2*pi*ii*random RR))});

	sols := pointArray {x0};
	PathTrackCount := 0;
	<< "Mixed volume: " << MV << endl;
	while length sols < MV do (
		petalSystem := polySystem specializeSystem (nextP(p0),polys);

		newSols := track(
			petalSystem,
			targetSystem,
			track(
				targetSystem,
				petalSystem,
				points sols,
				NumericalAlgebraicGeometry$gamma=>random CC),
			NumericalAlgebraicGeometry$gamma=>random CC);

		PathTrackCount = PathTrackCount + #newSols * 2;
		for sol in newSols do (
			if not member(sol,sols) then (
				appendPoint(sols,sol);
			);
		);
		<< "#sols: " << length sols << endl;
	);

	<< "#Path tracks: " << PathTrackCount << endl;
);

runNaiveDynamicMonodromy parametrizedCyclic 8
