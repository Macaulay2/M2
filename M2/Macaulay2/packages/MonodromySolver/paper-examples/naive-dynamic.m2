needsPackage "MonodromySolver"
needsPackage "ExampleIdeals"

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
	PathTrackCount
);

end
restart
needs "naive-dynamic.m2"
needs "cyclic.m2"
L = for i to 10 list 
runNaiveDynamicMonodromy parametrizedCyclic 7
sum L / 10. -- 7976.2

restart
load "naive-dynamic.m2"
needs "cyclic.m2"
elapsedTime runNaiveDynamicMonodromy parametrizedCyclic 10

restart
load "naive-dynamic.m2"
needs "cyclic.m2"
elapsedTime runNaiveDynamicMonodromy parametrizedCyclic 11

restart
load "naive-dynamic.m2"
load "large-examples/noon10.m2"
elapsedTime runNaiveDynamicMonodromy parametrizedNoon 10 
