setRandomSeed 0;
needsPackage "MonodromySolver"
needs "cyclic.m2"

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
		PathTrackCount = PathTrackCount + length sols;
    	    	sols1 := select(track(
			targetSystem,
			petalSystem,
			points sols,
			NumericalAlgebraicGeometry$gamma=>random CC), 
		    s->status s==Regular);
		PathTrackCount = PathTrackCount + length sols1;	
		newSols := select(track(
			petalSystem,
			targetSystem,
			sols1,
			NumericalAlgebraicGeometry$gamma=>random CC),
		    s->status s==Regular);
		for sol in newSols do (
			if not member(sol,sols) then (
				appendPoint(sols,sol);
			);
		);
		<< "#sols: " << length sols << endl;
	);
    	if length sols > MV then (
	    print "FAILED!!!";
	    infinity
	    ) else (
	    << "#Path tracks: " << PathTrackCount << endl;
	    PathTrackCount
	    )
)

end
restart
needs "naive-dynamic.m2"
L = for i to 10 list 
runNaiveDynamicMonodromy parametrizedCyclic 7
sum L / 10. -- 7486

restart
load "naive-dynamic.m2"
elapsedTime runNaiveDynamicMonodromy parametrizedCyclic 10

restart
load "naive-dynamic.m2"
elapsedTime runNaiveDynamicMonodromy parametrizedCyclic 11

restart
load "naive-dynamic.m2"
load "large-examples/noon10.m2"
elapsedTime runNaiveDynamicMonodromy parametrizedNoon 10 
-- #Path tracks: 495692
     -- 7651.17 seconds elapsed
