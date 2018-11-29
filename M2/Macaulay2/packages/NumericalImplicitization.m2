newPackage("NumericalImplicitization",
    Headline => "NumericalImplicitization",
    Version => "1.0.4",
    Date => "December 21, 2017",
    Authors => {
        {Name => "Justin Chen",
	 Email => "jchen@math.berkeley.edu",
         HomePage => "https://math.berkeley.edu/~jchen"},
        {Name => "Joe Kileel",
	 Email => "jkileel@princeton.edu",
	 HomePage => "http://www.math.berkeley.edu/~jkileel"}
        },
    PackageImports => {},
    PackageExports => {"NumericalAlgebraicGeometry"},
    Reload => true,
    DebuggingMode => true
    )
    export {
        "numericalSourceSample",
        "numericalImageSample",
        "numericalNullity",
            "Precondition",
        "numericalImageDim",
        "numericalHilbertFunction",
            "isGraded",
	    --"Threshold",
	"NumericalInterpolationTable",
	    "hilbertFunctionArgument",
	    "hilbertFunctionValue",
	    "imagePoints",
	    "interpolationBasis",
	    "interpolationSVD",
	"extractImageEquations",
	    "attemptExact",
	"numericalImageDegree",
	    "repeats",
            "maxAttempts",
            "traceThreshold",
            "pointEqualityThreshold",
            "maxThreads",
	"PseudoWitnessSet",
	    "isCompletePseudoWitnessSet",
            "imageDegree",
	    "sourceEquations",
            "sourceSlice",
            "imageSlice",
            "witnessPointPairs",
            "traceTest",
	"isOnImage"
    }

-- software options (none for sample nor refine): default is M2engine throughout
-- n.b.: precision loss from LAPACK in SVD computation
-- Magic numbers: 8 (and 3) decimal places in extractImageEquations, 10 in fiberSlice, 50 in smartTrack (for parallelization), 4 (translationMagnitude) in doTraceTest


NumericalInterpolationTable = new Type of HashTable
NumericalInterpolationTable.synonym = "numerical interpolation table"
globalAssignment NumericalInterpolationTable
net NumericalInterpolationTable := T -> (
    	(net ofClass class T | ", indicating") ||
	("the number of degree " | (toString T.hilbertFunctionArgument) | 
        " forms in the ideal of the image is " | (toString T.hilbertFunctionValue))
)

PseudoWitnessSet = new Type of HashTable
PseudoWitnessSet.synonym = "pseudo-witness set"
globalAssignment PseudoWitnessSet
net PseudoWitnessSet := W -> (
    	(net ofClass class W | ", indicating") ||
	("the degree of the image is " | (toString W.imageDegree))
)


checkRings = method(Options => {symbol isGraded => true})
-- Turns F to a list of polynomials, checks if the underlying coefficient fields of F and I are the same complex field, and converts F, I to the affine cone if isGraded is false
checkRings (Thing, Ideal) := Sequence => opts -> (F, I) -> (
    if class F === RingMap then F = F.matrix;
    if class F === Matrix then F = flatten entries F;
    if not class F === List then error "Expected map to be given by a list of polynomials";
    if not ring first F === ring I then error "Expected same rings for ideal and map";
    if not class coefficientRing ring I === ComplexField then print "Warning: expected coefficient field to be complex numbers";
    if not opts.isGraded then (
        t := symbol t;
        R := (coefficientRing ring I)(monoid[append(gens ring I, t)]);
        toR := map(R, ring I);
        ((last gens R)*append(apply(F, f -> toR(f)), 1_R), toR(I))
    ) else (F, I)
)


numericalSourceSample = method(Options => {Software => M2engine})
numericalSourceSample (Ideal, ZZ) := List => opts -> (I, sampleSize) -> ( --outputs a list of random sample points of V(I)
    if I == 0 then toList(1..sampleSize)/(i -> point{apply(gens(ring(I)), x -> random coefficientRing(ring(I)))})
    else numericalSourceSample(I, first components(numericalIrreducibleDecomposition(I, opts)), sampleSize)
)
numericalSourceSample (Ideal, Thing, ZZ) := List => opts -> (I, W, sampleSize) -> (
    if I == 0 then return numericalSourceSample(I, sampleSize);
    samplePoints := toList(1..sampleSize)/(i -> sample(W));
    if precision(ring(I)) <= precision(ring(first (first samplePoints)#Coordinates)) then return samplePoints;
    refine(polySystem(I_*), samplePoints, Bits => precision(ring(I)))
)
numericalSourceSample (Ideal) := Point => opts -> (I) -> first numericalSourceSample(I, 1, opts)
numericalSourceSample (Ideal, Thing) := Point => opts -> (I, W) -> first numericalSourceSample(I, W, 1, opts)

    
numericalImageSample = method(Options => {Software => M2engine})
numericalImageSample (Thing, Ideal, ZZ) := List => opts -> (F, I, sampleSize) -> (
    (F, I) = checkRings(F, I);
    apply(numericalEval(F, numericalSourceSample(I, sampleSize, opts), false), p -> point p)
)
numericalImageSample (Thing, Ideal) := Point => opts -> (F, I) -> first numericalImageSample(F, I, 1, opts)


numericalDimensions = method(Options => {Software => M2engine})
numericalDimensions (Thing, Ideal, Thing) := List => opts -> (F, I, p) -> ( --outputs {dim(V(I)), dim(F(V(I))}
    (F, I) = checkRings(F, I);
    if p === {} then p = numericalSourceSample(I, Software => opts.Software);
    sourceJacobian := sub(transpose(jacobian(I)), matrix p);
    dF := sub(transpose(jacobian(matrix{F})), matrix p);
    sourceDim := numericalNullity(sourceJacobian, false);
    {sourceDim, sourceDim - numericalNullity(sourceJacobian || dF, false)}
)
numericalDimensions (Thing, Ideal) := ZZ => opts -> (F, I) -> numericalDimensions(F, I, {}, opts)


numericalImageDim = method(Options => {Software => M2engine})
numericalImageDim (Thing, Ideal, Point) := ZZ => opts -> (F, I, p) -> last numericalDimensions(F, I, p, opts)
numericalImageDim (Thing, Ideal) := ZZ => opts -> (F, I) -> last numericalDimensions(F, I, opts)


numericalNullity = method(Options => {symbol Threshold => 200, Verbose => false, symbol Precondition => true})
numericalNullity (List, Boolean) := List => opts -> (M, keepSVD) -> ( -- M is a list of 1-element lists of row matrices (to normalize rows easily)
    isZero := true;
    if opts.Verbose then (
        if opts.Precondition then (
            print "Performing normalization preconditioning ...";
            time A := matrix apply(M, row -> if row#0 == 0 then row else (isZero = false; (1/norm(2,row#0))*row));
            if isZero then return if keepSVD then {numcols A, 0} else numcols A;
        ) else A = matrix M;
	print "Computing numerical kernel ...";
	time (S, U, Vt) := SVD(A, DivideConquer => true);
    ) else (
        if opts.Precondition then (
            A = matrix apply(M, row -> if row#0 == 0 then row else (isZero = false; (1/norm(2,row#0))*row));
            if isZero then return if keepSVD then {numcols A, 0} else numcols A;
        ) else A = matrix M;
        (S, U, Vt) = SVD(A, DivideConquer => true);
    );
    largestGap := (#S, opts.Threshold);
    for i from 1 to #S-1 do (
        if S#i == 0 then (
            if first largestGap == #S then largestGap = (i, "infinity");
            break;
        ) else if S#(i-1)/S#i > last largestGap then largestGap = (i, S#(i-1)/S#i);
    );
    if keepSVD then {numcols A - first largestGap, (S, U, Vt)} else numcols A - first largestGap
)
numericalNullity (Matrix, Boolean) := ZZ => opts -> (M, keepSVD) -> numericalNullity(apply(entries M, row -> {matrix{row}}), keepSVD, opts)
numericalNullity Matrix := ZZ => opts -> M -> numericalNullity(M, false, opts)


numericalHilbertFunction = method(Options => {
	Software => M2engine, 
	Threshold => 200, 
	Verbose => true, 
	isGraded => true})
numericalHilbertFunction (Thing, Ideal, List, ZZ) := NumericalInterpolationTable => opts -> (F, I, sampleImagePoints, d) -> ( --outputs a degree d interpolation table for F(V(I))
    (F, I) = checkRings(F, I, isGraded => opts.isGraded);
    if not opts.isGraded then sampleImagePoints = apply(sampleImagePoints, p -> {append(p#Coordinates, 1_(coefficientRing ring I))});
    y := symbol y;
    allMonomials := basis(d, (coefficientRing ring I)[y_0..y_(#F-1)]);
    if #sampleImagePoints < numcols allMonomials then (
        if opts.Verbose then (
            print "Sampling image points ...";
            time sampleImagePoints = join(sampleImagePoints, numericalImageSample(F, I, numcols allMonomials - #sampleImagePoints, Software => opts.Software));
        ) else sampleImagePoints = join(sampleImagePoints, numericalImageSample(F, I, numcols allMonomials - #sampleImagePoints, Software => opts.Software));
    );
    if opts.Verbose then (
        print "Creating interpolation matrix ...";
        time interpolationMatrix := apply(toList(0..<numcols allMonomials), i -> {sub(allMonomials, matrix sampleImagePoints#i)});
    ) else (
        interpolationMatrix = apply(toList(0..<numcols allMonomials), i -> {sub(allMonomials, matrix sampleImagePoints#i)});
    );
    interpolationData := numericalNullity(interpolationMatrix, true, Verbose => opts.Verbose);
    new NumericalInterpolationTable from {
        symbol hilbertFunctionArgument => d,
        symbol hilbertFunctionValue => first interpolationData,
        symbol imagePoints => VerticalList sampleImagePoints,
	symbol interpolationBasis => allMonomials,
        symbol interpolationSVD => last interpolationData,
	symbol map => F
    }
)
numericalHilbertFunction (Thing, Ideal, ZZ) := NumericalInterpolationTable => opts -> (F, I, d) -> numericalHilbertFunction(F, I, {}, d, opts)



extractImageEquations = method(Options => {attemptExact => false})
extractImageEquations (NumericalInterpolationTable) := Matrix => opts -> T -> (
    s := #first T.interpolationSVD;
    kernelDim := T.hilbertFunctionValue;
    V := conjugate last T.interpolationSVD;
    allMonomials := transpose T.interpolationBasis;
    C := ring allMonomials;
    if kernelDim == 0 then return {map(C^1, C^1, 0), map(C^1, C^1, 0)};
    E := V^{s-kernelDim..s-1};
    if not opts.attemptExact then return (numcols allMonomials)*E*allMonomials;
    M := 1e9*(matrix apply(entries E, r -> apply(r, e -> round(8, e))));
    transpose matrix{apply(flatten entries(mingens ideal(M*allMonomials)), f -> roundPoly(3, f))}
)
extractImageEquations (Thing, Ideal, ZZ) := Matrix => opts -> (F, I, d) -> extractImageEquations(numericalHilbertFunction(F, I, d), opts)


roundPoly = method()
roundPoly (ZZ, RingElement) := RingElement => (n, f) -> (
	toBaseField := map(coefficientRing ring f, ring f);
	sum((terms f)/(t -> (coeff := toBaseField((last coefficients t)_(0,0)); round(n, coeff)*(first coefficients t)_(0,0))))
)


round (ZZ, ZZ) := ZZ => (n, x) -> x
round (ZZ, CC) := CC => (n, x) -> round(n, realPart x) + ii*round(n, imaginaryPart x)


numericalImageDegree = method(Options => {
	Software => M2engine, 
	Verbose => true, 
	symbol repeats => 3, 
	symbol traceThreshold => 1e-5, 
	symbol maxAttempts => 5, 
        symbol maxThreads => 1,
	symbol pointEqualityThreshold => 5, 
	isGraded => true})
numericalImageDegree (Thing, Ideal, Thing, Point) := PseudoWitnessSet => opts -> (F, I, W, sourcePoint) -> ( --outputs a pseudo-witness set for F(V(I))
    local pairTable, local imagePoint, local startSystem;
    y := symbol y;
    targetRing := (coefficientRing(ring(I)))[y_1..y_(#F)];
    dims := numericalDimensions(F, I, sourcePoint);
    numFailedTraceTests := 0;
    traceResult := opts.traceThreshold + 1;
    while not traceResult < opts.traceThreshold and numFailedTraceTests < opts.maxAttempts do (
        if numFailedTraceTests > 0 and not W === null then (
	    if W === {} and not I == 0 then W = first components numericalIrreducibleDecomposition(I, Software => opts.Software);
	    sourcePoint = numericalSourceSample(I, W);
	);
	newSamplePair := first numericalEval(F, {sourcePoint}, true);
        pullbackSliceData := randomCombinations(F, last dims, true);
        sliceTranslation := transpose sub(matrix{last pullbackSliceData}, matrix sourcePoint);
        pullbackSlice := (last pullbackSliceData) - flatten entries sliceTranslation;
        sliceCoefficients := promote((first pullbackSliceData) | (-1)*sliceTranslation, targetRing);
        if first dims > last dims then (
            fiberSlice := randomCombinations(gens(ring(I)) | {10_(ring(I))}, (first dims) - (last dims), false);
            fiberSlice = fiberSlice - flatten entries sub(matrix{fiberSlice}, matrix sourcePoint);
        ) else fiberSlice = {};
	squaredUpSource := (if I == 0 then {} else randomCombinations(I_*, #gens(ring(I)) - first dims, false));
	newStartSystem := squaredUpSource | fiberSlice | pullbackSlice;
        if numFailedTraceTests > 0 then (
            newTrackedPairs := numericalEval(F, smartTrack(startSystem, newStartSystem, apply(values pairTable, pair -> first pair), true, opts), true);
            pairTable = new MutableHashTable;
            for newPair in newTrackedPairs do (
                imagePoint = apply(flatten entries last newPair, c -> round(opts.pointEqualityThreshold, c));
                if not pairTable#?imagePoint then pairTable#imagePoint = newPair;
            );
        ) else pairTable = new MutableHashTable;
        imagePoint = apply(flatten entries last newSamplePair, c -> round(opts.pointEqualityThreshold, c));
        if not pairTable#?imagePoint then (
            pairTable#imagePoint = newSamplePair;
            if opts.Verbose and numFailedTraceTests > 0 then print "Added new image point";
        );
	startSystem = newStartSystem;
        pointPairs := monodromyLoop(F, last dims, startSystem, pairTable, opts);
        if opts.Verbose then print("Running trace test ...");
        traceResult = doTraceTest(F, last dims, startSystem, pointPairs, opts);
        if not traceResult < opts.traceThreshold then (
            if opts.Verbose then print("Failed trace test! Trace: " | toString traceResult);
            numFailedTraceTests = numFailedTraceTests + 1;
        );
    );
    if opts.Verbose then (
	if traceResult > opts.traceThreshold then (
            print("Degree of image should be at least " | #pointPairs);
            print("Consider changing parameters (repeats, maxAttempts, traceThreshold, pointEqualityThreshold) or reparametrizing for a better result. Alternatively, consider increasing precision (e.g. changing ground field to CC_100).");
        );
    );
    new PseudoWitnessSet from {
        symbol isCompletePseudoWitnessSet => traceResult < opts.traceThreshold,
        symbol imageDegree => #pointPairs,
        symbol map => F,
        symbol sourceEquations => I,
        symbol sourceSlice => transpose matrix{fiberSlice},
        symbol imageSlice => sliceCoefficients*((transpose vars targetRing) || matrix{{1_targetRing}}),
        symbol witnessPointPairs => VerticalList apply(pointPairs, pair -> (first pair, point last pair)),
	symbol traceTest => traceResult
    }
)
numericalImageDegree (Thing, Ideal) := PseudoWitnessSet => opts -> (F, I) -> (
    (F, I) = checkRings(F, I, isGraded => opts.isGraded);
    if opts.Verbose then print "Sampling point in source ...";
    W := if I == 0 then {} else first components numericalIrreducibleDecomposition(I, Software => opts.Software);
    numericalImageDegree(F, I, W, numericalSourceSample(I, W), opts)
)
numericalImageDegree(Thing, Ideal, Point) := PseudoWitnessSet => opts -> (F, I, p) -> (
    (F, I) = checkRings(F, I, isGraded => opts.isGraded);
    if not opts.isGraded then p = point{append(p#Coordinates, 1_(coefficientRing(ring(I))))};
    numericalImageDegree(F, I, null, p, opts)
)


smartTrack = method(Options => {
	Software => M2engine, 
	Verbose => true, 
	repeats => 3, 
	traceThreshold => 1e-5, 
	maxAttempts => 5, 
        maxThreads => 1,
	pointEqualityThreshold => 5, 
	isGraded => true})
smartTrack (List, List, List, Boolean) := List => opts -> (startSystem, targetSystem, startSolutions, doRefinements) -> (
    randomGamma := random(coefficientRing(ring(first(startSystem))));
    startSystem = polySystem startSystem;
    targetSystem = polySystem targetSystem;
    if #startSolutions > max(20, 2*opts.maxThreads) and opts.maxThreads > 1 then ( -- buggy
        startSolutionsList := pack(ceiling(#startSolutions/opts.maxThreads), startSolutions);
        threadList := {};
        for paths in startSolutionsList do (
            threadList = append(threadList, schedule(track, (startSystem, targetSystem, paths, gamma => randomGamma, Software => opts.Software)));
        );
        while any(threadList, t -> not isReady t) do sleep 1;
        targetSolutions := flatten apply(threadList, t -> taskResult t);
        if opts.Verbose then print("Finished tracking " | #targetSolutions | " paths in parallel");
    ) else targetSolutions = track(startSystem, targetSystem, startSolutions, gamma => randomGamma, Software => opts.Software);
    goodSols := select(targetSolutions, p -> p#?SolutionStatus and p#SolutionStatus == Regular);
    --if opts.Verbose and #goodSols < #startSolutions then print("Paths going to infinity: " | #startSolutions - #goodSols);
    goodSols
)


numericalEval = method()
numericalEval (List, List, Boolean) := List => (F, upstairsPoints, includeUpstairs) -> (
    matrixF := matrix{F}; 
    if includeUpstairs then apply(upstairsPoints, p -> (p, sub(matrixF, matrix p)))
    else apply(upstairsPoints, p -> sub(matrixF, matrix p))
)


randomCombinations = method()
randomCombinations (List, ZZ, Boolean) := List => (polys, c, keepCoeffs) -> ( --outputs a list of c random linear combinations of polys
    C := coefficientRing(ring(first polys));
    randomCoefficients := random(C^c, C^#polys);
    if not keepCoeffs then flatten entries(promote(randomCoefficients,ring(first polys))*transpose(matrix{polys}))
    else (randomCoefficients, flatten entries(promote(randomCoefficients,ring(first polys))*transpose(matrix{polys})))
)


monodromyLoop = method(Options => {
	Software => M2engine, 
	Verbose => true, 
	repeats => 3, 
	traceThreshold => 1e-5, 
	maxAttempts => 5, 
        maxThreads => 1,
	pointEqualityThreshold => 5, 
	isGraded => true})
monodromyLoop (List, ZZ, List, MutableHashTable) := List => opts -> (F, imageDim, startSystem, pairTable) -> (
    numRepetitiveMonodromyLoops := 0;
    if opts.Verbose then print "Tracking monodromy loops ...";
    while numRepetitiveMonodromyLoops < opts.repeats do (
        previousNumImagePoints := #values pairTable;
        intermediateSystem1 := drop(startSystem, -imageDim) | randomCombinations(F | {10_(ring(first(F)))}, imageDim, false);
        intermediateSolutions1 := smartTrack(startSystem, intermediateSystem1, apply(values pairTable, pair -> first pair), false, opts);
        if #intermediateSolutions1 > 0 then (
            endSolutions := smartTrack(intermediateSystem1, startSystem, intermediateSolutions1, false, opts);
            if #endSolutions > 0 then (
                candidatePairs := numericalEval(F, endSolutions, true);
                for newPair in candidatePairs do (
                    imagePoint := apply(flatten entries last newPair, c -> round(opts.pointEqualityThreshold, c));
                    if not pairTable#?imagePoint then pairTable#imagePoint = newPair;
                );
            );
        );
        if previousNumImagePoints < #values pairTable then numRepetitiveMonodromyLoops = 0
        else numRepetitiveMonodromyLoops = numRepetitiveMonodromyLoops + 1;
        if opts.Verbose then print ("Points found: " | #values pairTable);
    );
    values pairTable
)


doTraceTest = method(Options => {
	Software => M2engine, 
	Verbose => true, 
	repeats => 3, 
	traceThreshold => 1e-5, 
	maxAttempts => 5, 
        maxThreads => 1,
	pointEqualityThreshold => 5, 
	isGraded => true})
doTraceTest (List, ZZ, List, List) := RR => opts -> (F, imageDim, startSystem, intersectionPointPairs) -> (
    C := coefficientRing(ring(first F));
    startUpstairsPoints := apply(intersectionPointPairs, pair -> first pair);
    startDownstairsPoints := apply(intersectionPointPairs, pair -> last pair);
    for translationMagnitude from 1 to 4 do (
        randomTranslation := 10^(4-translationMagnitude)*flatten entries(map(C^1, C^(#startSystem - imageDim), 0) | random(C^1, C^imageDim));
        gammas := {random(C), random(C)};
        firstStepSystem := startSystem + (first gammas)*randomTranslation;
        secondStepSystem := startSystem + (last gammas)*randomTranslation;
        firstStepUpstairsPoints := smartTrack(startSystem, firstStepSystem, startUpstairsPoints, true, opts);
        if #firstStepUpstairsPoints == #startUpstairsPoints then (
            secondStepUpstairsPoints := smartTrack(startSystem, secondStepSystem, startUpstairsPoints, true, opts);
            if #secondStepUpstairsPoints == #startUpstairsPoints then (
                firstStepDownstairsPoints := numericalEval(F, firstStepUpstairsPoints, false);
                secondStepDownstairsPoints := numericalEval(F, secondStepUpstairsPoints, false);
                traceList := (1/first gammas)*(firstStepDownstairsPoints - startDownstairsPoints) - (1/last gammas)*(secondStepDownstairsPoints - startDownstairsPoints);
                return norm(2,sum(traceList))
            );
        );
    );
    infinity
)


isOnImage = method(Options => {
	Software => M2engine, 
	Verbose => true, 
	repeats => 3, 
	traceThreshold => 1e-5, 
	maxAttempts => 5, 
        maxThreads => 1,
	pointEqualityThreshold => 5, 
	isGraded => true})
isOnImage (PseudoWitnessSet, Point) := Boolean => opts -> (W, q) -> (
    if not opts.isGraded then (
	q = point{append(q#Coordinates, 1_(ring(first(q#Coordinates))))};
    );
    if not W.isCompletePseudoWitnessSet then print "Warning: not a complete pseudo-witness set! May return false negative.";
    F := W.map;
    I := W.sourceEquations;
    if not ring first q#Coordinates === coefficientRing ring I then error "Point must have coordinates in the coefficient ring of the ideal.";
    fiberSlice := flatten entries W.sourceSlice;
    targetVariables := gens ring(W.imageSlice);
    pullbackSlice := flatten entries sub(W.imageSlice, apply(toList(0..<#targetVariables), i -> targetVariables#i => F#i));
    squaredUpSource := (if I == 0 then {} else randomCombinations(I_*, #gens(ring(I)) - #fiberSlice - #pullbackSlice, false));
    startUpstairsPoints := apply(W.witnessPointPairs, pair -> first pair);
    newPullbackSliceData := randomCombinations(F, #pullbackSlice, true);
    sliceCoefficients := first newPullbackSliceData;
    newPullbackSlice := last newPullbackSliceData;
    newPullbackSlice = newPullbackSlice - flatten entries (sliceCoefficients * promote(transpose(matrix q), coefficientRing(ring(I))));
    targetUpstairsPoints := smartTrack(squaredUpSource | fiberSlice | pullbackSlice, squaredUpSource | fiberSlice | newPullbackSlice, startUpstairsPoints, true, opts);
    imagePointTable := new HashTable from (numericalEval(F, targetUpstairsPoints, false)/(p -> (flatten entries p)/(c -> round(opts.pointEqualityThreshold, c)) => 0));
    imagePointTable#?(apply(flatten entries matrix q, c -> round(opts.pointEqualityThreshold, c)))
)
isOnImage (Thing, Ideal, Point) := Boolean => opts -> (F, I, q) -> isOnImage(numericalImageDegree(F, I, opts), q, opts)


isWellDefined NumericalInterpolationTable := Boolean => T -> (
    -- CHECK DATA STRUCTURE
    -- CHECK KEYS
    K := keys T;
    expectedKeys := set {
        symbol hilbertFunctionArgument, 
        symbol hilbertFunctionValue, 
        symbol imagePoints, 
        symbol interpolationBasis, 
        symbol interpolationSVD, 
        symbol map
    };
    if set K =!= expectedKeys then (
	if debugLevel > 0 then (
	    added := toList(K - expectedKeys);
	    missing := toList(expectedKeys - K);
	    if #added > 0 then << "-- unexpected key(s): " << toString added << endl;
	    if #missing > 0 then << "-- missing keys(s): " << toString missing << endl;
        );
        return false
    );
    -- CHECK TYPES
    if not instance(T.hilbertFunctionArgument, ZZ) then (
        if debugLevel > 0 then << "-- expected `hilbertFunctionArgument' to be an integer" << endl;
	return false
    );
    if not instance(T.hilbertFunctionValue, ZZ) then (
        if debugLevel > 0 then << "-- expected `hilbertFunctionValue' to be an integer" << endl;
	return false
    );
    if not instance(T.map, List) then (
        if debugLevel > 0 then << "-- expected `map' to be a list" << endl;
	return false
    );
    if not instance(T.interpolationBasis, Matrix) then (
        if debugLevel > 0 then << "-- expected `interpolationBasis' to be a matrix" << endl;
	return false
    );
    if not instance(T.interpolationSVD, Sequence) then (
        if debugLevel > 0 then << "-- expected `interpolationSVD' to be a sequence" << endl;
        return false
    );
    if not instance(first T.interpolationSVD, List) then (
        if debugLevel > 0 then << "-- expected first element of `interpolationSVD' to be a list" << endl;
        return false
    );
    if not all(first T.interpolationSVD, s -> instance(s, RR)) then (
        if debugLevel > 0 then << "-- expected first element of `interpolationSVD' to be a list of singular values" << endl;
        return false
    );
    if not all(drop(T.interpolationSVD, 1), M -> instance(M, Matrix)) then (
        if debugLevel > 0 then << "-- expected second and third elements of `interpolationSVD' to be matrices" << endl;
	return false
    );
    -- CHECK MATHEMATICAL STRUCTURE
    if not unique flatten last degrees T.interpolationBasis === {T.hilbertFunctionArgument} then (
        if debugLevel > 0 then << ("-- expected `interpolationBasis' to consist of monomials of degree " | T.hilbertFunctionArgument) << endl;
        return false
    );
    if not all({coefficientRing ring T.interpolationBasis, ring(T.interpolationSVD#2)}/class, C -> C === ComplexField) then (
        if debugLevel > 0 then << "-- expected ground field to be complex numbers" << endl;
        return false
    );
    numMonomials := binomial(#T.map + T.hilbertFunctionArgument - 1, T.hilbertFunctionArgument);
    if not #gens ring T.interpolationBasis === #T.map or not numcols T.interpolationBasis === numMonomials then (
        if debugLevel > 0 then << ("-- expected `interpolationBasis' to have " | numMonomials | " monomials in " | #T.map | " variables") << endl;
        return false
    );
    true
)


isWellDefined PseudoWitnessSet := Boolean => W -> (
    -- CHECK DATA STRUCTURE
    -- CHECK KEYS
    K := keys W;
    expectedKeys := set {
        symbol isCompletePseudoWitnessSet, 
        symbol imageDegree, 
        symbol map, 
        symbol sourceEquations, 
        symbol sourceSlice, 
        symbol imageSlice, 
        symbol witnessPointPairs, 
        symbol traceTest
    };
    if set K =!= expectedKeys then (
	if debugLevel > 0 then (
	    added := toList(K - expectedKeys);
	    missing := toList(expectedKeys - K);
	    if #added > 0 then << "-- unexpected key(s): " << toString added << endl;
	    if #missing > 0 then << "-- missing keys(s): " << toString missing << endl;
        );
        return false
    );
    -- CHECK TYPES
    if not instance(W.isCompletePseudoWitnessSet, Boolean) then (
        if debugLevel > 0 then << "-- expected `isCompletePseudoWitnessSet' to be a Boolean" << endl;
	return false
    );
    if not instance(W.imageDegree, ZZ) then (
        if debugLevel > 0 then << "-- expected `imageDegree' to be an integer" << endl;
	return false
    );
    if not instance(W.map, List) then (
        if debugLevel > 0 then << "-- expected `map' to be a list" << endl;
	return false
    );
    if not instance(W.sourceEquations, Ideal) then (
        if debugLevel > 0 then << "-- expected `sourceEquations' to be an ideal" << endl;
	return false
    );
    if not instance(W.sourceSlice, Matrix) then (
        if debugLevel > 0 then << "-- expected `sourceSlice' to be a matrix" << endl;
	return false
    );
    if not instance(W.imageSlice, Matrix) then (
        if debugLevel > 0 then << "-- expected `imageSlice' to be a matrix" << endl;
	return false
    );
    if not instance(W.witnessPointPairs, List) then (
        if debugLevel > 0 then << "-- expected `witnessPointPairs' to be a list" << endl;
	return false
    );
    if not all(W.witnessPointPairs, pair -> instance(pair, Sequence)) then (
        if debugLevel > 0 then << "-- expected `witnessPointPairs' to be a list of sequences" << endl;
        return false
    );
    if not all(W.witnessPointPairs, pair -> all(pair, p -> instance(p, Point))) then (
        if debugLevel > 0 then << "-- expected `witnessPointPairs' to be a list of sequences of points" << endl;
        return false
    );
    if not instance(W.traceTest, RR) then (
        if debugLevel > 0 then << "-- expected `traceTest' to be a real number" << endl;
	return false
    );
    -- CHECK MATHEMATICAL STRUCTURE
    R := ring W.sourceEquations;
    if not all(W.map, f -> ring f === R) then (
        if debugLevel > 0 then << "-- expected elements of `map' and `sourceEquations' to live in same ring" << endl;
        return false
    );
    if not all({R, ring W.imageSlice}/coefficientRing/class, C -> C === ComplexField) then (
        if debugLevel > 0 then << "-- expected ground field to be complex numbers" << endl;
        return false
    );
    if not all(W.witnessPointPairs, pair -> #(pair#0#Coordinates) === #gens R and #(pair#1#Coordinates) === #gens ring W.imageSlice) then (
        if debugLevel > 0 then << "-- number of coordinates in `witnessPointPairs' do not match" << endl;
        return false
    );
    true
)


beginDocumentation()

--Documention--
--<<docTemplate
doc ///
    Key
    	NumericalImplicitization
    Headline
    	implicitization using numerical algebraic geometry
    Description
    	Text
	    This package supports user-friendly calculation of basic invariants of the image of a polynomial 
	    map. The computational techniques (interpolation, homotopy continuation and monodromy) come 
            from numerical algebraic geometry.

	    Many varieties of interest in algebraic geometry and its applications are usefully described as 
	    images of polynomial maps, via a parametrization. Implicitization is the process of converting 
	    a parametric description of a variety into an intrinsic, or implicit, description. Classically, 
	    implicitization refers to the procedure of computing the defining equations of a parametrized 
	    variety, and in theory this is accomplished by finding the kernel of a ring homomorphism, via 
	    Gr&ouml;bner bases. In practice however, symbolic Gr&ouml;bner basis computations are often 
            time-consuming, even for medium-scale problems, and do not scale well with respect to the size 
            of the input.

	    Despite this, one would often like to know basic information about a parametrized variety, even 
	    when symbolic methods are prohibitively expensive. Examples of 
	    such information are discrete invariants such as the @TO2{numericalImageDim, "dimension"}@, the 
	    @TO2{numericalImageDegree, "degree"}@, or @TO2{numericalHilbertFunction, "Hilbert function"}@ 
	    values. Other examples include Boolean tests, for example whether or not a particular point 
	    @TO2{isOnImage, "lies on"}@ a parametrized variety. The goal of this package is to provide such 
	    information; in other words to numerically implicitize a parametrized variety.
    
	    {\em NumericalImplicitization} builds on top of existing numerical algebraic geometry software: 
	    @TO2{NumericalAlgebraicGeometry,"NAG4M2"}@, @TO Bertini@ and @TO PHCpack@.  Each of 
            these can be used for path tracking and point sampling; by default, the native engine 
	    NAG4M2 is used.  Current implementatation is for reduced and irreducible varieties.
    
	    {\bf Reference:} 
            
            [1] A.J. Sommese and C.W. Wampler, 
            The numerical solution of systems of polynomials.
            {\it World Scientific Publishing} (2005).
///

doc ///
    Key
    	numericalSourceSample
	(numericalSourceSample, Ideal, Thing, ZZ)
        (numericalSourceSample, Ideal, Thing)
	(numericalSourceSample, Ideal, ZZ)
        (numericalSourceSample, Ideal)
    Headline
    	samples a general point on a variety
    Usage
    	numericalSourceSample(I, s)
	numericalSourceSample(I)
    Inputs
	I:Ideal
	    which is prime, specifying a variety $V(I)$
	s:ZZ
	    the number of points to sample on $V(I)$
    Outputs
    	:List
	    of sample points on $V(I)$
    Description
	Text
	    Computes a list of sample points on a variety numerically. If $I$ is the zero ideal 
	    in a polynomial ring of dimension $n$, then an $n$-tuple of random elements in the 
	    ground field is returned. Otherwise, a @TO2{numericalIrreducibleDecomposition, 
            "numerical irreducible decomposition"}@ of $I$ is computed, which is then used 
            to sample points.

	    If $s$ is unspecified, then it is assumed that $s = 1$. In this case, the single 
	    point is returned, rather than a list.

	    In the example below, we sample a point from $A^3$ and then $3$ points from
	    $V(x^2 + y^2 + z^2 - 1)$ in $A^3$.
            
        Example
            R = CC[x,y,z];
            numericalSourceSample(ideal 0_R)
            I = ideal(x^2 + y^2 + z^2 - 1);
            numericalSourceSample(I, 3)
    Caveat
	Since numerical irreducible decompositions are done over CC, if $I$ is not the zero 
	ideal, then the output will be a point in complex space 
	(regardless of the ground field of the ring of $I$).
    SeeAlso
        numericalImageSample
///

doc ///
    Key
    	numericalImageSample
	(numericalImageSample, Thing, Ideal, ZZ)
	(numericalImageSample, Thing, Ideal)
    Headline
    	samples general points on the image of a variety
    Usage
    	numericalImageSample(F, I, s)
	numericalImageSample(F, I)
    Inputs
    	F:Thing
	    a list, or matrix, or ring map, specifying a map
	I:Ideal
	    which is prime, specifying a source variety $V(I)$
	s:ZZ
	    the number of points to sample in $F(V(I))$
    Outputs
    	:List
	    of sample points on $F(V(I)))$
    Description
	Text
	    Computes a list of sample points on the image of a variety numerically. 
	    This function calls @TO numericalSourceSample@.

	    If $s$ is unspecified, then it is assumed that $s = 1$. In this case, 
	    the single point is returned, rather than a list.

	    The following samples a point from the twisted cubic. We then 
            independently verify that this point does lie on the twisted cubic.
            
        Example
            R = CC[s,t];
            F = {s^3,s^2*t,s*t^2,t^3};
            p = numericalImageSample(F, ideal 0_R)
            A = matrix{p#Coordinates_{0,1,2}, p#Coordinates_{1,2,3}};
            numericalRank A == 1
    	Text
        
	    Here is how to sample a point from the Grassmannian $Gr(3,5)$ of 
	    $P^2$'s in $P^4$, under its Pl&uuml;cker embedding.
            We take maximal minors of a $3 x 5$ matrix, whose row span
            gives a $P^2$ in $P^4$.
            
        Example
            R = CC[x_(1,1)..x_(3,5)];
            F = (minors(3, genericMatrix(R, 3, 5)))_*;
            numericalImageSample(F, ideal 0_R)
    SeeAlso
        numericalSourceSample
///

doc ///
    Key
    	numericalImageDim
	(numericalImageDim, Thing, Ideal, Point)
	(numericalImageDim, Thing, Ideal)
    Headline
    	computes the dimension of the image of a variety
    Usage
    	numericalImageDim(F, I, p)
	numericalImageDim(F, I)
    Inputs
    	F:Thing
	    a list, or matrix, or ring map, specifying a map
	I:Ideal
	    which is prime, specifying a source variety $V(I)$
	p:Point
	    a sample point on $V(I)$
    Outputs
    	:ZZ
	    the dimension of $F(V(I)))$
    Description
	Text
	    Computes the dimension of the image of a variety numerically. 
	    Even if the source variety and map are projective, the affine (= Krull) 
            dimension is returned. This ensures consistency with @TO dim@.

	    The following computes the affine dimension of the Grassmannian $Gr(3,5)$ 
            of $P^2$'s in $P^4$, under its Pl&uuml;cker embedding.
            
        Example
            R = CC[x_(1,1)..x_(3,5)];
            F = (minors(3, genericMatrix(R, 3, 5)))_*;
            numericalImageDim(F, ideal 0_R)
        Text
        
            For comparison, here is how to do the same computation symbolically.
            
        Example
            R = QQ[x_(1,1)..x_(3,5)];
            F = (minors(3, genericMatrix(R, 3, 5)))_*;
            dim ker map(R,QQ[y_0..y_(#F-1)],F)
        Text
        
            Here is an example where direct symbolic computation fails to terminate quickly. 
	    Part of the Alexander-Hirschowitz theorem states that the $14$th secant 
	    variety of the $4$th Veronese of $\mathbb{P}^4$ has affine dimension $69$, rather than 
	    the expected $14*4 + 13 + 1 = 70$. We numerically verify this below:
            
        Example
            R = CC[a_(1,1)..a_(14,5)];
            F = sum(1..14, i -> basis(4, R, Variables=>toList(a_(i,1)..a_(i,5))));
            time numericalImageDim(F, ideal 0_R)
    	Text
        
	    {\bf Reference} 
            
            [1] J. Alexander, A. Hirschowitz, 
            Polynomial interpolation in several variables.
            {\it J. Alg. Geom.} 
            {\bf 4} (2) (1995), 201-222.
///

doc ///
    Key
    	numericalHilbertFunction
	(numericalHilbertFunction, Thing, Ideal, List, ZZ)
	(numericalHilbertFunction, Thing, Ideal, ZZ)
        [numericalHilbertFunction, Threshold]
    Headline
    	computes the values of the Hilbert function for the image of a variety
    Usage
    	numericalHilbertFunction(F, I, S, d)
	numericalHilbertFunction(F, I, d)
    Inputs
    	F:Thing
	    a list, or matrix, or ring map, specifying a map
	I:Ideal
	    which is prime, specifying a source variety $V(I)$
	S:List
	    of general points on $F(V(I))$
    	d:ZZ
	    the argument of the Hilbert function of $F(V(I))$
    Outputs
    	:NumericalInterpolationTable
	    containing the number of linearly independent degree $d$ 
            forms in the ideal of the projective closure of $F(V(I))$, 
            along with approximations of those forms
    Description
	Text
	    Computes values of the Hilbert function of the image 
            of a variety, by numerical interpolation. This technique 
            circumvents the calculation of the kernel of the 
            associated ring map.

            In order to speed up computation, the list S of points 
            can be precomputed (see @TO numericalImageSample@). 
            This list of points can then be re-used in multiple 
            interpolation computations (which can yield a dramatic 
            speedup over performing separate sampling instances, 
            if the ideal $I$ is not the zero ideal).

            We compute the number of quartics in the ideal of the 
            twisted cubic to be $22$. One can verify this by inspection 
            as follows: quartics in the coordinate ring pull back to 
            forms of degree $12$ on $P^1$, of which there 
            are $13$; thus there are $35 - 13 = 22$ quartics in the 
            defining ideal.
            
        Example
            R = CC[s,t];
            F = {s^3,s^2*t,s*t^2,t^3};
            numericalHilbertFunction(F, ideal 0_R, 4)
        Text
        
            The following computes the number of Pl&uuml;cker quadrics in the 
            defining ideal of the Grassmannian $Gr(3,5)$ of $P^2$'s in $P^4$.
            
        Example
            R = CC[x_(1,1)..x_(3,5)];
            F = (minors(3, genericMatrix(R, 3, 5)))_*;
            S = numericalImageSample(F, ideal 0_R, 60);
            numericalHilbertFunction(F, ideal 0_R, S, 2)
        Text
        
            The option {\tt Threshold} specifies the minimal gap (= ratio 
            of consecutive singular 
            values) for determining the numerical rank of a matrix. If the 
            largest gap is greater than this threshold, then all singular 
            values after the largest gap are considered as numerically 
            zero; if all gaps are less than this threshold, then the matrix 
            is considered numerically full rank. The default value is $200$.
    Caveat
        The option {\tt Threshold} may require tuning by the user.
        If the value of {\tt Threshold} is too small, the numerical rank may
        be smaller than the true rank (and vice versa if the value of {\tt Threshold}
        is too large).
    SeeAlso
    	NumericalInterpolationTable
        extractImageEquations
///

doc ///
    Key
        isGraded
        [numericalHilbertFunction, isGraded]
        [numericalImageDegree, isGraded]
        [isOnImage, isGraded]
    Headline
        whether input is homogeneous
    Usage
        numericalHilbertFunction(..., isGraded => true)
        numericalImageDegree(..., isGraded => true)
        isOnImage(..., isGraded => true)
    Description
        Text
            Specifies whether or not input (i.e. the ideal $I$ and map $F$)
            is graded. If false, input will be homogenized with respect 
            to a new variable, and internally the target variety is treated
            as the affine cone over its projective closure. Default value is true.
    SeeAlso
        numericalHilbertFunction
        numericalImageDegree
        isOnImage
///

doc ///
    Key
    	NumericalInterpolationTable
        (net, NumericalInterpolationTable)
        hilbertFunctionArgument
        hilbertFunctionValue
        imagePoints
	interpolationBasis
        interpolationSVD
    Headline
    	the class of all NumericalInterpolationTables
    Description
	Text
    	    This is a type of hashtable storing the output of a 
            polynomial interpolation computation, with the following keys: 
        Code
            UL {
                TEX "\\bf hilbertFunctionArgument: the argument, d, to the Hilbert function",
                TEX "\\bf hilbertFunctionValue: the value of the Hilbert function at d",
                TEX "\\bf imagePoints: a VerticalList of sample points on the image",
		TEX "\\bf interpolationBasis: a matrix, consisting of the degree d monomials",
                {TEX "\\bf interpolationSVD: the SVD of the interpolation matrix, 
                obtained by evaluating degree d monomials at the sample points"},
		TEX "\\bf map: the map F, of which the image is under consideration"
                }
        -- Example
        --     R = CC[x_(1,1)..x_(3,5)];
        --     F = (minors(3, genericMatrix(R, 3, 5)))_*;
        --     T = numericalHilbertFunction(F, ideal 0_R, 2, Verbose => false)
        --     (T.hilbertFunctionArgument, T.hilbertFunctionValue)
    SeeAlso
    	numericalHilbertFunction
///

doc ///
    Key
        [numericalHilbertFunction, Verbose]
    	[numericalImageDegree, Verbose]
	[isOnImage, Verbose]
        [numericalNullity, Verbose]
    Headline
    	display detailed output
    Usage
        numericalImageDegree(..., Verbose => true)
	numericalHilbertFunction(..., Verbose => true)
	isOnImage(..., Verbose => true)
        numericalNullity..., Verbose => true)
    Description
	Text
    	    Determines whether or not detailed output is displayed 
            during an interpolation or monodromy computation, 
            including timings for various intermediate computations. 
            Default value is true.
    SeeAlso
        numericalHilbertFunction
    	numericalImageDegree
	isOnImage
///

doc ///
    Key
    	extractImageEquations
        (extractImageEquations, Thing, Ideal, ZZ)
	(extractImageEquations, NumericalInterpolationTable)
        attemptExact
        [extractImageEquations, attemptExact]
    Headline
    	finds implicit equations in a fixed degree for the image of a variety
    Usage
        extractImageEquations(F, I, d)
    	extractImageEquations T
    Inputs
        T:NumericalInterpolationTable
            a numerical interpolation table for $F(V(I))$ of degree $d$
    	F:Thing
	    a list, or matrix, or ring map, specifying a map
	I:Ideal
	    which is prime, specifying a source variety $V(I)$
    	d:ZZ
	    the argument of the Hilbert function of $F(V(I))$
    Outputs
    	:Matrix
	    of implicit degree d equations for $F(V(I))$
    Description
	Text
	    Finds (approximate) implicit degree $d$ equations for the image of a variety. 
            This is done via a numerical interpolation computation for the image.

	    If a numerical interpolation table has already been computed, then 
            to avoid repetitive calculation one may run this function with the interpolation 
            table as input.

            We determine the defining quadrics of the twisted cubic. This is done with 
            the option attemptExact, as follows:
            
        Example
            R = CC[s,t];
            F = {s^3,s^2*t,s*t^2,t^3};
            extractImageEquations (F, ideal 0_R, 2, attemptExact => true)
        Text
        
            Here is how to do the same computation symbolically:
            
        Example
            transpose gens ker map(QQ[s,t], QQ[y_0..y_3], {s^3,s^2*t,s*t^2,t^3})
        Text
        
	    We determine the $5$ Pl&uuml;cker quadrics defining the Grassmannian 
            $Gr(3,5)$.
            
        Example
            R = CC[x_(1,1)..x_(3,5)]; I = ideal 0_R;
            F = (minors(3, genericMatrix(R, 3, 5)))_*;
	    T = numericalHilbertFunction(F, I, 2, Verbose => false);
	    extractImageEquations(T, attemptExact => true)
    Caveat
    	The option attemptExact is experimental, and may result in inaccurate equations.
    SeeAlso
    	numericalHilbertFunction
        NumericalInterpolationTable
///

doc ///
    Key
    	numericalImageDegree
	(numericalImageDegree, Thing, Ideal, Thing, Point)
	(numericalImageDegree, Thing, Ideal, Point)
	(numericalImageDegree, Thing, Ideal)
        repeats
    	[numericalImageDegree, repeats]
        maxAttempts
    	[numericalImageDegree, maxAttempts]
        traceThreshold
    	[numericalImageDegree, traceThreshold]
        pointEqualityThreshold
    	[numericalImageDegree, pointEqualityThreshold]
        [isOnImage, pointEqualityThreshold]
    Headline
    	computes a pseudo-witness set for the image of a variety
    Usage
    	numericalImageDegree(F, I, p)
	numericalImageDegree(F, I)
    Inputs
    	F:Thing
	    a list, or matrix, or ring map, specifying a map
	I:Ideal
	    which is prime, specifying a source variety $V(I)$
	p:Point
	    a general point on $V(I)$
    Outputs
    	:PseudoWitnessSet
	    containing the degree of the projective closure of $F(V(I))$, 
            along with a pseudo-witness set for $F(V(I))$
    Description
	Text
	    This method computes the degree of the image of a variety, 
            along with a @TO2{PseudoWitnessSet, "pseudo-witness set"}@
            (cf. Definition 4 in [1])
            for it, by tracking monodromy loops with homotopy continuation 
            and then applying the trace test. If the trace test fails, only a lower 
            bound for the degree and an incomplete pseudo-witness set is 
            returned. This technique circumvents the calculation of the kernel 
            of the associated ring map.

            The following computes the degree of the Grassmannian $Gr(3,5)$ 
            of $P^2$'s in $P^4$.
            
        Example
            R = CC[x_(1,1)..x_(3,5)];
            F = (minors(3, genericMatrix(R, 3, 5)))_*;
            W = numericalImageDegree(F, ideal 0_R)
            W.isCompletePseudoWitnessSet
            W.imageDegree
        Text
        
            This method can also handle cases where the parameterization 
            has positive dimensional fibers. In the example below, we verify that 
            the variety of $3 x 3 x 3$ tensors of border rank $<= 4$, i.e. the $4$th secant 
            variety of $P^2 x P^2 x P^2$, has degree $9$. This is a hypersurface, 
            with defining equation known as Strassen's invariant,
            and it is also a defective secant variety (meaning its dimension is less
            than expected). Here, the parametrization has $10$ dimensional fibers.
            
        CannedExample
            i6 : R = CC[a_(0,0)..a_(3,2), b_(0,0)..b_(3,2), c_(0,0)..c_(3,2)];
            
            i7 : F = toList apply((0,0,0)..(2,2,2), (i,j,k) ->
                    a_(0,i)*b_(0,j)*c_(0,k) +
                    a_(1,i)*b_(1,j)*c_(1,k) +
                    a_(2,i)*b_(2,j)*c_(2,k) +
                    a_(3,i)*b_(3,j)*c_(3,k));
                    
            i8 : numericalImageDegree(F, ideal 0_R, repeats => 2)
            Sampling point in source ...
            Tracking monodromy loops ...
            Points found: 1
            Points found: 2
            Points found: 3
            Points found: 5
            Points found: 7
            Points found: 9
            Points found: 9
            Points found: 9
            Running trace test ...
            
            o8 = a pseudo-witness set, indicating
                the degree of the image is 9
            
            o8 : PseudoWitnessSet
        -- Example
            -- R = CC[a_(0,0)..a_(3,2), b_(0,0)..b_(3,2), c_(0,0)..c_(3,2)];
            -- F = toList apply((0,0,0)..(2,2,2), (i,j,k) -> 
                -- a_(0,i)*b_(0,j)*c_(0,k) + 
                -- a_(1,i)*b_(1,j)*c_(1,k) + 
                -- a_(2,i)*b_(2,j)*c_(2,k) + 
                -- a_(3,i)*b_(3,j)*c_(3,k));
            -- numericalImageDegree(F, ideal 0_R, repeats => 2)
        Text
        
            Finally, this method has a large number of optional inputs which may be 
            specified by the user to fit a particular problem instance. 

    	    {\tt repeats} sets the maximum number of consecutive repetitive 
            monodromy loops when computing a pseudo-witness set. A repetitive 
            monodromy loop is one where no new points in the image are discovered. 
            After this many consecutive repetitive monodromy loops occur, the trace 
            test is applied to determine if a complete pseudo-witness set has 
            been found. The default value is $3$.

    	    {\tt maxAttempts} sets the maximum number of times the trace test will 
            be attempted when computing a pseudo-witness set. After a trace test 
            fails, a new slice is chosen, the previous points are tracked to the new 
            slice, and monodromy is performed anew. If the trace test has failed 
            {\tt maxAttempts} many times, an incomplete pseudo-witness set is returned. 
            The default value is $10$.
            
            Here is an example in which a badly chosen random seed results in a 
            failed trace test on the first attempt.  In later attempts, the trace test 
            passes and the degree of the twisted cubic is correctly computed to be $3$.
            
        Example
            setRandomSeed 6
            R = CC[s,t];
            F = basis(3, R);
            numericalImageDegree(F, ideal 0_R)
        Text
        
            We compare this with the native Macaulay2 function {\tt degree} (using
            symbolic computation).
            
        Example
            degree ker map(QQ[s,t], QQ[y_0..y_3], {s^3,s^2*t,s*t^2,t^3})
        Text
        
    	    {\tt traceThreshold} sets the threshold for a pseudo-witness set to pass 
            the trace test. The trace test for a complete exact pseudo-witness set is 
            $0$; large nonzero values indicate failure (the larger the value, the worse 
            the failure). The default value is $1e-5$. Caution: setting the value of this 
            threshold too high may result in the trace test returning false positives.

    	    {\tt pointEqualityThreshold} sets the threshold for determing point equality. 
            If this option has value $n$, then two points are considered equal iff their 
            first $n$ significant digits agree (equivalently, in scientific notation, the 
            exponents and first $n$ digits of the mantissa agree). The default value is $5$. 

            {\bf Reference}
            
            [1] J. D. Hauenstein and A. J. Sommese, 
            Witness sets of projections.
            {\it Appl. Math. Comput.}
            {\bf 217}(7) (2010), 3349-3354.
            
            [2] V. Strassen, 
            The asymptotic spectrum of tensors.
            {\it J. Reine. Angew. Math.}
            {\bf 384} (1988), 102-152.
    SeeAlso
    	PseudoWitnessSet
///

doc ///
    Key
        PseudoWitnessSet
        (net, PseudoWitnessSet)
        isCompletePseudoWitnessSet
        imageDegree
	sourceEquations
        sourceSlice
        imageSlice
        witnessPointPairs
        traceTest
    Headline
    	the class of all PseudoWitnessSets
    Description
	Text
            This is a type of hashtable storing the output of a 
            pseudo-witness set computation using monodromy, 
            with the following keys:
        Code
            UL {
                {TEX "\\bf isCompletePseudoWitnessSet: whether or not the 
                pseudo-witness set has passed the trace test, according to the trace test threshold"},
                TEX "\\bf imageDegree: the number of image points found by monodromy",
                TEX "\\bf map: the map F, of which the image is under consideration",
                TEX "\\bf sourceEquations: the defining ideal I of the source variety",
                {TEX "\\bf sourceSlice: additional equations to form a zero-dimensional system 
                (only needed if the map is not finite-to-one)"},
                TEX "\\bf imageSlice: a general complementary-dimensional linear space to F(V(I))",
                {TEX "\\bf witnessPointPairs: a VerticalList of 2-point sequences (p, F(p)), 
                where p lies on V(I) and F(p) lies on imageSlice"},
                TEX "\\bf traceTest: the result of the trace test applied to witnessPointPairs"
                }
        Text
	    The following example demonstrates the output for the 
            $3$-uple embedding of $P^1$ into $P^3$, whose image is the twisted cubic:
            
	Example
            R = CC[s,t];
            W = numericalImageDegree(basis(3,R), ideal 0_R, Verbose => false);
            peek W
        Text
	    [1] J. D. Hauenstein and A. J. Sommese, 
            Witness sets of projections.
            {\it Appl. Math. Comput.}
            {\bf 217}(7) (2010), 3349-3354.
    SeeAlso
    	numericalImageDegree
///

doc ///
    Key
        [numericalImageDegree, Software]
        [numericalSourceSample, Software]
        [numericalImageSample, Software]
        [numericalImageDim, Software]
        [numericalHilbertFunction, Software]
        [isOnImage, Software]
    Headline
    	specify software for homotopy continuation
    Usage
        numericalImageDegree(..., Software => M2engine)
        numericalImageSample(..., Software => M2engine)
        numericalImageDim(..., Software => M2engine)
        numericalHilbertFunction(..., Software => M2engine)
        isOnImage(..., Software => M2engine)
    Description
	Text
    	    This option specifies the software used for polynomial homotopy 
            continuation (used for path tracking) and numerical irreducible 
            decompositions (used for sampling points). The default value is 
            M2engine (native to Macaulay2). Other possible values are 
            @TO Bertini@ and @TO PHCpack@ (only if the user has these 
            packages installed).
    SeeAlso
        numericalImageDegree
        numericalImageSample
        numericalImageDim
        numericalHilbertFunction
        isOnImage
///

doc ///
    Key
        maxThreads
    	[numericalImageDegree, maxThreads]
        [isOnImage, maxThreads]
    Headline
    	specify maximum number of processor threads
    Usage
        numericalImageDegree(..., maxThreads => allowableThreads)
    Description
	Text
    	    Sets the maximum number of processor threads that will be used 
            for parallel computation. This divides the number of paths to track 
            in each monodromy loop into the set of processors, as evenly as 
            possible. The value of this option should always be less than the 
            environment variable allowableThreads. Default value is $1$.
    Caveat
        This feature is under development. Unexpected errors may be printed to 
        output while computing a pseudo-witness set - however, the loop will still 
        attempt to run after errors, and an answer will still be returned.
        
        If the number of paths to track is too low (i.e. $<= 20$), parallel computing will not be used.
    SeeAlso
    	numericalImageDegree
///

doc ///
    Key
    	isOnImage
	(isOnImage, PseudoWitnessSet, Point)
	(isOnImage, Thing, Ideal, Point)
        [isOnImage, maxAttempts]
        [isOnImage, repeats]
        [isOnImage, traceThreshold]
    Headline
    	whether a point lies on the image of a variety
    Usage
    	isOnImage(W, p)
	isOnImage(F, I, p)
    Inputs
        W:PseudoWitnessSet
            a pseudo-witness set for $F(V(I))$
	p:Point
	    a point in the ambient space of $F(V(I))$
        F:Thing
	    a list, or matrix, or ring map, specifying a map
	I:Ideal
	    which is prime, specifying a source variety $V(I)$
    Outputs
    	:Boolean
	    whether or not the point $p$ lies on $F(V(I))$
    Description
	Text
	    This method determines if a point in the ambient target space 
            lies on the image of a variety. This is done via computing a 
            pseudo-witness set for the image.

            If a pseudo-witness set has already been computed, then 
            to avoid repetitive calculation one may run this function with the 
            pseudo-witness set as input.

            The following determines whether or not a point lies on the
            Grassmannian $Gr(3,5)$ of $P^2$'s in $P^4$.
            
        Example
            R = CC[x_(1,1)..x_(3,5)]; I = ideal 0_R;
            F = (minors(3, genericMatrix(R, 3, 5)))_*;
            W = numericalImageDegree(F, I, repeats => 2, Verbose => false);
            q = numericalImageSample(F, I)
            isOnImage(W, q)
            isOnImage(W, point random(CC^1, CC^#F))
            isOnImage(W, point{{1_CC,0,0,0,0,0,0,0,0,0}})
    SeeAlso
    	PseudoWitnessSet
        numericalImageDegree
///

doc ///
    Key
	(isWellDefined, PseudoWitnessSet)
	(isWellDefined, NumericalInterpolationTable)
    Headline
    	whether a point lies on the image of a variety
    Usage
    	isWellDefined X
    Inputs
        X:
            a pseudo-witness set or numerical interpolation table
    Outputs
    	:Boolean
	    whether or not the input represents a well-defined pseudo-witness set or numerical interpolation table
    Description
	Text
            This method checks that the underlying data type of an object represents a well-defined 
            pseudo-witness set or numerical interpolation table. 

            For @TO NumericalInterpolationTable@: this method checks that the underlying @TO HashTable@
            has the following keys: $hilbertFunctionArgument, hilbertFunctionValue, imagePoints, 
            interpolationBasis, interpolationSVD, map$, and that these keys have the expected types.

            For @TO PseudoWitnessSet@: this method checks that the underlying @TO HashTable@
            has the following keys: $isCompletePseudoWitnessSet, imageDegree, map, sourceEquations, 
            sourceSlice, imageSlice, witnessPointPairs, traceTest$, and that these keys have the expected
            types.
    SeeAlso
    	PseudoWitnessSet
        NumericalInterpolationTable
///

doc ///
    Key
        numericalNullity
        (numericalNullity, Matrix)
        (numericalNullity, Matrix, Boolean)
        (numericalNullity, List, Boolean)
        [numericalNullity, Threshold]
        Precondition
        [numericalNullity, Precondition]
    Headline
        numerical kernel dimension of a matrix
    Usage
        numericalNullity M
    Inputs
        M:Matrix
            with real or complex entries
    Outputs
        :ZZ
            dimension of the kernel of M
    Description
        Text
            This method computes the dimension of the kernel of a matrix 
            with real or complex entries numerically, via singular value 
            decomposition (see @TO SVD@). 
            
            If $\sigma_1 \ge \ldots \ge \sigma_n$ are the singular values of 
            $M$, then to establish numerical nullity we look for the first large gap 
            between two consecutive singular values. The gap between 
            $\sigma_i$ and $\sigma_{i+1}$ is large if $\sigma_i/\sigma_{i+1} > $
            @TO2{numericalHilbertFunction, "Threshold"}@.
            
            The optional input @TO Precondition@ specifies whether or not the
            rows of M will be normalized to have norm 1 before computing the SVD.
            This is useful if the matrix is dense (e.g. for an interpolation matrix),
            but not if the matrix is sparse (e.g. diagonal).
            
        Example
            numericalNullity(matrix{{2, 1}, {0, 0.001}}, Precondition => false)
            numericalNullity(map(CC^2,CC^2,0))
    SeeAlso
        SVD
        [numericalHilbertFunction, Threshold]
        numericalRank
///


TEST /// -- embedding cubic surface (with 3 singular points) in P^3 via 5 sections of O(2)
setRandomSeed 0
d = dim ker map(QQ[x,y,z,w]/ideal(x^3 - y*z*w), QQ[a_0..a_4], {x*w + 2*x*y, x*w-3*y^2, z^2, x^2 + y^2 + z^2 - w^2, 3*y*w - 2*x^2})
R = CC[x,y,z,w]
I = ideal(x^3 - y*z*w)
F = {x*w + 2*x*y, x*w-3*y^2, z^2, x^2 + y^2 + z^2 - w^2, 3*y*w - 2*x^2}
assert(numericalImageDim(F, I) == d)
-- Cf. also: non-homogeneous ideal (x^5 - y*z*w), kernel over finite fields
///

TEST /// -- twisted cubic
setRandomSeed 0
R = CC[s,t]
F = basis(3,R)
J = monomialCurveIdeal(QQ[a_0..a_3], {1,2,3})
assert(all(1..5, d -> (numericalHilbertFunction(F,ideal 0_R,d)).hilbertFunctionValue == numcols super basis(d,J)))
W = numericalImageDegree(F, ideal 0_R);
assert(isWellDefined W == true)
assert(W.imageDegree == 3)
assert(isOnImage(W, numericalImageSample(F,ideal 0_R)) == true)
assert(isOnImage(W, point random(CC^1,CC^(numcols F))) == false)
///

TEST /// -- Rational quartic curve in P^3
setRandomSeed 0
R = CC[s,t]
F = flatten entries basis(4, R) - set{s^2*t^2}
S = QQ[a_0..a_3]
I3 = super basis(3, ker map(QQ[s,t], S, {s^4,s^3*t,s*t^3,t^4}))
T = numericalHilbertFunction(F, ideal 0_R, 3);
assert(isWellDefined T == true)
M = extractImageEquations(T, attemptExact => true)
assert(image transpose M == image (map(ring M, S, gens ring M))(I3))
///

TEST /// -- Grassmannian Gr(3, 5) = G(2,4)
setRandomSeed 0
(k, n) = (3,5)
R = CC[x_(1,1)..x_(k,n)]
I = ideal 0_R
F = (minors(k, genericMatrix(R, k, n)))_*
assert(numericalImageDim(F, I) == 1 + k*(n-k))
T = numericalHilbertFunction(F, I, 2)
J = super basis(2, Grassmannian(k-1,n-1))
assert(T.hilbertFunctionValue == numcols J)
I2 = image transpose extractImageEquations(T, attemptExact => true)
assert(image (map(ring I2, ring J, gens ring I2))(J) == I2)
time W = numericalImageDegree(F, I, repeats => 2, Verbose => false)
assert(W.imageDegree == 5)
(n, m) = (5, 20)
pointList = numericalImageSample(F, I, n);
assert(all(pointList, q -> (tally apply(m, i -> isOnImage(W, q)))#true / m >= 8/10))
///

TEST /// -- random canonical curve of genus 4, under random projection to P^2 by cubics
setRandomSeed 0
R = CC[x_0..x_3]
I = ideal(random(2,R),random(3,R))
F = toList(1..3)/(i -> random(3,R))
assert((numericalImageDegree(F,I)).imageDegree == 18)
S = numericalImageSample(F,I,190);
assert((numericalHilbertFunction(F,I,S,18)).hilbertFunctionValue == 1)
///

TEST /// -- Segre + Veronese
setRandomSeed 0
-- Veronese surface P^2 in P^5
(d, n) = (2, 2)
R = CC[x_0..x_n]
F = basis(d, R)
I2 = ideal extractImageEquations(F, ideal 0_R, 2, attemptExact => true)
S = QQ[y_0..y_(binomial(d+n,d)-1)]
RQ = QQ[x_0..x_n]
J = ker map(RQ, S, basis(d, RQ))
assert((map(ring I2, S, gens ring I2))(J) == I2)
-- Segre P^2 x P^3
(n1, n2) = (2, 3)
R = CC[s_0..s_(n1), t_0..t_(n2)]
F = (ideal(s_0..s_(n1))*ideal(t_0..t_(n2)))_*
I2 = ideal extractImageEquations(F, ideal 0_R, 2, attemptExact => true)
RQ = QQ[s_0..s_(n1), t_0..t_(n2)]
S = QQ[y_0..y_((n1+1)*(n2+1)-1)]
J = ker map(RQ, S, (ideal(s_0..s_(n1))*ideal(t_0..t_(n2)))_*)
assert((map(ring I2, S, gens ring I2))(J) == I2)
///

TEST /// -- SO(n)
setRandomSeed 0
n = 4
R = CC[x_(1,1)..x_(n,n)]
A = genericMatrix(R,n,n)
I = ideal(A*transpose A - id_(R^n)) + ideal(det A - 1);
p = point{flatten entries id_(CC^n)}
F = gens R
assert(numericalImageDim(F,I,p) === n*(n-1)//2)
assert((numericalImageDegree(F,I,p, repeats=>2, traceThreshold => 1e-3, pointEqualityThreshold => 2)).imageDegree == 2^(n-1)*det matrix apply(toList(1..floor(n/2)), i -> apply(toList(1..floor(n/2)), j -> binomial(2*n - 2*i - 2*j, n - 2*i))))
///

TEST ///
assert(numericalNullity(matrix{{2, 1}, {0, 0.001}}, Precondition => false) == 1)
assert(numericalNullity(map(CC^2,CC^2,0)) == 2)
assert(numericalNullity(id_(CC^2)) == 0)
///

end--

restart
needsPackage "NumericalImplicitization"
loadPackage("NumericalImplicitization", Reload => true)
uninstallPackage "NumericalImplicitization"
installPackage "NumericalImplicitization"
installPackage("NumericalImplicitization", RemakeAllDocumentation => true)
viewHelp "NumericalImplicitization"
check "NumericalImplicitization"



-- high degree rational normal curve
R = CC[s,t],; F = basis(40,R); I = ideal 0_R;
numericalImageDim(F, I)
time tests = toList(1..100)/(i -> numericalImageDegree(F,I,repeats=>2,Verbose=>false));


-- Generic Pinched Veronese
R = CC[x_0..x_3]
F = toList(1..5)/(i -> random(10,R));
allowableThreads = maxAllowableThreads
numericalImageDegree(F,ideal 0_R,repeats=>2)


-- Trifocal variety
R=CC[a00,a01,a02,a03,a10,a11,a12,a13,a20,a21,a22,a23,b10,b11,b12,b13,b20,b21,b22,b23],;A = transpose genericMatrix(R,a00,4,3),;B = matrix{{0,0,0,1},{b10,b11,b12,b13},{b20,b21,b22,b23}},;C = matrix{{1_R,0,0,0},{0,1,0,0},{0,0,1,0}},;M = A||B||C,;F = flatten flatten apply(3, i-> apply(3, j-> apply(reverse subsets(3,2), k->det  submatrix(M,{i}|{j+3}|(k+{6,6}) , )  )   ));
allowableThreads = 4
elapsedTime numericalImageDegree(F,ideal 0_R,repeats=>2,maxThreads=>allowableThreads)


-- Tensor product surface
(a,b) = (3,1)
R=CC[s,t,u,v, Degrees=>{{1,0},{1,0},{0,1},{0,1}}]
Ix=intersect(ideal(s,u),ideal(t,v))
B=super basis({a,b},Ix)
C=matrix{{1_R,1,0,0,0,0},{0,1,1,0,0,0},{0,0,1,1,0,0},{0,0,0,1,1,1}}
F = C*transpose(B)
I = ideal 0_R
numericalImageDim(F,I)
W = numericalImageDegree(F,I)
T = numericalHilbertFunction(F,I,W.imageDegree)
extractImageEquations T


-- Undirected graphical model on 4 variables
setRandomSeed 0
loadPackage "GraphicalModels"
G = graph({1,2,3,4},{{1,2},{1,3},{2,3},{3,4}})
R = CC[x_(1,1),x_(1,2),x_(1,4),x_(2,2),x_(2,4),x_(3,3),x_(3,4),x_(4,4)]
M = matrix{{x_(1,1),x_(1,2),0,x_(1,4)},{x_(1,2),x_(2,2),0,x_(2,4)},{0,0,x_(3,3),x_(3,4)},{x_(1,4),x_(2,4),x_(3,4),x_(4,4)}}
F = flatten(for i from 1 to 4 list (
    for j from i to 4 list (
	det(submatrix'(M, {i-1}, {j-1}))
    )
))
I = ideal 0_R
numericalImageDim(F, I)
numericalImageDegree(F, I, repeats => 2)
T = numericalHilbertFunction(F, I, 2)
extractImageEquations(T, attemptExact => true)


-- Check approximate equations:
T = numericalHilbertFunction(F, ideal 0_R, 2);
E = extractImageEquations T;
all((toList T.imagePoints)/(p -> clean(1e-11, sub(E, toList(0..<#(p#Coordinates))/(i -> (gens ring E)#i => (p#Coordinates)#i)))), v -> v == 0)


--------------- Implicitization Challenge + variants

-- (line) secant of (P^1)^n, n = 5: degree 3256
n = 5
R = CC[a_1..a_n,b_1..b_n,s,t];
F = s*(terms product apply(toList(1..n), i->(1 + a_i))) + t*(terms product apply(toList(1..n), i->(1 + b_i)));
allowableThreads = maxAllowableThreads
time W = numericalImageDegree(F, ideal 0_R, repeats => 1, maxThreads => allowableThreads) 


-- Challenge: Hadamard square of line secant of (P^1)^4, degree 110, passed in 188.084 seconds
t = symbol t;
n = 4
R = CC[a_1..a_n,b_1..b_n, c_1..c_n, d_1..d_n,t_0..t_3];
F1 = t_0*(terms product apply(toList(1..n), i->(1 + a_i))) + t_1*(terms product apply(toList(1..n), i->(1 + b_i)));
F2 = t_2*(terms product apply(toList(1..n), i->(1 + c_i))) + t_3*(terms product apply(toList(1..n), i->(1 + d_i)));
F = apply(toList(0..15), i -> F1#i * F2#i);
allowableThreads = maxAllowableThreads
time W = numericalImageDegree(F, ideal 0_R, repeats => 1, maxThreads => allowableThreads)