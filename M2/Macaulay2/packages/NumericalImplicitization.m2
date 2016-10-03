newPackage("NumericalImplicitization",
    Headline => "NumericalImplicitization",
    Version => "1.0",
    Date => "June 29, 2016",
    Authors => {
        {Name => "Justin Chen",
	 Email => "jchen@math.berkeley.edu",
         HomePage => "https://math.berkeley.edu/~jchen"},
        {Name => "Joe Kileel",
	 Email => "jkileel@math.berkeley.edu",
	 HomePage => "http://www.math.berkeley.edu/~jkileel"}
        },
    PackageImports => {"MonodromySolver"},
    PackageExports => {"NumericalAlgebraicGeometry"},
    DebuggingMode => true
    )
    export {
        "numericalSourceSample",
	    "verboseOutput",
        "numericalImageSample",
        "numericalImageDim",
        "numericalHilbertFunction",
	    "SVDGapThreshold",
	"NumericalInterpolationTable",
	    "hilbertFunctionArgument",
	    "hilbertFunctionValue",
	    "imagePoints",
	    "interpolationBasis",
	    "interpolationSVD",
	"extractImageEquations",
	"numericalImageDegree",
            "maxThreads",
	    "maxRepetitiveMonodromies",
	    "traceTestThreshold",
            "maxTraceTests",
            "pointEqualityThreshold",
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

debug NumericalAlgebraicGeometry

hasAttribute = value Core#"private dictionary"#"hasAttribute"
getAttribute = value Core#"private dictionary"#"getAttribute"
ReverseDictionary = value Core#"private dictionary"#"ReverseDictionary"

NumericalInterpolationTable = new Type of HashTable
globalAssignment NumericalInterpolationTable
net NumericalInterpolationTable := X -> (
	if hasAttribute(X, ReverseDictionary) then toString getAttribute(X, ReverseDictionary)
	else "NumericalInterpolationTable"
)

PseudoWitnessSet = new Type of HashTable
globalAssignment PseudoWitnessSet
net PseudoWitnessSet := X -> (
	if hasAttribute(X, ReverseDictionary) then toString getAttribute(X, ReverseDictionary)
	else "PseudoWitnessSet"
)

-- software options (none for sample nor refine): default is M2engine throughout
-- n.b.: precision loss from LAPACK in SVD computation
-- Magic numbers: 8 (and 3) decimal places in extractImageEquations, 10 in fiberSlice, 50 in smartTrack (for parallelization), 4 (translationMagnitude) in doTraceTest
-- Allow F to be a matrix throughout?

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
numericalImageSample (List, Ideal, ZZ) := List => opts -> (F, I, sampleSize) -> (
    apply(numericalEval(F, numericalSourceSample(I, sampleSize, opts), false), p -> point p)
)
numericalImageSample (List, Ideal) := Point => opts -> (F, I) -> first numericalImageSample(F, I, 1, opts)


numericalDims = method(Options => {Software => M2engine})
numericalDims (List, Ideal, Point) := List => opts -> (F, I, sourcePoint) -> ( --outputs {dim(V(I)), dim(F(V(I))}
    sourceJacobian := sub(transpose(jacobian(I)), matrix sourcePoint);
    dF := sub(transpose(jacobian(matrix{F})), matrix sourcePoint);
    sourceDim := numericalNullity(sourceJacobian, false);
    {sourceDim, sourceDim - numericalNullity(sourceJacobian || dF, false)}
)
numericalDims (List, Ideal) := ZZ => opts -> (F, I) -> numericalDims(F, I, numericalSourceSample(I, opts))


numericalImageDim = method(Options => {Software => M2engine})
numericalImageDim (List, Ideal, Point) := ZZ => opts -> (F, I, sourcePoint) -> last numericalDims(F, I, sourcePoint, opts)
numericalImageDim (List, Ideal) := ZZ => opts -> (F, I) -> last numericalDims(F, I, opts)


numericalNullity = method(Options => {symbol SVDGapThreshold => 200, symbol verboseOutput => false})
numericalNullity (List, Boolean) := List => opts -> (M, keepSVD) -> ( -- M is a list of 1-element lists of row matrices, to make taking matrix M immediate
    isZero := true;
    if opts.verboseOutput then (
	print "Performing normalization preconditioning ...";
	time normalizedM := matrix apply(M, row -> if row#0 == 0 then row else (isZero = false; (1/norm(2,row#0))*row));
	print "Computing numerical kernel ...";
	if isZero then return if keepSVD then {numcols normalizedM, 0} else numcols normalizedM;
	time (S, U, Vt) := SVD(normalizedM, DivideConquer => true);
    ) else (
        normalizedM = matrix apply(M, row -> if row#0 == 0 then row else (isZero = false; (1/norm(2,row#0))*row));
        if isZero then return if keepSVD then {numcols normalizedM, 0} else numcols normalizedM;
        (S, U, Vt) = SVD(normalizedM, DivideConquer => true);
    );
    largestGap := (#S, opts.SVDGapThreshold);
    for i from 1 to #S-1 do (
        if S#i == 0 then (
            if first largestGap == #S then largestGap = (i, "infinity");
            break;
        ) else if S#(i-1)/S#i > last largestGap then largestGap = (i, S#(i-1)/S#i);
    );
    if keepSVD then {numcols normalizedM - first largestGap, (S, U, Vt)} else numcols normalizedM - first largestGap
)
numericalNullity (Matrix, Boolean) := List => opts -> (M, keepSVD) -> numericalNullity(apply(entries M, row -> {matrix{row}}), keepSVD)


numericalHilbertFunction = method(Options => {Software => M2engine, SVDGapThreshold => 200, verboseOutput => true})
numericalHilbertFunction (List, Ideal, List, ZZ) := NumericalInterpolationTable => opts -> (F, I, sampleImagePoints, d) -> ( --outputs a degree d interpolation table for F(V(I))
    C := coefficientRing(ring(I));
    if not all(F | I_*, f -> isHomogeneous f) then ( -- work in an affine chart of the projective closure
        F = F | {1_C};
        sampleImagePoints = apply(sampleImagePoints, p -> point{append(p#Coordinates, 1_C)});
    );
    y := symbol y;
    targetRing := C[y_1..y_(#F)];
    allMonomials := basis(d, targetRing);
    if #sampleImagePoints < numcols allMonomials then (
        if opts.verboseOutput then (
            print "Sampling image points ...";
            time sampleImagePoints = join(sampleImagePoints, numericalImageSample(F, I, numcols allMonomials - #sampleImagePoints, Software => opts.Software));
        ) else sampleImagePoints = join(sampleImagePoints, numericalImageSample(F, I, numcols allMonomials - #sampleImagePoints, Software => opts.Software));
    );
    if opts.verboseOutput then (
        print "Creating interpolation matrix ...";
        time interpolationMatrix := apply(toList(0..<numcols allMonomials), i -> {sub(allMonomials, matrix sampleImagePoints#i)});
    ) else (
        interpolationMatrix = apply(toList(0..<numcols allMonomials), i -> {sub(allMonomials, matrix sampleImagePoints#i)});
    );
    interpolationData := numericalNullity(interpolationMatrix, true, verboseOutput => opts.verboseOutput);
    if opts.verboseOutput then print("Hilbert function value: " | first interpolationData);
    new NumericalInterpolationTable from {
        symbol hilbertFunctionArgument => d,
        symbol hilbertFunctionValue => first interpolationData,
        symbol imagePoints => VerticalList sampleImagePoints,
	symbol interpolationBasis => allMonomials,
        symbol interpolationSVD => last interpolationData,
	symbol map => F
    }
)
numericalHilbertFunction (List, Ideal, ZZ) := NumericalInterpolationTable => opts -> (F, I, d) -> numericalHilbertFunction(F, I, {}, d, opts)


extractImageEquations = method()
extractImageEquations (NumericalInterpolationTable) := Matrix => T -> (
    s := #first T.interpolationSVD;
    kernelDim := T.hilbertFunctionValue;
    V := conjugate last T.interpolationSVD;
    allMonomials := transpose T.interpolationBasis;
    C := ring allMonomials;
    if kernelDim == 0 then return {map(C^1, C^1, 0), map(C^1, C^1, 0)};
    E := V^{s-kernelDim..s-1};
    M := 1e9*(matrix apply(entries E, r -> apply(r, e -> round(8, e))));
    transpose matrix{apply(flatten entries(mingens ideal(M*allMonomials)), f -> roundPoly(3, f))}
)
extractImageEquations (List, Ideal, ZZ) := Matrix => (F, I, d) -> extractImageEquations(numericalHilbertFunction(F, I, d))


roundPoly = method()
roundPoly (ZZ, RingElement) := RingElement => (n, f) -> (
	toBaseField := map(coefficientRing ring f, ring f);
	sum((terms f)/(t -> (coeff := toBaseField((last coefficients t)_(0,0)); round(n, coeff)*(first coefficients t)_(0,0))))
)


round (ZZ, ZZ) := ZZ => (n, x) -> x
round (ZZ, CC) := ZZ => (n, x) -> round(n, realPart x) + ii*round(n, imaginaryPart x)

numericalImageDegree = method(Options => {Software => M2engine, verboseOutput => true, symbol maxThreads => 1, symbol maxRepetitiveMonodromies => 4, symbol traceTestThreshold => 1e-5, symbol maxTraceTests => 10, symbol pointEqualityThreshold => 5})
numericalImageDegree (List, Ideal, Thing, Point) := PseudoWitnessSet => opts -> (F, I, W, sourcePoint) -> ( --outputs a pseudo-witness set for F(V(I))
    if opts.Software === MonodromySolver then return numericalImageDegree'MonodromySolver(F,I,W,sourcePoint,opts);
    local newSamplePair, local pullbackSliceData, local pullbackSlice, local sliceTranslation, local sliceCoefficients, local fiberSlice;
    local squaredUpSource, local startSystem, local newStartSystem, local pointPairs, local pairTable, local imagePointString;
    printingPrecisionStore := printingPrecision;
    printingPrecision = opts.pointEqualityThreshold;
    y := symbol y;
    targetRing := (coefficientRing(ring(I)))[y_1..y_(#F)];
    dims := numericalDims(F, I, sourcePoint);
    numFailedTraceTests := 0;
    traceResult := opts.traceTestThreshold + 1;
    while not traceResult < opts.traceTestThreshold and numFailedTraceTests < opts.maxTraceTests do (
        if numFailedTraceTests > 0 then (
	    if W === {} and not I == 0 then W = first components numericalIrreducibleDecomposition(I, Software => opts.Software);
	    sourcePoint = numericalSourceSample(I, W);
	);
	newSamplePair = first numericalEval(F, {sourcePoint}, true);
        pullbackSliceData = randomCombinations(F, last dims, true);
        sliceTranslation = transpose sub(matrix{last pullbackSliceData}, matrix sourcePoint);
        pullbackSlice = (last pullbackSliceData) - flatten entries sliceTranslation;
        sliceCoefficients = promote((first pullbackSliceData) | (-1)*sliceTranslation, targetRing);
        if first dims > last dims then (
            fiberSlice = randomCombinations(gens(ring(I)) | {10_(ring(I))}, (first dims) - (last dims), false);
            fiberSlice = fiberSlice - flatten entries sub(matrix{fiberSlice}, matrix sourcePoint);
        ) else fiberSlice = {};
	squaredUpSource = (if I == 0 then {} else randomCombinations(I_*, #gens(ring(I)) - first dims, false));
	newStartSystem = squaredUpSource | fiberSlice | pullbackSlice;
        if numFailedTraceTests > 0 then (
            newTrackedPairs := numericalEval(F, smartTrack(startSystem, newStartSystem, apply(values pairTable, pair -> first pair), true, Software => opts.Software, verboseOutput => opts.verboseOutput, maxThreads => opts.maxThreads), true);
            pairTable = new MutableHashTable;
            for newPair in newTrackedPairs do (
                imagePointString = apply(flatten entries last newPair, c -> toString c);
                if not pairTable#?imagePointString then pairTable#imagePointString = newPair;
            );
        ) else pairTable = new MutableHashTable;
        imagePointString = apply(flatten entries last newSamplePair, c -> toString c);
        if not pairTable#?imagePointString then (
            pairTable#imagePointString = newSamplePair;
            if opts.verboseOutput and numFailedTraceTests > 0 then print "Added new image point";
        );
	startSystem = newStartSystem;
        pointPairs = monodromyLoop(F, last dims, startSystem, pairTable, Software => opts.Software, verboseOutput => opts.verboseOutput, maxThreads => opts.maxThreads, maxRepetitiveMonodromies => opts.maxRepetitiveMonodromies);
        if opts.verboseOutput then print("Running trace test ...");
        traceResult = doTraceTest(F, last dims, startSystem, pointPairs, Software => opts.Software, verboseOutput => opts.verboseOutput);
        if not traceResult < opts.traceTestThreshold then (
            if opts.verboseOutput then print("Failed trace test! Trace: " | toString traceResult);
            numFailedTraceTests = numFailedTraceTests + 1;
        );
    );
    if opts.verboseOutput then (
        if traceResult < opts.traceTestThreshold then print("Degree of image: " | #pointPairs) else (
            print("Degree of image should be at least " | #pointPairs);
            print("Consider changing parameters (maxRepetitiveMonodromies, maxTraceTests, traceTestThreshold, pointEqualityThreshold) or reparametrizing for a better result. Alternatively, consider changing the ground field to e.g. CC_100.");
        );
    );
    printingPrecision = printingPrecisionStore;
    new PseudoWitnessSet from {
        symbol isCompletePseudoWitnessSet => traceResult < opts.traceTestThreshold,
        symbol imageDegree => #pointPairs,
        symbol map => F,
        symbol sourceEquations => I,
        symbol sourceSlice => transpose matrix{fiberSlice},
        symbol imageSlice => sliceCoefficients*((transpose vars targetRing) || matrix{{1_targetRing}}),
        symbol witnessPointPairs => VerticalList apply(pointPairs, pair -> (first pair, point last pair)),
	symbol traceTest => traceResult
    }
)

numericalImageDegree'MonodromySolver = method(Options => {Software => M2engine, verboseOutput => true, symbol maxThreads => 1, symbol maxRepetitiveMonodromies => 4, symbol traceTestThreshold => 1e-5, symbol maxTraceTests => 10, symbol pointEqualityThreshold => 5})
numericalImageDegree'MonodromySolver (List, Ideal, Thing, Point) := PseudoWitnessSet => opts -> (F, I, W, sourcePoint) -> ( --outputs a pseudo-witness set for F(V(I))
    local newSamplePair, local pullbackSliceData, local pullbackSlice, local sliceTranslation, local sliceCoefficients, local fiberSlice;
    local squaredUpSource, local startSystem, local newStartSystem, local pointPairs, local pairTable, local imagePointString;
    printingPrecisionStore := printingPrecision;
    printingPrecision = opts.pointEqualityThreshold;
    y := symbol y;
    targetRing := (coefficientRing(ring(I)))[y_1..y_(#F)];
    dims := numericalDims(F, I, sourcePoint);
    numFailedTraceTests := 0;
    traceResult := opts.traceTestThreshold + 1;
    while not traceResult < opts.traceTestThreshold and numFailedTraceTests < opts.maxTraceTests do (
        if numFailedTraceTests > 0 then (
	    if W == {} and not I == 0 then W = first components numericalIrreducibleDecomposition(I, Software => opts.Software);
	    sourcePoint = numericalSourceSample(I, W);
	    );
	(combs, toCR) := parametricCombinations(F, dims);
	polys := polySystem(combs || transpose gens (toCR I)); 
	(p0,x0) := createSeedPair(polys, coordinates sourcePoint);
	nnodes := 5; nedges := 1;
	elapsedTime (vertex,npaths) := monodromySolve(squareUp polys,p0,{x0},NumberOfNodes=>nnodes, NumberOfEdges=>nedges, 
	    SelectEdgeAndDirection=>selectBestEdgeAndDirection, Potential=>potentialLowerBound, 
	    NumberOfRepeats=>100, Verbose=>true
	    );
	sols := points vertex.PartialSols;
	{* 
	-- look at residuals 	
	polys0 := specializeSystem(vertex.BasePoint,polys);
    	for i from 0 to length sols-1 do (
	    x0 := sols_i;
	    print x0;
	    print evaluate(polySystem polys0,x0);
	    );
	*}
	traceResult = opts.traceTestThreshold / 2; -- fake success!!!    
	);
    pointPairs = {};
    if opts.verboseOutput then (
        if traceResult < opts.traceTestThreshold then print("Degree of image: " | #pointPairs) else (
            print("Degree of image should be at least " | #pointPairs);
            print("Consider changing parameters (maxRepetitiveMonodromies, maxTraceTests, traceTestThreshold, pointEqualityThreshold) or reparametrizing for a better result. Alternatively, consider changing the ground field to e.g. CC_100.");
            );
    	);
    new PseudoWitnessSet from {
        symbol isCompletePseudoWitnessSet => traceResult < opts.traceTestThreshold,
        symbol imageDegree => #sols, -- this is wrong in general !!!
        symbol map => F,
        symbol sourceEquations => I,
        symbol sourceSlice => null, --transpose matrix{fiberSlice},
        symbol imageSlice => null, --sliceCoefficients*((transpose vars targetRing) || matrix{{1_targetRing}}),
        symbol witnessPointPairs => null, --VerticalList apply(pointPairs, pair -> (first pair, point last pair)),
	symbol traceTest => traceResult
    }
)
numericalImageDegree (List, Ideal) := PseudoWitnessSet => opts -> (F, I) -> (
    if opts.verboseOutput then print "Sampling point in source ...";
    W := if I == 0 then {} else first components numericalIrreducibleDecomposition(I, 
	Software => if opts.Software =!= MonodromySolver then opts.Software else M2engine);
    numericalImageDegree(F, I, W, numericalSourceSample(I, W), opts)
)
numericalImageDegree(List, Ideal, Point) := PseudoWitnessSet => opts -> (F, I, p) -> numericalImageDegree(F, I, {}, p, opts)


smartTrack = method(Options => {Software => M2engine, verboseOutput => true, maxThreads => 1})
smartTrack (List, List, List, Boolean) := List => opts -> (startSystem, targetSystem, startSolutions, doRefinements) -> (
    randomGamma := random(coefficientRing(ring(first(startSystem))));
    startSystem = polySystem startSystem;
    targetSystem = polySystem targetSystem;
    if #startSolutions > max(10, 2*opts.maxThreads) and opts.maxThreads > 1 then ( --currently buggy: prints many errors!
        startSolutionsList := pack(ceiling(#startSolutions/opts.maxThreads), startSolutions);
        threadList := {};
        for paths in startSolutionsList do (
            threadList = append(threadList, schedule(track, (startSystem, targetSystem, paths, gamma => randomGamma, Software => opts.Software)));
        );
        while any(threadList, t -> not isReady t) do sleep 1;
        targetSolutions := flatten apply(threadList, t -> taskResult t);
        if opts.verboseOutput then print("Finished tracking " | #targetSolutions | " paths in parallel");
    ) else targetSolutions = track(startSystem, targetSystem, startSolutions, gamma => randomGamma, Software => opts.Software);
    goodSols := select(targetSolutions, p -> p#?SolutionStatus and p#SolutionStatus == Regular);
    --if opts.verboseOutput and #goodSols < #startSolutions then print("Paths going to infinity: " | #startSolutions - #goodSols);
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

parametricCombinations = method()
parametricCombinations (List, List) := (polys, dims) -> ( 
    (c',c) := toSequence dims;
    polysM := transpose matrix {polys};
    R := ring polysM;
    a := symbol a;
    b := symbol b;
    fiberSlices := c' > c;
    C := (coefficientRing R)[a_(0,0)..a_(#polys,c-1), if fiberSlices then b_(0,0)..b_(numgens R,c'-c-1) else {}];
    CR := C (monoid[gens R]);
    toCR := map(CR,R,gens CR);
    coeffs := genericMatrix(C, c, #polys);
    combs := coeffs * toCR polysM + genericMatrix(C,a_(#polys,0),c,1); 
    if fiberSlices then ( 
	coeffs = genericMatrix(C,  b_(0,0), c'-c, numgens R);
	combs = combs || (coeffs * toCR transpose vars R + genericMatrix(C,b_(numgens R,0),c'-c,1)); 
	);
    (combs,toCR)
    )

monodromyLoop = method(Options => {Software => M2engine, verboseOutput => true, maxThreads => 1, maxRepetitiveMonodromies => 4})
monodromyLoop (List, ZZ, List, MutableHashTable) := List => opts -> (F, imageDim, startSystem, pairTable) -> (
    numRepetitiveMonodromyLoops := 0;
    local intermediateSystem1, local intermediateSolutions1, local endSolutions;
    local candidatePairs, local imagePointString, local previousNumImagePoints;
    if opts.verboseOutput then print "Tracking monodromy loops ...";
    while numRepetitiveMonodromyLoops < opts.maxRepetitiveMonodromies do (
        previousNumImagePoints = #values pairTable;
        intermediateSystem1 = drop(startSystem, -imageDim) | randomCombinations(F | {10_(ring(first(F)))}, imageDim, false);
        intermediateSolutions1 = smartTrack(startSystem, intermediateSystem1, apply(values pairTable, pair -> first pair), false, Software => opts.Software, verboseOutput => opts.verboseOutput, maxThreads => opts.maxThreads);
        if #intermediateSolutions1 > 0 then (
            endSolutions = smartTrack(intermediateSystem1, startSystem, intermediateSolutions1, false, Software => opts.Software, verboseOutput => opts.verboseOutput, maxThreads => opts.maxThreads);
            if #endSolutions > 0 then (
                candidatePairs = numericalEval(F, endSolutions, true);
                for newPair in candidatePairs do (
                    imagePointString = apply(flatten entries last newPair, c -> toString c);
                    if not pairTable#?imagePointString then pairTable#imagePointString = newPair;
                );
            );
        );
        if previousNumImagePoints < #values pairTable then numRepetitiveMonodromyLoops = 0
        else numRepetitiveMonodromyLoops = numRepetitiveMonodromyLoops + 1;
        if opts.verboseOutput then print ("Points found: " | #values pairTable);
    );
    values pairTable
)


doTraceTest = method(Options => {Software => M2engine, verboseOutput => true})
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
                --if opts.verboseOutput then print("Trace test: " | toString norm(2,sum(traceList)));
                return norm(2,sum(traceList))
            );
        );
    );
    infinity
)


isOnImage = method(Options => {Software => M2engine})--add threshold for point equality here and throughout?
isOnImage (PseudoWitnessSet, Point) := Boolean => opts -> (W, p) -> (
    if not W.isCompletePseudoWitnessSet then print "Warning: not a complete pseudo-witness set! May return false negative.";
    F := W.map;
    I := W.sourceEquations;
    fiberSlice := flatten entries W.sourceSlice;
    targetVariables := gens ring(W.imageSlice);
    pullbackSlice := flatten entries sub(W.imageSlice, apply(toList(0..<#targetVariables), i -> targetVariables#i => F#i));
    squaredUpSource := (if I == 0 then {} else randomCombinations(I_*, #gens(ring(I)) - #fiberSlice - #pullbackSlice, false));
    startUpstairsPoints := apply(W.witnessPointPairs, pair -> first pair);
    newPullbackSliceData := randomCombinations(F, #pullbackSlice, true);
    sliceCoefficients := first newPullbackSliceData;
    newPullbackSlice := last newPullbackSliceData;
    newPullbackSlice = newPullbackSlice - flatten entries (sliceCoefficients * promote(transpose(matrix p), coefficientRing(ring(I))));
    targetUpstairsPoints := smartTrack(squaredUpSource | fiberSlice | pullbackSlice, squaredUpSource | fiberSlice | newPullbackSlice, startUpstairsPoints, true, opts);
    any(numericalEval(F, targetUpstairsPoints, false), q -> point q == p)
    )
isOnImage (List, Ideal, Point) := Boolean -> (F, I, p) -> isOnImage(numericalImageDegree(F, I), p)


beginDocumentation()

--Documention--
--<<docTemplate
doc ///
    Key
    	NumericalImplicitization
    Headline
    	a Macaulay2 implicitization package
    Description
    	Text
	    Allows for user-friendly computation of the basic invariants of the image of a polynomial map. Based on numerical algebraic geometry. The techniques used are interpolation, homotopy continuation and monodromy. NumericalImplicitization is geared toward large-scale and applied problems, where symbolic methods are too time consuming or fail to terminate. Current implementation is for irreducible source varieties, affine or projective, and regular maps (= morphisms) that are not necessarily finite-to-one.
///

doc ///
    Key
    	numericalSourceSample
	(numericalSourceSample, Ideal, Thing, ZZ)
        (numericalSourceSample, Ideal, Thing)
	(numericalSourceSample, Ideal, ZZ)
        (numericalSourceSample, Ideal)
    Headline
    	samples a general point from a variety
    Usage
    	numericalSourceSample(I, s)
	numericalSourceSample(I)
    Inputs
	I:Ideal
	    which is prime, specifying a variety V(I)
	s:ZZ
	    the number of points to sample on V(I)
    Outputs
    	:List
	    of sample points on V(I)
    Description
	Text
	    Computes a list of sample points on a variety numerically. If I is the zero ideal in a polynomial ring of dimension n, then an n-tuple of random elements in the ground field is returned. Otherwise, a numerical irreducible decomposition of I is computed (see @TO numericalIrreducibleDecomposition@), which is then used to sample points. @BR{}@ @BR{}@
	    If s is unspecified, then it is assumed that s = 1. In this case, the single point is returned, rather than a list.
        Example
            R = CC[x_1..x_3]
            numericalSourceSample(ideal 0_R)
            I = ideal(sum(apply(gens R, v -> v^2)) - 1)
            numericalSourceSample(I, 3)
    Caveat
	Since numerical irreducible decompositions are done over CC, if I is not the zero ideal, then the output will be a point in complex space (regardless of the ground field of the ring of I).
    SeeAlso
        numericalImageSample
///

doc ///
    Key
    	numericalImageSample
	(numericalImageSample, List, Ideal, ZZ)
	(numericalImageSample, List, Ideal)
    Headline
    	samples a general point from the image of a variety
    Usage
    	numericalImageSample(F, I, s)
	numericalImageSample(F, I)
    Inputs
    	F:List
	    of polynomials, specifying a map
	I:Ideal
	    which is prime, specifying a source variety V(I)
	s:ZZ
	    the number of points to sample in F(V(I))
    Outputs
    	:List
	    of sample points on F(V(I)))
    Description
	Text
	    Computes a list of sample points on the image of a variety numerically. This function calls @TO numericalSourceSample@. @BR{}@ @BR{}@
	    If s is unspecified, then it is assumed that s = 1. In this case, the single point is returned, rather than a list.
        Example
            R = CC[x_(1,1)..x_(3,5)]
            F = (minors(3, genericMatrix(R, 3, 5)))_*;
            numericalImageSample(F, ideal 0_R)
    SeeAlso
        numericalSourceSample
///

doc ///
    Key
    	numericalImageDim
	(numericalImageDim, List, Ideal, Point)
	(numericalImageDim, List, Ideal)
    Headline
    	computes the dimension of the image of a variety
    Usage
    	numericalImageDim(F, I, p)
	numericalImageDim(F, I)
    Inputs
    	F:List
	    of polynomials, specifying a map
	I:Ideal
	    which is prime, specifying a source variety V(I)
	p:Point
	    a sample point on V(I)
    Outputs
    	:ZZ
	    the dimension of F(V(I)))
    Description
	Text
	    Computes the dimension of the image of a variety numerically. 
	    Even if the source variety and map are projective, the affine (= Krull) dimension is returned - this ensures consistency with @TO dim@.
        Example
            R = CC[x_(1,1)..x_(3,5)]
            F = (minors(3, genericMatrix(R, 3, 5)))_*;
            numericalImageDim(F, ideal 0_R)
        Text
            The following example verifies part of the Alexander-Hirschowitz theorem: the 14th secant variety of the 4th Veronese of P^4 has affine dimension 69, rather than 70.
        Example
            R = CC[a_(1,1)..a_(14,5)]
            F = sum(1..14, i -> flatten entries basis(4, R, Variables=>toList(a_(i,1)..a_(i,5))));
            time numericalImageDim(F, ideal 0_R)
///

doc ///
    Key
    	numericalHilbertFunction
	(numericalHilbertFunction, List, Ideal, List, ZZ)
	(numericalHilbertFunction, List, Ideal, ZZ)
    Headline
    	computes the values of the Hilbert function for the image of a variety
    Usage
    	numericalHilbertFunction(F, I, S, d)
	numericalHilbertFunction(F, I, d)
    Inputs
    	F:List
	    of polynomials, specifying a map
	I:Ideal
	    which is prime, specifying a source variety V(I)
	S:List
	    of general points on F(V(I))
    	d:ZZ
	    the argument of the Hilbert function of F(V(I))
    Outputs
    	:NumericalInterpolationTable
	    containing the number of linearly independent degree d forms in the ideal of the projective closure of F(V(I)), along with approximations of those forms
    Description
	Text
	    Computes values of the Hilbert function of the image of a variety, by numerical interpolation. This technique circumvents the calculation of the kernel of the associated ring map. @BR{}@ @BR{}@
            In order to speed up computation, the list S of points can be precomputed (see @TO numericalImageSample@). This list of points can then be re-used in multiple interpolation computations (which yields a dramatic speedup over performing separate sampling instances).
        Example
            R = CC[x_(1,1)..x_(3,5)]
            F = (minors(3, genericMatrix(R, 3, 5)))_*;
            S = numericalImageSample(F, ideal 0_R, 60);
            numericalHilbertFunction(F, ideal 0_R, S, 2)
    SeeAlso
    	NumericalInterpolationTable
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
    	    This type is a hashtable storing the output of a polynomial interpolation computation, with the following keys: 
        Code
            UL {
                {TEX "\\bf hilbertFunctionArgument: the argument, d, to the Hilbert function"},
                TEX "\\bf hilbertFunctionValue: the value of the Hilbert function at d",
                TEX "\\bf imagePoints: a VerticalList of sample points on the image",
		TEX "\\bf interpolationBasis: a matrix, consisting of the degree d monomials",
                TEX "\\bf interpolationSVD: the SVD of the interpolation matrix, obtained by evaluating degree d monomials at the sample points",
		TEX "\\bf map: the map F, of which the image is under consideration"
                }
        Example
            R = CC[x_(1,1)..x_(3,5)]
            F = (minors(3, genericMatrix(R, 3, 5)))_*;
            T = numericalHilbertFunction(F, ideal 0_R, 2, verboseOutput => false)
            (T.hilbertFunctionArgument, T.hilbertFunctionValue)
    SeeAlso
    	numericalHilbertFunction
///

doc ///
    Key
        SVDGapThreshold
        [numericalHilbertFunction, SVDGapThreshold]
    Headline
    	specify minimal threshold for numerical rank
    Usage
        numericalHilbertFunction(..., SVDGapThreshold => 200)
    Description
	Text
    	    Specifies the minimal gap (= ratio of consecutive singular values) for determining the numerical rank of a matrix. If the largest gap is greater than this threshold, then all singular values after the largest gap are considered as numerically zero; if all gaps are less than this threshold, then the matrix is considered numerically full rank. Default value is 200.
    SeeAlso
        numericalHilbertFunction
///

doc ///
    Key
        verboseOutput
        [numericalHilbertFunction, verboseOutput]
    	[numericalImageDegree, verboseOutput]
    Headline
    	display detailed output
    Usage
        numericalImageDegree(..., verboseOutput => true)
    Description
	Text
    	    Determines whether or not detailed output is displayed during an interpolation or monodromy computation, including timings for various intermediate computations. Default value is true.
    SeeAlso
        numericalHilbertFunction
    	numericalImageDegree
///

doc ///
    Key
    	extractImageEquations
	(extractImageEquations, NumericalInterpolationTable)
	(extractImageEquations, List, Ideal, ZZ)
    Headline
    	finds implicit equations in a fixed degree for the image of a variety
    Usage
    	extractImageEquations T
	extractImageEquations(F, I, d)
    Inputs
        T:NumericalInterpolationTable
            a numerical interpolation table for F(V(I)) of degree d
    	F:List
	    of polynomials, specifying a map
	I:Ideal
	    which is prime, specifying a source variety V(I)
    	d:ZZ
	    the argument of the Hilbert function of F(V(I))
    Outputs
    	:Matrix
	    of implicit degree d equations for F(V(I))
    Description
	Text
	    Finds implicit degree d equations for the image of a variety. This is done via a numerical interpolation computation for the image. @BR{}@ @BR{}@
	    It may be useful to compute the numerical interpolation table first, and then run this function with the interpolation table as input. @BR{}@ @BR{}@
	    The following example demonstrates the output, giving the 5 Plucker quadrics defining the Grassmannian Gr(3,5):
        Example
            R = CC[x_(1,1)..x_(3,5)]; I = ideal 0_R;
            F = (minors(3, genericMatrix(R, 3, 5)))_*;
	    T = numericalHilbertFunction(F, I, 2, verboseOutput => false);
	    extractImageEquations T
    SeeAlso
    	numericalHilbertFunction
        NumericalInterpolationTable
    Caveat
    	An LLL basis reduction is performed to simplify the approximate equations of the image. This may result in slightly inaccurate equations.
///

doc ///
    Key
    	numericalImageDegree
	(numericalImageDegree, List, Ideal, Thing, Point)
	(numericalImageDegree, List, Ideal, Point)
	(numericalImageDegree, List, Ideal)
    Headline
    	computes a pseudo-witness set for the image of a variety
    Usage
    	numericalImageDegree(F, I, p)
	numericalImageDegree(F, I)
    Inputs
    	F:List
	    of polynomials, specifying a map
	I:Ideal
	    which is prime, specifying a source variety V(I)
	p:Point
	    a general point on V(I)
    Outputs
    	:PseudoWitnessSet
	    containing the degree of the projective closure of F(V(I)), along with a pseudo-witness set for F(V(I))
    Description
	Text
	    Computes the degree of the image of a variety, along with a pseudo-witness set for it, by tracking monodromy loops with homotopy continuation and then applying the trace test. If the trace test fails, only a lower bound for the degree and an incomplete pseudo-witness set is returned. This technique circumvents the calculation of the kernel of the associated ring map.
        Example
            R = CC[x_(1,1)..x_(3,5)]
            F = (minors(3, genericMatrix(R, 3, 5)))_*;
            W = numericalImageDegree(F, ideal 0_R)
            W.isCompletePseudoWitnessSet
            W.imageDegree
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
            This type is a hashtable storing the output of a pseudo-witness set computation using monodromy, with the following keys:
        Code
            UL {
                {TEX "\\bf isCompletePseudoWitnessSet: whether or not the pseudo-witness set has passed the trace test, according to the trace test threshold"},
                TEX "\\bf imageDegree: the number of image points found by monodromy",
                TEX "\\bf map: the map F, of which the image is under consideration",
                TEX "\\bf sourceEquations: the defining ideal I of the source variety",
                TEX "\\bf sourceSlice: additional equations to form a zero-dimensional system (only needed if the map is not finite-to-one)",
                TEX "\\bf imageSlice: a general complementary-dimensional linear space to F(V(I))",
                TEX "\\bf witnessPointPairs: a VerticalList of 2-point sequences (p, F(p)), where p lies on V(I) and F(p) lies on imageSlice",
		TEX "\\bf traceTest: the result of the trace test applied to witnessPointPairs"
                }
        Text
	    The following example demonstrates the output for the 3-uple embedding of P^1 into P^3, whose image is the twisted cubic:
	Example
            R = CC[s,t];
            W = numericalImageDegree(flatten entries basis(3,R), ideal 0_R, verboseOutput => false);
            peek W
    SeeAlso
    	numericalImageDegree
	traceTestThreshold
///

doc ///
    Key
        maxRepetitiveMonodromies
    	[numericalImageDegree, maxRepetitiveMonodromies]
    Headline
    	specify maximum number of repetitive monodromy loops
    Usage
        numericalImageDegree(..., maxRepetitiveMonodromies => 4)
    Description
	Text
    	    Sets the maximum number of consecutive repetitive monodromy loops when computing a pseudo-witness set. A repetitive monodromy loop is one where no new points in the image are discovered. After this many consecutive repetitive monodromy loops occur, the trace test is applied to determine if a complete pseudo-witness set has been found. Default value is 4.
    SeeAlso
    	numericalImageDegree
        PseudoWitnessSet
///

doc ///
    Key
        maxTraceTests
    	[numericalImageDegree, maxTraceTests]
    Headline
    	specify maximum number of trace tests to run
    Usage
        numericalImageDegree(..., maxTraceTests => 10)
    Description
	Text
    	    Sets the maximum number of times the trace test will be attempted when computing a pseudo-witness set. After each failed trace test, a new slice is chosen, the previous points are tracked to the new slice, and monodromy is performed again. If the trace test has failed this many times, an incomplete pseudo-witness set is returned. Default value is 10.
    SeeAlso
    	numericalImageDegree
///

doc ///
    Key
        traceTestThreshold
    	[numericalImageDegree, traceTestThreshold]
    Headline
    	specify threshold for trace test
    Usage
        numericalImageDegree(..., traceTestThreshold => 1e-5)
    Description
	Text
    	    Sets the threshold for a pseudo-witness set to pass the trace test. The trace test for a complete exact pseudo-witness set is 0; large nonzero values indicate failure (the larger the value, the worse the failure). Default value is 1e-5.
    Caveat
        Setting the value of this threshold too high may result in the trace test returning false positives.
    SeeAlso
    	numericalImageDegree
        PseudoWitnessSet
///

doc ///
    Key
        pointEqualityThreshold
    	[numericalImageDegree, pointEqualityThreshold]
    Headline
    	specify threshold for point equality
    Usage
        numericalImageDegree(..., pointEqualityThreshold => 5)
    Description
	Text
    	    Sets the threshold for determing point equality. If this option has value n, then two points are considered equal iff their first n significant digits agree (equivalently, in scientific notation, the exponents and first n digits of the mantissa agree). Default value is 5. 
    SeeAlso
    	numericalImageDegree
///

doc ///
    Key
        maxThreads
    	[numericalImageDegree, maxThreads]
    Headline
    	specify maximum number of processor threads
    Usage
        numericalImageDegree(..., maxThreads => allowableThreads)
    Description
	Text
    	    Sets the maximum number of processor threads that will be used for parallel computation. This divides the number of paths to track in each monodromy loop into the set of processors, as evenly as possible. The value of this option should always be less than the environment variable allowableThreads. Default value is 1.
    Caveat
        This feature is under development. If this option value is larger than 1, then unexpected errors may be printed to output while computing a pseudo-witness set (although the loop will still attempt to run after errors). @BR{}@ @BR{}@
        If the number of paths to track is too low (e.g. < 50), parallel computing will not be used.
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
    	    Specifies the software used in polynomial homotopy continuation (used for path tracking) and numerical irreducible decompositions (used for sampling points). Default value is M2engine.
    SeeAlso
        numericalImageDegree
        numericalImageSample
        numericalImageDim
        numericalHilbertFunction
        isOnImage
///

doc ///
    Key
    	isOnImage
	(isOnImage, PseudoWitnessSet, Point)
	(isOnImage, List, Ideal, Point)
    Headline
    	determines if a point lies on the image of a variety
    Usage
    	isOnImage(W, p)
	isOnImage(F, I, p)
    Inputs
        W:PseudoWitnessSet
            a pseudo-witness set for F(V(I))
	p:Point
	    a point in the ambient space of F(V(I))
        F:List
	    of polynomials, specifying a map
	I:Ideal
	    which is prime, specifying a source variety V(I)
    Outputs
    	:Boolean
	    whether or not the point p lies on F(V(I))
    Description
	Text
	    Determines if a point in the ambient target space lies on the image of a variety. This is done via computing a pseudo-witness set for the image. @BR{}@ @BR{}@
	    It may be useful to compute the pseudo-witness set first, and then run this function with the pseudo-witness set as input.
        Example
            R = CC[x_(1,1)..x_(3,5)]; I = ideal 0_R;
            F = (minors(3, genericMatrix(R, 3, 5)))_*;
            W = numericalImageDegree(F, I, verboseOutput => false)
            p = numericalImageSample(F, I)
            isOnImage(W, p)
    SeeAlso
    	PseudoWitnessSet
        numericalImageDegree
///

TEST /// -- embedding cubic surface (with 3 singular points) in P^3 via 5 sections of O(2)
d = dim ker map(QQ[x,y,z,w]/ideal(x^3 - y*z*w), QQ[a_1..a_5], {x*w + 2*x*y, x*w-3*y^2, z^2, x^2 + y^2 + z^2 - w^2, 3*y*w - 2*x^2})
-- kernel takes ~ 5 seconds
R = CC[x,y,z,w]
I = ideal(x^3 - y*z*w)
F = {x*w + 2*x*y, x*w-3*y^2, z^2, x^2 + y^2 + z^2 - w^2, 3*y*w - 2*x^2}
assert(numericalImageDim(F, I) == d) -- numericalImageDim takes ~1.5 seconds
///

TEST /// -- twisted cubic
R = CC[s,t]
F = flatten entries basis(3,R)
J = monomialCurveIdeal(QQ[x_0..x_3], {1,2,3})
assert(all(1..5, d -> numericalHilbertFunction(F,ideal 0_R,d) == numcols super basis(d,J)))
///

TEST /// -- Rational quartic curve in P^3
R = CC[s,t]
F = flatten entries basis(4, R) - set{s^2*t^2}
h5 = numcols basis(5, ker map(QQ[s,t], QQ[x,y,z,w], {s^4,s^3*t,s*t^3,t^4}))
assert(numericalHilbertFunction(F, ideal(0_R), 5) == h5)
elapsedTime numericalHilbertFunction(F,ideal 0_R,10)
///

TEST /// -- random canonical curve of genus 4, under random projection to P^2 by cubics
R = CC[x_0..x_3]
I = ideal(random(2,R),random(3,R))
F = toList(1..3)/(i -> random(3,R))
assert(numericalImageDegree(F,I) == 18)
///


end--

restart
loadPackage("NumericalImplicitization", Reload => true)
uninstallPackage "NumericalImplicitization"
installPackage "NumericalImplicitization"
installPackage("NumericalImplicitization", RemakeAllDocumentation => true)
viewHelp "NumericalImplicitization"


--Large degree rational normal curve
R = CC[s,t],; F = flatten entries basis(40,R);
numericalImageDegree(F,ideal 0_R,maxRepetitiveMonodromies=>2)
elapsedTime tests = toList(1..100)/(i -> numericalImageDegree(F,ideal 0_R,maxRepetitiveMonodromies=>2,verboseOutput=>false));


--Pinched Veronese
R = CC[x_0..x_3]
F = toList(1..5)/(i -> random(10,R));
allowableThreads = maxAllowableThreads
numericalImageDegree(F,ideal 0_R,maxRepetitiveMonodromies=>2,maxThreads=>maxAllowableThreads)


--Trifocal variety
R=CC[a00,a01,a02,a03,a10,a11,a12,a13,a20,a21,a22,a23,b10,b11,b12,b13,b20,b21,b22,b23],;A = transpose genericMatrix(R,a00,4,3),;B = matrix{{0,0,0,1},{b10,b11,b12,b13},{b20,b21,b22,b23}},;C = matrix{{1_R,0,0,0},{0,1,0,0},{0,0,1,0}},;M = A||B||C,;F = flatten flatten apply(3, i-> apply(3, j-> apply(reverse subsets(3,2), k->det  submatrix(M,{i}|{j+3}|(k+{6,6}) , )  )   ));
allowableThreads = 4
elapsedTime numericalImageDegree(F,ideal 0_R,maxRepetitiveMonodromies=>2,maxThreads=>allowableThreads)


-- checkHilbert test
n = 4,; d = 10,; R = CC[s,t],; F = flatten entries basis(n,R);
elapsedTime numericalHilbertFunction(F,ideal 0_R,d)
binomial(n+d,d) - oo.hilbertFunctionValue
checkLowerBoundHilbert(F,ideal 0_R,d,n*d+1,verboseOutput=>false)
elapsedTime toList(1..10)/(i -> checkLowerBoundHilbert(F,ideal 0_R,d,n*d+1,verboseOutput=>false))
S = QQ[x_0,x_1],; time J = ker map(S,QQ[a_0..a_n],flatten entries basis(n,S));
hilbertFunction(d,J)


-- Tensor product surface
(a,b) = (3,1)
R=CC[s,t,u,v, Degrees=>{{1,0},{1,0},{0,1},{0,1}}]
Ix=intersect(ideal(s,u),ideal(t,v))
B=super basis({a,b},Ix)
C=matrix{{1_R,1,0,0,0,0},{0,1,1,0,0,0},{0,0,1,1,0,0},{0,0,0,1,1,1}}
F = flatten entries(C*transpose(B))
I = ideal 0_R
numericalImageDim(F,I)
W = numericalImageDegree(F,I)
T = numericalHilbertFunction(F,I,W.imageDegree)
extractImageEquations T


--------------- attemptedNiceEquations tests - finding nice defining quadrics
-- run this command:
extractImageEquations(F,ideal 0_R,2)
-- for each of the following:

-- Gr(3,5)
R = CC[x_(1,1)..x_(3,5)]; F = (minors(3, genericMatrix(R, 3, 5)))_*;

-- rational normal curves
R = CC[s,t],; F = flatten entries basis(3,R);
R = CC[s,t],; F = flatten entries basis(4,R);
R = CC[s,t],; F = flatten entries basis(5,R); -- incorrect equations

-- Veronese surface P^2 in P^5
s = symbol s,; R = CC[s_0..s_2],; F = flatten entries basis(2,R);

-- Segre P^1 x P^1
s = symbol s,; t = symbol t,; R = CC[s_0,s_1,t_0,t_1],; F = (ideal(s_0,s_1)*ideal(t_0,t_1))_*;
