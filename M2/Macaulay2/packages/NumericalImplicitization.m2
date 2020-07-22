newPackage("NumericalImplicitization",
    Headline => "numerical invariants of images of varieties",
    Version => "2.1.0",
    Date => "May 18, 2019",
    Authors => {
        {Name => "Justin Chen",
	 Email => "jchen646@math.gatech.edu",
         HomePage => "https://people.math.gatech.edu/~jchen646/"},
        {Name => "Joe Kileel",
	 Email => "jkileel@math.princeton.edu",
	 HomePage => "https://web.math.princeton.edu/~jkileel/"}
        },
    PackageExports => {"NumericalAlgebraicGeometry"},
    Certification => {	
	"journal name" => "The Journal of Software for Algebra and Geometry",	
	"journal URI" => "http://j-sag.org/",	
	"article title" => "Numerical implicitization",	
	"acceptance date" => "11 April 2019",	
	"published article URI" => "https://msp.org/jsag/2019/9-1/p07.xhtml",	
	"published article DOI" => "10.2140/jsag.2019.9.55",	
	"published code URI" => "https://msp.org/jsag/2019/9-1/jsag-v9-n1-x07-NumericalImplicitization.m2",	
	"repository code URI" => "http://github.com/Macaulay2/M2/blob/master/M2/Macaulay2/packages/NumericalImplicitization.m2",	
	"release at publication" => "2f801d123692462f4a65ccb135d411be425c28bd",	    -- git commit number in hex	
	"version at publication" => "2.1.0",	
	"volume number" => "9",	
	"volume URI" => "https://msp.org/jsag/2019/9-1/"	
	}
    )
    export {
        "numericalSourceSample",
        "numericalImageSample",
	"numericalEval",
        "numericalNullity",
        "Precondition",
	"SVDGap",
        "numericalImageDim",
        "numericalHilbertFunction",
        "ConvertToCone",
	"NumericalInterpolationTable",
        "hilbertFunctionArgument",
        "hilbertFunctionValue",
        "UseSLP",
        "imagePoints",
        "interpolationBasis",
        "interpolationSVD",
        "interpolationMatrix",
	"extractImageEquations",
        "AttemptZZ",
	"numericalImageDegree",
	"pseudoWitnessSet",
        "DoRefinements",
	"DoTraceTest",
        "MaxAttempts",
	"MaxPoints",
	"MaxThreads",
	"Repeats",
        "TraceThreshold",
        -- "Endgame",
	"PseudoWitnessSet",
        "isCompletePseudoWitnessSet",
        "sourceEquations",
        "sourceSlice",
        "generalCombinations",
        "imageSlice",
        "witnessPointPairs",
	"isOnImage"
    }

-- software options: default is M2engine throughout

NumericalInterpolationTable = new Type of HashTable
NumericalInterpolationTable.synonym = "numerical interpolation table"
globalAssignment NumericalInterpolationTable
net NumericalInterpolationTable := T -> (
    	(net ofClass class T | ", indicating") ||
	("the space of degree " | (toString T.hilbertFunctionArgument) | 
        " forms in the ideal of the image has dimension " | (toString T.hilbertFunctionValue))
)

PseudoWitnessSet = new Type of HashTable
PseudoWitnessSet.synonym = "pseudo-witness set"
globalAssignment PseudoWitnessSet
net PseudoWitnessSet := W -> (
    	(net ofClass class W | ", indicating") ||
	("the image has degree " | (toString W.degree))
)


checkRings = method(Options => {symbol ConvertToCone => false})
-- checks if the rings of F and I agree and have floating point arithmetic, and converts F, I, pts to the affine cone if ConvertToCone is false
checkRings (Matrix, Ideal, List) := Sequence => opts -> (F, I, pts) -> (
    k := coefficientRing ring I;
    if not numrows F == 1 then error "Expected map to be given by a 1-row matrix of polynomials";
    if not ring F === ring I then error "Expected same rings for ideal and map";
    if not instance(class(1_k), InexactFieldFamily) then error "Expected coefficient field with floating point arithmetic";
    if opts.ConvertToCone then (
        JJ := getSymbol "JJ";
        S := k(monoid[append(gens ring I, JJ)]);
        toS := map(S, ring I);
        ((last gens S)*(toS F | matrix{{1_S}}), toS(I), pts/(p -> point{append(p#Coordinates, 1_k)}))
    ) else (F, I, pts)
)


numericalSourceSample = method(Options => {Software => M2engine})
numericalSourceSample (Ideal, Thing, ZZ) := List => opts -> (I, W, sampleSize) -> (
    R := ring I;
    if I == 0 then ( k := coefficientRing R; return (entries random(k^(sampleSize), k^(#gens R)))/(p -> {p})/point; );
    samplePoints := if instance(W, Point) and not I.cache.?WitnessSet then (
    	d := first numericalDimensions(vars R, I, W);
    	squaredUpSource := randomSlice(gens I, #gens R - d, {});
	startSys := squaredUpSource | randomSlice(vars R, d, {W, "source"});
    	flatten apply(sampleSize, i -> track(startSys, squaredUpSource | randomSlice(vars R, d, {}), {W}, opts))
    ) else (
	if not I.cache.?WitnessSet then I.cache.WitnessSet = if instance(W, WitnessSet) then W else first components(numericalIrreducibleDecomposition(I, opts));
	apply(sampleSize, i -> sample I.cache.WitnessSet)
    );
    if precision R <= precision ring samplePoints#0#Coordinates#0 then samplePoints else refine(polySystem(I_*), samplePoints, Bits => precision R)
)
numericalSourceSample (Ideal, WitnessSet) := List => opts -> (I, W) -> numericalSourceSample(I, W, 1, opts)
numericalSourceSample (Ideal, Point) := List => opts -> (I, p) -> numericalSourceSample(I, p, 1, opts)
numericalSourceSample (Ideal, ZZ) := List => opts -> (I, sampleSize) -> numericalSourceSample(I, null, sampleSize)
numericalSourceSample Ideal := List => opts -> I -> numericalSourceSample(I, 1, opts)

    
numericalImageSample = method(Options => options numericalSourceSample)
numericalImageSample (Matrix, Ideal, List, ZZ) := List => opts -> (F, I, pts, sampleSize) -> (
    samplePoints := if #pts > 0 then numericalSourceSample(I, pts#0, sampleSize-#pts, opts) else numericalSourceSample(I, sampleSize, opts);
    numericalEval(F, samplePoints, false) /point
)
numericalImageSample (Matrix, Ideal, ZZ) := List => opts -> (F, I, sampleSize) -> numericalImageSample(F, I, {}, sampleSize, opts)
numericalImageSample (Matrix, Ideal) := List => opts -> (F, I) -> numericalImageSample(F, I, {}, 1, opts)
numericalImageSample (List, Ideal, List, ZZ) := List => opts -> (F, I, pts, sampleSize) -> numericalImageSample(matrix{F}, I, pts, sampleSize, opts)
numericalImageSample (List, Ideal, ZZ) := List => opts -> (F, I, sampleSize) -> numericalImageSample(matrix{F}, I, {}, sampleSize, opts)
numericalImageSample (List, Ideal) := List => opts -> (F, I) -> numericalImageSample(matrix{F}, I, {}, 1, opts)
numericalImageSample (RingMap, Ideal, List, ZZ) := List => opts -> (F, I, pts, sampleSize) -> numericalImageSample(F.matrix, I, pts, sampleSize, opts)
numericalImageSample (RingMap, Ideal, ZZ) := List => opts -> (F, I, sampleSize) -> numericalImageSample(F.matrix, I, {}, sampleSize, opts)
numericalImageSample (RingMap, Ideal) := List => opts -> (F, I) -> numericalImageSample(F.matrix, I, {}, 1, opts)


numericalEval = method()
numericalEval (Matrix, List, Boolean) := List => (F, upstairsPoints, includeUpstairs) -> ( -- returns a list of either matrices, or pairs of the form (Point, Matrix)
    evalPts := upstairsPoints/(p -> (p, sub(F, matrix p)));
    if includeUpstairs then evalPts else evalPts/last
)


numericalDimensions = method(Options => options numericalSourceSample)
numericalDimensions (Matrix, Ideal, Point) := List => opts -> (F, I, p) -> ( --outputs {dim(V(I)), dim(F(V(I))}
    (F, I, p) = checkRings(F, I, {p});
    p0 := 1/norm(2, matrix p#0)*(matrix p#0);
    dF := sub(transpose jacobian F, p0);
    if I == 0 then return {#gens ring I, #gens ring I - numericalNullity(dF, false)};
    sourceJacobian := sub(transpose jacobian I, p0);
    sourceDim := numericalNullity(sourceJacobian, false);
    {sourceDim, sourceDim - numericalNullity(sourceJacobian || dF, false)}
)
numericalDimensions (Matrix, Ideal) := ZZ => opts -> (F, I) -> numericalDimensions(F, I, first numericalSourceSample(I, Software => opts.Software), opts)


numericalImageDim = method(Options => options numericalSourceSample)
numericalImageDim (Matrix, Ideal, Point) := ZZ => opts -> (F, I, p) -> last numericalDimensions(F, I, p, opts)
numericalImageDim (Matrix, Ideal) := ZZ => opts -> (F, I) -> last numericalDimensions(F, I, opts)
numericalImageDim (List, Ideal, Point) := ZZ => opts -> (F, I, p) -> last numericalDimensions(matrix{F}, I, p, opts)
numericalImageDim (List, Ideal) := ZZ => opts -> (F, I) -> last numericalDimensions(matrix{F}, I, opts)
numericalImageDim (RingMap, Ideal, Point) := ZZ => opts -> (F, I, p) -> last numericalDimensions(F.matrix, I, p, opts)
numericalImageDim (RingMap, Ideal) := ZZ => opts -> (F, I) -> last numericalDimensions(F.matrix, I, opts)


-- converts M to a list of 1-element lists of row matrices (to normalize rows easily)
-- listForm satisfies M == matrix listForm M (if numrows M, numcols M > 0), and this conversion is fast
listForm Matrix := A -> apply(entries A, r -> {matrix{r}})


rowScale := (L, s) -> matrix flatten apply(L, r -> if r#0 == 0 then {} else {(s/norm(2,r#0))*r}) -- deletes any zero rows
-- doubleScale := L -> transpose rowScale((entries transpose rowScale(L,1))/(r -> {matrix{r}}), sqrt(#L/(numcols(L#0#0))))


numericalNullity = method(Options => {symbol SVDGap => 1e5, Verbose => false, symbol Precondition => false})
numericalNullity (List, Boolean) := List => opts -> (M, keepSVD) -> (
    if matrix M == 0 then return if keepSVD then {numcols M#0#0, 0} else numcols M#0#0;
    if opts.Verbose then print "Performing normalization preconditioning ...";
    T := timing A := if opts.Precondition then rowScale(M, 1) else matrix M;
    if opts.Verbose then print("     -- used " | toString(T#0) | " seconds");
    if opts.Verbose then print "Computing numerical kernel ...";
    T = timing (S, U, Vt) := SVD A; -- do not use DivideConquer => true!
    if opts.Verbose then print("     -- used " | toString(T#0) | " seconds");
    largestGap := (#S, opts.SVDGap);
    for i from 1 to #S-1 do (
        if S#i == 0 then ( largestGap = (i, "infinity"); break; )
        else if S#(i-1)/S#i > last largestGap then ( largestGap = (i, S#(i-1)/S#i); break; );
    );
    if keepSVD then {numcols A - first largestGap, (S, U, Vt)} else numcols A - first largestGap
)
numericalNullity (Matrix, Boolean) := ZZ => opts -> (M, keepSVD) -> if numrows M == 0 then numcols M else numericalNullity(listForm M, keepSVD, opts)
numericalNullity Matrix := ZZ => opts -> M -> numericalNullity(M, false, opts)


debug needsPackage "SLPexpressions"

monomialGate = method()
monomialGate (RingElement, List, List) := ProductGate => (m, varList, expList) -> (
     productGate flatten apply(#gens ring m, i -> apply(expList#i, j -> varList#i))
)
monomialGate (RingElement, List) := ProductGate => (m, varList) -> monomialGate(m, varList, first exponents m)


makeInterpolationMatrix = method()
makeInterpolationMatrix (Matrix, List) := List => (mons, pts) -> (
    X := apply(#gens ring mons, i -> inputGate ("x"|i));
    Y := matrix{apply(flatten entries mons, m -> monomialGate(m, X))};
    -- E := makeEvaluator(Y, matrix{X});
    E := makeSLProgram(matrix{X}, Y);
    out := mutableMatrix(ring pts#0, numrows Y, numcols Y);
    apply(pts/mutableMatrix, p -> (
        evaluate(E, p, out);
        {matrix out}
    ))
)


numericalHilbertFunction = method(Options => {
    symbol ConvertToCone => false,
    symbol Precondition => true,
    Software => M2engine,
    symbol SVDGap => 1e5,
    symbol UseSLP => false,
    Verbose => true})
numericalHilbertFunction (Matrix, Ideal, List, ZZ) := NumericalInterpolationTable => opts -> (F, I, sampleImagePoints, d) -> ( --outputs a degree d interpolation table for F(V(I))
    (F, I, sampleImagePoints) = checkRings(F, I, sampleImagePoints, ConvertToCone => opts.ConvertToCone);
    y := getSymbol "y";
    allMonomials := basis(d, (coefficientRing ring I)(monoid[y_0..y_(numcols F-1)]));
    N := numcols allMonomials;
    if #sampleImagePoints < N then (
        if opts.Verbose then print "Sampling image points ...";
    	T := timing sampleImagePoints = sampleImagePoints | numericalImageSample(F, I, sampleImagePoints, N, Software => opts.Software);
	if opts.Verbose then print("     -- used " | toString(T#0) | " seconds");
    );
    sampleImagePoints = apply(sampleImagePoints/matrix, p -> 1/norm(2,p)*p);
    if opts.Verbose then print "Creating interpolation matrix ...";
    T = timing A := if opts.UseSLP then makeInterpolationMatrix(allMonomials, sampleImagePoints) else apply(sampleImagePoints, p -> {sub(allMonomials, p)});
    if opts.Verbose then print("     -- used " | toString(T#0) | " seconds");
    interpolationData := numericalNullity(A, true, Precondition => opts.Precondition, SVDGap => opts.SVDGap, Verbose => opts.Verbose);
    new NumericalInterpolationTable from {
        symbol hilbertFunctionArgument => d,
        symbol hilbertFunctionValue => first interpolationData,
        symbol imagePoints => VerticalList sampleImagePoints,
	symbol interpolationBasis => allMonomials,
        symbol interpolationSVD => last interpolationData,
        symbol interpolationMatrix => matrix A,
	symbol map => F
    }
)
numericalHilbertFunction (Matrix, Ideal, ZZ) := NumericalInterpolationTable => opts -> (F, I, d) -> numericalHilbertFunction(F, I, {}, d, opts)
numericalHilbertFunction (List, Ideal, List, ZZ) := NumericalInterpolationTable => opts -> (F, I, sampleImagePoints, d) -> numericalHilbertFunction(matrix{F}, I, sampleImagePoints, d, opts)
numericalHilbertFunction (List, Ideal, ZZ) := NumericalInterpolationTable => opts -> (F, I, d) -> numericalHilbertFunction(matrix{F}, I, {}, d, opts)
numericalHilbertFunction (RingMap, Ideal, List, ZZ) := NumericalInterpolationTable => opts -> (F, I, sampleImagePoints, d) -> numericalHilbertFunction(F.matrix, I, sampleImagePoints, d, opts)
numericalHilbertFunction (RingMap, Ideal, ZZ) := NumericalInterpolationTable => opts -> (F, I, d) -> numericalHilbertFunction(F.matrix, I, {}, d, opts)


realPartMatrix := A -> matrix apply(entries A, r -> r/realPart)
imPartMatrix := A -> if class ring A === RealField then 0 else matrix apply(entries A, r -> r/imaginaryPart)


extractImageEquations = method(Options => {symbol Threshold => 5, symbol AttemptZZ => false})
extractImageEquations NumericalInterpolationTable := Matrix => opts -> T -> (
    n := opts.Threshold;
    (V, mons) := (last T.interpolationSVD, T.interpolationBasis);
    A := clean(10.0^(-n), conjugate transpose V^{numrows V-T.hilbertFunctionValue..numrows V-1});
    if not opts.AttemptZZ === false then (
        if opts.AttemptZZ === 2 then (
            B := if class ring A === ComplexField then matrix table(numrows A, numcols A, (i,j) -> matrix{{realPart A_(i,j),imaginaryPart A_(i,j)}}) else A;
            C := matrix apply(entries B, r -> r/(e -> lift(round(10^(1+n)*round(n, e)), ZZ)));
            D := submatrix(LLL(C), numcols A..numcols C-1);
            E := mons*colReduce(sub(D, ring mons), 10.0^(-n));
        ) else (
            A = T.interpolationMatrix;
            B = random(RR)*realPartMatrix A + random(RR)*imPartMatrix A;
            C = matrix apply(entries B, r -> r/(e -> lift(round(10^(1+n)*round(n, e)), ZZ)));
            D = submatrix(LLL(id_(ZZ^(numcols C)) || C), toList (0..<numcols mons), toList(0..<T.hilbertFunctionValue));
            E = mons*sub(D, ring mons);
        );
        val := sub(E, T.imagePoints#0);
        if clean(10.0^(-n), val) != 0 then (
            << "Warning: some of the integer equations may be inexact. Their values at a sample image point are " << val << endl;
        );
        E
    ) else mons*sub(A, ring mons)
)
extractImageEquations (Matrix, Ideal, ZZ) := Matrix => opts -> (F, I, d) -> extractImageEquations(numericalHilbertFunction(F, I, d), opts)
extractImageEquations (List, Ideal, ZZ) := Matrix => opts -> (F, I, d) -> extractImageEquations(numericalHilbertFunction(matrix{F}, I, d), opts)
extractImageEquations (RingMap, Ideal, ZZ) := Matrix => opts -> (F, I, d) -> extractImageEquations(numericalHilbertFunction(F.matrix, I, d), opts)


round (ZZ, ZZ) := ZZ => (n, x) -> x
round (ZZ, CC) := CC => (n, x) -> round(n, realPart x) + ii*round(n, imaginaryPart x)
round (ZZ, BasicList) := BasicList => (n, L) -> L/round_n
round (ZZ, Matrix) := Matrix => (n, M) -> matrix(entries M/round_n)
round (ZZ, RingElement) := RingElement => (n, f) -> (
    C := coefficients f;
    ((C#0)*round(n, lift(C#1, coefficientRing ring f)))_(0,0)
)


pseudoWitnessSet = method(Options => {
    symbol DoRefinements => false,
    symbol DoTraceTest => true,
    symbol MaxAttempts => 5,
    symbol MaxPoints => infinity,
    symbol MaxThreads => 1,
    Software => M2engine,
    symbol Repeats => 3,
    symbol TraceThreshold => 1e-5,
    symbol Threshold => 5,
    -- symbol Endgame => false,
    Verbose => true})
pseudoWitnessSet (Matrix, Ideal, List, Thing) := PseudoWitnessSet => opts -> (F, I, pointPairs, sliceMatrix) -> ( --outputs a pseudo-witness set for F(V(I))
    local imagePointString, local pairTable, local startSystem;
    y := getSymbol "y";
    k := coefficientRing ring I;
    targetRing := k(monoid[y_1..y_(numcols F)]);
    if #pointPairs == 0 then error "Expected source point";
    sourcePoint := pointPairs#0#0;
    dims := numericalDimensions(F, I, sourcePoint);
    numAttempts := 0;
    traceResult := opts.TraceThreshold + 1;
    (fiberSlice, fiberdim) := ({}, first dims - last dims);
    while not traceResult < opts.TraceThreshold and numAttempts < opts.MaxAttempts do (
        if numAttempts > 0 then sourcePoint = first numericalSourceSample(I, sourcePoint, Software => opts.Software);
        pullbackSlice := if sliceMatrix === null then randomSlice(F, last dims, {sourcePoint, "source"}) else (
            if numAttempts == 0 and not all(pointPairs, pair -> clean((10.0)^(-opts.Threshold), sub(sliceMatrix, matrix pair#0)) == 0) then error "Expected input points to lie on input slice";
            flatten entries sliceMatrix
        );
        squaredUpSource := if I == 0 then {} else randomSlice(gens I, #gens ring I - first dims, {});
        if fiberdim > 0 then (
	    fiberSlice = randomSlice(vars ring I, fiberdim, {sourcePoint, "source"});
            if numAttempts == 0 and #pointPairs > 1 then pointPairs = numericalEval(F, {sourcePoint} | flatten apply(toList(1..#pointPairs-1), i -> (
		codimSlice := randomSlice(F - sub(matrix pointPairs#i#1, ring F), first dims - fiberdim, {});
		localFiberSlice := codimSlice | squaredUpSource | randomSlice(vars ring I, fiberdim, {pointPairs#i#0, "source"});
		globalFiberSlice := codimSlice | squaredUpSource | fiberSlice;
		myTrack(localFiberSlice, globalFiberSlice, {pointPairs#i#0})
	    )), true);
        );
	newStartSystem := squaredUpSource | fiberSlice | pullbackSlice;
        newPairs := if numAttempts > 0 then numericalEval(F, myTrack(startSystem, newStartSystem, (values pairTable)/first, opts), true) else pointPairs/(pair -> (pair#0, matrix pair#1));
	if #newPairs == 0 then (
            if opts.Verbose then print "Failed to track old points to new slice. Retrying...";
            numAttempts = numAttempts + 1;
            continue;
        );
        pairTable = new MutableHashTable;
        for pair in newPairs do (
            imagePointString = toString round(opts.Threshold, last pair);
            if not pairTable#?imagePointString then pairTable#imagePointString = pair;
        );       
	startSystem = newStartSystem;
        pointPairs = monodromyLoop(F, last dims, startSystem, pairTable, opts);
	if not opts.DoTraceTest then break;
	if opts.DoRefinements then (
	    if opts.Verbose then print "Refining solutions...";
	    pointPairs = numericalEval(F, refine(startSystem, pointPairs/first, Bits => precision ring I), true);
	);
	if opts.Verbose then print("Running trace test ...");
	traceResult = traceTest(F, last dims, pointPairs, startSystem, opts);
	if not traceResult < opts.TraceThreshold and opts.Verbose then print("Failed trace test! Trace: " | toString traceResult);
    	numAttempts = numAttempts + 1;
    );
    if opts.Verbose then (
	if traceResult > opts.TraceThreshold then (
            print("Degree of image should be at least " | #pointPairs);
            print("Consider changing parameters (Repeats, MaxAttempts, Threshold) or reparametrizing for a better result.");
            -- Alternatively, consider increasing precision (e.g. changing ground field to CC_100).
        );
    );
    new PseudoWitnessSet from {
        symbol isCompletePseudoWitnessSet => traceResult < opts.TraceThreshold,
        symbol degree => #pointPairs,
        symbol map => F,
        symbol sourceEquations => I,
        symbol generalCombinations => matrix{squaredUpSource},
        symbol sourceSlice => matrix{fiberSlice},
        symbol imageSlice => matrix{pullbackSlice},
        symbol witnessPointPairs => VerticalList apply(pointPairs, pair -> (pair#0, point pair#1)),
	symbol trace => traceResult
    }
)
pseudoWitnessSet(Matrix, Ideal, Point) := PseudoWitnessSet => opts -> (F, I, p) -> (
    (F, I, p) = checkRings(F, I, {p});
    pseudoWitnessSet(F, I, numericalEval(F, p, true), null, opts)
)
pseudoWitnessSet (Matrix, Ideal) := PseudoWitnessSet => opts -> (F, I) -> (
    if opts.Verbose then print "Sampling point in source ...";
    pseudoWitnessSet(F, I, first numericalSourceSample I, opts)
)
pseudoWitnessSet(List, Ideal, List, Thing) := PseudoWitnessSet => opts -> (F, I, pointPairs, L) -> pseudoWitnessSet(matrix{F}, I, pointPairs, L, opts)
pseudoWitnessSet(List, Ideal, Point) := PseudoWitnessSet => opts -> (F, I, p) -> pseudoWitnessSet(matrix{F}, I, p, opts)
pseudoWitnessSet (List, Ideal) := PseudoWitnessSet => opts -> (F, I) -> pseudoWitnessSet(matrix{F}, I, opts)
pseudoWitnessSet(RingMap, Ideal, List, Thing) := PseudoWitnessSet => opts -> (F, I, pointPairs, L) -> pseudoWitnessSet(F.matrix, I, pointPairs, L, opts)
pseudoWitnessSet(RingMap, Ideal, Point) := PseudoWitnessSet => opts -> (F, I, p) -> pseudoWitnessSet(F.matrix, I, p, opts)
pseudoWitnessSet (RingMap, Ideal) := PseudoWitnessSet => opts -> (F, I) -> pseudoWitnessSet(F.matrix, I, opts)


numericalImageDegree = method(Options => options pseudoWitnessSet)
numericalImageDegree PseudoWitnessSet := ZZ => opts -> W -> W.degree
numericalImageDegree (Matrix, Ideal) := ZZ => opts -> (F, I) -> (pseudoWitnessSet(F, I, opts)).degree
numericalImageDegree (List, Ideal) := ZZ => opts -> (F, I) -> (pseudoWitnessSet(matrix{F}, I, opts)).degree
numericalImageDegree (RingMap, Ideal) := ZZ => opts -> (F, I) -> (pseudoWitnessSet(F.matrix, I, opts)).degree


myTrack = method(Options => options pseudoWitnessSet)
myTrack (List, List, List) := List => opts -> (startSystem, targetSystem, startSolutions) -> (
    k := coefficientRing ring startSystem#0;
    randomGamma := random k;
    if #startSolutions > max(10, 2*opts.MaxThreads) and opts.MaxThreads > 1 then ( -- prints many errors, but continues to run
        --setIOExclusive(); -- buggy: causes isReady to indefinitely hang
	startSolutionsList := pack(ceiling(#startSolutions/opts.MaxThreads), startSolutions);
        threadList := {};
        for paths in startSolutionsList do (
            threadList = append(threadList, schedule(x -> timing track x, (startSystem, targetSystem, paths, gamma => randomGamma, Software => opts.Software)));
        );
        while not all(threadList, isReady) do sleep 1;
	results := delete(null, threadList/taskResult);
        targetSolutions := flatten(results/last);
        if opts.Verbose then print("Finished tracking " | #targetSolutions | " paths in parallel, in " | toString sum(results/first) | " seconds");
    ) else ( -- if startSolutions is empty then error is thrown!
    	T := timing targetSolutions = track(startSystem, targetSystem, startSolutions, gamma => randomGamma, Software => opts.Software);
	if opts.Verbose and T#0 > 1 then print ("     -- used " | toString(T#0) | " seconds");
    );
    goodSols := select(targetSolutions, p -> p#?SolutionStatus and p#SolutionStatus == Regular);
    if opts.Verbose and #goodSols < #startSolutions then print("Paths going to infinity: " | #startSolutions - #goodSols | " out of " | #startSolutions);
    if opts.DoRefinements then goodSols = apply(refine(polySystem targetSystem, goodSols, Bits => precision k), p -> point sub(matrix p, k));
    goodSols
)


randomSlice = method() -- returns a list of c random linear combinations of polys (row matrix) passing through (optional source or target) point, via translation
randomSlice (Matrix, ZZ, List) := List => (polys, c, pointData) -> (
    R := ring polys;
    coeffs := random(R^(numcols polys), R^c);
    G := polys*coeffs;
    flatten entries(G - if #pointData == 0 then 0 else sub(if pointData#1 == "source" then sub(G, matrix pointData#0) else (matrix pointData#0)*coeffs, R))
)


monodromyLoop = method(Options => options pseudoWitnessSet)
monodromyLoop (Matrix, ZZ, List, MutableHashTable) := List => opts -> (F, imageDim, startSystem, pairTable) -> (
    numRepetitiveMonodromyLoops := 0;
    numPts := {#values pairTable};
    if opts.Verbose then print "Tracking monodromy loops ...";
    while numRepetitiveMonodromyLoops < opts.Repeats do (
	intermediateSystem1 := drop(startSystem, -imageDim) | randomSlice(F | matrix{{10_(ring F)}}, imageDim, {});
        startSols := (values pairTable)/first;
        -- increment := if opts.Endgame and #startSols > 100 and #numPts > 5 and all((firstDifference firstDifference numPts)_{-3..-1}, d -> d < 0) then (
            -- startSols = startSols_(randomInts(#startSols, max(100, #startSols//10)));
            -- 1/4
        -- ) else 1;
        intermediateSolutions1 := myTrack(startSystem, intermediateSystem1, startSols, opts);
        if #intermediateSolutions1 > 0 then (
            endSolutions := myTrack(intermediateSystem1, startSystem, intermediateSolutions1, opts);
            if #endSolutions > 0 then (
                candidatePairs := numericalEval(F, endSolutions, true);
                for pair in candidatePairs do (
                    imagePointString := toString round(opts.Threshold, last pair);
                    if not pairTable#?imagePointString then pairTable#imagePointString = pair;
                );
            );
        );
        if numPts#-1 < #values pairTable then numRepetitiveMonodromyLoops = 0
        else numRepetitiveMonodromyLoops = numRepetitiveMonodromyLoops + 1;
        numPts = append(numPts, #values pairTable);
        if opts.Verbose then print ("Points found: " | numPts#-1);
        if numPts#-1 >= opts.MaxPoints then break;
    );
    values pairTable
)


traceTest = method(Options => options pseudoWitnessSet)
traceTest (Matrix, ZZ, List, List) := RR => opts -> (F, imageDim, intersectionPointPairs, startSystem) -> (
    C := coefficientRing ring F;
    startUpstairsPoints := intersectionPointPairs/first;
    startDownstairsPoints := intersectionPointPairs/last;
    for translationMagnitude in {0,1,3,2,-1,5,-2,6} do (
        randomTranslation := 10^(translationMagnitude)*flatten entries(map(C^1, C^(#startSystem - imageDim), 0) | random(C^1, C^imageDim));
        gammas := {random C, random C};
        firstStepSystem := startSystem + (first gammas)*randomTranslation;
        secondStepSystem := startSystem + (last gammas)*randomTranslation;
        firstStepUpstairsPoints := myTrack(startSystem, firstStepSystem, startUpstairsPoints, opts);
        if #firstStepUpstairsPoints == #startUpstairsPoints then (
            secondStepUpstairsPoints := myTrack(startSystem, secondStepSystem, startUpstairsPoints, opts);
            if #secondStepUpstairsPoints == #startUpstairsPoints then (
                firstStepDownstairsPoints := numericalEval(F, firstStepUpstairsPoints, false);
                secondStepDownstairsPoints := numericalEval(F, secondStepUpstairsPoints, false);
                traceList := (1/first gammas)*(firstStepDownstairsPoints - startDownstairsPoints) - (1/last gammas)*(secondStepDownstairsPoints - startDownstairsPoints);
                return norm(2,sum traceList);
            );
        );
    );
    infinity
)


isOnImage = method(Options => {
    MaxThreads => 1,
    Software => M2engine,
    Threshold => 5,
    Verbose => true})
isOnImage (PseudoWitnessSet, Point) := Boolean => opts -> (W, q) -> (
    q = matrix q;
    if not W.isCompletePseudoWitnessSet then print "Warning: not a complete pseudo-witness set! May return false negative.";
    F := W.map;
    I := W.sourceEquations;
    if not ring q === coefficientRing ring I then error "Point must have coordinates in the coefficient ring of the ideal.";
    fiberSlice := flatten entries W.sourceSlice;
    pullbackSlice := flatten entries W.imageSlice;
    squaredUpSource := flatten entries W.generalCombinations;
    startUpstairsPoints := W.witnessPointPairs /first;
    newPullbackSlice := randomSlice(F, #pullbackSlice, {q, "target"});
    targetUpstairsPoints := myTrack(squaredUpSource | fiberSlice | pullbackSlice, squaredUpSource | fiberSlice | newPullbackSlice, startUpstairsPoints, opts);
    imagePointTable := hashTable apply(numericalEval(F, targetUpstairsPoints, false), p -> round(opts.Threshold, p) => 0);
    imagePointTable#?(round(opts.Threshold, q))
)
isOnImage (Matrix, Ideal, Point) := Boolean => opts -> (F, I, q) -> isOnImage(pseudoWitnessSet(F, I, opts), q, opts)
isOnImage (List, Ideal, Point) := Boolean => opts -> (F, I, q) -> isOnImage(matrix{F}, I, q, opts)
isOnImage (RingMap, Ideal, Point) := Boolean => opts -> (F, I, q) -> isOnImage(F.matrix, I, q, opts)


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
	symbol interpolationMatrix,
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
    if not instance(T.map, Matrix) then (
        if debugLevel > 0 then << "-- expected `map' to be a matrix" << endl;
	return false
    );
    if not instance(T.interpolationBasis, Matrix) then (
        if debugLevel > 0 then << "-- expected `interpolationBasis' to be a matrix" << endl;
	return false
    );
    if not instance(T.interpolationMatrix, Matrix) then (
        if debugLevel > 0 then << "-- expected `interpolationMatrix' to be a matrix" << endl;
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
    numMonomials := binomial(numcols T.map + T.hilbertFunctionArgument - 1, T.hilbertFunctionArgument);
    if not #gens ring T.interpolationBasis === numcols T.map or not numcols T.interpolationBasis === numMonomials then (
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
        symbol degree,
        symbol map,
        symbol sourceEquations,
        symbol sourceSlice,
        symbol generalCombinations,
        symbol imageSlice,
        symbol witnessPointPairs,
        symbol trace
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
    if not instance(W.degree, ZZ) then (
        if debugLevel > 0 then << "-- expected `degree' to be an integer" << endl;
	return false
    );
    if not instance(W.map, Matrix) then (
        if debugLevel > 0 then << "-- expected `map' to be a matrix" << endl;
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
    if not instance(W.generalCombinations, Matrix) then (
        if debugLevel > 0 then << "-- expected `generalCombinations' to be a matrix" << endl;
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
    if not instance(W.trace, RR) then (
        if debugLevel > 0 then << "-- expected `trace' to be a real number" << endl;
	return false
    );
    -- CHECK MATHEMATICAL STRUCTURE
    R := ring W.sourceEquations;
    if not R === ring W.map then (
        if debugLevel > 0 then << "-- expected `map' and `sourceEquations' to have the same ring" << endl;
        return false
    );
    if not instance(class 1_(coefficientRing R), InexactFieldFamily) then (
        if debugLevel > 0 then << "-- expected ground field to have floating point arithmetic" << endl;
        return false
    );
    if not all(W.witnessPointPairs, pair -> #(pair#0#Coordinates) === #gens R and #(pair#1#Coordinates) === numcols W.map) then (
        if debugLevel > 0 then << "-- number of coordinates in `witnessPointPairs' do not match" << endl;
        return false
    );
    if not all(W.witnessPointPairs/first, p -> clean(1e-10, sub(gens W.sourceEquations, matrix p)) == 0) then (
	if debugLevel > 0 then << " -- expected first components of `witnessPointPairs' to satisfy `sourceEquations'" << endl;
	return false
    );
    if not all(W.witnessPointPairs, pair -> clean(1e-10, matrix last pair - sub(W.map, matrix first pair)) == 0) then (
	if debugLevel > 0 then << " -- expected components `witnessPointPairs' to correspond under `map'" << endl;
	return false
    );
    if not all(W.witnessPointPairs/first/matrix, p -> clean(1e-10, sub(W.imageSlice, p)) == 0) then (
	if debugLevel > 0 then << " -- expected second components of `witnessPointPairs' to lie on `imageSlice'" << endl;
	return false
    );
    true
)


-- firstDifference = method()
-- firstDifference List := List => L -> drop(L, 1) - drop(L, -1)


-- randomInts = method()
-- randomInts (ZZ, ZZ) := List => (n, s) -> (
     -- L := toList(0..<n);
     -- apply(s, i -> ( a := L#(random(#L)); L = L - set{a}; a ))
-- )


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
	    This package supports user-friendly calculation of basic invariants of the image 
            of a polynomial map. The computational techniques (interpolation, homotopy 
            continuation and monodromy) come from numerical algebraic geometry.

	    Many varieties of interest in algebraic geometry and its applications are usefully 
            described as images of polynomial maps, via a parametrization. Implicitization is the
            process of converting a parametric description of a variety into an intrinsic, or implicit,
            description. Classically, implicitization refers to the procedure of computing the defining
            equations of a parametrized variety, and in theory this is accomplished by finding the
            kernel of a ring homomorphism, via Gr&ouml;bner bases. In practice however, 
            symbolic Gr&ouml;bner basis computations are often time consuming, even for 
            medium scale problems, and do not scale well with respect to the size of the input.

	    Despite this, one would often like to know basic information about a parametrized 
            variety, even when symbolic methods are prohibitively expensive. Examples of 
	    such information are discrete invariants such as the 
            @TO2{numericalImageDim, "dimension"}@, the 
	    @TO2{pseudoWitnessSet, "degree"}@, or 
            @TO2{numericalHilbertFunction, "Hilbert function"}@ 
	    values. Other examples include Boolean tests, for example whether a particular point 
	    @TO2{isOnImage, "lies on"}@ a parametrized variety. The goal of this package is to
            provide such information; in other words to numerically implicitize a parametrized variety.
    
	    {\em NumericalImplicitization} builds on existing numerical algebraic geometry software: 
	    @TO2{NumericalAlgebraicGeometry,"NAG4M2"}@, @TO Bertini@ and 
            @TO PHCpack@. The user may specify any of these to use for path tracking and 
            point sampling; by default, the native software NAG4M2 is used. Currently, all methods 
            are implemented for reduced and irreducible varieties.
    
	    {\bf Reference:} 
            
            [1] A.J. Sommese and C.W. Wampler, 
            The numerical solution of systems of polynomials.
            {\it World Scientific Publishing} (2005).
///

doc ///
    Key
    	numericalSourceSample
	(numericalSourceSample, Ideal, Thing, ZZ)
        (numericalSourceSample, Ideal, WitnessSet)
        (numericalSourceSample, Ideal, Point)
	(numericalSourceSample, Ideal, ZZ)
        (numericalSourceSample, Ideal)
    Headline
    	samples a general point on a variety
    Usage
        numericalSourceSample(I, W, s)
        numericalSourceSample(I, p, s)
        numericalSourceSample(I, W)
        numericalSourceSample(I, p)
    	numericalSourceSample(I, s)
	numericalSourceSample(I)
    Inputs
	I:Ideal
	    which is prime, specifying a variety $V(I)$
	W:WitnessSet
            a witness set for $V(I)$
        p:Point
            a point on the source $V(I)$
        s:ZZ
	    the number of points to sample on the source $V(I)$
    Outputs
    	:List
	    of sample points on the source $V(I)$
    Consequences
        Item
            If $I$ is not the zero ideal, and an initlal point $p$ is not specified, then a numerical
            irreducible decomposition of $I$ is performed, and cached under {\tt I.cache.WitnessSet}.
    Description
	Text
	    This method computes a list of sample points on a variety numerically. If $I$ is the 
            zero ideal in a polynomial ring of dimension $n$, then an $n$-tuple of random 
            elements in the ground field is returned. Otherwise, a 
            @TO2{numericalIrreducibleDecomposition, "numerical irreducible decomposition"}@ 
            of $I$ is computed, which is then used to sample points.

	    If the number of points $s$ is unspecified, then it is assumed that $s = 1$.
            
            One can provide a witness set for $V(I)$ if a witness set is already known. 
            Alternatively, one can provide an initial point $p$ on the source $V(I)$, which is then used to 
            generate additional points on the source $V(I)$. This can be much quicker than performing
            a numerical irreducible decomposition.

	    In the example below, we sample a point from $A^3$ and then $3$ points from
	    $V(x^2 + y^2 + z^2 - 1)$ in $A^3$.
            
        Example
            R = CC[x,y,z];
            samp = numericalSourceSample(ideal 0_R)
            samp#0
            I = ideal(x^2 + y^2 + z^2 - 1);
            numericalSourceSample(I, 3)
        Text
            
            In the following example, we sample a point from $SO(5)$, by starting with the 
            identity matrix as an initial point:
            
        Example
            n = 5
            R = RR[a_(1,1)..a_(n,n)]
            A = genericMatrix(R,n,n);
            I = ideal(A*transpose A - id_(R^n));
            p = point id_(RR^n)
            time q = first numericalSourceSample(I, p)
            O = matrix pack(n, q#Coordinates/realPart)
            clean(1e-10, O*transpose O - id_(RR^n)) == 0
    Caveat
	Since numerical irreducible decompositions are done over @TO CC@, if $I$ is not the zero 
	ideal, then the output will be a point in complex space 
	(regardless of the ground field of the ring of $I$).
    SeeAlso
        numericalImageSample
///

doc ///
    Key
    	numericalImageSample
        (numericalImageSample, Matrix, Ideal, List, ZZ)
	(numericalImageSample, Matrix, Ideal, ZZ)
	(numericalImageSample, Matrix, Ideal)
        (numericalImageSample, List, Ideal, List, ZZ)
        (numericalImageSample, List, Ideal, ZZ)
	(numericalImageSample, List, Ideal)
        (numericalImageSample, RingMap, Ideal, List, ZZ)
        (numericalImageSample, RingMap, Ideal, ZZ)
	(numericalImageSample, RingMap, Ideal)
    Headline
    	samples general points on the image of a variety
    Usage
    	numericalImageSample(F, I, P, s)
        numericalImageSample(F, I, s)
	numericalImageSample(F, I)
    Inputs
    	F:
	    a @TO2{Matrix, "matrix"}@, or @TO2{List, "list"}@, or 
	    @TO2{RingMap, "ring map"}@, specifying a map
	I:Ideal
	    which is prime, specifying a source variety $V(I)$
	P:List
            of points on $F(V(I))$
        s:ZZ
	    the number of points to sample in $F(V(I))$
    Outputs
    	:List
	    of sample points on $F(V(I)))$
    Description
	Text
	    This method computes a list of sample points on the image of a variety 
            numerically, by calling @TO numericalSourceSample@.

	    If the number of points $s$ is unspecified, then it is assumed that $s = 1$.
            
            One can optionally provide an initial list of points $P$ on $F(V(I))$, which 
            will then be completed to a list of $s$ points on $F(V(I))$.

	    The following example samples a point from the twisted cubic. We then 
            independently verify that this point does lie on the twisted cubic.
            
        Example
            R = CC[s,t];
            F = {s^3,s^2*t,s*t^2,t^3};
            p = first numericalImageSample(F, ideal 0_R)
            A = matrix{p#Coordinates_{0,1,2}, p#Coordinates_{1,2,3}};
	    numericalNullity A == 2
    	Text
        
	    Here is how to sample a point from the Grassmannian $Gr(3,5)$ of 
	    $P^2$'s in $P^4$, under its Pl&uuml;cker embedding in $P^9$.
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
	(numericalImageDim, Matrix, Ideal, Point)
	(numericalImageDim, Matrix, Ideal)
        (numericalImageDim, List, Ideal, Point)
	(numericalImageDim, List, Ideal)
        (numericalImageDim, RingMap, Ideal, Point)
	(numericalImageDim, RingMap, Ideal)
    Headline
    	computes the dimension of the image of a variety
    Usage
    	numericalImageDim(F, I, p)
	numericalImageDim(F, I)
    Inputs
    	F:
	    a @TO2{Matrix, "matrix"}@, or @TO2{List, "list"}@, or 
	    @TO2{RingMap, "ring map"}@, specifying a map
	I:Ideal
	    which is prime, specifying a source variety $V(I)$
	p:Point
	    a sample point on the source $V(I)$
    Outputs
    	:ZZ
	    the dimension of $F(V(I)))$
    Description
	Text
	    The method computes the dimension of the image of a variety numerically. 
	    Even if the source variety and map are projective, the affine (Krull) 
            dimension is returned. This ensures consistency with @TO dim@.

	    The following example computes the affine dimension of the Grassmannian 
            $Gr(3,5)$ of $P^2$'s in $P^4$, under its Pl&uuml;cker embedding in $P^9$.
            
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
        
            Next is an example where direct symbolic computation fails to terminate quickly. 
	    Part of the Alexander-Hirschowitz theorem states that the $14$th secant 
	    variety of the $4$th Veronese of $P^4$ has affine dimension $69$, rather than 
	    the expected $14*4 + 13 + 1 = 70$. See J. Alexander, A. Hirschowitz, $Polynomial
            interpolation in several variables$, J. Alg. Geom. 4(2) (1995), 201-222. We 
            numerically verify this below.
            
        Example
            R = CC[a_(1,1)..a_(14,5)];
            F = sum(1..14, i -> basis(4, R, Variables=>toList(a_(i,1)..a_(i,5))));
            time numericalImageDim(F, ideal 0_R)
///

doc ///
    Key
        numericalNullity
        (numericalNullity, Matrix)
        (numericalNullity, Matrix, Boolean)
        (numericalNullity, List, Boolean)
        Precondition
	[numericalHilbertFunction, Precondition]
        [numericalNullity, Precondition]
	SVDGap
	[numericalHilbertFunction, SVDGap]
        [numericalNullity, SVDGap]
    Headline
        computes numerical kernel dimension of a matrix
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
            $M$, then to establish the nullity numerically we look for the 
	    largest "significant" gap between two consecutive singular values, where 
            the gap between $\sigma_i$ and $\sigma_{i+1}$ is "significant" if the ratio 
	    $\sigma_i / \sigma_{i+1}$ exceeds the value of {\tt SVDGap}.
	    If a gap is found which is greater than this threshold, then all singular values 
            after this gap are considered as numerically zero; if all gaps are 
            less than this threshold, then the matrix is considered numerically full rank.
	    The default value of {\tt SVDGap} is $1e5$.
            
            The option {\tt Precondition} specifies whether the rows of 
	    M will be normalized to have norm $1$ before computing the SVD.
            This helps reveal nullity if the matrix is dense (e.g. for a generic 
            interpolation matrix), but not if the matrix is sparse (e.g. diagonal).
	    The default value is @TO false@.
            
        Example
            numericalNullity(matrix{{2, 1}, {0, 1e-5}}, Precondition => false)
            numericalNullity(map(CC^2,CC^2,0))    
    Caveat
        The option {\tt SVDGap} may require tuning by the user.
    SeeAlso
        SVD
        numericalRank
///

doc ///
    Key
    	numericalHilbertFunction
	(numericalHilbertFunction, Matrix, Ideal, List, ZZ)
	(numericalHilbertFunction, Matrix, Ideal, ZZ)
        (numericalHilbertFunction, List, Ideal, List, ZZ)
	(numericalHilbertFunction, List, Ideal, ZZ)
        (numericalHilbertFunction, RingMap, Ideal, List, ZZ)
	(numericalHilbertFunction, RingMap, Ideal, ZZ)
        UseSLP
        [numericalHilbertFunction, UseSLP]
    Headline
    	computes the values of the Hilbert function for the image of a variety
    Usage
    	numericalHilbertFunction(F, I, S, d)
	numericalHilbertFunction(F, I, d)
    Inputs
    	F:
	    a @TO2{Matrix, "matrix"}@, or @TO2{List, "list"}@, or 
	    @TO2{RingMap, "ring map"}@, specifying a map
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
	    This method computes values of the Hilbert function of the 
            image of a variety, by numerical interpolation. In more detail, 
            given a list $S$ of general points on $F(V(I))$ and a degree 
            $d$, the method forms a matrix whose entries are the 
            evaluations of monomials of degree $d$ at points in $S$. 
            The kernel of this interpolation matrix gives degree $d$ 
            equations of the image (provided the number of points in $S$
            is at least the number of degree $d$ monomials). This 
            technique circumvents the calculation of the kernel of the 
            associated ring map.

            In order to speed up computation, the list $S$ of points 
            can be precomputed (see @TO numericalImageSample@). 
            This list of points can then be re-used in multiple 
            interpolation computations (which can yield a large 
            speedup over performing separate sampling instances, 
            if the ideal $I$ is not the zero ideal).
            
            For a further speedup, the option {\tt UseSLP} allows for 
            the usage of @TO2{SLPexpressions, "straight-line programs"}@
            in creating the interpolation matrix.

            In the following, we compute the dimension of the space of 
            quartics in the ideal of the twisted cubic and obtain the expected 
            answer, $22$. Note that one can verify this by dimension counting:
            quartics in the coordinate ring pull back to forms of degree 
            $12$ on $P^1$, of which there is a $13$-dimensional
            space; thus the space of quartics in the 
            defining ideal has dimension $35 - 13 = 22$.
            
        Example
            R = CC[s,t]
            F = basis(3, R)
            numericalHilbertFunction(F, ideal 0_R, 4)
        Text
        
            The following example computes the dimension of Pl&uuml;cker quadrics in 
            the defining ideal of the Grassmannian $Gr(3,5)$ of $P^2$'s in $P^4$,
	    in the ambient space $P^9$.
            
        Example
            R = CC[x_(1,1)..x_(3,5)];
            F = (minors(3, genericMatrix(R, 3, 5)))_*;
            S = numericalImageSample(F, ideal 0_R, 60);
            numericalHilbertFunction(F, ideal 0_R, S, 2, UseSLP => true)
    SeeAlso
    	NumericalInterpolationTable
        extractImageEquations
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
        interpolationMatrix
    Headline
    	the class of all NumericalInterpolationTables
    Description
	Text
    	    This is a type of hash table storing the output of a 
            polynomial interpolation computation, with the following keys: 
        Code
            UL {
                TEX "\\bf hilbertFunctionArgument: the argument, $d$, to the Hilbert function",
                TEX "\\bf hilbertFunctionValue: the value of the Hilbert function at $d$",
                TEX "\\bf imagePoints: a (vertical) list of sample points on the image",
		TEX "\\bf interpolationBasis: a matrix consisting of the degree $d$ monomials",
                TEX "\\bf interpolationSVD: the singular value decomposition of the interpolation matrix",
                TEX "\\bf interpolationMatrix: the matrix obtained by evaluating degree $d$ monomials at the sample points",
		TEX "\\bf map: the map $F$, of which the image is under consideration"
                }
        Example
            R = CC[x_(1,1)..x_(3,5)];
            F = (minors(3, genericMatrix(R, 3, 5)))_*;
            T = numericalHilbertFunction(F, ideal 0_R, 2, Verbose => false)
            (T.hilbertFunctionArgument, T.hilbertFunctionValue)
    SeeAlso
    	numericalHilbertFunction
///

doc ///
    Key
    	extractImageEquations
        (extractImageEquations, Matrix, Ideal, ZZ)
        (extractImageEquations, List, Ideal, ZZ)
        (extractImageEquations, RingMap, Ideal, ZZ)
	(extractImageEquations, NumericalInterpolationTable)
        [extractImageEquations, Threshold]
        AttemptZZ
        [extractImageEquations, AttemptZZ]
    Headline
    	finds implicit equations in a fixed degree for the image of a variety
    Usage
        extractImageEquations(F, I, d)
    	extractImageEquations T
    Inputs
        T:NumericalInterpolationTable
            a numerical interpolation table for $F(V(I))$ of degree $d$
    	F:
	    a @TO2{Matrix, "matrix"}@, or @TO2{List, "list"}@, or 
	    @TO2{RingMap, "ring map"}@, specifying a map
	I:Ideal
	    which is prime, specifying a source variety $V(I)$
    	d:ZZ
	    the argument of the Hilbert function of $F(V(I))$
    Outputs
    	:Matrix
	    of implicit degree d equations for $F(V(I))$
    Description
	Text
	    This method finds (approximate) implicit degree $d$ equations for the image 
            of a variety, by @TO2{numericalHilbertFunction, "numerical interpolation"}@. 
            The option {\tt AttemptZZ} specifies whether to use the @TO LLL@ algorithm
            to compute "short" equations over @TO ZZ@. The default value is @TO false@.

	    If a numerical interpolation table has already been computed, then 
            to avoid repetitive calculation one may run this function with the interpolation 
            table as input.

            For example, we determine the defining quadrics of the twisted cubic, as follows.
            
        Example
            R = CC[s,t]
            F = basis(3, R)
            extractImageEquations(F, ideal 0_R, 2, AttemptZZ => true)
        Text
        
            Here is how to do the same computation symbolically.
            
        Example
            gens ker map(QQ[s,t], QQ[y_0..y_3], {s^3,s^2*t,s*t^2,t^3})
        Text
        
	    We determine the $5$ Pl&uuml;cker quadrics defining the Grassmannian 
            $Gr(3,5)$ of $P^2$'s in $P^4$, in the ambient space $P^9$.
            
        Example
            R = CC[x_(1,1)..x_(3,5)]; I = ideal 0_R;
            F = (minors(3, genericMatrix(R, 3, 5)))_*;
	    T = numericalHilbertFunction(F, I, 2, Verbose => false);
	    extractImageEquations(T, AttemptZZ => true)
        Text
        
    	    The option {\tt Threshold} sets the threshold for rounding the interpolation matrix. 
            If this option has value $n$, then the interpolation matrix will be rounded
            to $n$ decimal digits, after which LLL will be performed. The default value is $5$.
    SeeAlso
    	numericalHilbertFunction
        NumericalInterpolationTable
///

doc ///
    Key
    	numericalImageDegree
	(numericalImageDegree, PseudoWitnessSet)
	(numericalImageDegree, Matrix, Ideal)
        (numericalImageDegree, List, Ideal)
        (numericalImageDegree, RingMap, Ideal)
    Headline
    	computes the degree of the image of a variety
    Usage
    	numericalImageDegree W
	numericalImageDegree(F, I)
    Inputs
        W:PseudoWitnessSet
            a pseudo-witness set for $F(V(I))$
        F:
	    a @TO2{Matrix, "matrix"}@, or @TO2{List, "list"}@, or 
	    @TO2{RingMap, "ring map"}@, specifying a map
	I:Ideal
	    which is prime, specifying a source variety $V(I)$
    Outputs
    	:ZZ
	    the degree of $F(V(I))$
    Description
	Text
	    This method the degree of the image of a variety, by computing a 
            pseudo-witness set for the image (cf.
	    @TO2{pseudoWitnessSet, "pseudo-witness set"}@ for more on the
	    computational techniques and options used).

            If a pseudo-witness set has already been computed, then 
            to avoid repetitive calculation one may run this function with the 
            pseudo-witness set as input.

            The following example determines the degree of the
            Grassmannian $Gr(3,5)$ of $P^2$'s in $P^4$, 
	    under its Pl&uuml;cker embedding in $P^9$.
            
        Example
            R = CC[x_(1,1)..x_(3,5)]; I = ideal 0_R;
            F = (minors(3, genericMatrix(R, 3, 5)))_*;
	    numericalImageDegree(F, I, Repeats => 2, Verbose => false)
    SeeAlso
    	pseudoWitnessSet
	PseudoWitnessSet
///

doc ///
    Key
    	pseudoWitnessSet
        (pseudoWitnessSet, Matrix, Ideal)
	(pseudoWitnessSet, Matrix, Ideal, Point)
	(pseudoWitnessSet, Matrix, Ideal, List, Thing)
        (pseudoWitnessSet, List, Ideal)
	(pseudoWitnessSet, List, Ideal, Point)
	(pseudoWitnessSet, List, Ideal, List, Thing)
        (pseudoWitnessSet, RingMap, Ideal)
	(pseudoWitnessSet, RingMap, Ideal, Point)
	(pseudoWitnessSet, RingMap, Ideal, List, Thing)
        Repeats
    	[pseudoWitnessSet, Repeats]
	[numericalImageDegree, Repeats]
        MaxAttempts
    	[pseudoWitnessSet, MaxAttempts]
	[numericalImageDegree, MaxAttempts]
        MaxPoints
    	[pseudoWitnessSet, MaxPoints]
        [numericalImageDegree, MaxPoints]
	DoRefinements
        [pseudoWitnessSet, DoRefinements]
	[numericalImageDegree, DoRefinements]
	DoTraceTest
	[pseudoWitnessSet, DoTraceTest]
        [numericalImageDegree, DoTraceTest]
	TraceThreshold
    	[pseudoWitnessSet, TraceThreshold]
    	[numericalImageDegree, TraceThreshold]
	[pseudoWitnessSet, Threshold]
        [numericalImageDegree, Threshold]
	[isOnImage, Threshold]
    Headline
    	computes a pseudo-witness set for the image of a variety
    Usage
	pseudoWitnessSet(F, I)
	pseudoWitnessSet(F, I, p)
	pseudoWitnessSet(F, I, P, L)
    Inputs
    	F:
	    a @TO2{Matrix, "matrix"}@, or @TO2{List, "list"}@, or 
	    @TO2{RingMap, "ring map"}@, specifying a map
	I:Ideal
	    which is prime, specifying a source variety $V(I)$
	p:Point
	    a general point on the source $V(I)$
	P:List
	    of pairs $(p, q)$ with $p$ a general point on the source $V(I)$,
	    and $q = F(p)$. In this case an input slice $L$ must also be 
            provided, and $q$ should additionally lie on $L$.
	L:Matrix
	    representing a linear slice of $F(V(I))$. The format
	    should be a row matrix, whose entries are linear forms
	    in the ambient target space of $F(V(I))$. If this is provided
	    then a nonempty list $P$ of point pairs must also be provided.
    Outputs
    	:PseudoWitnessSet
	    a pseudo-witness set for $F(V(I))$
    Description
	Text
	    This method computes a @TO2{PseudoWitnessSet, "pseudo-witness set"}@
            for the image of a variety, by computing the intersection of the 
	    image with a complementary-dimensional linear slice via tracking 
	    monodromy loops with homotopy continuation, and then applying the 
            trace test. If the trace test fails, only a 
            lower bound for the degree and an incomplete pseudo-witness set 
            is returned. This technique circumvents the calculation of the 
            kernel of the associated ring map.
	    
	    The method also allows the user to provide a particular linear slice $L$ of the 
	    image. In this case a list of point pairs $(p, q)$ such that $p$ is in $V(I)$,
	    $q = F(p)$, and $q$ is in $L$, must be provided (to have an initial input point to 
	    the monodromy - even if it only consists of a single such pair). 
	    The method then applies monodromy to try to compute the entire intersection 
	    $F(V(I))\cap L$. If no linear slice is given, then a random 
	    complementary-dimensional linear slice will be chosen, in which case no 
	    seed is needed, as an initial point pair will be chosen to lie on the slice.

            The following example computes the degree of the Grassmannian 
            $Gr(3,5)$ of $P^2$'s in $P^4$, under its Pl&uuml;cker embedding in $P^9$.
            
        Example
            R = CC[x_(1,1)..x_(3,5)];
            F = (minors(3, genericMatrix(R, 3, 5)))_*;
            W = pseudoWitnessSet(F, ideal 0_R)
            W.isCompletePseudoWitnessSet
            W.degree
        Text
        
            This method can also handle cases where the parameterization 
            has positive dimensional fibers. In the example below, we verify that 
            the variety of $3 x 3 x 3$ tensors of border rank $<= 4$, i.e. the $4$th secant 
            variety of $P^2 x P^2 x P^2$, has degree $9$. This is a hypersurface, 
            with defining equation known as Strassen's invariant,
            and it is also a defective secant variety (meaning its dimension is less
            than expected). Here, the parametrization has $10$ dimensional fibers.
	    For more on this example, see V. Strassen, $The asymptotic spectrum of tensors$, 
	    J. Reine Angew. Math. 384 (1988), 102-152.
            
        CannedExample
            i6 : R = CC[a_(0,0)..a_(3,2), b_(0,0)..b_(3,2), c_(0,0)..c_(3,2)];
            
            i7 : F = toList apply((0,0,0)..(2,2,2), (i,j,k) ->
                    a_(0,i)*b_(0,j)*c_(0,k) +
                    a_(1,i)*b_(1,j)*c_(1,k) +
                    a_(2,i)*b_(2,j)*c_(2,k) +
                    a_(3,i)*b_(3,j)*c_(3,k));
                    
            i8 : pseudoWitnessSet(F, ideal 0_R, Repeats => 2)
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
        Text
        
            Finally, this method has a large number of optional inputs which may be 
            specified by the user to fit a particular problem instance. 

    	    The option {\tt Repeats} sets the maximum number of consecutive repetitive 
            monodromy loops when computing a pseudo-witness set. A repetitive 
            monodromy loop is one where no new points in the image are discovered. 
            After this many consecutive repetitive monodromy loops occur, the trace 
            test is applied to determine if a complete pseudo-witness set has 
            been found. The default value is $3$.

    	    The option {\tt MaxAttempts} sets the maximum number of times the trace test 
            will be attempted when computing a pseudo-witness set. After a trace test 
            fails, a new slice is chosen, the previous points are tracked to the new 
            slice, and monodromy is performed anew. If the trace test has failed 
            {\tt MaxAttempts} many times, an incomplete pseudo-witness set is returned. 
            The default value is $5$.
            
            Here is an example in which a badly chosen random seed results in a 
            failed trace test on the first attempt.  In later attempts, the trace test 
            passes and the degree of the twisted cubic is correctly computed to be $3$.
            
        Example
            setRandomSeed 10
            R = CC[s,t]
            F = basis(3, R)
            pseudoWitnessSet(F, ideal 0_R)
        Text
        
            We compare this with the native $Macaulay2$ function 
	    @TO2{(degree, Ideal), "degree"}@ (using a symbolic Gr&ouml;bner basis computation).
            
        Example
            degree ker map(QQ[s,t], QQ[y_0..y_3], {s^3,s^2*t,s*t^2,t^3})
        Text
            
            The option {\tt MaxPoints} sets a number of points such that if more than this 
	    number of points is found following a monodromy loop, then the method gracefully 
	    exits. The option is especially useful in the case that the user specifies a 
	    linear slice $L$ (as discussed above) which is in special position with respect to 
	    $F(V(I))$ (e.g. if $F(V(I))\cap L$ is positive-dimensional). The default value 
	    is @TO infinity@.
            
            The option {\tt DoRefinements} specifies whether or not to refine solution points found 
            via monodromy. Refinement of points may improve their accuracy. If the value of this
            option is true, then refinement occurs after every tracking (which may increase the time 
            for computation). The default value is @TO false@.
	    
	    The option {\tt DoTraceTest} specifies whether or not to run the trace test. This is
	    useful when the user specifies a special linear slice $L$ (as in the discussion on
	    {\tt MaxPoints} above). The default value is @TO true@.
            
    	    The option {\tt TraceThreshold} sets the threshold for a pseudo-witness set to pass 
            the trace test. The trace for a complete exact pseudo-witness set is 
            $0$; large nonzero values indicate failure (the larger the value, the worse 
            the failure). The default value is $1e-5$.

    	    The option {\tt Threshold} sets the threshold for determing point equality. 
            If this option has value $n$, then two points are considered equal iff their 
            first $n$ significant digits agree (equivalently, in scientific notation, the 
            exponents and first $n$ digits of the mantissa agree). The default value is $5$. 
    SeeAlso
    	PseudoWitnessSet
	numericalImageDegree
///

doc ///
    Key
        PseudoWitnessSet
        (net, PseudoWitnessSet)
        isCompletePseudoWitnessSet
	sourceEquations
        sourceSlice
        generalCombinations
        imageSlice
        witnessPointPairs
    Headline
    	the class of all pseudo-witness sets
    Description
	Text
            This is a type of hash table storing the output of a 
            pseudo-witness set computation using monodromy, 
            with the following keys:
        Code
            UL {
                {TEX "\\bf isCompletePseudoWitnessSet: whether the pseudo-witness set has passed the trace test, according to the trace test threshold"},
                TEX "\\bf degree: the number of image points found by monodromy",
                TEX "\\bf map: the map $F$, of which the image is under consideration",
                TEX "\\bf sourceEquations: the defining ideal $I$ of the source variety",
                {TEX "\\bf sourceSlice: additional equations to form a zero-dimensional system (only needed if the map is not finite-to-one)"},
                {TEX "\\bf generalCombinations: additional equations to form a zero-dimensional system (only needed if the source ideal is not a complete intersection)"},
                TEX "\\bf imageSlice: the pullback under F of a general complementary-dimensional linear space to $F(V(I))$",
                {TEX "\\bf witnessPointPairs: a vertical list of 2-point sequences $(p, F(p))$, where $p$ lies on the source $V(I)$ and $F(p)$ lies on imageSlice"},
                TEX "\\bf trace: the result of the trace test applied to witnessPointPairs"
                }
        Text
	    For a discussion of pseudo-witness sets, 
	    see J.D. Hauenstein and A.J. Sommese, $Witness sets of projections$, 
	    Appl. Math. Comput. 217(7) (2010), 3349-3354. 
	    
	    The following example demonstrates the output for the 
            degree $3$ embedding of $P^1$ into $P^3$, whose image is the twisted cubic.
            
	Example
            R = CC[s,t];
            W = pseudoWitnessSet(basis(3,R), ideal 0_R, Verbose => false);
            peek W
    SeeAlso
    	pseudoWitnessSet
	numericalImageDegree
///

doc ///
    Key
    	isOnImage
	(isOnImage, PseudoWitnessSet, Point)
	(isOnImage, Matrix, Ideal, Point)
        (isOnImage, List, Ideal, Point)
        (isOnImage, RingMap, Ideal, Point)
    Headline
    	tests whether a point lies on the image of a variety
    Usage
    	isOnImage(W, p)
	isOnImage(F, I, p)
    Inputs
        W:PseudoWitnessSet
            a pseudo-witness set for $F(V(I))$
	p:Point
	    a point in the ambient space of $F(V(I))$
        F:
	    a @TO2{Matrix, "matrix"}@, or @TO2{List, "list"}@, or 
	    @TO2{RingMap, "ring map"}@, specifying a map
	I:Ideal
	    which is prime, specifying a source variety $V(I)$
    Outputs
    	:Boolean
	    whether the point $p$ lies on $F(V(I))$
    Description
	Text
	    This method determines if a point in the ambient target space 
            lies on the image of a variety. This is done via computing a 
            pseudo-witness set for the image.

            If a pseudo-witness set has already been computed, then 
            to avoid repetitive calculation one may run this function with the 
            pseudo-witness set as input.

            The following example determines whether a point lies on the
            Grassmannian $Gr(3,5)$ of $P^2$'s in $P^4$, 
	    under its Pl&uuml;cker embedding in $P^9$.
            
        Example
            R = CC[x_(1,1)..x_(3,5)]; I = ideal 0_R;
            F = (minors(3, genericMatrix(R, 3, 5)))_*;
            W = pseudoWitnessSet(F, I, Repeats => 2, Verbose => false);
            q = first numericalImageSample(F, I)
            isOnImage(W, q)
            isOnImage(W, point random(CC^1, CC^#F))
            isOnImage(W, point{{1_CC,0,0,0,0,0,0,0,0,0}})
    SeeAlso
    	pseudoWitnessSet
	PseudoWitnessSet
///

doc ///
    Key
        [numericalImageDegree, Verbose]
	[pseudoWitnessSet, Verbose]
        [numericalHilbertFunction, Verbose]
        [numericalNullity, Verbose]
        [isOnImage, Verbose]
    Headline
    	display detailed output
    Usage
        pseudoWitnessSet(..., Verbose => true)
	numericalImageDegree(..., Verbose => true)
	numericalHilbertFunction(..., Verbose => true)
	isOnImage(..., Verbose => true)
        numericalNullity..., Verbose => true)
    Description
	Text
    	    This option determines whether detailed output is displayed 
            during an interpolation or monodromy computation, 
            including timings for various intermediate computations. 
            The default value is @TO true@.
    SeeAlso
        numericalHilbertFunction
	numericalImageDegree
    	pseudoWitnessSet
	isOnImage
///

doc ///
    Key
        ConvertToCone
        [numericalHilbertFunction, ConvertToCone]
    Headline
        specifies whether to convert image to a cone
    Usage
        numericalHilbertFunction(..., ConvertToCone => false)
    Description
        Text
            This option specifies whether to replace the image $F(V(I))$ with 
	    the cone over $F(V(I))$. 
	    If true, then internally the target variety is treated
            as the affine cone over its projective closure - to be precise,
	    the map $F$ is replaced with $t[F, 1]$, where $t$ is a new variable. 
	    The default value is @TO false@.
	    
	    Since @TO numericalHilbertFunction@ works by interpolating monomials
	    (and thus only finds graded relations in the ideal of the image), 
	    this option is necessary when the map is not homogeneous.
	    The following example demonstrates this for an affine rational curve.
	    
    	Example
	    R = CC[t]
	    F = {t, t^4, t^6}
	    I = ideal 0_R
	    (numericalHilbertFunction(F, I, 3, Verbose => false)).hilbertFunctionValue == 0
	    T = numericalHilbertFunction(F, I, 3, ConvertToCone => true)
	    extractImageEquations(T, AttemptZZ => true)
    SeeAlso
        numericalHilbertFunction
///

doc ///
    Key
        MaxThreads
	[numericalImageDegree, MaxThreads]
    	[pseudoWitnessSet, MaxThreads]
        [isOnImage, MaxThreads]
    Headline
    	specifies the maximum number of processor threads
    Usage
        pseudoWitnessSet(..., MaxThreads => allowableThreads)
	numericalImageDegree(..., MaxThreads => allowableThreads)
	isOnImage(..., MaxThreads => allowableThreads)
    Description
	Text
    	    This option sets the maximum number of processor threads that will be used 
            for parallel computation. This distributes the paths to track in each 
            monodromy loop among the processors as evenly as possible. 
            The value of this option should not exceed the value of the variable 
            {\tt allowableThreads}. The default value is $1$.
    Caveat
        Parallel computation in $Macaulay2$ is under development. Unexpected errors 
        may be printed to output while computing a pseudo-witness set - however, the 
        loop will still run, and an answer will still be returned.
        
        If the number of paths to track is too low (i.e. less than or equal to $20$), 
	then parallel computing will not be used.
    SeeAlso
    	numericalImageDegree
	pseudoWitnessSet
	isOnImage
///

doc ///
    Key
        [pseudoWitnessSet, Software]
	[numericalImageDegree, Software]
	[numericalSourceSample, Software]
        [numericalImageSample, Software]
        [numericalImageDim, Software]
        [numericalHilbertFunction, Software]
        [isOnImage, Software]
    Headline
    	specify software for homotopy continuation
    Usage
        pseudoWitnessSet(..., Software => M2engine)
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
            M2engine (native to $Macaulay2$). Other possible values are 
            @TO Bertini@ and @TO PHCpack@ (only if the user has these 
            packages installed).
    SeeAlso
        pseudoWitnessSet
	numericalImageDegree
	numericalSourceSample
        numericalImageSample
        numericalImageDim
        numericalHilbertFunction
        isOnImage
///

undocumented {
    numericalEval,
    (numericalEval, Matrix, List, Boolean),
    (isWellDefined, PseudoWitnessSet),
    (isWellDefined, NumericalInterpolationTable)
}


TEST /// -- embedding cubic surface (with 3 singular points) in P^3 via 5 sections of O(2)
setRandomSeed 0
elapsedTime d = dim ker map(QQ[x,y,z,w]/ideal(x^3 - y*z*w), QQ[a_0..a_4], {x*w + 2*x*y, x*w-3*y^2, z^2, x^2 + y^2 + z^2 - w^2, 3*y*w - 2*x^2})
R = CC[x,y,z,w]
I = ideal(x^3 - y*z*w)
F = {x*w + 2*x*y, x*w-3*y^2, z^2, x^2 + y^2 + z^2 - w^2, 3*y*w - 2*x^2}
assert(numericalImageDim(F, I) == d)
-- Cf. also: non-homogeneous ideal (x^5 - y*z*w) (~35 seconds for GB computation), kernel over finite fields
///

TEST /// -- twisted cubic
setRandomSeed 0
R = CC[s,t]
F = basis(3,R)
J = monomialCurveIdeal(QQ[a_0..a_3], {1,2,3})
assert(all(1..5, d -> (numericalHilbertFunction(F,ideal 0_R,d)).hilbertFunctionValue == numcols super basis(d,J)))
assert(all(1..5, d -> (numericalHilbertFunction(F,ideal 0_R,d,UseSLP=>true)).hilbertFunctionValue == numcols super basis(d,J)))
W = pseudoWitnessSet(F, ideal 0_R);
assert(W.degree == 3)
assert(isOnImage(W, first numericalImageSample(F,ideal 0_R)) == true)
assert(isOnImage(W, point random(CC^1,CC^(numcols F))) == false)
///

TEST /// -- Rational quartic curve in P^3
setRandomSeed 0
R = CC[s,t]
F = flatten entries basis(4, R) - set{s^2*t^2}
I = ideal 0_R
S = QQ[a_0..a_3]
I3 = super basis(3, ker map(QQ[s,t], S, {s^4,s^3*t,s*t^3,t^4}))
T = numericalHilbertFunction(F, I, 3);
M = extractImageEquations(T, AttemptZZ => true)
assert(image M == image (map(ring M, S, gens ring M))(I3))
elapsedTime PW = pseudoWitnessSet(F,I)
assert(PW.degree == 4)
///

TEST /// -- Grassmannian Gr(3, 5) = G(P^2,P^4)
setRandomSeed 0
(k, n) = (3,5)
R = CC[x_(1,1)..x_(k,n)]
I = ideal 0_R
F = (minors(k, genericMatrix(R, k, n)))_*
assert(numericalImageDim(F, I) == 1 + k*(n-k))
T = numericalHilbertFunction(F, I, 2)
J = super basis(2, Grassmannian(k-1,n-1))
assert(T.hilbertFunctionValue == numcols J)
I2 = image extractImageEquations(T, AttemptZZ => true)
assert(image (map(ring I2, ring J, gens ring I2))(J) == I2)
time W = pseudoWitnessSet(F, I, Repeats => 2)
assert(W.degree == 5)
(n, m) = (5, 10)
pointList = numericalImageSample(F, I, n);
assert(all(pointList, q -> (tally apply(m, i -> isOnImage(W, q)))#true / m >= 8/10))
///

TEST /// -- random canonical curve of genus 4, under random projection to P^2 by cubics
setRandomSeed 0
R = CC[x_0..x_3]
I = ideal(random(2,R),random(3,R))
F = random(R^1,R^{3:-3})
assert(numericalImageDegree(F,I) == 18)
assert((numericalHilbertFunction(F,I,18)).hilbertFunctionValue == 1)
///

TEST /// -- Segre + Veronese
setRandomSeed 0
-- Veronese surface P^2 in P^5
(d, n) = (2, 2)
R = CC[x_0..x_n]
F = basis(d, R)
PW = pseudoWitnessSet(F, ideal 0_R)
assert(PW.degree == 4)
assert((pseudoWitnessSet(PW.map, PW.sourceEquations, PW.witnessPointPairs_{1}, PW.imageSlice)).degree == 4)
assert((pseudoWitnessSet(PW.map, PW.sourceEquations, PW.witnessPointPairs_{0,2}, PW.imageSlice)).degree == 4)
I2 = ideal extractImageEquations(F, ideal 0_R, 2, AttemptZZ => true)
S = QQ[y_0..y_(binomial(d+n,d)-1)]
RQ = QQ[x_0..x_n]
J = ker map(RQ, S, basis(d, RQ))
assert((map(ring I2, S, gens ring I2))(J) == I2)
-- Segre P^2 x P^3
(n1, n2) = (2, 4)
R = CC[s_0..s_(n1), t_0..t_(n2)]
F = (ideal(s_0..s_(n1))*ideal(t_0..t_(n2)))_*
I2 = ideal extractImageEquations(F, ideal 0_R, 2, AttemptZZ => true)
RQ = QQ[s_0..s_(n1), t_0..t_(n2)]
S = QQ[y_0..y_((n1+1)*(n2+1)-1)]
J = ker map(RQ, S, (ideal(s_0..s_(n1))*ideal(t_0..t_(n2)))_*)
assert((map(ring I2, S, gens ring I2))(J) == I2)
///

TEST /// -- Iterated Veronese
(d1,d2) = (2,3)
R = CC[x_0..x_(d1)]
I = ideal(x_0*x_2 - x_1^2)
W = first components numericalIrreducibleDecomposition I
I.cache.WitnessSet = W;
S = CC[y_0..y_(binomial(d1+d2,d2)-1)]
F = map(R,S,basis(d2,R))
eps = 1e-10
p1 = first numericalSourceSample(I)
assert(clean(eps, sub(gens I, matrix p1)) == 0)
p2 = first numericalSourceSample(I, W)
assert(clean(eps, sub(gens I, matrix p2)) == 0)
p3 = first numericalSourceSample(I, p1)
assert(clean(eps, sub(gens I, matrix p3)) == 0)
P = numericalSourceSample(I, W, 10)
assert(all(P/matrix, p -> clean(eps, sub(gens I, p)) == 0))
q1 = numericalImageSample(F, I)
S = numericalImageSample(F,I,55);
T = numericalHilbertFunction(F,I,S,2)
assert(T.hilbertFunctionValue == 42)
assert(isWellDefined T)
PW = pseudoWitnessSet(F, I, p1)
assert(isWellDefined PW)
assert(PW.degree == 6)
assert(numericalImageDegree(F, I) == 6)
assert((pseudoWitnessSet(F, I, {PW.witnessPointPairs#0}, PW.imageSlice)).degree == 6)
assert((pseudoWitnessSet(F, I, PW.witnessPointPairs, PW.imageSlice)).degree == 6)
///

TEST /// -- Orthogonal group O(n)
setRandomSeed 0
n = 4
R = CC[x_0..x_(n^2-1)]
A = genericMatrix(R,n,n)
I = ideal(A*transpose A - id_(R^n));
F = vars R
p = point id_((coefficientRing R)^n)
q = first numericalSourceSample(I, p)
assert(numericalImageDim(F,I,p) == binomial(n,2))
degSOn = 2^(n-1)*det matrix table(floor(n/2), floor(n/2), (i,j) -> binomial(2*n - 2*i - 2*j - 4, n - 2*i - 2))
elapsedTime PW = pseudoWitnessSet(F,I,p, Repeats=>2, Threshold=>3, MaxThreads=>allowableThreads)
assert(numericalImageDegree PW == degSOn)
///

TEST /// -- Twisted cubic projections
R = CC[x_0..x_3]
I = monomialCurveIdeal(R, {1,2,3})
F1 = random(R^1, R^{3:-1})
p = numericalSourceSample I
imagePts = numericalImageSample(F1, I, 10);
assert(numericalImageDim(F1, I, p#0) == 2)
assert((numericalHilbertFunction(F1, I, imagePts, 2)).hilbertFunctionValue == 0)
assert((numericalHilbertFunction(F1, I, imagePts, 3)).hilbertFunctionValue == 1)
F2 = (gens R)_{0,2,3}
T = numericalHilbertFunction(F2, I, 3)
nodalCubic = ideal extractImageEquations(T, AttemptZZ => true)
S = ring nodalCubic
assert(nodalCubic == ideal(S_1^3 - S_0*S_2^2))
F3 = (gens R)_{0,1,2}
assert((numericalHilbertFunction(F3, I, 2)).hilbertFunctionValue == 1)
assert((pseudoWitnessSet(F2, I, p#0)).degree == 3)
///

TEST /// -- 3x3 matrices with double eigenvalue
S = QQ[a_(0,0)..a_(2,2), lambda, mu]
B = transpose genericMatrix(S,3,3)
J = ideal(B*transpose(B) - id_(S^3), det(B)-1)
n = B*diagonalMatrix{lambda,lambda,mu}*transpose(B)
T = QQ[p_(0,0),p_(0,1),p_(0,2),p_(1,1),p_(1,2),p_(2,2)]
J = ker map(S/J,T,{n_(0,0),n_(0,1),n_(0,2),n_(1,1),n_(1,2),n_(2,2)})
R = CC[a_(0,0)..a_(2,2), lambda, mu]
A = transpose genericMatrix(R,3,3)
I = ideal(A*transpose(A) - id_(R^3), det(A)-1)
m = A*diagonalMatrix{lambda,lambda,mu}*transpose(A)
F = {m_(0,0),m_(0,1),m_(0,2),m_(1,1),m_(1,2),m_(2,2)}
p = point{flatten entries diagonalMatrix {-1,-1,1} | {-1_CC, 1}}
assert(numericalImageDim(F,I,p) == dim J)
assert((pseudoWitnessSet(F,I,p)).degree == degree J)
///

TEST /// -- Numerical nullity tests
assert(numericalNullity(matrix{{2, 1}, {0, 1e-7}}, Precondition => false) == 1)
assert(numericalNullity(map(CC^2,CC^2,0)) == 2)
assert(numericalNullity(id_(CC^2)) == 0)
assert(numericalNullity(random(CC^2,CC^2)) == 0)
assert(numericalNullity(random(CC^0,CC^2)) == 2)
assert(numericalNullity(random(CC^2,CC^0)) == 0)
assert(numericalNullity(random(CC^0,CC^0)) == 0)
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


-- Defaults: 10 in monodromyLoop for affine term, 20 in myTrack (for parallelization), {0,1,3,2,-1,5,-2,6} for translationMagnitude in traceTest

-- Future: Alpha-certify option, improvements to interpolation (using non-monomial bases, Jacobi SVD)


-- Guarantee correct number of sample points, starting from initial point p
goodPts = {}
sampleSize = 100
eps = 1e-10
elapsedTime while #goodPts < sampleSize do (
    q = first numericalSourceSample(I, p);
    if q#?SolutionStatus and q#SolutionStatus === Regular and clean(eps, sub(gens I, matrix q)) == 0 then goodPts = goodPts | {q};
)

-- high degree rational normal curve
R = CC[s,t],; F = basis(40,R); I = ideal 0_R;
numericalImageDim(F, I)
time tests = toList(1..100)/(i -> pseudoWitnessSet(F,I,Repeats=>2,Verbose=>false));


-- Generic Pinched Veronese
R = CC[x_0..x_3]
F = toList(1..5)/(i -> random(10,R));
allowableThreads = maxAllowableThreads
pseudoWitnessSet(F,ideal 0_R,Repeats=>2)


-- Trifocal variety
R=CC[a00,a01,a02,a03,a10,a11,a12,a13,a20,a21,a22,a23,b10,b11,b12,b13,b20,b21,b22,b23],;A = transpose genericMatrix(R,a00,4,3),;B = matrix{{0,0,0,1},{b10,b11,b12,b13},{b20,b21,b22,b23}},;C = matrix{{1_R,0,0,0},{0,1,0,0},{0,0,1,0}},;M = A||B||C,;F = flatten flatten apply(3, i-> apply(3, j-> apply(reverse subsets(3,2), k->det  submatrix(M,{i}|{j+3}|(k+{6,6}) , )  )   ));
allowableThreads = 4
elapsedTime pseudoWitnessSet(F,ideal 0_R,Repeats=>2,MaxThreads=>allowableThreads)


-- Tensor product surface
(a,b) = (3,1)
R=CC[s,t,u,v, Degrees=>{{1,0},{1,0},{0,1},{0,1}}]
Ix=intersect(ideal(s,u),ideal(t,v))
B=super basis({a,b},Ix)
C=matrix{{1_R,1,0,0,0,0},{0,1,1,0,0,0},{0,0,1,1,0,0},{0,0,0,1,1,1}}
F = C*transpose(B)
I = ideal 0_R
numericalImageDim(F,I)
W = pseudoWitnessSet(F,I)
T = numericalHilbertFunction(F,I,W.degree)
extractImageEquations(T, AttemptZZ => true)


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
pseudoWitnessSet(F, I, Repeats => 2)
T = numericalHilbertFunction(F, I, 2)
extractImageEquations(T, AttemptZZ => true)


-- Check approximate equations:
T = numericalHilbertFunction(F, ideal 0_R, 2);
E = extractImageEquations(T, AttemptZZ => true);
all((toList T.imagePoints)/(p -> clean(1e-11, sub(E, toList(0..<#(p#Coordinates))/(i -> (gens ring E)#i => (p#Coordinates)#i)))), v -> v == 0)


--------------- Implicitization Challenge + variants

-- (line) secant of (P^1)^n in P^31, n = 5: degree 3256
n = 5
R = CC[a_1..a_n,b_1..b_n,s,t];
F = s*(terms product apply(toList(1..n), i-> 1 + a_i)) + t*(terms product apply(toList(1..n), i-> 1 + b_i));
allowableThreads = maxAllowableThreads
time W = pseudoWitnessSet(F, ideal 0_R, Repeats => 1)
elapsedTime W = pseudoWitnessSet(F, ideal 0_R, Repeats => 1, MaxThreads => allowableThreads) 


-- Challenge: Hadamard square of line secant of (P^1)^4 in P^15, degree 110, passed in 188.084 seconds
t = symbol t;
n = 4
R = CC[a_1..a_n,b_1..b_n, c_1..c_n, d_1..d_n,t_0..t_3];
F1 = t_0*(terms product apply(toList(1..n), i->(1 + a_i))) + t_1*(terms product apply(toList(1..n), i->(1 + b_i)));
F2 = t_2*(terms product apply(toList(1..n), i->(1 + c_i))) + t_3*(terms product apply(toList(1..n), i->(1 + d_i)));
F = apply(toList(0..15), i -> F1#i * F2#i);
allowableThreads = maxAllowableThreads
time W = pseudoWitnessSet(F, ideal 0_R, Repeats => 1, MaxThreads => allowableThreads)


-- precision tests

R = CC_54[s,t]; I = ideal 0_R; W = pseudoWitnessSet(basis(3, R), I)
toList W.witnessPointPairs /first/(p -> p#Coordinates )/first/ring

prec = 500
setDefault(Precision => prec)
R = CC_prec[s,t]; I = ideal 0_R; F = basis(3, R);
W = pseudoWitnessSet(F, I)

R = CC[s,t]; F = basis(4, R); I = ideal 0_R
T = numericalHilbertFunction(F, I, 2)
A = matrix T.interpolationMatrix

prec = 5
printingPrecision = 16
setDefault(Precision => prec)
R = CC_prec[x_0..x_3]
R = CC[x_0..x_2]
I = ideal random(R^1,R^{-2,-3})
I = ideal(random(2,R), random(3,R))
F = random(R^1,R^{3:-3})
F = matrix{toList(1..3)/(i -> random(3,R))}
d = 18

-- ConvertToCone tests
R = CC[x_1..x_5]
F = vars R
I = ideal(x_1*x_2^2 - x_3^2, x_1*x_4^2 - x_5^2)
elapsedTime I.cache.WitnessSet = first components numericalIrreducibleDecomposition I
time pseudoWitnessSet(F,I)
F = (gens R)_(toList(0,1,2))
I = ideal(x_1-x_3*x_4,x_2-x_3*x_5,x_4*x_5-1)

-- SO(5)
n = 5
R = CC[x_0..x_(n^2-1)]
A = genericMatrix(R,n,n)
I = ideal(A*transpose A - id_(R^n));
F = vars R
p = point id_((coefficientRing R)^n)
q = first numericalSourceSample(I, p)
allowableThreads = 4
elapsedTime PW = pseudoWitnessSet(F,I,p,Threshold=>3,MaxThreads=>allowableThreads, MaxPoints=>384) -- passed in 153.496 seconds

-- Dim challenge example
jacI=(d,l,n)->(S=CC[x_(0,1)..x_(n,l),c_0..c_(binomial(l,n)-1)];R= S[X_0..X_n];
M=for i from 1 to l list matrix{toList (x_(0,i)..x_(n,i))};
H=for b in(for i from 0 to#subsets(M,n)-1 list for a in(subsets(M,n))_i list{a})
list matrix b;
P=for t from 0 to #H-1 list for j from 0 to n list(-1)^(j)*(minors(n,H_t))_(n-j);
F=sum for i from 0 to #P-1 list c_(i)*(sum for j from 0 to n list P_i_j*X_j)^d;
I=transpose substitute((coefficients F)#1,S))
t=13
time F = jacI(t,t+1,2);
time numericalImageDim(F, ideal 0_S)