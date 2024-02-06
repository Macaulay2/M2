newPackage("NumericalImplicitization",
    Headline => "numerical invariants of images of varieties",
    AuxiliaryFiles => true,
    Version => "2.2.0",
    Date => "November 24, 2020",
    Authors => {
        {Name => "Justin Chen",
	 Email => "justin.chen@math.gatech.edu",
         HomePage => "https://people.math.gatech.edu/~jchen646/"},
        {Name => "Joe Kileel",
	 Email => "jkileel@math.princeton.edu",
	 HomePage => "https://web.math.princeton.edu/~jkileel/"}
        },
    Keywords => {"Numerical Algebraic Geometry"},
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
	"isOnImage",
        "realPoint",
        "optimizeNelderMead",
        "lineSearch",
        "Initial"
    }

-- Point sampling code
load "NumericalImplicitization/approxPoint.m2"
    
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
    -- samplePoints := if instance(W, Point) and not I.cache.?WitnessSet then (
    	-- d := first numericalDimensions(vars R, I, W);
    	-- squaredUpSource := randomSlice(gens I, #gens R - d, {});
	-- startSys := squaredUpSource | randomSlice(vars R, d, {W, "source"});
    	-- flatten apply(sampleSize, i -> track(startSys, squaredUpSource | randomSlice(vars R, d, {}), {W}, opts))) -- track not reliable, often fails
    if instance(opts.Software, FunctionClosure) then return apply(sampleSize, i -> (opts.Software) I);
    if not I.cache.?WitnessSet then I.cache.WitnessSet = if instance(W, WitnessSet) then W else first components(numericalIrreducibleDecomposition(I, opts));
    samplePoints := apply(sampleSize, i -> sample I.cache.WitnessSet);
    if precision R <= precision ring samplePoints#0#Coordinates#0 then samplePoints else refine(polySystem(I_*), samplePoints, Bits => precision R)
)
numericalSourceSample (Ideal, WitnessSet) := List => opts -> (I, W) -> numericalSourceSample(I, W, 1, opts)
numericalSourceSample (Ideal, Point) := List => opts -> (I, p) -> numericalSourceSample(I, p, 1, opts)
numericalSourceSample (Ideal, ZZ) := List => opts -> (I, sampleSize) -> numericalSourceSample(I, null, sampleSize, opts)
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
    goodSols := select(targetSolutions, p -> p.cache#?SolutionStatus and status p == Regular);
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
    if not all(W.witnessPointPairs/first, p -> clean(1e-5, sub(gens W.sourceEquations, matrix p)) == 0) then (
	if debugLevel > 0 then << " -- expected first components of `witnessPointPairs' to satisfy `sourceEquations'" << endl;
	return false
    );
    if not all(W.witnessPointPairs, pair -> clean(1e-5, matrix last pair - sub(W.map, matrix first pair)) == 0) then (
	if debugLevel > 0 then << " -- expected components `witnessPointPairs' to correspond under `map'" << endl;
	return false
    );
    if not all(W.witnessPointPairs/first/matrix, p -> clean(1e-5, sub(W.imageSlice, p)) == 0) then (
	if debugLevel > 0 then << " -- expected second components of `witnessPointPairs' to lie on `imageSlice'" << endl;
	return false
    );
    true
)

beginDocumentation()

load "NumericalImplicitization/doc.m2"
load "NumericalImplicitization/tests.m2"

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
