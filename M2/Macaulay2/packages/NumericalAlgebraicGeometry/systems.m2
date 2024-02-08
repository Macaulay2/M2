------------------------------------------------------
-- routines for 0-dim solution sets 
-- not included in other files
-- (loaded by  ../NumericalAlgebraicGeometry.m2)
------------------------------------------------------

-- helper functions for "square down":
-- orthonormal basis for col(L) using SVD
ONB = L -> (
    (S,U,Vt) := SVD L;
    r := # select(S,s->not areEqual(s,0));
    U_{0..r-1}
    )
-- orthonormal basis for subspace of col(L) that is perpendicular to col(M)
perp = (M, L) -> if areEqual(norm L, 0) then M else (
    Lortho := ONB L;
    Lperp := M-Lortho*conjugate transpose Lortho * M;
    ONB Lperp
    )
-- finding a square subsystem of maximal rank
rowSelector = method(Options=>{"block size"=>1,"target rank"=>null,Verbose=>false})
rowSelector (AbstractPoint, AbstractPoint, System) := o -> (y0, c0, GS) -> (
    (n, m, N) := (numVariables GS, numParameters GS, numFunctions GS);
    blockSize := o#"block size";
    numBlocks := ceiling(N/blockSize);
    numIters := 0;
    L := matrix{for i from 1 to n list 0_CC}; -- initial "basis" for row space
    r := 0;
    goodRows := {};
    diffIndices := {};
    while (r < n and numIters < numBlocks) do (
    	diffIndices = for j from numIters*blockSize to min((numIters+1)*blockSize, N)-1 list j;
	if o.Verbose then << "processing rows " << first diffIndices << " thru " << last diffIndices << endl;
    	newRows := evaluateJacobian(GS^diffIndices, y0, c0);
    	for j from 0 to numrows newRows - 1 do (
	    tmp := transpose perp(transpose newRows^{j}, transpose L);
	    if not areEqual(0, norm tmp) then (
		if o.Verbose then << "added row " << blockSize*numIters+j << endl;
	    	if areEqual(norm L^{0}, 0) then L = tmp else L = L || tmp;
	    	goodRows = append(goodRows, blockSize*numIters+j);
		);
    	    );
    	r = numericalRank L;
    	numIters = numIters+1;
	);
    if o.Verbose then << "the rows selected are " << goodRows << endl;
    goodRows
    )

-- stashes and returns "squared up" subsystem, according to given strategy
squareUp = method(Options => {Field => null, Strategy => null, "block size"=>1, "target rank" => null, Verbose=>false}) -- squares up a polynomial system (presented as a one-column matrix)
squareUp System := o -> P -> if P.?SquaredUpSystem then P.SquaredUpSystem else squareUp(P, numVariables P, o)
squareUp (System, ZZ) := o -> (P, n) -> (
    m := numFunctions P;
    if m<=n then "expect more equations than second argument";
    C := if instance(o.Field, Nothing) then (
	if instance(P, PolySystem) then coefficientRing ring P
	else default CC
	) else o.Field;
    if instance(o.Strategy, Nothing) or o.Strategy == "random matrix" then (
    	M := if class C === ComplexField then sub(randomOrthonormalRows(n,m), C) else random(C^n,C^m);
    	squareUp(P,M,o)
	) 
    else if o.Strategy == "slack variables" then error "strategy not implemented"
    else error "strategy not implemented"
    )
squareUp(System, Matrix) := o -> (P, M) -> (
    P.SquareUpMatrix = M;
    P.SquaredUpSystem = if instance(P, PolySystem) then polySystem (M*P.PolyMap) else gateSystem(parameters P, vars P, M * gateMatrix P)
    )
-- todo: squareUp(System, ...) not implemented, override w/ PolySystem and GateSystem

--- is this ever used???
squareUpMatrix = method()
squareUpMatrix System := P -> if P.?SquareUpMatrix then P.SquareUpMatrix else (
    n := P.NumberOfVariables;
    C := coefficientRing ring P;
    map(C^n)
    )

---------------------------------------------------
-- below is what was formerly known as "squareDown"
-- (needs a point for evaluation of the jacobian; uses rowSelector above)
squareUp (AbstractPoint, AbstractPoint, GateSystem) := o -> (p0, x0, F) -> (
    keptRows := rowSelector(p0, x0, F, "block size" => o#"block size", Verbose=>o.Verbose);
    F.SquaredUpSystem = F^keptRows
    )
squareUp (AbstractPoint, GateSystem) := o -> (x0, F) -> squareUp(point{{}}, x0, F, o)

TEST ///
needsPackage "NumericalSchubertCalculus"
needsPackage "NumericalAlgebraicGeometry"
-- Schubert calculus on the Grassmannian of m-planes in C^2m
-- derksen example
k = 1 -- k=2 (more interesting)
n = 3 -- n=4 (...)
m = 2*k
p = 2*n - m
assert(m+p == 2*n)
prob1 = apply(4, i -> apply(k, j -> n-k))
-- synthesize solution
prob1Instance = randomSchubertProblemInstance(prob1, m, m+p)
K0s = last \ prob1Instance
p0 = fold(apply(K0s, K0 -> matrix{flatten entries K0}), (a,b) -> a|b)
elapsedTime X0s = solveSchubertProblem(prob1Instance, m, m+p);
length realPoints apply(X0s, m -> point matrix{flatten entries m})
X0 = first X0s
X0 = X0 * (inverse X0^{0..m-1})
x0 = matrix{flatten entries X0^{m..m+p-1}}
allMinors = (k, M) -> (
    (m, n) := (numrows M, numcols M);
    flatten apply(subsets(m,k), R -> apply(subsets(n,k), C -> det(M_C^R)))
    )
-- setup equations
X = gateMatrix for i from 1 to p list for j from 1 to m list x_(i,j)
P = gateMatrix{vars flatten flatten for i from 1 to #prob1 list for j from 1 to m+p list for k from 1 to m+p list K_(i,j,k)}
O = inputGate 1
Z = inputGate 0
I = gateMatrix apply(m, i-> apply(m,j->if i==j then O else Z))
Ks = for i from 1 to #prob1 list matrix for j from 1 to m+p list for k from 1 to m+p list K_(i,j,k)
elapsedTime rankConstraints = transpose gateMatrix{flatten flatten apply(prob1, Ks, (S, K) -> apply(#S, i -> (
	j := S#i;
	M := (I || X) | K_{0..p+i-j}; 
	allMinors(m+p-j+1, M)
	)
    )
)};
G = gateSystem(P, matrix{flatten entries X}, rankConstraints)
norm evaluate(G, p0, x0)
assert (numericalRank evaluateJacobian(G, p0, x0)==numVariables G)
GSq = squareUp(point p0, point x0, G)
assert(numVariables GSq == numFunctions GSq)
assert(norm evaluate(GSq, p0, x0) < 1e-8)
/// 
 
