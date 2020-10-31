newPackage("DeterminantalRepresentations",
	AuxiliaryFiles => false,
	Version => "1.3.0",
	Date => "October 28, 2019",
	Authors => {
		{Name => "Justin Chen",
		Email => "jchen646@gatech.edu"},
		{Name => "Papri Dey",
		Email => "papridey@berkeley.edu"}
	},
	Headline => "determinantal representations",
	Keywords => {"Representation Theory", "Commutative Algebra"},
	HomePage => "https://github.com/papridey/DeterminantalRepresentations",
	PackageExports => {"NumericalAlgebraicGeometry"},
	DebuggingMode => false,
        Reload => false,
	Certification => {
	     "journal name" => "The Journal of Software for Algebra and Geometry",
	     "journal URI" => "http://j-sag.org/",
	     "article title" => "Computing symmetric determinantal representations",
	     "acceptance date" => "5 December 2019",
	     "published article URI" => "https://msp.org/jsag/2020/10-1/p02.xhtml",
	     "published article DOI" => "https://doi.org/10.2140/jsag.2020.10.9",
	     "published code URI" => "https://msp.org/jsag/2020/10-1/jsag-v10-n1-x02-DeterminantalRepresentations.m2",
	     "repository code URI" => "http://github.com/Macaulay2/M2/blob/master/M2/Macaulay2/packages/....m2",
	     "release at publication" => "f3c4030a3e66ae51f54ec24a89e1d5b1992a82eb",	    -- git commit number in hex
	     "version at publication" => "1.3.0",
	     "volume number" => "10",
	     "volume URI" => "https://msp.org/jsag/2020/10-1/"
	     }
)
export {
    "detRep",
    "HyperbolicPt",
    "bivariateDiagEntries",
    "orthogonalFromOrthostochastic",
    "linesOnCubicSurface",
    "doubleSixes",
    "generalizedMixedDiscriminant",
    "roundMatrix",
    "realPartMatrix",
    "hadamard",
    "coeffMatrices",
    "isOrthogonal",
    "isDoublyStochastic",
    "randomIntegerSymmetric",
    "randomOrthogonal",
    "randomPSD",
    "randomUnipotent",
    "cholesky",
    "companionMatrix"
}

detRep = method(Options => {
    Tolerance => 1e-6, 
    HyperbolicPt => null,
    Software => M2engine})
detRep RingElement := List => opts -> f -> (
    (n, d) := (#support f, first degree f);
    if d == 2 then quadraticDetRep(f, Tolerance => opts.Tolerance)
    else if n == 2 then bivariateDetRep(f, Tolerance => opts.Tolerance, Software => opts.Software)
    else if n == 3 then trivariateDetRep(f, opts)
    else if n == 4 and d == 3 then cubicSurfaceDetRep(f, Tolerance => opts.Tolerance)
    else error "Currently only implemented for quadrics, plane curves, and cubic surfaces"
)

-- Quadratic case

quadraticDetRep = method(Options => {Tolerance => 1e-6})
quadraticDetRep RingElement := Matrix => opts -> f -> (
    if first degree f > 2 then error "Not a quadratic polynomial";
    R := ring f;
    n := #gens R;
    k := ultimate(coefficientRing, R);
    b := sub(last coefficients(f, Monomials => gens R), k);
    A := sub(matrix table(n, n, (i,j) -> if i == j then (last coefficients(f, Monomials => {R_i^2}))_(0,0) else (1/2)*(last coefficients(f, Monomials => {R_i*R_j}))_(0,0)), k);
    Q := (1/4)*b*transpose(b) - A;
    E := clean(opts.Tolerance, eigenvectors(Q, Hermitian => true));
    if all(E#0, e -> e >= 0) and #select(E#0, e -> not(e == 0)) <= 3 then (
        posEvalues := positions(E#0, e -> e > 0);
        posEvectors := apply(posEvalues, i -> (E#0#i,matrix E#1_i));
        r := (1/2)*b + sqrt(posEvectors#0#0)*posEvectors#0#1;
        s := (1/2)*b - sqrt(posEvectors#0#0)*posEvectors#0#1;
        t := if #posEvalues >= 2 then sqrt(posEvectors#1#0)*posEvectors#1#1 else 0*b;
        u := if #posEvalues == 3 then sqrt(posEvectors#2#0)*posEvectors#2#1 else 0*b;
        L := apply(n, i -> matrix{{r_(i,0),t_(i,0) - ii*u_(i,0)},{t_(i,0)+ii*u_(i,0),s_(i,0)}});
        if not class k === ComplexField then L = L/realPartMatrix/clean_(opts.Tolerance);
        if k === QQ then L = L/roundMatrix_(ceiling(log_10(1/opts.Tolerance)));
        {id_(R^2) + sum apply(n, i -> R_i*sub(L#i, R))}
    ) else (
        E = clean(opts.Tolerance, eigenvectors(A, Hermitian => true));
        if any(E#0, e -> e > 0) then ( print "No determinantal representation"; return; );
        C := cholesky((-1)*A, opts);
        if not class k === ComplexField then C = clean(opts.Tolerance, realPartMatrix C);
        if k === QQ then C = roundMatrix(ceiling(log_10(1/opts.Tolerance)), C);
        {(id_(R^n) | transpose C*transpose vars R) || ((vars R*C) | matrix{{1+(vars R)*b}})}
    )
)

-- Bivariate code

cubicBivariateDetRep = method(Options => options quadraticDetRep)
cubicBivariateDetRep RingElement := List => opts -> f -> (
    eps := opts.Tolerance;
    R := ring f;
    k := ultimate(coefficientRing, R);
    (D1, D2, diag1, diag2) := bivariateDiagEntries(f, opts)/entries/flatten;
    if first degree f > 3 then error "Not a cubic polynomial";
    S := RR(monoid[getSymbol "q"]);
    varSet := support f;
    if #uniqueUpToTol(D1, opts) == 1 or #uniqueUpToTol(D2, opts) == 1 then (
        return {id_(R^3)+R_0*sub(diagonalMatrix D1,R)+R_1*sub(diagonalMatrix D2,R)};
    );
    q21 := (diag2#1-D2#2-S_0*(D2#1-D2#2))/(D2#0-D2#2);
    q12 := (diag1#1-D1#2-S_0*(D1#1-D1#2))/(D1#0-D1#2);
    q11 := (diag2#0-D2#2-q12*(D2#1-D2#2))/(D2#0-D2#2);
    q22 := S_0;
    if not clean(eps, q11 - (diag1#0-D1#2-q21*(D1#1-D1#2))/(D1#0-D1#2)) == 0 then print "Not compatible";
    Q := clean(eps, matrix{{q11,q12,1-q12-q11},{q21,q22,1-q21-q22},{1-q11-q21,1-q12-q22,1-(1-q11-q12)-(1-q21-q22)}});
    oEq := ((q11-1)*(q22-1) + (q21-1)*(q12-1) - 1)^2 - 4*q11*q22*q12*q21;
    L0 := apply(if clean(eps, oEq) == 0 then {0} else eigenvalues companionMatrix oEq, r -> realPartMatrix sub(Q, S_0=>r));
    L := flatten apply(select(clean(eps, L0), isDoublyStochastic), M -> orthogonalFromOrthostochastic(M, opts));
    if k === QQ then (
        numDigits := ceiling(-log_10(eps));
        (D1, D2) = (D1/round_numDigits, D2/round_numDigits);
        L = L/roundMatrix_numDigits;
    );
    (D1, D2) = (D1, D2)/diagonalMatrix_k;
    L = uniqueUpToTol(apply(L, M -> {D1, M*D2*transpose M}), opts);
    apply(if k === QQ then L else clean(eps, L), l -> id_(R^3) + sum apply(#l, i -> varSet#i*sub(l#i, R)))
)

orthogonalFromOrthostochastic = method(Options => options quadraticDetRep)
orthogonalFromOrthostochastic Matrix := List => opts -> M -> (
    if min(flatten entries M) < 0 then return {};
    N := matrix apply(entries M, r -> r/sqrt);
    d := numrows M;
    sgn := drop(sort toList((set{1,-1}) ^** (d-1))/deepSplice/toList, -1);
    validRows := {{{N^{0}}}};
    for i from 1 to numrows N-1 do (
        validRows = flatten apply(validRows, rowset -> apply(select(sgn/(S -> hadamard(matrix{{1} | S}, N^{i})), candidate -> clean(opts.Tolerance, matrix rowset * transpose candidate) == 0), r -> append(rowset, {r})));
    );
    unique validRows/matrix
)

-- General bivariate case 

bivariateDetRep = method(Options => options quadraticDetRep ++ {Software => M2engine, Strategy => "DirectSystem"})
bivariateDetRep RingElement := List => opts -> f -> (
    eps := opts.Tolerance;
    R := ring f;
    d := first degree f;
    k := ultimate(coefficientRing, R);
    if isHomogeneous f then (
        (x, y) := toSequence support f;
        g := sub(f, x => 1);
        Z := toList(d-(first degree g):0) | apply(eigenvalues companionMatrix g, r -> -1/r);
        if not all(Z, z -> clean(eps, z - realPart z) == 0) then error "Not a real zero polynomial";
        return {(x*id_(R^d) + y*sub(realPartMatrix diagonalMatrix Z, R))};
    );
    (D1, D2, diag1, diag2) := bivariateDiagEntries(f, Tolerance => eps);
    y = getSymbol "y";
    (A1, A2) := (D1, D2)/(M -> sub(diagonalMatrix M, R));
    matrixList := if opts.Strategy == "DirectSystem" then ( -- via solving polynomial system numerically
        S := R/(ideal gens R)^(d+1);
        mons := lift(super basis(ideal(S_1^2)), R);
        C := last coefficients(f, Monomials => mons);
        T := RR(monoid[y_1..y_(binomial(d,2))]);
        S = T(monoid[gens R]);
        A := genericSkewMatrix(T, d);
        B := matrix table(d, d, (i,j) -> if i == j then diag2_(i,0) else A_(min(i,j),max(i,j)));
        G := det(id_(S^d) + S_0*sub(diagonalMatrix D1, S) + S_1*sub(B, S));
        C1 := last coefficients(G, Monomials => sub(mons, S)) - sub(C, S);
        P := polySystem sub(clean(eps, C1), T);
        if debugLevel > 0 then print ("Solving " | binomial(d,2) | " x " | binomial(d,2) | " polynomial system ...");
        sols := select(solveSystem(P, Software => opts.Software), p -> not status p === RefinementFailure);
        realSols := realPoints apply(sols, p -> point{p#Coordinates/clean_eps});
        indices := sort subsets(d, 2);
        H := hashTable apply(binomial(d,2), i -> indices#i => i);
        apply(realSols/(p -> p#Coordinates/realPart), sol -> matrix table(d, d, (i,j) -> if i == j then (diag2_(i,0))_R else sol#(H#{min(i,j),max(i,j)})))
    ) else if opts.Strategy == "Orthogonal" then ( -- via orthogonal matrices
        T = RR(monoid[y_0..y_(d^2-1)]);
        A = genericMatrix(T,d,d);
        L := minors(1, (transpose A)*D1-diag1)+minors(1, A*D2-diag2);
        allOnes := transpose matrix{apply(d, i -> 1_T)};
        rowsum := minors(1, A*allOnes - allOnes);
        colsum := minors(1, (transpose A)*allOnes - allOnes);
        J := minors(1, A*transpose A - id_(T^d)) + sub(L + rowsum + colsum, apply(gens T, v -> v => v^2));
        if debugLevel > 0 then print "Computing orthogonal matrices numerically ...";
        N := numericalIrreducibleDecomposition(J, Software => opts.Software);
        realSols = realPoints apply(N#0, W -> point{W#Points#0#Coordinates/clean_eps});
        apply(realSols/(p -> sub(matrix pack(d, p#Coordinates/realPart), R)), M -> transpose M*A2*M)
    );
    if k === QQ then matrixList = matrixList/roundMatrix_(ceiling(-log_10(eps)));
    apply(matrixList, M -> sum{id_(R^d), R_0*A1, R_1*M})
)

-- Helper functions for bivariate case

bivariateDiagEntries = method(Options => options quadraticDetRep)
bivariateDiagEntries RingElement := Sequence => opts -> f -> ( -- returns diagonal entries and eigenvalues of coefficient matrices
    d := first degree f;
    k := coefficientRing ring f;
    V := support f;
    if #V > 2 then error "Not a bivariate polynomial";
    (R1, R2) := (k(monoid[V#0]), k(monoid[V#1]));
    (f1, f2) := (sub(sub(f, V#1 => 0), R1), sub(sub(f, V#0 => 0), R2));
    (r1, r2) := (f1, f2)/companionMatrix/eigenvalues;
    D1 := reverse sort(apply(r1,r -> -1/r) | toList(d-#r1:0));
    D2 := reverse sort(apply(r2,r -> -1/r) | toList(d-#r2:0));
    apply(D1 | D2, r -> (
	imPart := clean(opts.Tolerance, imaginaryPart r);
	if not imPart == 0 then (
	    print("Imaginary part " | toString(imPart) | " exceeded tolerance for real-rootedness");
	    error("Not a real zero polynomial - no monic symmetric determinantal representation of size " | d);
    	);
    ));
    (D1, D2) = (D1/realPart, D2/realPart);
    if #uniqueUpToTol(D1, opts) == 1 or #uniqueUpToTol(D2, opts) == 1 then return (D1, D2, {}, {})/(L -> transpose matrix{L});
    C1 := realPartMatrix sub(last coefficients(f, Monomials=>apply(d, i -> V#0*V#1^i)),k);
    G1 := sub(matrix table(d,d,(i,j) -> sum apply(subsets(toList(0..<d)-set{j},i), s -> product(D2_s))), RR);
    diag1 := addScaleToMajorize(flatten entries solve(G1, sub(C1, RR), ClosestFit => true), D1, G1, opts);
    C2 := realPartMatrix sub(last coefficients(f, Monomials=>apply(d, i -> V#0^i*V#1)),k);
    G2 := sub(matrix table(d,d,(i,j) -> sum apply(subsets(toList(0..<d)-set{j},i), s -> product(D1_s))), RR);
    diag2 := addScaleToMajorize(flatten entries solve(G2, sub(C2, RR), ClosestFit => true), D2, G2, opts);
    (D1, D2, diag1, diag2)/(L -> transpose matrix{L})
)

addScaleToMajorize = method(Options => options quadraticDetRep)
addScaleToMajorize (List, List, Matrix) := List => opts -> (v, w, A) -> (
    if isMajorized(v, w, opts) then return v;
    K := gens ker A;
    if clean(opts.Tolerance, K) == 0 then error (toString(w) | " cannot be majorized by " | toString(v));
    (w, K) = (rsort w, flatten entries K);
    ineqs := apply(select(subsets(#K), s -> clean(opts.Tolerance, sum K_s) != 0), s -> (K_s, w_(toList(0..<#s)) - v_s)/sum);
    tmax := min apply(select(ineqs, p -> p#0 > 0), p -> p#1/p#0);
    tmin := max apply(select(ineqs, p -> p#0 < 0), p -> p#1/p#0);
    if tmin > tmax then error (toString(w) | " cannot be majorized by " | toString(v));
    t1 := 0.5*(tmax + tmin);
    t0 := if tmax - tmin >= 1 then ( if floor t1 >= tmin then floor t1 else ceiling t1 ) else t1;
    v + t0*K
)

isMajorized = method(Options => options quadraticDetRep)
isMajorized (List, List) := Boolean => opts -> (v, w) -> ( -- true if v is majorized by w
    (v,w) = (v,w)/rsort;
    if not clean(opts.Tolerance, sum v - sum w) == 0 then return false;
    all(#v, k -> clean(opts.Tolerance, sum(w_{0..k}) - sum(v_{0..k})) >= 0)
)

-- Trivariate case

trivariateDetRep = method(Options => options detRep)
trivariateDetRep RingElement := List => opts -> f -> (
    V := support f;
    if #V > 3 or not isHomogeneous f then error "Expected a homogeneous polynomial in 3 variables";
    (k, d, x) := (coefficientRing ring f, first degree f, V#0);
    (e, eps) := (opts.HyperbolicPt, opts.Tolerance);
    A := if e =!= null then sub(realPartMatrix(e | random(k^3,k^2)), k) else id_(k^3);
    F := sub(f, matrix{V}*A);
    c := last coefficients(F, Monomials => {x^d});
    if c == 0 then error "Expected polynomial to be hyperbolic with respect to (1,0,0). Try specifying a point with the option HyperbolicPt";
    c = sub(c_(0,0), k);
    c0 := if k === QQ and (odd d or c > 0) then lift(c^(1/d), QQ) else c^(1/d);
    F = 1/c*sub(sub(F, x => 1), k(monoid[delete(x,V)]));
    reps := if d == 3 then cubicBivariateDetRep(F, Tolerance => eps) else bivariateDetRep(F, Software => opts.Software, Tolerance => eps);
    reps = apply(reps, r -> sub(c0*homogenize(sub(r, ring f), x), matrix{V}*(id_(k^3) // A)));
    if k === QQ then reps else reps/clean_eps
)

-- Cubic surface

linesOnCubicSurface = method(Options => options quadraticDetRep)
linesOnCubicSurface RingElement := List => opts -> f -> (
    if not(isHomogeneous f and (degree f)#0 == 3 and #gens ring f == 4) then error "Expected a homogeneous cubic in 4 variables";
    a := symbol a;
    R0 := CC(monoid[a_0..a_3]);
    R := R0(monoid[gens ring f]);
    f = sub(f, R);
    allLines := uniqueUpToTol(flatten apply(subsets(gens R, 2), s -> (
        (z, w) := toSequence(gens R - set s);
        F := sub(f, {z => R0_0*s#0 + R0_1*s#1, w => R0_2*s#0 + R0_3*s#1});
        I := sub(ideal last coefficients F, R0);
        sols := solveSystem polySystem I;
        apply(sols/(p -> clean(opts.Tolerance, p#Coordinates)), p -> matrix{insert(index w, 0, insert(index z, -1, {p#0,p#1})), insert(index w, -1, insert(index z, 0, {p#2,p#3}))})
    )), opts);
    uniqueLines := {};
    for l in allLines do if all(uniqueLines, m -> numericalRank(m || l) > 2) then uniqueLines = append(uniqueLines, l);
    uniqueLines
)

-- doubleSix = method(Options => options quadraticDetRep)
-- doubleSix List := List => opts -> lineSet -> (
--     eps := opts.Tolerance;
--     L := lineSet#0;
--     meetL := select(delete(L, lineSet), l -> clean(eps, det(L || l)) == 0);
--     skewL := delete(L, lineSet) - set meetL;
--     (excepDivs, candSet, j) := ({0}, {}, 0);
--     for i to 3 do (
--         candSet = toList(1..#skewL-1) - set excepDivs;
--         j = position(candSet, k -> all(excepDivs, e -> not clean(eps, det(skewL#e || skewL#k)) == 0));
-- 	if j === null then ( print "Found no exceptional divisor"; break; );
--         excepDivs = append(excepDivs, candSet#j);
--     );
--     excepDivs = toList apply(5, i -> skewL#(excepDivs#i));
--     conic0 := (select(skewL_{5..15}, l -> all(excepDivs, e -> clean(eps, det(l || e)) == 0)))#0;
--     conics := {};
--     for i to 4 do (
--         candSet = toList(0..#meetL-1) - set conics;
--         j = position(candSet, k -> #select(excepDivs, e -> clean(eps, det(e || meetL#k)) == 0) == 4);
-- 	if j === null then ( print "Found no conic"; break; );
--         conics = append(conics, candSet#j);
--     );
--     ds := {{L} | excepDivs, {conic0} | toList apply(5, i -> meetL#(conics#i))};
--     {ds#0, (ds#1)_(inversePermutation apply(6, i -> position(ds#0, l -> not clean(eps, det(l || ds#1#i)) == 0)))}
-- )
-- doubleSix RingElement := List => opts -> f -> doubleSix(linesOnCubicSurface f, opts)

doubleSixes = method(Options => options quadraticDetRep)
doubleSixes List := List => opts -> lineSet -> (
    eps := opts.Tolerance;
    H := hashTable apply(lineSet, l -> l => select(delete(l,lineSet), m -> clean(eps, det(l || m)) != 0));
    skewPairs := select(subsets(lineSet,2), s -> clean(eps, det(s#0 || s#1)) != 0);
    skewPairHash := hashTable apply(skewPairs, s -> s => set(H#(s#0)) * set(H#(s#1)));
    ds := {};
    for s in skewPairs do (
	if #unique(ds/set) >= 72 then break;
	for t in subsets(toList skewPairHash#s,4) do if all(subsets(t,2), u -> clean(eps, det(u#0 || u#1)) != 0) then ds = append(ds, s | t);
    );
    ds = (unique(ds/set))/toList;
    for i to 35 list (
	l0 := first ds;
	l1 := first select(drop(ds, 1), s -> all(s, l -> #select(l0, m -> clean(eps, det(m || l)) == 0) == 5));
	ds = ds - set{l0,l1};
	{l0, apply(#l1, i -> first select(l1, l -> clean(eps, det(l || l0#i)) != 0))}
    )
)

cubicSurfaceDetRep = method(Options => options quadraticDetRep)
cubicSurfaceDetRep (RingElement, List) := Matrix => opts -> (f, lineSet) -> (
    ds := first doubleSixes(lineSet, opts);
    tritangents := apply({{0,1},{1,2},{2,0},{0,2},{1,0},{2,1}}, s -> approxKer(ds#1#(s#0) || ds#0#(s#1)));
    tritangents = apply(tritangents, p -> if p == 0 then 0_(ring f) else (vars ring f*p)_(0,0));
    matrix{{0, tritangents#0, tritangents#3}, {tritangents#4, 0, tritangents#1}, {tritangents#2, tritangents#5, 0}}
)
cubicSurfaceDetRep RingElement := Matrix => opts -> f -> cubicSurfaceDetRep(f, linesOnCubicSurface f, opts)

-- Generalized mixed discriminant

generalizedMixedDiscriminant = method()
generalizedMixedDiscriminant List := RingElement => L -> (
    T := tally L;
    m := #keys T;
    k := #L;
    n := numcols L#0;
    Sk := subsets(n, k);
    Skv := unique permutations flatten apply(m, i -> toList((T#((keys T)#i)):i));
    sum flatten table(Sk, Skv, (alpha, sigma) -> det matrix table(k, k, (i,j) -> ((keys T)#(sigma#i))_(alpha#i,alpha#j)))
)

-- General helper functions

uniqueUpToTol = method(Options => options quadraticDetRep)
uniqueUpToTol List := List => opts -> L -> delete(null, apply(#L, i -> if not any(i, j -> areEqual(L#i, L#j, opts)) then L#i))

clean (RR,BasicList) := BasicList => (eps, L) -> L/clean_eps

isHomogeneous RR := x -> true

round (ZZ,CC) := (n,x) -> round(n, realPart x) + ii*round(n,imaginaryPart x)
round (ZZ,ZZ) := (n,x) -> x

roundMatrix = method() -- only accepts real matrices
roundMatrix (ZZ, Matrix) := Matrix => (n, A) -> matrix apply(entries A, r -> r/(e -> (round(n,0.0+e))^QQ))

realPartMatrix = method()
realPartMatrix Matrix := Matrix => A -> matrix apply(entries A, r -> r/realPart)

approxKer = method(Options => options quadraticDetRep)
approxKer Matrix := Matrix => opts -> A -> (
    d := numcols A;
    (S,U,Vh) := SVD A;
    n := #select(S, s -> clean(opts.Tolerance, s) == 0);
    conjugate transpose Vh^{d-n..d-1}
)

hadamard = method()
hadamard (Matrix, Matrix) := Matrix => (A, B) -> (
    if not(numcols A == numcols B and numrows A == numrows B) then error "Expected same size matrices";
    matrix table(numrows A, numcols A, (i,j) -> A_(i,j)*B_(i,j))
)

coeffMatrices = method()
coeffMatrices Matrix := List => M -> (
    V := gens ring M;
    (m, n) := (numrows M, numcols M);
    apply(V, v -> matrix table(m, n, (i,j) -> last coefficients(M_(i,j), Monomials => {v})))
)

-- Tests for matrix classes

isOrthogonal = method(Options => options quadraticDetRep)
isOrthogonal Matrix := Boolean => opts -> A -> (
    if not numcols A == numrows A then (
        if debugLevel > 0 then print "Not a square matrix";
        return false;
    );
    delta := A*transpose A - id_((ring A)^(numcols A));
    if instance(class 1_(ultimate(coefficientRing, ring A)), InexactFieldFamily) then delta = clean(opts.Tolerance, delta);
    delta == 0
)

isDoublyStochastic = method(Options => options quadraticDetRep)
isDoublyStochastic Matrix := Boolean => opts -> A -> (
    n := numcols A;
    if not numrows A == n then ( if debugLevel > 0 then print "Not a square matrix"; return false; );
    if not class(ultimate(coefficientRing, ring A)) === RealField then ( if debugLevel > 0 then print "Not a real matrix"; return false; );
    if not min flatten entries A >= 0 then ( if debugLevel > 0 then print "Not all entries are nonnegative"; return false; );
    v := matrix{toList(n:1_RR)};
    if not clean(opts.Tolerance, v*A - v) == 0 and clean(opts.Tolerance, A*transpose v - transpose v) == 0 then ( if debugLevel > 0 then print "Not doubly stochastic"; return false; );
    true
)

-- Construct various types of matrices

randomIntegerSymmetric = method()
randomIntegerSymmetric (ZZ, Ring) := Matrix => (d, R) -> ( 
    A := random(ZZ^d,ZZ^d); 
    sub(A + transpose A, R)
)
randomIntegerSymmetric ZZ := Matrix => d -> randomIntegerSymmetric(d, ZZ)

randomOrthogonal = method()
randomOrthogonal (ZZ, Thing) := Matrix => (n, R) -> (
    k := if instance(R, InexactFieldFamily) then R else ultimate(coefficientRing, R);
    A := random(k^n, k^n);
    S := A - transpose A;
    sub((-1)*(id_(k^n) // (S - id_(k^n)))*(S + id_(k^n)), R)
)
randomOrthogonal ZZ := Matrix => n -> randomOrthogonal(n, RR)

randomPSD = method()
randomPSD (ZZ, ZZ, RR) := Matrix => (n, r, s) -> (
    if r > n then error "Expected rank to be less than size";
    O := randomOrthogonal(n, RR);
    transpose O*diagonalMatrix(toList apply(r, i -> random(0.0, s)) | toList(n-r:0))*O
)
randomPSD (ZZ, RR) := Matrix => (n, s) -> randomPSD(n, n, s)
randomPSD (ZZ, ZZ) := Matrix => (n, r) -> randomPSD(n, r, 1.0)
randomPSD ZZ := Matrix => n -> randomPSD(n, n, 1.0)

randomUnipotent = method()
randomUnipotent (ZZ, Thing) := Matrix => (n, R) -> (
    k := if instance(R, InexactFieldFamily) then R else ultimate(coefficientRing, R);
    sub(matrix table(n, n, (i,j) -> if i == j then 1_k else if i < j then random k else 0), R)
)
randomUnipotent ZZ := Matrix => n -> randomUnipotent(n, QQ)

cholesky = method(Options => options quadraticDetRep)
cholesky Matrix := Matrix => opts -> A -> (
    n := numcols A;
    if not n == numrows A then error "Expected square matrix";
    if not clean(opts.Tolerance, A - transpose A) == 0 then error "Expected symmetric matrix";
    if min clean(opts.Tolerance, eigenvalues A) < 0 then error "Expected positive semidefinite matrix";
    L := new MutableHashTable;
    for i from 0 to n-1 do (
	for j from 0 to i do (
	    L#(i,j) = if i == j then sqrt(max(0, A_(i,j) - sum apply(j, k -> (L#(j,k))^2)))
	    else if L#(j,j) == 0 then 0 else (1/L#(j,j))*(A_(i,j) - sum apply(j, k -> L#(i,k)*L#(j,k)));
    	)
    );
    clean(opts.Tolerance, matrix table(n, n, (i,j) -> if i >= j then L#(i,j) else 0))
)

companionMatrix = method()
companionMatrix RingElement := Matrix => f -> (
     (n, k, x) := ((degree f)#0, ultimate(coefficientRing, ring f), (support f)#0);
     C := sub(last coefficients(f, Monomials => apply(n+1, i -> x^i)), k);
     (map(k^1,k^(n-1),0) || id_(k^(n-1))) | submatrix'((-1/C_(n,0))*C,{n},)
)

-- Documentation --
-- <<docTemplate

beginDocumentation()

doc ///
    Key
        DeterminantalRepresentations
    Headline
    	computing determinantal representations of polynomials
    Description
    	Text
	    The goal of this package is to compute symmetric determinantal representations of
            real polynomials. A polynomial $f$ in $\mathbb{R}[x_1, \ldots, x_n]$ 
            of total degree $d$ (not necessarily homogeneous) is called determinantal if $f$
            is the determinant of a matrix of linear forms - in other words, there exist
            matrices $A_0, \ldots, A_n\in \mathbb{R}^{d\times d}$ such that 
            $f(x_1, \ldots, x_n) = det(A_0 + x_1A_1 + \ldots + x_nA_n)$. The matrix pencil 
            $A_0 + x_1A_1 + \ldots + x_nA_n$ is said to give a determinantal representation
            of $f$ of size $d$. If the matrices $A_i$ can be chosen to be all symmetric, then
            the determinantal representation is called symmetric. The determinantal
            representation is called definite if $A_0$ is positive definite, and monic if 
            $A_0 = I_d$ is the identity matrix. 
            
            Deciding whether or not a degree $d$ polynomial has a determinantal
            representation of size $d$ is in general difficult, and computing such a
            representation even more so. Computing (monic) symmetric determinantal
            representations (even in 2 variables) is of interest owing to a connection with
            real-zero and hyperbolic polynomials, due to a celebrated theorem of 
            Helton-Vinnikov. In general, determinantal polynomials also have connections to
            convex algebraic geometry and semidefinite programming.
            
            Currently, the functions in this package are geared towards computing monic
            symmetric determinantal representations of quadrics, as well as plane curves of
            low degree (i.e., cubics and quartics). The algorithms implemented in this 
            package can be found in [1], [2].
            
            Additionally, a number of helper functions are included for creating/working
            with various classes of matrices, which may be of general interest (and
            are not limited to the scope of determinantal representations). These include:
            creating/testing orthogonal, symmetric, doubly stochastic, unipotent, and 
            positive semidefinite matrices, Hadamard products, Cholesky decomposition, 
            and lifting/rounding matrices from @TO CC@ to @TO RR@/@TO QQ@.
            
        Text
            {\bf References}:
        Code
            UL {
                HREF {"http://arxiv.org/abs/1708.09559", "[1] Dey, P., Definite Determinantal Representations via Orthostochastic Matrices, arXiv:1708.09559"}, 
                HREF {"http://arxiv.org/abs/1708.09557", "[2] Dey, P., Definite Determinantal Representations of Multivariate Polynomials, arXiv:1708.09557"}
            }
            
///

doc ///
    Key
        detRep
        (detRep, RingElement)
        [detRep, Tolerance]
    Headline
        compute determinantal representations
    Usage
        detRep f
        detRep(f, Tolerance => 1e-6)
    Inputs
        f:RingElement
            a polynomial with real coefficients
    Outputs
        :List
            of matrices, each giving a determinantal representation of $f$
    Description
        Text
            This method is a wrapper function for the various methods implemented in this
            package. Currently it accepts quadrics (in any number of variables), bivariate 
            polynomials or homogeneous trivariate polynomials, and cubic polynomials in 
            4 variables. By default, all polynomials are assumed to have real coefficients.
            For any polynomial falling in one of these categories, the user may call this method,
            and the correct algorithm will be automatically applied. For details on each case,
            see the pages below.
    SeeAlso
        "Determinantal representations of quadrics"
        "Determinantal representations of bivariate polynomials"
        "Determinantal representations of hyperbolic plane cubics"
///

doc ///
    Key
        "Determinantal representations of quadrics"
    Usage
        detRep f
        detRep(f, Tolerance => 1e-6)
    Inputs
        f:RingElement
            a quadric with real coefficients
    Outputs
        :Matrix
            giving a determinantal representation of $f$
    Description
        Text
            This page demonstrates how the method @TO detRep@ computes a monic 
            symmetric determinantal representation of a real quadric $f$ (in any number of 
            variables), or returns false if no such representation exists.
            
            If a quadratic determinantal representation of size $2$ exists, then it is 
            returned. Otherwise, the method will find a determinantal representation of size
            $n+1$, where $n$ is the number of variables (if it exists). If no monic symmetric
            determinantal representation exists, then @TO null@ is returned.
            
            When working over an @TO InexactFieldFamily@ like @TO RR@ or 
            @TO CC@, the option {\tt Tolerance} can be used to specify the internal
            threshold for checking equality (any floating point number below the tolerance 
            is treated as numerically zero).
        
        Example
            R = RR[x1, x2, x3, x4]
            f = 260*x1^2+180*x1*x2-25*x2^2-140*x1*x3-170*x2*x3-121*x3^2+248*x1*x4+94*x2*x4-142*x3*x4+35*x4^2+36*x1+18*x2+2*x3+20*x4+1
            A = first detRep f
            clean(1e-10, f - det A)
            g = -61*x1^2-96*x1*x2-177*x2^2-126*x1*x3-202*x2*x3-86*x3^2-94*x1*x4-190*x2*x4-140*x3*x4-67*x4^2+8*x1+3*x2+5*x3+3*x4+1
            B = first detRep g
            clean(1e-10, g - det B)
    SeeAlso
        detRep
///

doc ///
    Key
        bivariateDiagEntries
        (bivariateDiagEntries, RingElement)
        [bivariateDiagEntries, Tolerance]
    Headline
        computes diagonal entries and eigenvalues for a determinantal representation of a bivariate polynomial
    Usage
        bivariateDiagEntries f
        bivariateDiagEntries(f, Tolerance => 1e-6)
    Inputs
        f:RingElement
            a bivariate polynomial with real coefficients
    Outputs
        :Sequence
            of eigenvalues and diagonal entries of a determinantal representation of $f$
    Description
        Text
            This method computes the eigenvalues and diagonal entries of a monic
            symmetric determinantal representation of a real bivariate polynomial $f$, or
            gives an error if certain necessary conditions for existence of such a 
            representation are not met. For a symmetric determinantal representation 
            $f = det(I + x_1A_1 + x_2A_2)$, this method computes diagonal entries and
            eigenvalues of $A_1$ and $A_2$. The output is a 4-tuple of column vectors: 
            (eigenvalues of A_1, eigenvalues of $A_2$, diagonal entries of $A_1$,
            diagonal entries of $A_2$).
            
            When working over an @TO InexactFieldFamily@ like @TO RR@ or 
            @TO CC@, the option {\tt Tolerance} can be used to specify the internal
            threshold for checking equality (any floating point number below the tolerance 
            is treated as numerically zero).
        
        Example
            R = RR[x1, x2]
            f = 15*x1^2 + 20*x1*x2 - 36*x2^2 + 20*x1 + 16*x2 + 1
            bivariateDiagEntries f
    SeeAlso
        detRep
        "Determinantal representations of bivariate polynomials"
        "Determinantal representations of hyperbolic plane cubics"
///

doc ///
    Key
        "Determinantal representations of bivariate polynomials"
        [detRep, Software]
    Description
        Text
            This page demonstrates how the method @TO detRep@ computes a 
            monic symmetric determinantal representation of a bivariate polynomial, 
            if such a representation exists. 
            
            First, if the polynomial $f$ is homogeneous, then over 
            @TO CC@, $f$ splits as a product of linear forms, and it 
            admits a real symmetric determinantal representation if and only if
            all the linear factors are defined over @TO RR@. If
            this is the case, this method returns the diagonal matrix
            of linear factors of $f$, which is a symmetric determinantal
            representation:
            
        Example
            R = RR[x,y]
            detRep(x^2 - 3*y^2)
            detRep(x^5+6*x^4*y-2*x^3*y^2-36*x^2*y^3+x*y^4+30*y^5)
        
        Text
            Here it is assumed that the dehomogenization of $f$ (with
            respect to the first variable in its @TO support@) is 
            monic - this can always be achieved by rescaling.
            
            Now suppose $f$ is not homogeneous. For a
            symmetric determinantal representation $f = det(I + x_1A_1 + x_2A_2)$, by
            suitable conjugation one may assume $A_1 = D_1$ is a diagonal matrix. We
            also have that $D_1$ and the diagonal entries of $A_2$ can be found using 
            the method @TO bivariateDiagEntries@. From this data,
            here are 2 approaches to numerically compute a determinantal
            representation of $f$,  which can be specified by the option 
            {\tt Strategy}.
            
            The first (and default) strategy is "DirectSystem", which computes the 
            off-diagonal entries of $A_2$ directly as solutions to a 
            $(d choose 2)\times (d choose 2)$ polynomial system.
            
            The second strategy is "Orthogonal", which computes the orthogonal 
            change-of-basis matrices $V$ such that $VA_2V^T = D_2$ is diagonal. 
            Since $D_2$ can be found using @TO bivariateDiagEntries@, $A_2$ can 
            be recovered from $V$.
            
            Both strategies use numerical algebraic geometry, specifically a 
            @TO numericalIrreducibleDecomposition@.
            
            When working over an @TO InexactFieldFamily@ like @TO RR@ or 
            @TO CC@, the option {\tt Tolerance} can be used to specify the internal
            threshold for checking equality (any floating point number below the tolerance 
            is treated as numerically zero).
            The option @TO Software@ specifies the numerical algebraic
            geometry software used to perform a numerical irreducible decomposition: by
            default, the native routines provided by Macaulay2 are used, although other 
            valid options include BERTINI and PHCPACK (if the user has these installed 
            on their system).
        
        Example
            R = RR[x1, x2]
            f=(1/2)*(x1^4+x2^4-3*x1^2-3*x2^2+x1^2*x2^2)+1
            repList = detRep f;
            #repList
            repList#0
            all(repList, A -> clean(1e-10, f - det A) == 0)
    Caveat
        As this algorithm implements relatively brute-force algorithms, it may not 
        terminate for non-homogeneous polynomials of large degree 
        (e.g., degree >= 5).
    SeeAlso
        detRep
        bivariateDiagEntries
///

doc ///
    Key
        "Determinantal representations of hyperbolic plane cubics"
        HyperbolicPt
        [detRep, HyperbolicPt]
    Description
        Text
            This page demonstrates how the method @TO detRep@ computes monic symmetric
            determinantal representations of a hyperbolic cubic $f$ in $3$ variables, or gives 
            an error if certain necessary conditions for existence of such a representation are not 
            met. First, the polynomial is dehomogenized to obtain a bivariate polynomial. Next, if 
            $f = det(I + x_1A_1 + x_2A_2)$ is a symmetric determinantal representation, 
            then by suitable conjugation one may assume $A_1 = D_1$ is a diagonal matrix.
            Since $A_2$ is symmetric, there exists an orthogonal change-of-basis matrix 
            $V$ such that $VA_2V^T = D_2$ is diagonal. Since $D_1, D_2$ can be found
            using the method @TO bivariateDiagEntries@, to find a symmetric determinantal 
            representation of $f$ it suffices to compute the possible orthogonal matrices 
            $V$. This method computes the orthostochastic matrices which are the
            Hadamard squares of $V$, via the algorithm in [1], and returns the 
            associated determinantal representation (using the method 
            @TO orthogonalFromOrthostochastic@ - see that method for more on the
            possible orthogonal matrices returned).
            
            For a generic hyperbolic polynomial of degree $d$ in 3 variables, the number of
            definite determinantal representations is $2^g$, where $g = (d-1)(d-2)/2$ is the
            genus of the plane curve.
            
            For plane curves in special position, the option {\tt HyperbolicPt} allows 
            the user to specify a point $e$ such that the polynomial is hyperbolic with
            respect to $e$. In this case, a general coordinate change which brings $e$ to 
            $(1,0,0)$ is applied before computing the determinantal representation (which will 
            then be inverted before giving the result - note that the representation will no longer 
            be monic in general).
            
            When working over an @TO InexactFieldFamily@ like @TO RR@ or 
            @TO CC@, the option {\tt Tolerance} can be used to specify the internal
            threshold for checking equality (any floating point number below the tolerance 
            is treated as numerically zero).
        
        Example
            R = RR[x1, x2, x3]
            f = 6*x1^3+36*x1^2*x2+66*x1*x2^2+36*x2^3+11*x1^2*x3+42*x1*x2*x3+36*x2^2*x3+6*x1*x3^2+11*x2*x3^2+x3^3
            repList = detRep f
            all(repList, A -> clean(1e-10, f - det A) == 0)
            g = product gens R -- hyperbolic with respect to (1,1,1)
            B = clean(1e-6, first detRep(g, HyperbolicPt => matrix{{1_RR},{1},{1}}))
            clean(1e-6, g - det B)
    SeeAlso
        detRep
        bivariateDiagEntries
        orthogonalFromOrthostochastic
        "Determinantal representations of bivariate polynomials"
///

doc ///
    Key
        orthogonalFromOrthostochastic
        (orthogonalFromOrthostochastic, Matrix)
        [orthogonalFromOrthostochastic, Tolerance]
    Headline
        computes orthogonal matrices for a given orthostochastic matrix
    Usage
        orthogonalFromOrthostochastic A
        orthogonalFromOrthostochastic(A, Tolerance => 1e-6)
    Inputs
        A:Matrix
            an orthostochastic matrix
    Outputs
        :List
            of orthogonal matrices whose Hadamard square is $A$
    Description
        Text
            This method computes orthogonal matrices whose Hadamard square is a 
            given orthostochastic matrix. This is a helper function to 
            @TO detRep@, which computes symmetric
            determinantal representations of real cubic bivariate polynomials. 
            
            Given a $n\times n$ orthostochastic matrix $A$, there are $2^{n^2}$ possible
            matrices whose Hadamard square is $A$ (not all of which will be orthogonal in
            general though). Let $G\cong (\ZZ/2\ZZ)^n$ be the group of diagonal matrices 
            with diagonal entries equal to &plusmn;1. Then $G \times G$ acts (by
            $(g_1, g_2) O = g_1Og_2$) on the set of orthogonal matrices whose
            Hadamard square is $A$. This method computes all such orthogonal matrices,
            modulo the action of $G\times G$. The representative for each orbit is chosen 
            so that the first row and column will have all nonnegative entries, and modulo this 
            restriction on the signs, the algorithm is essentially a brute-force search. Note that 
            for generic choices of the orthostochastic matrix $A$, there will be exactly one 
            $G\times G$-orbit of orthogonal matrices with Hadamard square equal to $A$.
            
            When working over an @TO InexactFieldFamily@ like @TO RR@ or 
            @TO CC@, the option {\tt Tolerance} can be used to specify the internal
            threshold for checking equality (any floating point number below the tolerance 
            is treated as numerically zero).
        
        Example
            O = randomOrthogonal 4
            A = hadamard(O, O)
            orthogonalFromOrthostochastic A
    SeeAlso
        detRep
///

doc ///
    Key
        generalizedMixedDiscriminant
        (generalizedMixedDiscriminant, List)
    Headline
        computes generalized mixed discriminant of a list of matrices
    Usage
        generalizedMixedDiscriminant L
    Inputs
        L:List
            an $n$-tuple of $n\times n$ matrices
    Outputs
        :RingElement
            the generalized mixed discriminant of L
    Description
        Text
            This method computes the generalized mixed discriminant of an $n$-tuple 
            of $n\times n$ matrices. The generalized mixed discriminants give formulas
            for the coefficients of a determinantal polynomial, which are polynomial in 
            the entries of the representing matrices. Thus, computing determinantal
            representations can be viewed as solving a system of specializations of 
            generalized mixed discriminants, i.e., recovering a set of matrices from 
            its generalized mixed discriminants.
                 
        Example
            n = 3
            R = QQ[a_(1,1)..a_(n,n),b_(1,1)..b_(n,n),c_(1,1)..c_(n,n)][x_1..x_n]
            A = sub(transpose genericMatrix(coefficientRing R,n,n), R)
            B = sub(transpose genericMatrix(coefficientRing R,b_(1,1),n,n), R)
            C = sub(transpose genericMatrix(coefficientRing R,c_(1,1),n,n), R)
            P = det(id_(R^n) + x_1*A + x_2*B + x_3*C);
	    gmd = generalizedMixedDiscriminant({A,B,C})
	    coeff = (last coefficients(P, Monomials => {x_1*x_2*x_3}))_(0,0)
            gmd == coeff
///

doc ///
    Key
        isDoublyStochastic
        (isDoublyStochastic, Matrix)
        [isDoublyStochastic, Tolerance]
    Headline
        whether a matrix is doubly stochastic
    Usage
        isDoublyStochastic A
    Inputs
        A:Matrix
    Outputs
        :Boolean
            whether $A$ is a doubly stochastic matrix
    Description
        Text
            This method determines whether a given matrix is doubly stochastic, i.e., is 
            a real square matrix with all entries nonnegative and all row and column sums
            equal to $1$. 
            
            When working over an @TO InexactFieldFamily@ like @TO RR@ or 
            @TO CC@, the option {\tt Tolerance} can be used to specify the internal
            threshold for checking equality (any floating point number below the tolerance 
            is treated as numerically zero).
                 
        Example
            O = randomOrthogonal 3
            A = hadamard(O, O)
            isDoublyStochastic A
///

doc ///
    Key
        isOrthogonal
        (isOrthogonal, Matrix)
        [isOrthogonal, Tolerance]
    Headline
        whether a matrix is orthogonal
    Usage
        isOrthogonal A
    Inputs
        A:Matrix
    Outputs
        :Boolean
            whether $A$ is orthogonal
    Description
        Text
            This method determines whether a given matrix is orthogonal, i.e., has 
            inverse equal to its transpose. 
            
            When working over an @TO InexactFieldFamily@ like @TO RR@ or 
            @TO CC@, the option {\tt Tolerance} can be used to specify the internal
            threshold for checking equality (any floating point number below the tolerance 
            is treated as numerically zero). If the given matrix does not have floating point
            entries, then this option is not used.
                 
        Example
            O1 = randomOrthogonal 5
            isOrthogonal O1
            O2 = randomOrthogonal(5, QQ)
            isOrthogonal O2
    SeeAlso
        randomOrthogonal
///

doc ///
    Key
        randomOrthogonal
        (randomOrthogonal, ZZ)
        (randomOrthogonal, ZZ, Thing)
    Headline
        constructs a random special orthogonal matrix
    Usage
        randomOrthogonal n
        randomOrthogonal(n, R)
    Inputs
        n:ZZ
        R:Ring
            or @TO RR@ or @TO CC@ (of type @TO InexactFieldFamily@)
    Outputs
        :Matrix
            a random $n\times n$ special orthogonal matrix, over the ring $R$
    Description
        Text
            This method returns a random orthogonal matrix of a given size $n$.
            The orthogonal matrix is constructed via Cayley's correspondence,
            which gives a bijection between skew-symmetric matrices, and 
            orthogonal matrices $O$ which do not have $1$ as an eigenvalue
            (i.e., $O - I$ is invertible). Up to changing signs of rows, any orthogonal 
            matrix can be obtained this way: if $G\cong (\ZZ/2\ZZ)^n$ 
            is the group of diagonal matrices with diagonal entries equal to 
            &plusmn;1, acting on $n\times n$ matrices by left multiplication, then 
            (as one may check) every $G$-orbit contains a matrix 
            that does not have $1$ as an eigenvalue (if the characteristic is not 2).
            
            Note that the matrices which feature in the Cayley correspondence have
            determinant $(-1)^n$, so this method scales by $-1$ to return a special
            orthogonal matrix. Thus the matrices returned by this method do not have 
            $-1$ as an eigenvalue.
            
            By default a matrix over @TO RR@ is returned. This method also accepts
            a ring as an (optional) argument, in which case a special orthogonal matrix
            over the ring is returned, with entries in the 
            @TO2{coefficientRing, "base coefficient ring"}@.
            
        Example
            O1 = randomOrthogonal 5
            isOrthogonal O1
            eigenvalues O1
            det O1
            R = QQ[x,y]
            O2 = randomOrthogonal(5, R)
            isOrthogonal O2
            det(O2), det(O2+id_(R^5))
    SeeAlso
        isOrthogonal
///

doc ///
    Key
        randomIntegerSymmetric
        (randomIntegerSymmetric, ZZ)
        (randomIntegerSymmetric, ZZ, Ring)
    Headline
        constructs a random integer symmetric matrix
    Usage
        randomIntegerSymmetric n
        randomIntegerSymmetric(n, R)
    Inputs
        n:ZZ
        R:Ring
            or @TO RR@ or @TO CC@ (of type @TO InexactFieldFamily@)
    Outputs
        :Matrix
            a random $n\times n$ symmetric matrix with integer entries, over the ring $R$
    Description
        Text
            This method returns a random symmetric matrix of a given size $n$ with 
            integer entries. This can in turn be specialized to any ring, which may be
            provided as an argument. 
                 
        Example
            randomIntegerSymmetric 5
            randomIntegerSymmetric 20
            R = RR[x,y]
            randomIntegerSymmetric(3, R)
    Caveat
        The entries of the constructed matrix will be integers between 0 and 18, inclusive.
    SeeAlso
        genericSymmetricMatrix
///

doc ///
    Key
        randomPSD
        (randomPSD, ZZ)
        (randomPSD, ZZ, ZZ)
        (randomPSD, ZZ, RR)
        (randomPSD, ZZ, ZZ, RR)
    Headline
        constructs a random positive semidefinite matrix
    Usage
        randomPSD n
        randomPSD(n, r)
        randomPSD(n, s)
        randomPSD(n, r, s)
    Inputs
        n:ZZ
            the size of the output matrix
        r:ZZ
            the desired rank
        s:RR
            an upper bound on the spectral radius (= largest eigenvalue)
    Outputs
        :Matrix
            a random $n\times n$ positive semidefinite matrix with real entries,
            of rank $r$, with eigenvalues between $0$ and $s$.
    Description
        Text
            This method returns a random symmetric positive semidefinite real matrix
            of a given size $n$. The rank $r$ can also be specified: by default,
            the matrix will be full rank (with probability 1). An upper bound $s$ on the 
            spectral radius can also be specified: by default, the matrix will have spectral
            radius $<= 1$.
                 
        Example
            randomPSD 5
            A1 = randomPSD(5, 3)
            A2 = randomPSD(5, 3.0)
            (A1, A2)/eigenvectors -- note the difference!
            A3 = randomPSD(5, 3, 7.0)
            eigenvectors(A3, Hermitian => true)
    Caveat
        This method works by choosing the eigenvectors and eigenvalues independently
        randomly. The distribution on the (compact) set of PSD matrices of bounded
        spectral radius may not be uniform or statistically desirable (cf. Wishart distribution).
    SeeAlso
        cholesky
///

doc ///
    Key
        randomUnipotent
        (randomUnipotent, ZZ)
        (randomUnipotent, ZZ, Thing)
    Headline
        constructs a random unipotent matrix
    Usage
        randomUnipotent n
        randomUnipotent(n, R)
    Inputs
        n:ZZ
        R:Ring
            or @TO RR@ or @TO CC@ (of type @TO InexactFieldFamily@)
    Outputs
        :Matrix
            a random $n\times n$ unipotent matrix, over the ring $R$
    Description
        Text
            This method returns a random unipotent matrix of a given size $n$,
            which is upper triangular with all diagonal entries equal to $1$.
            if a ring $R$ is provided, then the output is a matrix over $R$ - 
            by default, the output is a matrix over @TO QQ@.

        Example
            randomUnipotent 5
            randomUnipotent(3, CC)
            randomUnipotent(3, RR[x,y])
///

doc ///
    Key
        cholesky
        (cholesky, Matrix)
        [cholesky, Tolerance]
    Headline
        computes the Cholesky decomposition of a positive semidefinite matrix
    Usage
        cholesky A
    Inputs
        A:
            an $n\times n$ PSD matrix
    Outputs
        L:Matrix
            a lower-triangular matrix, with $A = LL^T$
    Description
        Text
            This method computes the Cholesky decomposition of a symmetric positive
            semidefinite matrix $A$, which is a factorization $A = LL^T$, where $L$ is 
            lower-triangular. If $A$ is not positive-definite, then the Cholesky 
            decomposition is not unique - in this case, this method will attempt to
	    give an output which is as sparse as possible.
            
            When working over an @TO InexactFieldFamily@ like @TO RR@ or 
            @TO CC@, the option {\tt Tolerance} can be used to specify the internal
            threshold for checking equality (any floating point number below the tolerance 
            is treated as numerically zero).
                 
        Example
            A = randomPSD 5 -- 5x5 PSD of full rank
            L = cholesky A
            clean(1e-12, A - L*transpose L) == 0
	    B = randomPSD(7, 3) -- 7x7 PSD matrix of rank 3
	    L = cholesky B
	    clean(1e-12, B - L*transpose L) == 0
///

doc ///
    Key
        hadamard
        (hadamard, Matrix, Matrix)
    Headline
        computes the Hadamard product of two matrices
    Usage
        hadamard(A, B)
    Inputs
        A:
            an $m\times n$ matrix
        B:
            an $m\times n$ matrix
    Outputs
        :Matrix
            the Hadamard product of $A$ and $B$
    Description
        Text
            This method computes the Hadamard product of two matrices $A, B$ of the
            same size. The Hadamard product is defined as the componentwise product, 
            i.e., if $A, B$ are $m\times n$ matrices, then the Hadamard product is the 
            $m\times n$ matrix with $(i,j)$ entry equal to $A_{i,j}*B_{i,j}$.
                 
        Example
            (A1, A2) = (random(ZZ^2, ZZ^3), random(ZZ^2, ZZ^3))
            hadamard(A1, A2)
///

doc ///
    Key
        coeffMatrices
        (coeffMatrices, Matrix)
    Headline
        gets coefficient matrices for a matrix of linear forms
    Usage
        coeffMatrices(M)
    Inputs
        M:Matrix
            of linear forms
    Outputs
        :List
            of matrices
    Description
        Text
            Given a linear matrix pencil $M = I_d + x_1A_1 + ... + x_nA_n$,
            this method returns the list of matrices $A_1, ..., A_n$.
                 
        Example
            R = QQ[x,y,z]
            M = id_(R^3) + random(R^3,R^{3:-1})
            coeffs = coeffMatrices M
            M - sum(#gens R, i -> R_i*coeffs#i)
    Caveat
        This method does not return the constant term, or coefficients 
        of terms of degree $> 1$.
///

doc ///
    Key
        companionMatrix
        (companionMatrix, RingElement)
    Headline
        companion matrix of a univariate polynomial
    Usage
        companionMatrix f
    Inputs
        f:RingElement
            a univariate polynomial
    Outputs
        :Matrix
    Description
        Text
            For a monic univariate polynomial $f$ of degree $d$, this
            method returns the companion matrix $C(f)$, which is a 
            $d\times d$ matrix whose characteristic polynomial is $f$. 
            Explicitly, $C(f)$ has entries $1$ on the first subdiagonal
            (the diagonal below the main diagonal), negative coefficients
            of $f$ in the last column (other than the leading coefficient 
            of $1$), and $0$ elsewhere.
            
            If $f$ is not monic, then this method returns the companion 
            matrix of the normalized monic polynomial $(1/a_n)f$, where
            $a_n$ is the leading coefficient of $f$.
                 
        Example
            R = CC[x]
            eigenvalues companionMatrix(9*x^2 - 1)
            f = x^10 + sum(10, i -> random(i, R))
            C = companionMatrix f
            clean(1e-10, f - det(x*id_(R^10) - C))
            all(eigenvalues C, z -> clean(1e-10, sub(f, R_0 => z)) == 0)
///

doc ///
    Key
        realPartMatrix
        (realPartMatrix, Matrix)
    Headline
        real part of a matrix over CC
    Usage
        realPartMatrix A
    Inputs
        A:Matrix
            with complex entries
    Outputs
        :Matrix
            the real matrix whose entries are real parts of entries of A
    Description
        Text
            Given a complex matrix, this method returns a real matrix obtained by taking 
            the real part of each entry. It leaves matrices over @TO RR@ and @TO QQ@
            unchanged.
                 
        Example
            A = random(RR^3,RR^5)
            A == realPartMatrix A
            B = sub(A, CC)
            C = realPartMatrix B
            clean(1e-10, A - C) == 0
            D = random(QQ^3, QQ^1)
            D == realPartMatrix D
            
        Text
            If the matrix is over a polynomial ring, but has entries defined over the base
            field (e.g., when taking @TO coefficients@), then it is necessary to @TO sub@
            into the base field first:
            
        Example
            R = CC[x,y]
            f = random(2,R)
            C = last coefficients f
            realPartMatrix sub(C, coefficientRing R)
    SeeAlso
        roundMatrix
///

doc ///
    Key
        roundMatrix
        (roundMatrix, ZZ, Matrix)
    Headline
        lifts matrix over RR to matrix over QQ
    Usage
        roundMatrix(n, A)
    Inputs
        A:Matrix
            with real entries
        n:ZZ
            a threshold for rounding digits
    Outputs
        :Matrix
            a matrix over QQ, obtained by rounding entries of A
    Description
        Text
            This method converts a real matrix to a rational matrix, by rounding each 
            entry. The input $n$ specifies the number of (decimal) digits used for rounding.
                 
        Example
            A = matrix{{1, 2.5, -13/17}, {2*pi, 4.7, sqrt(2)}}
            roundMatrix(5, A)
    SeeAlso
        realPartMatrix
///

undocumented {
    linesOnCubicSurface,
    (linesOnCubicSurface, RingElement),
    doubleSixes,
    (doubleSixes, List),
    cubicSurfaceDetRep,
    (cubicSurfaceDetRep, RingElement),
    (cubicSurfaceDetRep, RingElement, List)
}

-----------------------------

-- TESTS

TEST /// -- Quadratic case: over QQ, RR, CC
S = QQ[x1,x2,x3]
f = 1 - 8*x1*x2 - 4*x1*x3 - 100*x2^2 - 12*x2*x3 - x3^2 - 5*x1^2
M = first detRep(f, Tolerance => 1e-10)
assert(clean(1e-9, sub(f - det M, RR(monoid[gens S]))) == 0)
SRR = RR[x1,x2,x3]
fRR = sub(f, SRR)
M = first detRep fRR
assert(0 == clean(1e-10, fRR - det M))
SCC = CC[x1,x2,x3]
fCC = sub(f, SCC)
M = first detRep fCC
assert(0 == clean(1e-10, fCC - det M))
R = RR[x1,x2,x3,x4]
f = det sum({id_(R^2)} | apply(gens R, v -> v*randomIntegerSymmetric(2, R)))
M = first detRep f
assert(clean(1e-10, f - det M) == 0)
///

TEST /// -- Quadratic case: size > 2
d = 7
R = RR[x_1..x_d]
A1 = randomPSD d; 
f1 = ((vars R)*(-1)*A1*transpose vars R + vars R*random(RR^d,RR^1) + 1)_(0,0)
M = first detRep f1
assert(clean(1e-12, f1 - det M) == 0)
A2 = randomPSD(d, d//2)
f2 = ((vars R)*(-1)*A2*transpose vars R + vars R*random(RR^d,RR^1) + 1)_(0,0)
M = first detRep f2
assert(clean(1e-12, f2 - det M) == 0)
coeffs = coeffMatrices M
assert(clean(1e-10, M - sum({id_(R^(d+1))} | apply(#gens R, i -> R_i*coeffs#i))) == 0)
///

TEST /// -- Cubic case: over QQ, RR, CC
S=QQ[x1,x2,x3]
f = 6*x1^3+36*x1^2*x2+66*x1*x2^2+36*x2^3+11*x1^2*x3+42*x1*x2*x3+36*x2^2*x3+6*x1*x3^2+11*x2*x3^2+x3^3
reps = detRep(f, Tolerance => 1e-12)
assert(all(reps, A -> clean(1e-10, sub(f - det A, RR(monoid[gens S]))) == 0))
SRR=RR[x1,x2,x3]
fRR=sub(f, SRR)
reps = detRep fRR
assert(all(reps, L -> clean(1e-10, fRR - det L) == 0))
SCC=CC[x1,x2,x3]
fCC=sub(f, SCC)
reps = detRep fCC
assert(all(reps, L -> clean(1e-9, fCC - det L) == 0))
reps = detRep(fCC, HyperbolicPt => matrix{{3_RR},{2},{-7}}) -- representations are not monic
assert(all(reps, L -> clean(1e-6, fCC - det L) == 0))
///

TEST /// -- Cubic case: homogeneous, 3 variables
S = RR[x,y,z]
F = det sum(gens S, v -> v*sub(randomPSD 3, S)) -- nondegenerate
M = first detRep F
assert(clean(1e-4, F - det M) == 0)
F = (random(1,S))^3 -- degenerate
M = first detRep(F, Tolerance => 1e-3)
assert(clean(1e-5, F - det M) == 0)
F = 162*y^3 - 23*y^2*z + 99*y^2*x - 8*y*z^2 - 10*y*z*x + 18*y*x^2 + z^3 - z^2*x - z*x^2 + x^3 -- degenerate (Example 2.17 in [Dey1])		
M = first detRep F		
assert(clean(1e-10, F - det M) == 0)
///

TEST /// -- Specific threshold tests
M = matrix{{0.5322,0.3711,0.0967},{0.4356,0.2578,0.3066},{0.0322,0.3711,0.5967}}
assert(orthogonalFromOrthostochastic M === {})
assert(#orthogonalFromOrthostochastic(M, Tolerance => 1e-4) > 0)
S = RR[x,y,z]
l1 = .182627222080409*x+.00411844060723943*y+.677316669916152*z -- y coefficient much smaller
assert((try detRep l1^3) === null)
A = first detRep(l1^3, Tolerance => 1e-4)
assert(clean(1e-10, l1^3 - det A) == 0)
l2 = .000267436534581389*x + .622384659384624*y + .608050635739978*z -- x coefficient much smaller
assert((try detRep l2^3) === null)
A = first detRep(l2^3, Tolerance => 5e-2) -- !!
assert(clean(1e-3, l2^3 - det A) == 0)
///

TEST /// -- Degenerate cubics
R = RR[x,y,z]
D222 = diagonalMatrix {2_R,2,2}
D444 = diagonalMatrix {4_R,4,4}
D123 = diagonalMatrix {1_R,2,3}
D456 = diagonalMatrix {4_R,5,6}
D445 = diagonalMatrix {4_R,4,5}
D455 = diagonalMatrix {4_R,5,5}
D225 = diagonalMatrix {2_R,2,5}
D122 = diagonalMatrix {1_R,2,2}
D233 = diagonalMatrix {2_R,3,3}
D223 = diagonalMatrix {2_R,2,3}
D557 = diagonalMatrix {5_R,5,-7}
D577 = diagonalMatrix {5_R,-7,-7}
degenCubicList = {
-- A1, A2 both repeated eigenvalue of multiplicity 3
    det(R_0*id_(R^3) + R_1*D222 + R_2*D444),
    det(R_0*id_(R^3) + R_1*D222 + R_2*D222),
-- A1 distinct eigenvalues, A2 repeated eigenvalue of multiplicity 3
    det(R_0*id_(R^3) + R_1*D123 + R_2*D444),
    det(R_0*id_(R^3) + R_1*D123 + R_2*D222),
-- A1 repeated eigenvalue of multiplicity 3, A2 distinct eigenvalues
    det(R_0*id_(R^3) + R_1*D222 + R_2*D456),
    det(R_0*id_(R^3) + R_1*D444 + R_2*D456),
-- A1 distinct eigenvalues, A2 repeated eigenvalue of multiplicity 2
    det(R_0*id_(R^3) + R_1*D123 + R_2*D445),
    det(R_0*id_(R^3) + R_1*D123 + R_2*D455),
    det(R_0*id_(R^3) + R_1*D123 + R_2*D225),
    det(R_0*id_(R^3) + R_1*D123 + R_2*D122),
-- A1 repeated eigenvalue of multiplicity 2, A2 distinct eigenvalues
    det(R_0*id_(R^3) + R_1*D233 + R_2*D456),
    det(R_0*id_(R^3) + R_1*D223 + R_2*D456),
-- A1, A2 both repeated eigenvalue of multiplicity 2
    det(R_0*id_(R^3) + R_1*D223 + R_2*D557),
    det(R_0*id_(R^3) + R_1*D233 + R_2*D557), -- non-diagonal
    det(R_0*id_(R^3) + R_1*D223 + R_2*D577),
    det(R_0*id_(R^3) + R_1*D233 + R_2*D577) -- non-diagonal
}
assert(all(degenCubicList, f -> (
    reps = detRep(f, Tolerance => 1e-4);
    all(reps, A -> clean(1e-3, f - det A) == 0)
)))
///

TEST /// -- HyperbolicPt test
R = RR[x,y,z]
f = product gens R
assert((try detRep f) === null)
e = transpose matrix{{1_RR,1,1}}
assert(clean(1e-10, f - det first detRep(f, HyperbolicPt => e)) == 0)
f = (2*x - 3*y)*(4*y + z)*(7*x - 11*z)
assert((try detRep f) === null)
setRandomSeed 0
assert(clean(1e-10, f - det first detRep(f, HyperbolicPt => e)) == 0)
///

TEST /// -- Higher degree case
R=RR[x,y]
f = x^5+6*x^4*y-2*x^3*y^2-36*x^2*y^3+x*y^4+30*y^5
M = first detRep f
assert(clean(1e-13, f - det M) == 0)
f = 24*x^4+(49680/289)*x^3*y+50*x^3+(123518/289)*x^2*y^2+(72507/289)*x^2*y+35*x^2+(124740/289)*x*y^3+(112402/289)*x*y^2+(32022/289)*x*y+10*x+144*y^4+180*y^3+80*y^2+15*y+1
sols = detRep f
assert(all(sols, M -> clean(1e-10, f - det M) == 0))
n = 4
A = randomIntegerSymmetric(n, R)
f = det(id_(R^n) + R_0*diagonalMatrix {4,3,2,1_R} + R_1*A)
(D1, D2, diag1, diag2) = bivariateDiagEntries f
sols = detRep f;
assert(all(sols, M -> clean(1e-6, f - det M) == 0))
///

TEST /// -- cubic surface (Bernd)
eps = 1e-10
R = CC[x,y,z,w]
f = homogenize(3*x^3+2*x^2*y+x*y^2+6*y^3+7*x^2*z+8*x*y*z+3*y^2*z+8*x*z^2+3*y*z^2+8*z^3+8*x^2+7*x*y+9*y^2+7*x*z+3*y*z+8*z^2+2*x+4*y+8*z+1, w)
lineSet = linesOnCubicSurface f
assert(#lineSet == 27)
-- ds = first doubleSixes lineSet
-- assert(all(subsets(ds#1, 2), s -> not clean(eps, det(s#0 || s#1)) == 0))
-- assert(all(ds#1, l -> #select(ds#0, m -> clean(eps, det(l || m)) == 0) == 5))
-- assert(all(6, i -> not clean(eps, det(ds#0#i || ds#1#i)) == 0))
-- M = detRep f
-- g1 = M_(0,1)*M_(1,2)*M_(2,0)
-- g2 = M_(0,2)*M_(1,0)*M_(2,1)
-- assert(clean(eps, det M - (g1 + g2)) == 0)
-- a1 = sub(last coefficients(g1, Monomials => {x^3, w^3}), coefficientRing ring f)
-- a2 = sub(last coefficients(g2, Monomials => {x^3, w^3}), coefficientRing ring f)
-- b = sub(last coefficients(f, Monomials => {x^3, w^3}), coefficientRing ring f)
-- c = solve(a1 | a2, b)
-- f - (c_(0,0)*g1 + c_(1,0)*g2)
///

TEST /// -- Clebsch cubic surface (cf. https://blogs.ams.org/visualinsight/2016/02/15/27-lines-on-a-cubic-surface/)
eps = 1e-10
R = CC[x,y,z,w]
f = 81*(x^3 + y^3 + z^3) - 189*(x^2*y + x^2*z + x*y^2 + x*z^2 + y^2*z + y*z^2) + 54*x*y*z + 126*w*(x*y + x*z + y*z) - 9*w*(x^2 + y^2 + z^2) - 9*w^2*(x+y+z) + w^3
elapsedTime lineSet = linesOnCubicSurface(f, Tolerance => eps)
assert(#lineSet == 27)
assert(all(lineSet, m -> clean(eps, m - realPartMatrix m) == 0)) -- all real lines
///

TEST /// -- Generalized mixed discriminant
n = 4
R = QQ[a_(1,1)..a_(n,n),b_(1,1)..b_(n,n)][x_1,x_2]  -- bivariate quartic
A = sub(transpose genericMatrix(coefficientRing R,n,n), R)
B = sub(transpose genericMatrix(coefficientRing R,b_(1,1),n,n), R)
P = det(id_(R^n) + x_1*A + x_2*B);
assert((last coefficients(P, Monomials => {x_1*x_2}))_(0,0) == generalizedMixedDiscriminant({A,B}))
assert((last coefficients(P, Monomials => {x_1^3*x_2}))_(0,0) == generalizedMixedDiscriminant({A,A,A,B}))
n = 3
R = QQ[a_(1,1)..a_(n,n),b_(1,1)..b_(n,n),c_(1,1)..c_(n,n)][x_1..x_n] -- trivariate cubic
A = sub(transpose genericMatrix(coefficientRing R,n,n), R)
B = sub(transpose genericMatrix(coefficientRing R,b_(1,1),n,n), R)
C = sub(transpose genericMatrix(coefficientRing R,c_(1,1),n,n), R)
P = det(id_(R^n) + x_1*A + x_2*B + x_3*C);
assert((last coefficients(P, Monomials => {x_1*x_2*x_3}))_(0,0) == generalizedMixedDiscriminant({A,B,C}))
///

TEST /// -- isOrthogonal, isDoublyStochastic
assert(isOrthogonal id_(ZZ^5))
assert(isOrthogonal id_(QQ^5))
assert(isOrthogonal id_((CC[x,y])^5))
R = RR[x,y]
I = id_(R^5)
assert(isOrthogonal I and isDoublyStochastic I)
assert(clean(1e-10, hadamard(I, I) - I) == 0)
O1 = randomOrthogonal 5
A = hadamard(O1, O1)
O = first orthogonalFromOrthostochastic(A, Tolerance => 1e-10)
assert(isOrthogonal(O, Tolerance=>1e-5) and isDoublyStochastic A and clean(1e-8, hadamard(O, O) - A) == 0)
///

TEST /// -- cholesky, randomPSD
eps = 1e-15
A = randomPSD 5
E = eigenvectors(A, Hermitian => true)
assert(clean(eps, A - E#1*diagonalMatrix(E#0)*transpose E#1) == 0)
L = cholesky A
assert(clean(eps, A - L*transpose L) == 0)
///

TEST /// -- companion matrix / root test
R = RR[x]
f1 = sum(4, i -> random(i,R))
f2 = sum(40, i -> random(i,R))
f3 = (R_0)^2 - 1
f4 = (R_0 - 1)^3
f5 = (R_0 + 1)^6
polys = {f1,f2,f3,f4,f5}
rtList = polys/companionMatrix/eigenvalues;
assert(all(#polys, i -> all(rtList#i, r -> clean(1e-5, sub(polys#i, R_0 => r)) == 0)))
///

end--
restart
debug needsPackage "DeterminantalRepresentations"
debug loadPackage("DeterminantalRepresentations", Reload => true)
uninstallPackage "DeterminantalRepresentations"
installPackage "DeterminantalRepresentations"
installPackage("DeterminantalRepresentations", RemakeAllDocumentation => true)
viewHelp "DeterminantalRepresentations"
check "DeterminantalRepresentations"

-- Quartic examples
R = RR[x1,x2]
f=(1/2)*(x1^4+x2^4-3*x1^2-3*x2^2+x1^2*x2^2)+1
reps = detRep f;
time repList = bivariateDetRep(f, Strategy => "Orthogonal", Software => BERTINI) -- SLOW!

f = 24*x1^4+(49680/289)*x1^3*x2+50*x1^3+(123518/289)*x1^2*x2^2+(72507/289)*x1^2*x2+35*x1^2+(124740/289)*x1*x2^3+(112402/289)*x1*x2^2+(32022/289)*x1*x2+10*x1+144*x2^4+180*x2^3+80*x2^2+15*x2+1

-- Higher degree bivariate
n = 5
R = RR[x,y]
f = det(id_(R^n) + x*sub(diagonalMatrix toList(1..n),R) + y*randomIntegerSymmetric(n, R))

----------------------------------------------------

-- To do:

----------------------------------------------------
-- Old code

makeUvector = method()
makeUvector (List, ZZ) := List => (D, k) -> transpose matrix{apply(subsets(#D, k), s -> product(s, i -> D_i))}

makeUComp = method()
makeUComp (List, ZZ, ZZ) := List => (D, k, l) -> (
    Nk := subsets(#D, k);
    Nl := subsets(#D, l);
    transpose matrix{apply(Nl, s -> sum flatten((select(Nk, t -> #((set t)*(set s)) == 0))/(S -> product(D_S))))}
)

----------------------------------------------------

-- Nuij path (cf. Leykin-Plaumann)

clearAll

tOperator = method()
tOperator (RingElement, RingElement) := RingElement => (f, l) -> (
    R := ring f;
    f+(1-last gens coefficientRing R)*l*diff(R_0,f)
)

(n,d) = (2,3)
e = max(d,n)
k = CC
C = k[a_0..a_(binomial(n+d,d)-1),s]
R = C[t,x_1..x_n]
f = (basis(d,R)*matrix pack(1, drop(gens C, -1)))_(0,0)

g = sub(f, apply(drop(gens R, 1), v -> v => v*last gens C))
F = g
for i to e-1 do F = tOperator(F, (basis(1,R,Variables=>drop(gens R,1))*random(k^n,k^1))_(0,0))
system = sub(last coefficients(F, Monomials => basis(first degree f,R)), C)

S = k[gens C | {t}]
H = flatten entries sub(system, S)
sols = bertiniUserHomotopy(t,{s=>t},H,{point{toList(#gens S-2:0)}})

f = det(R_0*id_(R^d) + R_1*diagonalMatrix {1_R,2,3} + R_2*diagonalMatrix {4_R,5,6})
p = point sub(last coefficients f, CC)
sols = bertiniUserHomotopy(t,{s=>t},H-p#Coordinates,{p})

needsPackage "Bertini"
R = CC[x,a,t]
H = { (x^2-1)*a^3 + (x^2-2)*(1-a)^2}
sol1 = point {{1}}
sol2 = point {{ -1}}
S1= { sol1, sol2  }
S0 = bertiniUserHomotopy (t,{a=>t}, H, S1)

------------------------------------------------------------------

-- 27 lines on cubic surface from determinantal representation

needsPackage "NumericalImplicitization"
P2 = CC[x_0..x_2]
P3 = CC[z_0..z_3]
S = P2 ** P3
X = sub(matrix pack(1, gens P2), S)
Z = sub(vars P3, S)
M = sub(random(P3^3,P3^{3:-1}), S)
L = matrix apply(flatten entries(M*X), e -> {transpose(e // Z)})
clean(1e-10, M*X - L*transpose Z)
I = sub(minors(3, L), P2)
time pts = apply((numericalIrreducibleDecomposition I)#1, p -> p#Points#0#Coordinates)

-- Get exceptional divisor
L1 = ideal ((M*transpose matrix{pts#0})^{0,1})
clean(1e-13, det M % L1)

-- Get line through 2 points
pair = pts_{0,1}
newpair = {pair#0 + pair#1, pair#0 - pair#1}
newpair = {4*pair#0 + 7*pair#1, 9*pair#0 - (-3/4)*pair#1}
q = matrix apply(newpair, p -> {sub(gens I, matrix{p})})
l = sub(Z*gens ker q, P3)
clean(1e-10, sub(l, q^{0}))
clean(1e-10, sub(l, q^{1}))

sub(det M, P3) % l -- not 0!

F = extractImageEquations(gens I, ideal 0_P2, 3)
(map(P3, ring F, gens P3))(F)

G = sub(matrix{{det L_{1,2,3}, det L_{0,2,3}, det L_{0,1,3}, det L_{0,1,2}}}, P2)
q1 = matrix apply(newpair, p -> {sub(G, matrix{p})})
l1 = sub(Z*gens ker q1, P3)

q0 = sub(gens I, apply(#gens ring I, i -> (ring I)_i => newpair#0#i))
q1 = sub(gens I, apply(#gens ring I, i -> (ring I)_i => newpair#1#i))
P1 = CC[s,t]
l0 = extractImageEquations(s*sub(q0, P1) + t*sub(q1, P1), ideal 0_P1, 1)
l0 = (map(P3, ring l0, gens P3))(l0)
(sub(det M, P3) % l) - (sub(det M, P3) % l0)
