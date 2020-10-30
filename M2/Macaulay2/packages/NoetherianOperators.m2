-- -*- coding: utf-8 -*-
newPackage(
     "NoetherianOperators",
     PackageExports => {"NAGtypes"},
     Version => "1.0",
     Date => "July 12, 2020",
     Authors => {
        {Name => "Robert Krone", 
    	       Email => "krone@math.gatech.edu"},
        {Name => "Justin Chen", 
               Email => "jchen646@gatech.edu"},
        {Name => "Marc Harkonen", 
               Email => "harkonen@gatech.edu"},
        {Name => "Markus Wageringel",
               Email => "mwageringel@uos.de"}
     },
     Headline => "numerically compute local dual spaces, Hilbert functions, and Noetherian operators",
     PackageExports => {"Truncations", "Bertini"},
     PackageImports => {"Dmodules"},
     AuxiliaryFiles => true,
     DebuggingMode => true
)

export {
    "truncatedDual",
    "zeroDimensionalDual",
    "gCorners",
    "socles",
    "sCorners",
    "localHilbertRegularity",
    "eliminatingDual",
    "innerProduct",
    "reduceSpace",
    "orthogonalInSubspace",
    "DZ",
    "BM",
    "Normalize",
     "Rational",
    "ProduceSB",
    "numericalKernel",
    "numericalImage",
    "colReduce",
    "newGCorners",

    "DiffOp",
    "diffOp",

    --Data type keys OLD
    "Ops",
    "Prime",
    -- OLD

    "diffAlg", -- OLD
    "noetherianOperators",
    "numericalNoetherianOperators",
    "DependentSet",
    "noethOpsFromComponents",
    "coordinateChangeOps",
    --"sanityCheck",
    "rationalInterpolation",
    --"applyNOp",
    "InterpolationTolerance",
    "InterpolationDegreeLimit",
    "NoetherianDegreeLimit",
    "Sampler",
    "TrustedPoint",

    --functions from punctual Hilb approach
    "getNoetherianOperatorsHilb",
    "getIdealFromNoetherianOperators",
    "joinIdeals",
    "mapToPunctualHilbertScheme"

}

--TruncDualData private keys
protect \ {
    Igens,
    syl,
    strategy,
    deg,
    dBasis,
    hIgens,
    BMintegrals,
    BMcoefs,
    BMbasis,
    Seeds,
    Op
}

-----------------------------------------------------------------------------------------
load "NoetherianOperators/pointSampling.m2"
--
-----  Noetherian operator data structures
-- TODO remove this
SetOfNoethOps = new Type of HashTable;
SetOfNoethOps / Function := (N, f) -> (entries N) / f;
SetOfNoethOps#{Standard,AfterPrint} = x -> (
    o := () -> concatenate(interpreterDepth:"o");
    << endl;                 -- double space
    << o() << lineNumber;
    y := class x;
    << " : " << "set of Noetherian operators";
    if x#?Prime then << " over the prime "<<x#Prime
    else if x#?Point then << " evaluated at "<< x#Point;
    << endl;
)
SetOfNoethOps _* := N -> N.Ops;
SetOfNoethOps _ ZZ := (N, i) -> (N.Ops)#i;

setOfNoethOps = method(Options=>{Point=>null})
setOfNoethOps(Matrix) := o -> M -> (
    R := coefficientRing(ring M);
    if o.Point === null then error "needs prime or Point";
    setOfNoethOps(M, ideal(vars(R) - matrix(o#Point)), Point=>point o#Point)
)
setOfNoethOps(Matrix,Ideal) := o -> (M,P) -> (
    new SetOfNoethOps from {
        Gens => M,
        Ops => sort flatten entries M,
        Point => o#Point,
        Prime => P
    }
)

entries SetOfNoethOps := N -> N.Ops;
gens SetOfNoethOps := o -> N -> N.Gens;
net SetOfNoethOps := N -> net N.Ops;
netList SetOfNoethOps := opts -> N -> netList(N.Ops, opts);
numgens SetOfNoethOps := N -> #N.Ops;
ring SetOfNoethOps := N -> ring gens N;
sort SetOfNoethOps := opts -> N -> (
    new SetOfNoethOps from applyPairs(N, (i,j) -> if i === symbol Ops then (i, sort j) else (i,j))
)

-- dualSpace SetOfNoethOps := N -> (
--     R' := ring gens N;
--     if N#Point === null then error("needs a Point");
--     R := coefficientRing R';
--     R'toR := map(R,R',vars R);
--     dualSpace(R'toR (gens N), N#Point)
--     )

-- Create a dual space from a list of Noetherian operators
-- Caveat: if Noetherian operators have non-constant coefficients,
--          behavior is undefined.
dualSpace (List, Point) := (L, p) -> (
    gens := matrix{ L / (op -> keys op / (k -> op#k * k)) / sum };
    dualSpace(gens, p)
)
/// TEST
R = CC[x,y]
foo1 = new DiffOp from {x => 12, x^2*y => 3}
foo2 = new DiffOp from {1_R => 4, x*y => 4}
pt = point{{1_CC,2}}
a = dualSpace({foo1,foo2}, pt)
b = dualSpace(matrix{{12*x + 3*x^2*y, 4*x*y + 4}}, pt)
assert(gens a.Space == gens b.Space)
assert(point a == point b)
///

-- Maybe not needed?
NoethOp = new Type of HashTable;


-- Default tolerance value respectively for exact fields and inexact fields
defaultT = R -> if precision 1_R == infinity then 0 else 1e-6;
getTolerance = true >> opts -> R -> if not opts.?Tolerance or opts.Tolerance === null then defaultT(R) else opts.Tolerance;

shiftGens := (p,Igens) -> (
    R := ring Igens;
    sub(Igens, matrix{gens R + apply(p.Coordinates,c->sub(c,R))})
    )

listFactorial = L -> product(L, l->l!)

-- Legacy methods
----------------------------------
truncatedDual = method(Options => {Strategy => BM, Tolerance => null})
truncatedDual (Point,Ideal,ZZ) := o -> (p,I,d) -> (
    depVars := gens (ring I);
    L := numNoethOpsAtPoint(I,p, DependentSet => depVars, DegreeLimit => d, Tolerance => o.Tolerance);
    dualSpace(L, p)
)

zeroDimensionalDual = method(TypicalValue => DualSpace, Options => {Strategy => BM, Tolerance => null})
zeroDimensionalDual (Point,Ideal) := o -> (p,I) -> (
    depVars := gens (ring I);
    L := numNoethOpsAtPoint(I,p, DependentSet => depVars, Tolerance => o.Tolerance);
    dualSpace(L, p)
)
----------------------------------

--An object that stores the data for an ongoing iterative tuncated dual space computation
TruncDualData = new Type of MutableHashTable
initializeDualData = method(Options => {Strategy => BM})
initializeDualData (Matrix,Boolean,Number) := o -> (Igens,syl,t) -> (
    H := new MutableHashTable;
    R := ring Igens;
    H.Igens = Igens;
    H.syl = syl;
    H.strategy = o.Strategy;
    H.deg = 0;
    h := symbol h;
    S := (coefficientRing R)(monoid[{h}|gens R, MonomialOrder => {Weights => (numgens R+1):1, 1, (options R).MonomialOrder}]); --projectivization of R
    h = S_0;
    H.hIgens = homogenize(sub(Igens,S), h); 
    T := if syl then S else R;
    H.Seeds = dualSpace(matrix{{1_T}},origin(T));
    H.BMmatrix = innerProduct(polySpace if syl then H.hIgens else H.Igens, H.Seeds);
    H.BMintegrals = gens H.Seeds;
    H.BMcoefs = myKernel(H.BMmatrix,Tolerance=>t);
    H.BMbasis = H.BMcoefs;
    --print(H.BMmatrix,H.BMcoefs);
    H.dBasis = H.BMintegrals * H.BMcoefs;
    if H.syl then H.dBasis = (map(R,S,{1_R} | gens R)) H.dBasis;
    new TruncDualData from H
    )
truncDualData = initializeDualData

-- advances the truncated dual computation from whatever was stored in parameters up to degree d
nextTDD = method()
nextTDD (TruncDualData,Number) := (H,t) -> nextTDD(H.deg + 1,H,t)
nextTDD (ZZ,TruncDualData,Number) := (d,H,t) -> (
    R := ring H.Igens;
    S := ring H.hIgens;
    if H.strategy == DZ then (
	Rd := polySpace basis(0,d,R);
	Id := idealBasis(H.Igens,d);
	H.dBasis = gens orthogonalInSubspace(Id,Rd,t);
	); 
    if H.strategy == BM then (
	dehomog := map(R,S,{1_R} | gens R);
     	for e from H.deg+1 to d do (
	    (M,E) := BMmatrix H;
	    H.BMmatrix = M; H.BMintegrals = E;
	    H.BMcoefs = myKernel(M,Tolerance=>t);
	    --print(M,H.BMcoefs);
	    I := basisIndices(last coefficients E*H.BMcoefs, t);
	    H.BMbasis = submatrix(H.BMcoefs, I);
	    H.dBasis = if H.syl then dehomog(E*H.BMcoefs) else H.dBasis | E*H.BMcoefs;
	    if numcols H.BMcoefs == 0 then break;
	    --print (e, numrows M, numcols M, numcols H.dBasis, dim reduceSpace polySpace H.dBasis);
  	    );
    	);
    H.deg = d;
    H
    )
    
polySpace TruncDualData := o-> H -> polySpace(H.dBasis, Reduced=>false)
dualSpace (TruncDualData,Point) := (H,p) -> dualSpace(polySpace H,p)
homogPolySpace = method()
homogPolySpace TruncDualData := H -> polySpace(H.BMintegrals*H.BMcoefs) 


-- this version gets a piece of the eliminating DS from the "usual" truncated DS 
eliminatingDual = method(Options => {Tolerance => null})
eliminatingDual (Point,Ideal,List,ZZ) := o -> (p,I,ind,d) -> eliminatingDual (p,gens I,ind,d,o)
eliminatingDual (Point,Matrix,List,ZZ) := o -> (p,Igens,ind,d) -> (
    R := ring Igens;
    if d < 0 then return dualSpace(map(R^1,R^0,0),p);
    t := getTolerance(R,o);
    Igens = shiftGens(p,Igens);
    n := numgens R;
    if not all(ind, i->class i === ZZ) or not all(ind, i -> i>=0 and i<n)
    then error ("expected a list of nonnegative integers in the range [0," | n | "] as 2nd parameter");
    TDD := initializeDualData(Igens,false,t);
    RdBasis := dualSpace(TDD,p);
    dBold := 0;
    while dBold != dim RdBasis do (
	dBold = dim RdBasis;
	TDD = nextTDD(TDD,t);
	RdBasis = truncate(dualSpace(TDD,p),ind,d);
	--print(dualSpace(TDD,p),RdBasis);
	);
    RdBasis
    )

truncate (PolySpace, ZZ) := (L,d) -> (
    R := ring L;
    if not L.Reduced then L = reduceSpace L;
    tGens := select(flatten entries gens L, q -> first degree q <= d);
    if #tGens == 0 then polySpace map(R^1,R^0,0) else polySpace matrix{tGens}
    )
truncate (DualSpace, ZZ) := (L,d) -> dualSpace(truncate(L.Space,d),L.BasePoint)
truncate (PolySpace, List, ZZ) := (L,ind,d) -> (
    R := ring L;
    n := numgens R;
    if not all(ind, i->class i === ZZ) or not all(ind, i -> i>=0 and i<n)
    then error ("expected a list of nonnegative integers in the range [0," | n | "] as 2nd parameter");
    indC := flatten entries submatrix'(matrix{{0..n-1}},ind);
    T := newRing(R,MonomialOrder=>{#ind,#indC},Degrees=>{(#ind:{1,0})|(#indC:{0,1})});
    TtoR := map(R,T, (vars R)_ind | (vars R)_indC);
    varList := new MutableList from n:0;
    scan(#ind, i->(varList#(ind#i) = T_i));
    scan(#indC, i->(varList#(indC#i) = T_(#ind + i)));
    RtoT := map(T,R, matrix{toList varList});
    TL := reduceSpace polySpace RtoT gens L;
    TL = truncate(TL,d);
    polySpace(TtoR gens TL)
    )
truncate (DualSpace, List, ZZ) := (L,ind,d) -> dualSpace(truncate(L.Space,ind,d),L.BasePoint)

gCorners = method(TypicalValue => Matrix, Options => {Strategy => BM, Tolerance => null, ProduceSB => false})
gCorners (Point,Ideal) := o -> (p,I) -> gCorners(p,gens I,o)
gCorners (Point,Matrix) := o -> (p,Igens) -> (
    R := ring Igens;
    t := getTolerance(R,o);
    Igens = sub(Igens, matrix{gens R + apply(p.Coordinates,c->sub(c,R))});

    ecart := max apply(flatten entries Igens, g->(gDegree g - lDegree g)); --max ecart of generators
    GCs := {}; -- g-corners (as pairs: monomial, degree in homogenization)
    SBs := {}; -- standard basis elements (if o.ProduceSB)
    finalDegree := max(flatten entries Igens / gDegree);
    d := 0;
    dBasis := dBasisReduced := polySpace map(R^1,R^0,0); -- Sylvester truncated dual space
    TDD := initializeDualData(Igens,true,t,Strategy=>o.Strategy); -- initial parameters for computing truncated duals
    while d <= finalDegree do (
    	TDD = nextTDD(d,TDD,t);
	dBasis = polySpace TDD;
	dBasisReduced = reduceSpace(dBasis,Tolerance=>t);
	-- Find new g-corners based on what monomials are missing from dBasis.
	newGCs := newGCorners(dBasisReduced,GCs,d,ecart);
	GCs = GCs|newGCs;
	-- If o.ProduceSB then compute a standard basis element for each new g-corner.
	if o.ProduceSB and #newGCs > 0 then SBs = SBs|newSBasis(dBasis,newGCs,d,t);
	-- Update stopping degree if there were new g-corners found.
	if #newGCs > 0 then (
	    topLCMdegree := max apply(subsets(#GCs,2),s->(
		    (a,b) := (GCs#(s#0), GCs#(s#1));
		    tdegree := max(a#1 - first degree a#0, b#1 - first degree b#0);
    		    (first degree mylcm {a#0,b#0}) + tdegree
		    ));
	    finalDegree = max(finalDegree,topLCMdegree);
	    );
	if debugLevel >= 1 then << "-" << "- at degree " << d << ": dim " << dim dBasisReduced << ", new corners " << newGCs/first << endl;
	--print(d, finalDegree, dim dBasisReduced, newGCs/first);
	d = d+1;
	);
    GCs = if o.ProduceSB then SBs else GCs/first;
    sbReduce matrix {GCs}
    )

mylcm = L -> (
    k := #(first exponents L#0);
    exps := apply(k, i->max apply(L, l->(first exponents l)#i));
    (ring L#0)_exps
    )

-- computes s-corners from the g-corners
-- i.e. the maximal monomials not in the ideal generated by the g-corners
socles = method(TypicalValue => Matrix)
socles MonomialIdeal := I -> socles gens I
socles Matrix := gCs -> (
    R := ring gCs;
    n := numgens R;
    G := flatten entries gCs;
    candidates := subsets(G, n) / mylcm;
    S := select(candidates, c -> (
	    c != 0 and
	    all(G, g -> not isDivisible(c,g)) and
	    all(gens R, v -> any(G, g -> isDivisible(v*c,g)))
	    ));
    S = unique S;
    matrix{S}
    )
sCorners = socles

hilbertFunction DualSpace := L -> (
    if not L.Space.Reduced then L = reduceSpace L;
    tally(flatten entries gens L / first @@ degree)
    )
hilbertFunction(List,DualSpace) := (LL,L) -> (
    h := hilbertFunction L;
    apply(LL, d->(if h#?d then h#d else 0))
    )
hilbertFunction(ZZ,DualSpace) := (d,L) -> first hilbertFunction({d},L)
-- hilbertFunction(SetOfNoethOps) := N -> hilbertFunction(dualSpace N)

localHilbertRegularity = method(TypicalValue => ZZ, Options=>{Tolerance => null})
localHilbertRegularity(Point, Ideal) := o -> (p,I) -> localHilbertRegularity(p,gens I,o)
localHilbertRegularity(Point, Matrix) := o -> (p,Igens) -> (
    n := numgens ring Igens;
    gCs := gCorners(p,Igens,o);
    gCLists := apply(flatten entries gCs, l -> (listForm l)#0#0);
    LCMexp := apply(n, i -> max(apply(gCLists, l->l#i)));
    s := max{sum LCMexp - n + 1, 0}; -- an upperbound
    J := ideal gCs;
    HP := hilbertPolynomial(J, Projective=>false);
    while hilbertFunction(s-1,J) == sub(HP,(ring HP)_0 => s-1) do s = s-1;
    s
    )
     

-- Implementation of algorithm from 1996 paper of Bernard Mourrain.
-- M is the main matrix
-- E is the row matrix of all dual basis integrals (including 1)
-- B contains the most recently found generators (as coefficients in terms of E)
BMmatrix = H -> (
    Igens := if H.syl then H.hIgens else H.Igens;
    (M,E,B,Bfull,homogeneous,Seeds) := (H.BMmatrix,H.BMintegrals,H.BMbasis,H.BMcoefs,H.syl,H.Seeds);
    --print(M,E,(numcols B, numrows B),(numcols Bfull, numrows Bfull));
    R := ring Igens;
    n := numgens R;
    m := numcols Igens;
    snew := numcols B;
    offset := if homogeneous then 0 else dim Seeds;
    s := (numcols E - offset)//n; --number of dual space generators
    npairs := subsets(n,2);
    newMEs := apply(snew, i -> (
	    bcol := B_{i};
	    bpoly := (E*bcol)_(0,0);
	    E' := matrix {apply(n, k->(
			subs := matrix{apply(n, l->(if l > k then 0_R else (gens R)#l))};
			(gens R)#k * sub(bpoly,subs)))};
	    M' := innerProduct(polySpace Igens, polySpace E');
	    if not homogeneous then M' = map(R^(s+(numcols Bfull)),R^n,0) || M';
	    for j from 0 to s-1 do (
		w := apply(n,k->(bcol_(offset + j*n + k,0)));
		v := mutableMatrix(R,#npairs,n);
		for i from 0 to #npairs-1 do (
		    v_(i,npairs#i#0) =  w#(npairs#i#1);
		    v_(i,npairs#i#1) = -w#(npairs#i#0);
		    );
		M' = M' || new Matrix from v;
		);
	    (M',E')
	    ));
    
    if not homogeneous then
    M = transpose Bfull || M || map(R^(m + s*(1+#npairs) - numrows M),R^(numcols E),0)
    else M = map(R^(m + s*#npairs),R^0,0);
    (Mnew, Enew) := (matrix{newMEs/first}, matrix{newMEs/last});
    if #newMEs == 0 then (Mnew, Enew) = (map(R^(numrows M),R^0,0),map(R^(numrows E),R^0,0));
    M = M | Mnew;
    E = if homogeneous then Enew else E | Enew;
    (M,E)
    );


newGCorners = method()
newGCorners (PolySpace,List,ZZ,ZZ) := (dBasis,GCs,d,ecart) -> (
    R := ring dBasis;
    mons := flatten entries sort basis(d-ecart,d,R);
    dBasisMons := sort(flatten entries gens dBasis / gLeadMonomial);
    newGCs := {};
    i := 0;
    for m in mons do (
	while i < #dBasisMons and m > dBasisMons#i do i = i+1;
	if i < #dBasisMons and m == dBasisMons#i then (i = i+1; continue);
	if not any(GCs, g->(isDivisible(m,g#0) and gDegree m - gDegree(g#0) <= d - g#1)) then newGCs = append(newGCs, (m,d));
	);
    newGCs
    )

newSBasis = (dBasis,newGCs,d,t) -> (
    R := ring dBasis;
    Rd := sort basis(0,d,R);
    Id := orthogonalInSubspace(dBasis, polySpace Rd, t);
    iBasis := flatten entries gens reduceSpace(Id,Monomials=>Rd);
    new List from apply(newGCs, n->first select(1,iBasis, b->(lLeadMonomial b == n#0)))
    )


-- PolySpace of ideal basis through degree d-1.
idealBasis = method(Options => true)
idealBasis(Ideal, ZZ) := true >> opts -> (I,d) -> (
    R := ring I;
    if d < 1 then return map(R^1,R^0,0);
    --B := set{};
    V := if opts.?DependentSet then opts.DependentSet else gens R;
    matrix{ unique flatten entries (gens I ** basis(0,d-1,R, Variables => V))}
    --for g in flatten entries gens I do (
    --    if g == 0 then continue else
    --    B = B + set apply(flatten entries basis(0,d-1,R, Variables => V), m->m*g);
    --);
    --if #B == 0 then map(R^1,R^0,0) else matrix{toList B}
)

--lead monomial and lead monomial degree according to ordering associated with
--the ring (local) and reverse ordering (global)
lLeadMonomial = f -> leadMonomial last terms f
gLeadMonomial = f -> leadMonomial first terms f
lDegree = f -> first degree lLeadMonomial f
gDegree = f -> first degree gLeadMonomial f

--remove non-minimal standard basis elements
sbReduce = L -> (
    n:= numcols L;
    goodi := select(n, i->(
     	    all(0..n-1, j->(j == i or not isDivisible(lLeadMonomial L_(0,i), lLeadMonomial L_(0,j))))
	    ));
    L_goodi
    )

contract (PolySpace, PolySpace) := (S, T) -> (
    T = gens T;
    S = gens S;
    cols := for s in flatten entries S list transpose contract(s,T);
    matrix {cols}
    )

diff (PolySpace, PolySpace) := (S, T) -> (
    T = gens T;
    S = gens S;
    cols := for s in flatten entries S list transpose diff(s,T);
    matrix {cols}
    )



-- Matrix of inner products
-- PolySpace generators as rows, DualSpace generators as columns
innerProduct = method()
innerProduct (PolySpace, PolySpace) := (S, T) -> (
    M := last coefficients(gens S | gens T);
    Svec := submatrix(M,0..dim S-1);
    Tvec := submatrix'(M,0..dim S-1);
    (transpose Svec)*Tvec
    )
innerProduct (PolySpace, DualSpace) := (S, L) -> (
    Sshift := polySpace sub(gens S, matrix{(gens ring L) + coordinates L.BasePoint});
    innerProduct(Sshift, L.Space)
    )
innerProduct (RingElement, DualSpace) := (f, L) -> innerProduct(polySpace matrix{{f}}, L)
innerProduct (PolySpace, RingElement) := (S, l) -> innerProduct(S, polySpace matrix{{l}})
innerProduct (RingElement, RingElement) := (f, l) -> (
    M := last coefficients(matrix{{f,l}});
    ((transpose M_{0})*M_{1})_(0,0)
    )

reduceSpace = method(Options => {Monomials => null,Tolerance=>1e-6})
reduceSpace PolySpace := o -> S -> (
    if dim S == 0 then return polySpace(gens S,Reduced=>true);
    (mons,coefs) := coefficients(gens S, Monomials => o.Monomials);
    M := mons*(colReduce(coefs,Tolerance=>o.Tolerance));
    polySpace(M,Reduced=>true)
    )
reduceSpace DualSpace := o -> L -> dualSpace(reduceSpace L.Space,L.BasePoint)

orthogonalInSubspace = method()
orthogonalInSubspace (DualSpace, PolySpace, Number) := (D,S,t) -> (
    R := ring S;
    F := coefficientRing R;
    M := sub(innerProduct(S,D),F);
    K := myKernel(transpose M,Tolerance=>t);
    polySpace((gens S)*K, Reduced=>false)
    )
orthogonalInSubspace (PolySpace, PolySpace, Number) := (T,S,t) -> (
    T' := dualSpace(T, origin(ring S));
    orthogonalInSubspace(T',S,t)
    )

isDivisible = (a, b) -> (
     dif := (listForm a)#0#0 - (listForm b)#0#0;
     all(dif, i->(i >= 0))
     )

check PolySpace := S -> (
    assert(numrows gens S == 1);
    M := last coefficients gens S;
    assert(numgens numericalImage M == numcols M);
    )
check DualSpace := D -> check polySpace gens D

---------------------------------------------
-- Numerical Linear Algebra
---------------------------------------------

numericalImage = method(Options => {Tolerance => null})
numericalImage Matrix := o -> M -> (
    R := ultimate(coefficientRing, ring M);
    tol := getTolerance(R,o);
    numericalImage(M,tol)
    )
numericalImage (Matrix, Number) := o -> (M, tol) -> (
    R := ultimate(coefficientRing, ring M);
    M = sub(M, R);
    if numcols M == 0 then return M;
    if numrows M == 0 then return map(R^0,R^0,0);
    if precision 1_(ring M) < infinity then (
	(svs, U, Vt) := SVD M;
	cols := positions(svs, sv->(sv > tol));
	submatrix(U,,cols)
	) else (
	gens image M
	)
    )

-- numSymKernel = method(Options => {Tolerance => null})
-- numSymKernel(Matrix) := Matrix => o -> M -> (
--     if precision M < infinity then colReduce(numericalKernel(M,o),o)
--     else myKernel(M)
-- )

numericalKernel = method(Options => {Tolerance => null})
numericalKernel (Matrix) := Matrix => o -> M -> (
    R := ring M;
    tol := getTolerance(R,o);
    (m,n) := (numrows M, numcols M);
    if m == 0 then return id_(source M);
    if n == 0 then return map(R^0,R^0,0);
    (S,U,Vh) := SVD M;
    cols := positions(S, sv->(sv > tol));
    K := submatrix'(transpose Vh,,cols);
    if K == 0 then K else conjugate K
    )

--performs Gaussian reduction on M
colReduce = method(Options => {Tolerance => null, Normalize => true, Reverse => false})
colReduce Matrix := o -> M -> (
    if o.Reverse then M = matrix reverse(entries M);
    tol := getTolerance(ring M,o);
    if tol == 0 then M = gens gb M
    else (
    	M = mutableMatrix sub(M, ultimate(coefficientRing, ring M));
    	(m,n) := (numrows M, numcols M);
    	j := 0; --column of pivot
    	for i in reverse(0..m-1) do (
	    if debugLevel >= 1 then <<i<<"/"<<m-1<<endl;
	    if j >= n then break;
	    a := j + maxPosition apply(j..n-1, l->(abs M_(i,l)));
	    c := M_(i,a);
	    if abs c <= tol then (for k from j to n-1 do M_(i,k) = 0; continue);
	    columnSwap(M,a,j);
	    if o.Normalize then (columnMult(M,j,1/c); c = 1);
	    for k from 0 to n-1 do if k != j then columnAdd(M,k,-M_(i,k)/c,j);
	    j = j+1;
	    );
    	M = (new Matrix from M)_{0..j-1};
    	if precision M < infinity then M = clean(tol,M);
	);
    if o.Reverse then M = matrix reverse(entries M);
    M
    )

--a list of column indices for a basis of the column space of M
basisIndices = (M, tol) -> (
    M = new MutableMatrix from sub(M, ultimate(coefficientRing, ring M));
    (m,n) := (numrows M, numcols M);
    i := 0; --row of pivot
    I := new MutableList;
    for j from 0 to n-1 do (
	if i == m then break;
	a := i + maxPosition apply(i..m-1, l->(abs M_(l,j)));
	c := M_(a,j);
	if abs c <= tol then continue;
	I#(#I) = j;
	rowSwap(M,a,i);
	for l from 0 to n-1 do M_(i,l) = M_(i,l)/c; --rowMult(M,i,1/c); is bugged
	for k from 0 to m-1 do rowAdd(M,k,-M_(k,j),i);
	i = i+1;
	);
    new List from I
    )


--
-----  Noetherian operator code
--
memoRing = memoize( (R,diffVars) -> R(monoid[diffVars]))
diffAlg = method()
diffAlg(Ring) := R -> (
    diffVars := apply(gens R, i -> value("symbol d" | toString(i)) );
    memoRing(R,diffVars)
)
diffAlg(Ideal,Ring) := (P,R) -> (
    diffVars := apply(gens R, i -> value("symbol d" | toString(i)) );
    (R/P)(monoid[diffVars])
)

-- DiffOp is a type of hash table, which contains one key, Op. This is to make inheritance with NoethOp work.
-- The value of Op is a HashTable, with keys corresponding to partial monomials, 
--  and values corresponding to coefficients.
-- Constructors
DiffOp = new Type of HashTable
DiffOp.synonym = "differential operator"
new DiffOp from HashTable := (DD,H) -> (
    if #set(keys H / ring) > 1 then error"expected all elements in same ring";
    if not all(keys H, m -> monomials m == m) then error"keys must be pure monomials";
    H
)
new DiffOp from List := (DD,L) -> new DiffOp from hashTable L
diffOp = method()
diffOp DiffOp := D -> diffOp new HashTable from D
diffOp HashTable := H -> (
    H' := select(H, f -> f!= 0);
    if #keys H' == 0 then new ZeroDiffOp 
    else new DiffOp from H'
)
diffOp List := L -> diffOp hashTable L
-- Create DiffOp from Weyl algebra element. 
-- Output will be in ring R and R must contain the non
diffOp (RingElement, Ring) := (f,R) -> (
    R' := ring f;
    createDpairs R';
    (mon,coef) := coefficients(f, Variables => R'.dpairVars#1);
    -- Create the map from R' to R that maps x => x and dx => x
    rules := apply(R'.dpairVars#1, R'.dpairVars#0, (dx, x) -> dx => sub(x,R)) | apply(R'.dpairVars#0, x -> x => sub(x,R));
    liftMap := map(R,R', rules);
    diffOp apply(flatten entries liftMap(mon), flatten entries liftMap(coef), identity)
)
diffOp RingElement := f -> (
    R' := ring f;
    if not R'.?cache then R'.cache = new CacheTable;
    -- TODO maybe makeWA should create this key?
    if not R'.cache#?"preWA" then (
        createDpairs R';
        R'.cache#"preWA" = (coefficientRing R')(monoid[(R'.dpairVars#0)]);
    );
    R := R'.cache#"preWA";
    diffOp(f, R)
)



-- Vector space operations
DiffOp + DiffOp := (D1, D2) -> diffOp merge(D1, D2, (a,b) -> a+b)
RingElement * DiffOp := (r, D) -> diffOp applyValues(D, x -> r*x)
Number * DiffOp := (r, D) -> diffOp applyValues(D, x -> r*x)
DiffOp - DiffOp := (D1, D2) -> D1 + (-1)*D2
- DiffOp := D -> (-1)*D
-- Application of DiffOp
DiffOp RingElement := (D, f) -> keys D / (k -> (D)#k * diff(k, f)) // sum
-- Comparison
DiffOp ? DiffOp := (D1, D2) -> (
    --error"dbg";
    if instance(D1 - D2, ZeroDiffOp) then return symbol ==;
    m := max keys(D1 - D2);
    if not D2#?m then symbol >
    else if not D1#?m then symbol <
    else (D1#m) ? (D2#m)
)
DiffOp == DiffOp := (D1, D2) -> return (D1 ? D2) === (symbol ==)
-- Printing
-- Takes a monomial and returns an expression with
-- a "d" appended to each variable name
addDsymbol = x -> (
    R := ring x;
    e := first exponents x;
    product apply(numgens R, i -> (expression("d" | toString R_i))^(expression(e#i)))
)
expression DiffOp := D -> 
    rsort(keys D, MonomialOrder => Lex) / 
    (k -> (D)#k * if k == 1 then expression(1) else addDsymbol(k)) //
    sum
net DiffOp := D -> net expression D
toString DiffOp := D -> toString expression D
--tex TODO
-- other useful functions
ring DiffOp := D -> ring first keys D
substitute (DiffOp, Ring) := (D,R) -> applyPairs(D, (k,v) -> sub(k, R) => sub(v,R))
--right R action TODO

-- instances of ZeroDiffOp are differential operators that
-- act as the zero operator
ZeroDiffOp = new Type of DiffOp
new ZeroDiffOp := (DD) -> hashTable{}
ZeroDiffOp RingElement := (D,f) -> 0_(ring f)
toExternalString ZeroDiffOp := D -> "new ZeroDiffOp"
-- maybe ZeroDiffOp should have a ring? TODO
ring ZeroDiffOp := D -> error"the zero operator has no ring";
-- new ZeroDiffOp from Thing := (DD, x) -> error"not implemented"
-- new ZeroDiffOp of Thing from Thing := (DD, TT, x) -> error"not implemented"
-- new ZeroDiffOp of Thing := (DD, TT) -> error"not implemented"

-- Type used for interpolated differential operators
-- As in DiffOp, each key corresponds to a monomial,
-- but each value is the numerator and denominator of a rational function
InterpolatedDiffOp = new Type of DiffOp
new InterpolatedDiffOp from List := (TT, L) -> hashTable L
expression InterpolatedDiffOp := D -> 
    rsort(keys D, MonomialOrder => Lex) / 
    (k -> ((expression D#k#0)/(expression D#k#1)) * if k == 1 then expression(1) else addDsymbol(k)) //
    sum
net InterpolatedDiffOp := D -> net expression D
evaluate (InterpolatedDiffOp, Matrix) := (D, p) -> diffOp(applyValues(D, (n,d) -> 
    promote((evaluate(matrix{{n}},p))_(0,0)/(evaluate(matrix{{d}},p))_(0,0), ring D)))
evaluate (InterpolatedDiffOp, Point) := (D, p) -> evaluate(D, matrix p)


/// TEST
--DiffOp
R = QQ[x,y,z]
foo = diffOp{x => y, y=>2*x}
bar = diffOp{x^2 => z*x + 3, y => x}
foobar = diffOp{x => y, y=>3*x, x^2 => z*x + 3}
foo2 = diffOp{x^2*y => x}
foo3 = diffOp{1_R => 0}
assert(foobar == foo + bar)
assert(foo(x^2) == 2*x*y)
assert(foo3 - foo == diffOp{x => -y, 1_R => 0, y => -2*x})
assert(foo2 > foo)
assert(instance(foo3, ZeroDiffOp))
assert(try diffOp{x+y => x} then false else true)
assert(try diffOp{2*y*x^2 => x+z} then false else true)
-- needsPackage "Dmodules"
R' = makeWA(R)
wa = diffOp(x^2*dx - dx^2 + dy^3 + (x-3)*dx)
use ring wa
dop = diffOp({x => x^2+x-3, x^2 => -1, y^3 => 1})
assert(dop == wa)
-- InterpolatedDiffOp
a = new InterpolatedDiffOp from {x => (x^2+y, y^2+x), y^2*x => (z+2, x^2+z^3*x)}
assert((evaluate(a, point{{1,2,3}}))(x) == 3/5)
///

-- TODO fix
sanityCheck = (nops, I) -> (
    all(flatten table(nops, I_*, (N,i) -> (N i)%(N.Prime) == 0), identity)
)

myKernel = method(Options => {Tolerance => null})
myKernel Matrix := Matrix => opts -> MM -> (
    if precision MM < infinity then return colReduce(numericalKernel(MM,opts),opts);

    R := ring MM;
    M := transpose colReduce(transpose MM, Reverse=>true);
    (m,n) := (numrows M, numcols M);
    es := entries M;
    pivs := apply(m, i -> position(es#i, e -> (e != 0)));
    nonPivs := toList(0..<n) - set pivs;
    if #nonPivs == 0 then return map(R^n, R^0, 0);
    transpose matrix apply(nonPivs, j -> (
        apply(n, i -> (
		    pivRow := position(pivs, k -> i==k);
		    if pivRow =!= null then -M_(pivRow,j) / M_(pivRow,i)
            else if i == j then 1_R else 0
        ))
    ))
)

/// TEST
M = random(QQ^4,QQ^2) * random(QQ^2,QQ^4)
assert((myKernel M - gens kernel M) == 0)
///

-- dispatcher method
noetherianOperators = method(Options => true)
noetherianOperators (Ideal) := SetOfNoethOps => true >> opts -> I -> (
    strats := new HashTable from {
        "Hybrid" => hybridNoetherianOperators,
        "MacaulayMatrix" => noetherianOperatorsViaMacaulayMatrix,
        "PunctualHilbert" => getNoetherianOperatorsHilb,
    };
    strat := if opts.?Strategy then opts.Strategy else "MacaulayMatrix";
    if strats#?strat then strats#strat(I, opts) 
    else error ("expected Strategy to be one of: \"" | demark("\", \"", sort keys strats) | "\"")
)

noetherianOperators (Ideal, Ideal) := SetOfNoethOps => true >> opts -> (I,P) -> (
    strats := new HashTable from {
        "Hybrid" => hybridNoetherianOperators,
        "MacaulayMatrix" => noetherianOperatorsViaMacaulayMatrix,
        "PunctualHilbert" => getNoetherianOperatorsHilb,
    };
    strat := if opts.?Strategy then opts.Strategy else "MacaulayMatrix";
    if strats#?strat then strats#strat(I, P, opts) 
    else error ("expected Strategy to be one of: \"" | demark("\", \"", sort keys strats) | "\"")
)

-- I'm not sure what this is supposed to do.
-- Strategy => "Numerical" should probably call numNoethOpsAtPoint
-- noetherianOperators (Ideal, Matrix) := SetOfNoethOps => true >> opts -> (I, pt) -> (
--     strats := new HashTable from {
--         "Numerical" => numNoethOpsAtPoint,
--     };
--     strat := if opts.?Strategy then opts.Strategy else "Numerical";
--     if strats#?strat then strats#strat(I, pt, opts) 
--     else error ("expected Strategy to be one of: \"" | demark("\", \"", sort keys strats) | "\"")
-- 
-- )
-- noetherianOperators (Ideal, Point) := SetOfNoethOps => true >> opts -> (I, pt) -> noetherianOperators(I, matrix pt)

-- End dispatcher method


macaulayMatrixKernel := {Tolerance => 1e-6, DegreeLimit => -1} >> opts -> (I, kP) -> (
    S := ring I;
    L := 2: map(kP^1,kP^0,0);
    d := max(opts.DegreeLimit, 1);
    while true do (
        Ldim := numcols first L;
        dBasis := basis(0,d,S);
        polys := transpose idealBasis(I,d);
        M' := diff(dBasis, polys);
        M := (map(kP,S)) M';
        if debugLevel >= 1 then  <<"Cols: "<<numColumns M<<", rows: "<<numRows M<<endl;
        K := myKernel(M,Tolerance => opts.Tolerance);
        L = (K, dBasis);
        if opts.DegreeLimit >=0 or Ldim == numcols first L then break;
        d = d+1;
    );
    L
)

-- returns a list of Diff ops based on matrices M, dBasis
matrixToDiffOps = (M, dBasis) ->
    transpose entries M / 
        (c -> apply(flatten entries dBasis, c, identity)) /
        diffOp //
        sort



--noetherianOperatorsViaMacaulayMatrix = method(Options => {DegreeLimit => -1, DependentSet => null}) 
noetherianOperatorsViaMacaulayMatrix = method(Options => true) 
noetherianOperatorsViaMacaulayMatrix (Ideal, Ideal) := List => true >> opts -> (I, P) -> (
    R := ring I;
    m := if opts.?DegreeLimit then opts.DegreeLimit else -1;
    -- if m < 0 and precision R == infinity and P == radical I then (
    --     if debugLevel > 0 then <<"Precomputing Noetherian operator degree limit: ";
    --     m = 1;
    --     while not isSubset(P^m, I) do m = m+1;
    --     m=m-1;
    --     if debugLevel > 0 then <<m<<endl;
    -- );
    -- TODO

    t := getTolerance(R,opts);
    (depVars,indVars) := getDepIndVars(P, opts);
    -- use the original coefficient field if appropriate, else a rational function field
    F := if #indVars == 0 then coefficientRing R else frac((coefficientRing R)(monoid[indVars]));
    S := F(monoid[depVars]);
    PS := sub(P,S); IS := sub(I,S);
    -- extend the field only if the point is not specified to be rational
    kP := if opts.?Rational and opts.Rational then F else toField(S/PS);

    L := macaulayMatrixKernel(IS, kP, Tolerance => t, DegreeLimit => m);
    -- Clear denominators, create list of DiffOps
    matrixToDiffOps(liftColumns(lift(first L,S), R), sub(last L,R))
)

/// TEST
R = QQ[x,y,t]
I = ideal(x^2, y^2 - t*x)
noetherianOperatorsViaMacaulayMatrix(I)
///






getDepIndVars = true >> opts -> P -> (
    depVars := if not opts.?DependentSet then (
	   gens(ring P) - set support first independentSets P
	) else opts.DependentSet;
    indVars := gens(ring P) - set depVars;
    (depVars,indVars)
)


noetherianOperatorsViaMacaulayMatrix (Ideal) := List => true >> opts -> (I) -> noetherianOperatorsViaMacaulayMatrix(I, ideal gens radical I, opts)
noetherianOperatorsViaMacaulayMatrix (Ideal, Point) := List => true >> opts -> (I, p) -> (
    P := ideal ((gens ring I) - p.Coordinates);
    noetherianOperatorsViaMacaulayMatrix(I,P,opts)
)


-- Clears denominators of a matrix:
-- multiplies each column of M by the lcm of the denominators
liftColumns = (M,R') -> (
    if instance(ultimate(coefficientRing, ring M), InexactField) then return sub(M,R');
	cols := transpose entries M;
    lcms := cols / (col -> (
    	col / 
    	flatten @@ entries @@ last @@ coefficients // 
    	flatten / 
    	(c -> lift(c, coefficientRing ring c)) /
    	denominator //
    	lcm
    	));
    K :=transpose matrix apply(cols, lcms, (c, m) -> c / times_m);
    sub(K, R')
)

--numNoethOpsAtPoint = method(Options => options noetherianOperatorsViaMacaulayMatrix ++ options approxKer)
numNoethOpsAtPoint = method(Options => true)
numNoethOpsAtPoint (Ideal, Point) := List => true >> opts -> (I, p) -> numNoethOpsAtPoint(I, matrix p, opts)
numNoethOpsAtPoint (Ideal, Matrix) := List => true >> opts -> (I, p) -> (
    tol := if not opts.?Tolerance then defaultT(ring I) else opts.Tolerance;
    degLim := if not opts.?DegreeLimit then -1 else opts.DegreeLimit;
    -- if point is not in the correct ring, try to promote
    p = promote(p, coefficientRing ring I);
    R := ring I;
    (depVars,indVars) := getDepIndVars(I,opts);
    S := (coefficientRing R)(monoid[depVars]);
    subs := matrix{apply(numgens R, i->(
        if member(R_i,depVars) then R_i else p_(0,i)
        ))};
    RtoS := map(S,R,sub(subs,S));
    P := sub(ideal(subs - p),S);
    L := macaulayMatrixKernel(RtoS I, coefficientRing S, DegreeLimit => degLim, Tolerance => tol);
    matrixToDiffOps(promote(first L, R), sub(last L, R))
)
/// TEST
R = CC[x,y,t]
I = ideal(x^2, y^2 - t*x)
p = point{{0_CC,0,3}}
nops = numNoethOpsAtPoint(I, p, DependentSet => {x,y})
assert(all(nops, op -> abs((evaluate(matrix{{op(-t*x^3)}}, p))_(0,0)) < 1e-6))
///

hybridNoetherianOperators = method(Options => true)
hybridNoetherianOperators (Ideal, Ideal, Matrix) := SetOfNoethOps => true >> opts -> (I,P, pt) -> (
    R := ring I;
    (depVars,indVars) := getDepIndVars(P,opts);
    S := (frac((coefficientRing R)(monoid[indVars])))(monoid[depVars]);
    PS := sub(P, S);
    IS := sub(I,S);
    kP := toField(S/PS);
    -- TODO: this precision should be specifiable
    RCC := CC monoid R;
    nopsAtPoint := numNoethOpsAtPoint(sub(I,RCC), pt, opts, DependentSet => depVars / (i->sub(i,RCC)));
    sort flatten for op in nopsAtPoint list (
        dBasis := sub(matrix{keys op / (m -> R_(first exponents m))}, S);
        maxdeg := flatten entries dBasis / sum @@ degree // max;
        K := dBasis;
        for d from 0 to maxdeg - 1 do (
            if debugLevel >= 1 then <<"hybridNoetherianOperators: trying degree "<<d<<" multiples of generators"<<endl;
            G := transpose (gens IS ** basis(0,d,S));
            M := sub(diff(dBasis, G), kP);
            K = myKernel M;
            if numColumns K == 1 then break;
        );
        -- Clear denominators and return a DiffOp
        first matrixToDiffOps(liftColumns(lift(K, S), R), sub(dBasis, R))
        --diffOp apply(flatten entries dBasis, flatten entries KK, (m, f) -> sub(m,R) => f)
    )
)
hybridNoetherianOperators (Ideal, Ideal, Point) := SetOfNoethOps => true >> opts -> (I,P, pt) -> hybridNoetherianOperators(I,P, matrix pt, opts)

hybridNoetherianOperators (Ideal, Ideal) := SetOfNoethOps => true >> opts -> (I,P) -> (
    f := if opts.?Sampler then opts.Sampler else J -> first bertiniSample(1,first components bertiniPosDimSolve(J));
    hybridNoetherianOperators(I,P,f P, opts)
)

hybridNoetherianOperators (Ideal) := SetOfNoethOps => true >> opts -> I -> hybridNoetherianOperators(I, radical I, opts)

/// TEST
R = QQ[x,y,t]
I = ideal(x^2, y^2 - t*x)
a = hybridNoetherianOperators(I, Sampler => i -> point{{0_CC,0,3}})
b = noetherianOperators I
assert(all(a,b, (a,b) -> a==b))
///


--numericalNoetherianOperators = method(Options => {
--    Tolerance => 1e-6,
--    InterpolationTolerance => 1e-6,
--    InterpolationDegreeLimit => -1,
--    NoetherianDegreeLimit => 5,
--    DependentSet => null})
numericalNoetherianOperators = method(Options => true)
-- option TrustedPoint is a point with the "correct" dx-support
-- option Sampler is a function f(n, I) which computes a list of n distinct points on the variety of I
numericalNoetherianOperators(Ideal) := List => true >> opts -> (I) -> (
    tol := if not opts.?Tolerance then defaultT(CC) else opts.Tolerance;
    sampler := if opts.?Sampler then opts.Sampler else (
        ws := first components bertiniPosDimSolve(I);
        (n,I) -> bertiniSample(n,ws)
    );
    goodPoint := if opts.?TrustedPoint then point(opts.TrustedPoint) else first sampler(1,I);
    S := ring I;
    depSet := if not opts.?DependentSet then error"expected option DependentSet"
            else opts.DependentSet;
    indSet := gens S - set depSet;
    noethDegLim := if not opts.?NoetherianDegreeLimit then -1 else opts.NoetherianDegreeLimit;
    -- other valid options: InterpolationDegreeLimit, InterpolationTolerance
    R := CC monoid S;
    J := sub(I,R);



    nopsTemplate := numNoethOpsAtPoint(J, goodPoint, opts, DependentSet => depSet / (i -> sub(i,R)), Tolerance => tol, DegreeLimit => noethDegLim);
    -- TODO: cache found pointlist in ideal
    nopsTemplate / (tmpl -> interpolateFromTemplate(I, tmpl, opts, Tolerance => tol, Sampler => sampler))
)

-- if a point is given, computes the evaluated Nops
numericalNoetherianOperators(Ideal, Point) := SetOfNoethOps => true >> opts -> (I,pt) -> (
    if not opts.?DependentSet then error "expected option DependentSet";
    S := ring I;
    if ancestor(InexactField, class coefficientRing S) then numNoethOpsAtPoint(I, pt, opts)
    else if coefficientRing S === QQ then (
        -- TODO this shouldn't be hard-coded
        R := CC monoid S;
        numNoethOpsAtPoint(sub(I,R), pt, opts, DependentSet => (opts.DependentSet / (x -> sub(x, R))))
    )
    else error "expected an ideal in a polynomial ring over QQ, CC or RR"
)
numericalNoetherianOperators(Ideal, Matrix) := SetOfNoethOps => true >> opts -> (I,pt) -> numericalNoetherianOperators(I, point pt, opts)

///TEST
R = QQ[x,y,t]
I = ideal(x^2, y^2 - t*x)
p = point{{0_CC,0, 3}}
nnops = numericalNoetherianOperators(I, DependentSet => {x,y}, TrustedPoint => {{0_CC,0,12}})
enops = nnops / (op -> evaluate(op, point{{0,0,3_CC}}))
snops = numericalNoetherianOperators(I, point{{0,0,3_CC}}, DependentSet => {x,y})

S = ring first snops

enops = enops / (op -> 1/lift(op#(first sort keys op), coefficientRing ring op) * op)
enops = enops / (i -> sub(i, S))
snops = snops / (op -> 1/lift(op#(first sort keys op), coefficientRing ring op) * op)

assert(all(snops - enops, i -> all(values i, j -> abs(lift(j,coefficientRing ring first snops)) < 1e-6)))
///


interpolateFromTemplate = true >> opts -> (I, tmpl) -> (
    ptList := new List;
    opList := new List;
    -- (mon,coef) := coefficients(tmpl);
    S := ring tmpl;
    interpTol := if not opts.?InterpolationTolerance then defaultT(CC) else opts.InterpolationTolerance;
    sampler := if opts.?Sampler then opts.Sampler else (n,Q) -> first bertiniSample(n, first components bertiniPosDimSolve(Q));
    nops := keys tmpl / (m -> (
        d := 0;
        result := ("?","?");
        while(not opts.?InterpolationDegreeLimit or d <= opts.InterpolationDegreeLimit) do (
            numBasis := rsort basis(0, d, S);
            denBasis := rsort basis(0, d, S, Variables => gens S - set (opts.DependentSet / (x -> sub(x,S))));
            neededPoints := numColumns numBasis + numColumns denBasis + 1;
            if neededPoints - #ptList > 0 and debugLevel > 0 then 
                <<"Computing "<<neededPoints - #ptList<<" new specialized NOps"<<endl;
            newNops := newSpecializedNop(I, sampler, tmpl, neededPoints - #ptList, opts, Tolerance => opts.Tolerance);
            opList = opList | newNops#0;
            ptList = ptList | newNops#1;
            
            --liftedCoeffs := take(opList, neededPoints)  /
            --    (op -> lift(op#m, ultimate(coefficientRing, ring op)));
            liftedCoeffs := take(opList, neededPoints)  /
                (op -> lift(op#m, ultimate(coefficientRing, ring op)));
            neededPtList := take(ptList, neededPoints);
            try result = rationalInterpolation(neededPtList, liftedCoeffs, numBasis, denBasis, Tolerance => interpTol) then break
                else d = d+1;
        );
        result = result / (j -> cleanPoly(opts.Tolerance, j));
        (m => result)
    ));
    if debugLevel > 0 then <<"Done interpolating from template "<<tmpl<<endl;
    new InterpolatedDiffOp from nops
)

-- Create new specialized Noeth op at a random point using tmpl as a template
-- sampler(n,I) is a function that generates a list of n points on the variety of I
newSpecializedNop = true >> opts -> (I, sampler, tmpl,n) -> (
    if n < 1 then return {{},{}};
    --pts := bertiniSample(n,ws);
    pts := sampler(n,I);
    R := ring I;
    R' := ring tmpl;

    -- (mon,coef) := coefficients tmpl;
    -- bd := (map(coefficientRing R', R', vars coefficientRing R'))(mon);
    bd := keys tmpl;
    maxdeg := bd / sum @@ degree // max;
    bx := basis(0, maxdeg-1, R');
    M := diff(matrix{bd}, transpose (sub(gens I, R') ** bx));

    opPts := for pt in pts list (
        M' := evaluate(M, pt);
        K := numericalKernel(M', Tolerance=>opts.Tolerance);
        -- If we don't get one kernel element, try again
        if numColumns K != 1 then (
            if debugLevel > 0 then <<"newSpecializedNop: bad point, trying again"<<endl;
            continue
        )
        -- else {diffOp(matrix{bd}*colReduce(K, Tolerance => opts.Tolerance))_(0,0), pt}
        else {first matrixToDiffOps(promote(colReduce(K, Tolerance => opts.Tolerance), ring tmpl), matrix{bd}), pt}
    );
    if #opPts < n then opPts = opPts | newSpecializedNop(I, sampler, tmpl, n-#opPts, opts);
    return transpose opPts;
)


-- TODO maybe not needed anymore
formatNoethOps = xs -> fold(plus,
    expression 0,
    apply(xs, x -> (expression x#0#0) / (expression x#0#1) * x#1)
)

cleanComplex = (tol, x) -> clean(tol,realPart x) + ii*clean(tol, imaginaryPart x)
cleanPoly = (tol, x) -> (
    (mon,coef) := coefficients x;
    coef = matrix applyTable(entries coef, f -> cleanComplex(tol,sub(f,CC)));
    (mon * coef)_(0,0)
)

conjugate(Matrix) := Matrix => M -> (
    matrix table(numrows M, numcols M, (i,j) -> conjugate(M_(i,j)))
)

coordinateChangeOps = method() -----TODO: fix this, now expects polynomial coefficients
coordinateChangeOps(RingElement, RingMap) := RingElement => (D, f) -> (
    R := f.target;
    WA := ring D;
    A := f.matrix // vars R;
    A' := inverse A;
    (a,b) := coefficients D;
    b = sub(f sub(b,R), WA);
    

    psi := transpose (sub(transpose A',WA) * (transpose vars WA));
    a = (map(WA,WA,psi)) a;
    (a*b)_(0,0)
)


noethOpsFromComponents = method()
noethOpsFromComponents(HashTable) := List => H -> (
    nops := flatten values H;
    R := ring first nops;
    nops = unique (nops / (f -> sub(f, R)));
    Ps := apply(nops, D -> select(keys H, P -> any(H#P, D' -> D == sub(D',ring D))));
    
    mults := Ps / (Lp -> 
        if set Lp === set keys H then 1_R else (
            J := intersect(keys H - set Lp);
            (sub(gens J,R) * random(R^(#J_*), R^1))_(0,0)
        )
    );

    apply(mults, nops, (i,j) -> i*j)
)

-- Inputs
-- pts: list of points (each point as a row matrix)
-- vals: list of values (in CC)
-- numBasis: basis for numerator (row matrix)
-- denBasis: basis for denominator (row matrix)
-- Outputs a sequence (numerator, denominator)
rationalInterpolation = method(Options => {Tolerance => 1e-6})
rationalInterpolation(List, List, Matrix, Matrix) := Sequence => opts -> (pts, vals, numBasis, denBasis) -> (
    if numColumns numBasis + numColumns denBasis > #pts + 1 then error "Rational interpolation needs more points";
    R := ring numBasis_(0,0);
    nn := numColumns numBasis;
    nd := numColumns denBasis;
    testPt := pts#0;
    pts = drop(pts,1);
    vals = drop(vals, 1);
    M := apply(pts, vals, (pt,val) -> flatten entries(evaluate(numBasis, pt) | -val * evaluate(denBasis, pt)));
    M = matrix M;
    
    M = mingleMatrix(M, nn, nd);
    ker := numericalKernel(M, Tolerance => opts.Tolerance);
    K := colReduce(ker, Tolerance=>opts.Tolerance);
    if debugLevel > 1 then print(K);
    --remove bad columns using testPt
    denIdx := select(toList(0..<nn+nd), i -> odd i and i < 2*nd);
    numIdx := toList(0..<nn+nd) - set denIdx;
    idx := positions(0..<numColumns K, i -> 
            (norm(evaluate(matrix (numBasis * K^numIdx_i), testPt)) > opts.Tolerance) and 
	    (norm(evaluate(matrix (denBasis * K^denIdx_i), testPt)) > opts.Tolerance)
        ); --TODO: double check this
    if idx === {} then error "No fitting rational function found";
    norms := apply(idx, i -> entries K_i / abs // sum);
    -- minNorm := min(norms);
    -- minPos := position(norms, i -> abs(i - minNorm) < opts.Tolerance);
    K = unmingleVector(K_(idx#(minPosition(norms))), nn, nd);

    ((numBasis * K^{0..(nn - 1)})_(0,0), (denBasis * K^{nn .. (nn+nd-1)})_(0,0))
)
rationalInterpolation(List, List, Matrix) := (RingElement, RingElement) => opts -> (pts, vals, bas) -> (
    rationalInterpolation(pts,vals,bas,bas,opts)
)
rationalInterpolation(List,List,Ring) := opts -> (pts, vals,R) -> (
    d := 0;
    local i; local b;
    while (try (print d; b = rsort basis(0,d,R); i = rationalInterpolation(pts, vals, b, opts)) then false else true) do (
        if #pts < 2*numColumns b + 1 then (print ("At least " | toString(2*numColumns b + 1) | " points needed"); error"No fitting rational function found; more points needed");
        d = d+1;
    );
    i
)

mingleMatrix = (M, nn, nd) -> (
    entries M / (r -> mingle(r_{0..<nn}, r_{nn..<(nn+nd)})) // matrix
)

-- NOTE: this assumes that nd <= nn
unmingleVector = (V, nn, nd) -> (
    l := flatten entries V;
    ht := partition(i -> odd(i) and i < 2*nd, toList(0..<nn+nd));
    transpose matrix{l_(ht#false) | l_(ht#true)}
)


testMM = (nop, mm) -> (
    (sub(nop, ring mm) % mm) == 0
)

-- Divide by gcd of coefficients
modConstant = f -> (
    (mon, coe) := coefficients f;
    g := (flatten entries coe) / (i -> sub(i,QQ));
    f // gcd(g)
)





------- Noetherian operators code with the use of punctual Hilbert schemes
--------------------------------------------------------------------------
--------------------------------------------------------------------------

--- Computes the join of two ideals
joinIdeals = (J, K) -> 
(
    v := symbol v; 
    w := symbol w;
    R := ring J;
    n := numgens R;
    T := (coefficientRing R)[v_1..v_n, w_1..w_n];
    Q := ((map(T, R, toList(v_1..v_n))) J) + ((map(T, R, toList(w_1..w_n))) K);
    S := T / Q;
    F := map(S, R, apply(n, j -> v_(j+1) + w_(j+1)));
    ker F     
) 

--- This function returns the ring we shall use to parametrize the punctual Hilbert scheme
getHilb = (P, depVars) -> (
    R := ring P;
    varsHilb := apply(depVars, i -> value("symbol h" | toString(i)) );
    S := (frac(R/P))(monoid[varsHilb]);
    S
)

mapToPunctualHilbertScheme = (Q) -> (
    R := ring Q;
    P := radical Q;
    indVars := support first independentSets P;
    depVars := gens R - set indVars;	
    S := getHilb(P, depVars);
    mapRtoHilb(Q, P, S, depVars, indVars)
)

-- This map receives an ideal Q in R=QQ[x_1..x_n] primary to a maximal ideal P
-- and it returns an ideal I in S=(frac(R/P))[y_1..y_c] which is primary with respect to (y_1..y_c).
mapRtoHilb = (Q, P, S, depVars, indVars) -> (
    R := ring Q;
    n := numgens R;        
    m := 0; -- compute the exponent that determines the order of the diff ops
    while (Q : P^m) != ideal(1_R) do m = m + 1;       
    -- map from R into the "base changed" module of principal parts
    diag :=  ideal apply(depVars, w -> value(value("symbol h" | toString(w)))_S );
    L := apply(gens R, w -> if any(indVars, z -> z == w) 
	               then sub(w, S) else sub(w, S) + value(value("symbol h" | toString(w)))_S);
    mapRtoS := map(S, R, L);
    ideal mingens ((mapRtoS Q) + diag^m)    
)
-- old
liftNoethOp = (d, R) -> (
    m := flatten entries last coefficients d /
        (c -> lift(c, coefficientRing ring c)) /
        denominator //
        lcm;
    sub(d*m, R)
)
-- old
liftCoeffs = (l, R) -> (
    m := l /
        denominator //
        lcm;
    l / (f -> sub(f*m, R))
)

liftColumnsPunctualHilbert = (M, R') -> (
    cols := transpose entries M;
    lcms := cols / (c -> c / denominator // lcm );
    K := transpose matrix apply(cols, lcms, (c,m) -> c / times_m );
    sub(K,R')
)

unpackRow = (row, FF) -> (
   (mons, coeffs) := coefficients row;
   sub(coeffs, FF)
)    

-- This function returns a set of Noetherian operators given the ideal I in the punctual Hilbert scheme
-- that parametrizes the primary ideal Q.
invSystemFromHilbToNoethOps = (I, R, S, depVars) -> (
    mm := ideal vars S; -- maximal irrelevant ideal of S
    m := 0; -- compute the exponent that determines the order of the diff ops
    if debugLevel > 0 then <<"Precomputing Noetherian operator degree limit: ";
    while (I : mm^m) != ideal(1_S) do m = m + 1;  
    if debugLevel > 0 then <<m-1<<endl;
    FF := coefficientRing S;
    L := macaulayMatrixKernel(I,FF);
    StoR := map(R, S, apply(#depVars, i -> R_(index depVars#i)));
    matrixToDiffOps(liftColumnsPunctualHilbert(first L, R), StoR last L)

    -- allMons := basis(0, m-1, S); 
    -- gensI := flatten entries mingens I;
    -- diffMat := unpackRow(diff(gensI_0, allMons), FF);
    -- for i from 1 to length gensI - 1 do (
    --     auxMat := unpackRow(diff(gensI_i, allMons), FF);
    --     diffMat = diffMat || auxMat;
    -- );
    -- --R' := diffAlg R;
    -- --T := frac(R)[gens R'];
    -- if debugLevel > 0 then <<"Cols: " << numColumns diffMat <<", rows: "<< numRows diffMat<<endl;
    -- -- ker to myKernel? TODO
    -- K := mingens ker diffMat;
    
    -- monList := flatten entries StoR allMons;

    -- transpose entries K / 
    --     (l -> apply(monList, liftCoeffs(l, R), identity )) /
    --     diffOp //
    --     sort

    -- --noethOps := flatten entries StoT (allMons * K);
    -- --noethOps / (d -> liftNoethOp(d, R'))
    -- --diffVars := apply(depVars, w -> value("symbol d" | toString(w)) );
    -- --W := FF(monoid[diffVars]);
    -- ----D := R(monoid[diffVars]);
    -- --D := diffAlg(R);
    -- --mapStoW := map(W, S, gens W);
    -- --apply(noethOps, w -> liftNoethOp(mapStoW(w), R, D))   
)
   
-- This function can compute the Noetherian operators of a primary ideal Q.
-- Here we pass first through the punctual Hilbert scheme 
getNoetherianOperatorsHilb = method(Options => true)
getNoetherianOperatorsHilb Ideal := SetOfNoethOps => true >> opts -> Q -> (
    R := ring Q;
    P := radical Q;
    indVars := support first independentSets P;
    depVars := gens R - set indVars;	
    S := getHilb(P, depVars);
    I := mapRtoHilb(Q, P, S, depVars, indVars);
    invSystemFromHilbToNoethOps(I, R, S, depVars)
)

getNoetherianOperatorsHilb (Ideal, Ideal) := SetOfNoethOps => true >> opts -> (Q,P) -> (
    if P != radical Q then error "expected second argument to be the radical of the first"
    else getNoetherianOperatorsHilb(Q,opts)
)
/// TEST
R = QQ[x,y,t]
I = ideal(x^2, y^2 - t*x)
getNoetherianOperatorsHilb(I)
-- TODO add asserts
///

-- computes the annihilator ideal of a polynomial F in a polynomial ring 
-- Input: a DiffOp. Output: a zero-dimension ideal that corresponds with the annihilator
polynomialAnn = (F') -> (
    -- change the DiffOp to a RingElement. This assumes that
    -- coefficients are in the coefficient ring.
    F := keys F' / (k -> F'#k * k) // sum;
    deg := (degree F)_0;
    S := ring F;
    allMons := basis(1, deg + 1, S);
    diffMat := diff(allMons, F);
    (mons, coeffs) := coefficients diffMat;
    ideal mingens ideal (allMons * mingens ker coeffs)        
)

-- computes the annilihator of a vector space V of polynomials
-- typically one expects that V is close under differentiation
-- Input: a list which is a basis of V. Output: the ideal annihilator.
vectorAnn = (V) -> (
    intersect(apply(V, F -> polynomialAnn(F)))    
)    
  
--- Implements the inverse procedure of Noetherian operators
--- Given a prime ideal and a set of Noetherian operators, it computes the corresponding primary ideal
--- Input: L a list of Noetherian operators (inside R[dx_1,...,dx_n]); a prime ideal P.
--- Output: The corresponding primary ideal Q 
getIdealFromNoetherianOperators = method()
getIdealFromNoetherianOperators(List, Ideal) := (L, P) -> (
    R := ring P;
    if ring first L =!= R then error "expected Noetherian operators and prime in same ring";
    indVars := support first independentSets P;
    FF := frac(R/P);
    S := FF monoid R;

    mapDiff := map(S,R, vars S);
    mapCoef := map(coefficientRing S, R);
    V := L / (op -> applyPairs(op, (a,b) -> (mapDiff a, promote(mapCoef b,S))));

    I := vectorAnn(V);
    R' := R monoid R;
    coefs := liftColumnsPunctualHilbert(lift(last coefficients gens I, coefficientRing S), R);
    mons := (map(R', S, vars R'))(first coefficients gens I);
    I' := ideal(mons * coefs);

    X := R'/(I'+P);
    Lmap := apply(numgens R, i -> R_i => promote(R_i, R') + R'_i);
    mapRtoX := map(X, R, Lmap);
    Q := ker mapRtoX;
    for v in indVars do 
      Q = saturate(Q, ideal(v));
    Q
    --R' := ring L_0;
    --if not R' === diffAlg(R) then error "noetherian operators must be in the diffAlg of the prime";
    --S := FF[gens R];
    --V := apply(L, F -> sub(F, S));
    --I := vectorAnn(V);
    --I = I_* / (f -> liftNoethOp(f, R')) // ideal;
    
    --X := R'/(I+P);
    --Lmap := apply(numgens R, i -> R_i => promote(R_i, R') + R'_i);
    --mapRtoX := map(X, R, Lmap);
    --Q := ker mapRtoX;
    --for v in indVars do 
    --  Q = saturate(Q, ideal(v));
    --Q
)
/// TEST
R = QQ[x,y,t]
I = ideal(x^2, y^2 - t*x)
L = getNoetherianOperatorsHilb(I)
P = radical I
getIdealFromNoetherianOperators(L,P)
-- TODO add asserts
///


getIdealFromNoetherianOperators(SetOfNoethOps) := N -> (
    if not N.?Prime then error"expected symbolic Noetherian operators";
    getIdealFromNoetherianOperators(N.Ops, N.Prime)
)

 
----------------------------------------------------------




beginDocumentation()

doc ///
     Key
     	  NoetherianOperators
     Headline
     	  numerically compute local dual space and Hilbert functions
     Description
     	  Text
	       The @EM "NoetherianOperators"@ package includes algorithms for computing local dual 
	       spaces of polynomial ideals, and related local combinatorial data about its scheme structure.  These 
	       techniques are numerically stable, and can be used with floating point arithmetic over the complex numbers.  
	       They provide a viable alternative in this setting to purely symbolic methods such as standard bases.  
	       In particular, these methods can be used to compute initial ideals, local Hilbert functions and Hilbert regularity.

	       Methods for computing and manipulating local dual spaces:

	       @UL {
		   {TO "truncatedDual"},
		   {TO "zeroDimensionalDual"},
		   {TO "eliminatingDual"},
		   {TO "localHilbertRegularity"},
		   {TO "gCorners"},
		   {TO "socles"},
		   {TO "innerProduct"},
                   {TO "reduceSpace"}
		   }@

	       Auxiliary numerical linear algebra methods:

	       @UL {
		   {TO "numericalKernel"},
		   {TO "numericalImage"},
		   {TO "colReduce"},
		   }@

	       The algorithm used for computing truncated dual spaces is that of B. Mourrain ("Isolated points, duality and residues." 
	       J. Pure Appl. Algebra, 117/118:469–493, 1997).  To compute the initial ideal and Hilbert regularity of positive dimensional
	       ideals we use the algorithm of R. Krone ("Numerical algorithms for dual bases of positive-dimensional ideals." Journal of
               Algebra and Its Applications, 12(06):1350018, 2013.).  This package depends on the package @TO NAGtypes@.
///


doc ///
     Key
          truncatedDual
	  (truncatedDual,Point,Ideal,ZZ)
     Headline
          truncated dual space of a polynomial ideal
     Usage
          S = truncatedDual(p, I, d)
     Inputs
     	  p:Point
	  I:Ideal
	  d:ZZ
     Outputs
          S:DualSpace
     Description
          Text
	       Computes a basis for the local dual space of a polynomial ideal localized at point p, truncated at degree d.
	       Elements are expressed as elements of the polynomial ring of the ideal although this is an abuse of notation.
	       They are really elements of the dual ring.
	  Example
	       R = CC[x,y];
	       I = ideal{x^2, y*x}
	       truncatedDual(origin(R),I,3)
	  Text
	       The functionals in the dual at a point p are expressed in coordinates centered at p.
	  Example
	       p = point matrix{{0_CC, 1_CC}}
	       truncatedDual(p,I,3)
	  Text
	       Over inexact fields, the computation accounts for the possibility of small numerical error in the point p.
	       The optional argument @TO "Tolerance (NumericalHilbert)"@ can be set to adjust the tolerance of the numerical computations.
	       Higher degree dual computations generally require higher accuracy in the input and larger tolerance value to complete correctly.
	       
	       In this example, the point q is slightly away from the variety of I, but an appropriate @TT "Tolerance"@ value can overcome the error. 
	  Example
	       q = point matrix{{0_CC + 1e-10, 1_CC}}
	       tol = 1e-6;
	       S = truncatedDual(q,I,3, Tolerance => tol)
	       (m,c) = coefficients gens S;
	       m*clean(tol, c)
	  Text
	       See also @TO zeroDimensionalDual@.
///

///
R = CC[x,y]
I1 = ideal{x^2,x*y}
D1 = truncatedDual(origin R, I1, 4)
assert(hilbertFunction({0,1,2,3,4}, D1) == {1,2,1,1,1})
///

doc ///
     Key
          zeroDimensionalDual
	  (zeroDimensionalDual,Point,Ideal)
     Headline
          dual space of a zero-dimensional polynomial ideal
     Usage
          S = zeroDimensionalDual(p, I)
     Inputs
     	  p:Point
	  I:Ideal
     Outputs
          S:DualSpace
     Description
          Text
	       Computes a reduced basis of the dual space of a zero-dimensional ideal.  It does not check if the ideal is
	       zero-dimensional and if not then termination will fail.
	       Elements are expressed as elements of the polynomial ring of the ideal although this is an abuse of notation.
	       They are really elements of the dual ring.
	  Example
	       R = QQ[a,b];
	       I = ideal{a^3,b^3}
	       D = zeroDimensionalDual(origin(R), I)
	       dim D
	  Text
	       The dimension of the dual space at p is the multiplicity of the solution at p.
	  Example
	       S = CC[x,y];
	       J = ideal{(y-2)^2,y-x^2}
	       p = point matrix{{1.4142136_CC,2_CC}};
	       D = zeroDimensionalDual(p, J)
	       dim D
	  Text
	       See also @TO truncatedDual@.
     Caveat
	  The computation will not terminate if I is not locally zero-dimensional at the chosen point.  This is not checked.
///

TEST ///
R = CC[x,y]
I1 = ideal{x^2,x*y}
D1 = truncatedDual(origin R, I1, 4)
assert(hilbertFunction({0,1,2,3,4}, D1) == {1,2,1,1,1})
I2 = ideal{x^2,y^2}
D2 = zeroDimensionalDual(origin R, I2)
assert(hilbertFunction({0,1,2,3,4}, D2) == {1,2,1,0,0})
D2' = zeroDimensionalDual(point matrix{{1_CC,1}}, I2)
assert(dim D2' == 0)
///



///
     Key
          zeroDimensionalDual
	  (zeroDimensionalDual,Point,Ideal)
	  (zeroDimensionalDual,Point,Matrix)
     Headline
          dual space of a zero-dimensional polynomial ideal
     Usage
          S = zeroDimensionalDual(p, I)
     Inputs
     	  p:Point
	  I:Ideal
              or a one-row @TO Matrix@ of generators
     Outputs
          S:DualSpace
     Description
          Text
	       Computes a reduced basis of the dual space of a zero-dimensional ideal.  It does not check if the ideal is
	       zero-dimensional and if not then termination will fail.
	       Elements are expressed as elements of the polynomial ring of the ideal although this is an abuse of notation.
	       They are really elements of the dual ring.
	  Example
	       R = QQ[a,b];
	       I = ideal{a^3,b^3}
	       D = zeroDimensionalDual(origin(R), I)
	       dim D
	  Text
	       The dimension of the dual space at p is the multiplicity of the solution at p.
	  Example
	       S = CC[x,y];
	       J = ideal{(y-2)^2,y-x^2}
	       p = point matrix{{1.4142136,2}};
	       D = zeroDimensionalDual(p, J)
	       dim D
	  Text
	       See also @TO truncatedDual@.
     Caveat
	  The computation will not terminate if I is not locally zero-dimensional at the chosen point.  This is not checked.
///

///
R = CC[x,y]
I2 = ideal{x^2,y^2}
D2 = zeroDimensionalDual(origin R, I2)
assert(hilbertFunction({0,1,2,3,4}, D2) == {1,2,1,0,0})
D2' = zeroDimensionalDual(point matrix{{1,1}}, I2)
assert(dim D2' == 0)
///

doc ///
     Key 
          gCorners
	  (gCorners,Point,Ideal)
	  (gCorners,Point,Matrix)
	  [gCorners,ProduceSB]
	  ProduceSB
     Headline
          generators of the initial ideal of a polynomial ideal
     Usage
          G = gCorners(p, I)
     Inputs
     	  p:Point
	  I:Ideal
              or a one-row @TO Matrix@ of generators
     Outputs
          G:Matrix
	       generators of the initial ideal in a one-row matrix
     Description
          Text
	       Computes the generators of the initial ideal of an ideal, with respect to a local order.  The ring of the
	       ideal should be given a (global) monomial order and the local order will be taken to be the reverse order.
	       The point p is moved to the origin, so the monomial generators represent terms of the Taylor expansion at p.
	  Example
	       R = CC[x,y];
	       I = ideal{x^2-y^2}
	       p = point matrix{{1,1}};
	       --gCorners(p, I)
	  Text
	       If the optional argument @TT "ProduceSB"@ is set to true, the output is instead a matrix of elements of the ideal
	       with the p translated to the origin such that the lead terms generate the inital ideal, i.e. a standard basis.
	       Note that the coordinates of the standard basis elements are translated to be centered at the point p.
	  Example
	       --S = gCorners(p, I, ProduceSB=>true)
	       R = CC[x,y,z];
	       J = ideal{z*(x*y-4), x-y}
	       q = point matrix{{1.4142136, 1.4142136, 0}};
	       --gCorners(q, J, Tolerance=>1e-5)
	       --gCorners(q, J, ProduceSB=>true)
///

TEST ///
R = CC[x,y]
M = matrix {{x^2-x*y^2,x^3}}
--M = matrix {{x*y, y^2}}
p = point matrix{{0_CC,0_CC}}
q = point matrix{{0_CC,1_CC}}
--assert(numcols gCorners(p,M) == 2)
--assert(numcols gCorners(q,M) == 1)
LDZ = reduceSpace truncatedDual(p,M,5,Strategy=>DZ)
LBM = reduceSpace truncatedDual(p,M,5,Strategy=>BM)
assert(dim LDZ == dim LBM)
///

doc ///
     Key
          socles
          (socles,MonomialIdeal)
	  (socles,Matrix)
     Headline
          socle corners of a monomial ideal
     Usage
          S = socles I
	  S = socles G
     Inputs
          I:MonomialIdeal
	  G:Matrix
	       one row of g-corners
     Outputs
          S:Matrix
	       socle corners in a one-row matrix
     Description
          Text
	       Computes the maximal monomials which are not in the monomial ideal, i.e. the "socle-corners".
	  Example
	       R = CC[x,y,z];
	       I = monomialIdeal{x^2,y^2,z^2}
	       socles I
	       socles I^2
	       G = vars R
	       socles G
///

TEST ///
R = CC[x,y]
G1 = matrix{{x^2,x*y^2,y^4}}
assert(socles G1 == matrix {{x*y, y^3}})
G2 = matrix{{x*y^2,x^2*y^2,y^4}}
assert(socles G2 == matrix {{y^3}})
G3 = matrix{{x*y^2,x^2*y^2}}
assert(socles G3 == matrix {{}})
///

doc ///
     Key
          localHilbertRegularity
	  (localHilbertRegularity,Point,Matrix)
	  (localHilbertRegularity,Point,Ideal)
	  
     Headline
          regularity of the local Hilbert function of a polynomial ideal
     Usage
          d = localHilbertRegularity(p,I)
     Inputs
          p:Point
	  I:Ideal
              or a one-row @TO Matrix@ of generators
     Outputs
          d:ZZ
     Description
          Text
	       The gCorners of the ideal are computed in order to find the Hilbert polynomial, which is
	       compared to the Hilbert function to find the degree of regularity, which is the degree at
	       which the two become equal.
	  Example
	       R = CC[x,y];
	       I = ideal{x^2,x*y}
	       --d = localHilbertRegularity(origin R, I)
	       D = truncatedDual(origin R, I, 3)
	       L = hilbertFunction({0,1,2,3}, D)
	  Text
	       See also @TO gCorners@.
///


doc ///
     Key
          eliminatingDual
	  (eliminatingDual,Point,Matrix,List,ZZ)
	  (eliminatingDual,Point,Ideal,List,ZZ)
     Headline
          eliminating dual space of a polynomial ideal
     Usage
          S = eliminatingDual(p, I, v, d)
     Inputs
     	  p:Point
	  I:Ideal
              or a one-row @TO Matrix@ of generators
	  v:List
	       a list of the integers designating which variables to bound
	  d:ZZ
	       the degree bound for the designated variables
     Outputs
          S:DualSpace
     Description
          Text
	       Given a list of variable indices, compute the a basis for all dual elements
	       orthogonal to I which have total degree in the variables on the list bounded by d.
	  Example
	       R = CC[x,y];
	       I = ideal{x^2-y^3}
	       --bound the x degree to 2
	       --eliminatingDual(origin R, I, {0}, 2)
	  Text
	       This function generalizes @TO truncatedDual@ in that if v includes all the variables
	       in the ring, then its behavior is the same.
	  Example
	       0
	       --eliminatingDual(origin R, I, {0,1}, 2)
	  Text
	       See also @TO truncatedDual@.
     Caveat
	  The space of dual elements satisying the conditions is not in general of finite dimension.
	  If the dimension is infinite, this function will not terminate.  This is not checked.  To ensure
	  termination, the local dimension of I at p should not exceed the length of v, and certain genericity
	  constraints on the coordinates must be met.
///

doc ///
     Key
          innerProduct
	  (innerProduct,PolySpace,DualSpace)
	  (innerProduct,PolySpace,PolySpace)
	  (innerProduct,RingElement,DualSpace)
	  (innerProduct,PolySpace,RingElement)
	  (innerProduct,RingElement,RingElement)
     Headline
          Applies dual space functionals to polynomials
     Usage
          M = innerProduct(S, D)
     Inputs
	  S:PolySpace
	  D:DualSpace
     Outputs
          M:Matrix
	       containing the values of the generators of D applied to the generators of S
     Description
          Text
	       The dual space represents functionals from the polynomial ring to the base field.
	       Given a polySpace S with n generators f_1,...,f_n and a dualSpace D with m generators
	       p_1,...,p_m, innerProduct returns a nxm matrix M over the base field whose entries are p_j(f_i).
	  Text
	       A dual functional is applied to a polynomial by taking the standard inner product of their coefficient
	       vectors.  In other words, the functional represented by the monomial a acts on monomials in the
	       polynomial ring as a(a) = 1 and a(b) = 0 for all other monomials b.
	  Example
	       R = CC[x,y];
	       S = polySpace matrix{{x+y,2*x+y^2}};
	       D = dualSpace(matrix{{1,x,y}}, origin R);
	       M = innerProduct(S, D)
	  Text
	       @TT "innerProduct"@ can also be called with one or both inputs @ofClass RingElement@.  If both arguments
	       are single elements, the output is also a ring element rather than a matrix.
	  Example
	       innerProduct(S, 1+x)
	       innerProduct(x, D)
	       innerProduct(x, 1+x)
///

TEST ///
R = CC[x,y]
S = polySpace(basis(3,R))
P = innerProduct(S,S)
assert(all((0,0)..(3,3), i->(P_i == if i#0 == i#1 then 1 else 0)))
///

doc ///
     Key
          orthogonalInSubspace
	  (orthogonalInSubspace,DualSpace,PolySpace,Number)
	  (orthogonalInSubspace,PolySpace,PolySpace,Number)
     Headline
          Orthogonal of a space
     Usage
          S = orthogonalInSubspace(D, T, tol)
     Inputs
	  D:DualSpace
	       or @ofClass PolySpace@ a space of which to find the orthogonal
	  T:PolySpace
	       ambient space
	  tol:Number
	       a positive number, the numerical tolerance
     Outputs
          S:PolySpace
     Description
          Text
	       Computes the subspace of polynomial space T which is orthogonal to the dual space (or polynomial space) D.
	  Example
	       R = CC[x,y];
	       T = polySpace matrix{{1,x,y}};
	       D = dualSpace(matrix{{x-y}}, origin R);
	       S = orthogonalInSubspace(D, T, 1e-6)
///

doc ///
     Key
          reduceSpace
	  (reduceSpace,DualSpace)
	  (reduceSpace,PolySpace)
	  [reduceSpace,Monomials]
     Headline
          reduce the generators of a space
     Usage
          S = reduceSpace T
     Inputs
     	  T:DualSpace
	       or @ofClass PolySpace@
     Outputs
          S:DualSpace
	       or @ofClass PolySpace@
     Description
          Text
	       Reduces the generators of a DualSpace or PolySpace so that the new generators are linearly independent, and each has
	       a distinct lead monomial.  This is achieved by Gaussian reduction.
	  Example
	       R = CC[x,y];
	       T = polySpace matrix{{x,y,x-y+1e-10}}
	       S = reduceSpace T
	       S = reduceSpace(T, Tolerance=>1e-12)
///

doc ///
     Key
          (truncate,DualSpace,ZZ)
	  (truncate,PolySpace,ZZ)
     Headline
          truncate a polynomial space or dual space
     Usage
          S = truncate(T, d)
     Inputs
     	  T:DualSpace
	       or @ofClass PolySpace@
	  d:ZZ
	       the degree bound for the designated variables
     Outputs
          S:DualSpace
     Description
          Text
	       Truncates a dual space or polynomial space T so that the total degree of all terms does not exceed d.
	  Example
	       R = CC[x,y];
	       I = ideal {x-y};
	       D = truncatedDual(origin R, I, 5)
	       truncate(D, 3)
///

doc ///
     Key
	  (truncate,DualSpace,List,ZZ)
	  (truncate,PolySpace,List,ZZ)
     Headline
          truncate a polynomial space or dual space
     Usage
          S = truncate(T, v, d)
     Inputs
     	  T:DualSpace
	       or @ofClass PolySpace@
	  v:List
	       a list of the integers designating which variables to bound
	  d:ZZ
	       the degree bound for the designated variables
     Outputs
          S:DualSpace
     Description
          Text
	       Truncates a dual space or polynomial space T, so that the total degree of the specified variables is bounded by d.
	  Example
	       R = CC[x,y];
	       I = ideal {x,y};
	       D = zeroDimensionalDual(origin R, I^3)
	       --truncate the x degree to 1
	       truncate(D, {0}, 1)
///

doc ///
     Key
	  (hilbertFunction,ZZ,DualSpace)
	  (hilbertFunction,DualSpace)
	  (hilbertFunction,List,DualSpace)
     Usage
          k = hilbertFunction(d,S)
	  T = hilbertFunction(S)
	  K = hilbertFunction(D,S)
     Inputs
     	  d:
		an integer, or a @TO List@ of arguments
	  S:DualSpace
		a dual space
     Outputs
          T:Tally
	      of dimensions for each degree
	  k:ZZ
	      dimensions
	  K:List
	      of dimensions
     Description
          Text
	      counts the dimension of the dual space in each degree.  A single degree can be
	      specified, or a list of degrees.  If no degree is specified, then a Tally
	      is returned pairing each degree with its dimension.
	  Example
	       R = CC[x,y];
	       I = (ideal {x,y})^4;
	       D = zeroDimensionalDual(origin R, I)
	       hilbertFunction D
///

doc ///
     Key
          "Tolerance (NoetherianOperators)"
	  [localHilbertRegularity,Tolerance]
	  [eliminatingDual,Tolerance]
	  [gCorners,Tolerance]
	  [reduceSpace,Tolerance]
     Headline
          optional argument for numerical tolernace
     Description
          Text
	       Many of the numerical operations require a tolerance value, below which numbers are considered to be numerically zero.
	       {\tt Tolerance} should be given a non-negative real number.
	       Over exact fields the default value is zero, while for inexact fields the default is 1e-6.
	       
	       
	       
	       See also @TO Tolerance@.
///

doc ///
     Key
          "Strategy (NoetherianOperators)"
	  [gCorners,Strategy]
     Headline
          optional argument for dual space algorithm
     Description
          Text
	       The two available algorithm choices are

	       @UL {
		   {TOH "BM"},
		   {TOH "DZ"}
		   }@

	       Both should produce roughly 
	       the same output, but there may be differences in performance.  Generally @TT "BM"@ should be more efficient and is the default, 
	       but @TT "DZ"@ is left as an option to the user.

///

doc ///
     Key
	  DZ
     Headline
          Macaulay matrix algorithm for dual space computation
     Description
          Text
	       This algorithm is a strategy for computing the truncated dual space of an ideal I at degree d.
	       A matrix is formed with a column for each monomial in the ring of I of degree at most d, and a row for each
	       monomial multiple of a generator of I which has any terms of degree d or less, storing its coefficients.
	       Any vector in the kernel of this matrix is the coeffeicient vector of an element of the dual space.
	  
	       See: B.H. Dayton and Z. Zeng. Computing the multiplicity structure in solving polynomial systems. In M. Kauers,
	       editor, @EM "Proceedings of the 2005 International Symposium on Symbolic and Algebraic Computation"@, pages 116-123. ACM, 2005.
///

doc ///
     Key
	  BM
     Headline
          Mourrain algorithm for dual space computation
     Description
          Text
	       The Mourrain algorithm is a strategy for computing the truncated dual space of an ideal I at degree d.
	  
	       See: B. Mourrain. Isolated points, duality and residues. @EM "J. Pure Appl. Algebra"@, 117/118:469-493, 1997. 
	       Algorithms for algebra (Eindhoven, 1996).
///

doc ///
     Key
          numericalImage
	  (numericalImage,Matrix,Number)
	  (numericalImage,Matrix)
	  [numericalImage,Tolerance]
     Headline
          Image of a matrix
     Usage
          V = numericalImage(M, tol)
     Inputs
	  M:Matrix
	  tol:Number
	       a positive number, the numerical tolerance
     Outputs
          V:Matrix
     Description
          Text
	       Computes the image of a matrix M numerically using singular value decomposition.
	  Example
	       M = matrix {{1., 0, 1}, {0, 1, 1}, {1, 0, 1}}
	       numericalImage(M, 0.01)
	  Text
	       Singular values less than the tolerance are treated as zero.
	  Example
	       M = matrix {{0.999, 2}, {1, 2}}
	       numericalImage(M, 0.01)
///

TEST ///
M = matrix {{0.999, 2}, {1, 2}}
Mimage = numericalImage(M, 0.01)
assert(numcols Mimage == 1)
///

doc ///
     Key
          numericalKernel
	  (numericalKernel,Matrix)
	  [numericalKernel,Tolerance]
     Headline
          Kernel of a matrix
     Usage
          V = numericalKernel(M)
     Inputs
	  M:Matrix
     Outputs
          V:Matrix
     Description
          Text
	       Computes the kernel of a matrix M numerically using singular value decomposition.
	  Example
	       M = matrix {{1., 1, 1}}
	       numericalKernel(M, Tolerance=>0.01)
	  Text
	       Singular values less than the tolerance are treated as zero.
	  Example
	       M = matrix {{1., 1}, {1.001, 1}}
	       numericalKernel(M, Tolerance=>0.01)
///

doc ///
     Key
          colReduce
	  (colReduce,Matrix)
     Headline
          Column reduces a matrix
     Usage
          N = colReduce M
     Inputs
	  M:Matrix
     Outputs
          N:Matrix
	       in reduced column echelon form
     Description
          Text
	       Performs Gaussian column reduction on a matrix M, retaining only the linearly independent columns.
	  Example
	       M = matrix {{1., 2, 3}, {2, 4, 0}}
	       colReduce(M, Tolerance=>0.01) 
	  Text
	       Entries with absolute value below the tolerance are treated as zero and not used as pivots.
	  Example
	       N = matrix {{0.001, 0, 0}, {1, 1, 3}, {2, 2, 5.999}}
	       colReduce(N, Tolerance=>0.01)
///

TEST ///
N = matrix {{0.001, 0, 0}, {1, 1, 3}, {2, 2, 5.999}}
N = colReduce(N, Tolerance=>0.01)
assert(numcols N == 1)
///


doc ///
     Key
          getIdealFromNoetherianOperators
	  (getIdealFromNoetherianOperators, List, Ideal)
     Headline
          Computes a primary ideal corresponding to a list of Noetherian operators and a prime ideal
     Usage
          Q = getIdealFromNoetherianOperators(L, P)
     Inputs
	  L : List
	      a list of differential operators
	  P : Ideal
	      a prime ideal
     Outputs
          Q : Ideal
	       the primary ideal corresponding to L and P
     Description
          Text
	    This method contains an implementation of Algorithm 3.9 in @ HREF("https://arxiv.org/abs/2001.04700", "Primary ideals and their differential equations")@. 
	  
    	    Let $R$ be a polynomial ring $R = K[x_1,\ldots,x_n]$ over a field $K$ of characteristic zero. 
	    Consider the Weyl algebra $D = R<dx_1,\ldots,dx_n>$ and a list of differential operators $L = \{L_1,\ldots,L_m\} \,\subset\, D$.
	    Denote by $\mathcal{E} \,\subset\, D$ the $R$-bisubmodule of $D$ that is generated by $L_1,\ldots,L_m$.
    	    For a given prime ideal $P \,\subset\, R$, this method computes the $P$-primary ideal given as 
	    $$
	    Q = \{ f \,\in\, R\, \mid\, \delta\, \bullet\, f\, \in R \, \forall  \delta \in \mathcal{E} \}.
	    $$	    
	    
	    This method can be seen as the reverse operation of computing a set of Noetherian operators for a primary ideal.      
	    
    	    Next, we provide several examples to show the interplay between computing a set of Noetherian operators and then getting the back the original ideal.
	    
	    The first example shows an ideal that can be described with two different sets of Noetherian operators (this example appeared in Example 7.8 of @ HREF("https://arxiv.org/abs/2001.04700", "Primary ideals and their differential equations")@).
	  Example
	       R = QQ[x_1,x_2,x_3,x_4]
	       MM = matrix {{x_3,x_1,x_2},{x_1,x_2,x_4}}
	       P = minors(2,MM)
	       M = ideal{x_1^2,x_2^2,x_3^2,x_4^2} 
	       Q = joinIdeals(P,M);
               L1 = getNoetherianOperatorsHilb(Q) -- A set of Noetherian operators
	       Q1 = getIdealFromNoetherianOperators(L1.Ops, L1.Prime);
	       Q == Q1
	       L2 = getNoetherianOperatorsHilb(M) -- Another set of Noetherian operators
       	       Q2 = getIdealFromNoetherianOperators(L2.Ops, P);
	       Q == Q2
	  Text
	       The following example was given as the running example in the Introduction of @ HREF("https://arxiv.org/abs/2001.04700", "Primary ideals and their differential equations")@.
	  Example
	       Q = ideal(3*x_1^2*x_2^2-x_2^3*x_3-x_1^3*x_4-3*x_1*x_2*x_3*x_4+2*x_3^2*x_4^2,3*x_1^3*x_2*x_4-3*x_1*x_2^2*x_3*x_4-3*x_1^2*x_3*x_4^2+3*x_2*x_3^2*x_4^2+2*x_2^3-2*x_3*x_4^2,3*x_2^4*x_3-6*x_1*x_2^2*x_3*x_4+3*x_1^2*x_3*x_4^2+x_2^3-x_3*x_4^2,4*x_1*x_2^3*x_3+x_1^4*x_4-6*x_1^2*x_2*x_3*x_4-3*x_2^2*x_3^2*x_4+4*x_1*x_3^2*x_4^2,x_2^5-x_1*x_2^3*x_4-x_2^2*x_3*x_4^2+x_1*x_3*x_4^3,x_1*x_2^4-x_2^3*x_3*x_4-x_1*x_2*x_3*x_4^2+x_3^2*x_4^3,x_1^4*x_2-x_2^3*x_3^2-2*x_1^3*x_3*x_4+2*x_1*x_2*x_3^2*x_4,x_1^5-4*x_1^3*x_2*x_3+3*x_1*x_2^2*x_3^2+2*x_1^2*x_3^2*x_4-2*x_2*x_3^3*x_4,3*x_1^4*x_3*x_4-6*x_1^2*x_2*x_3^2*x_4+3*x_2^2*x_3^3*x_4+2*x_1^3*x_2+6*x_1*x_2^2*x_3-6*x_1^2*x_3*x_4-2*x_2*x_3^2*x_4,4*x_2^3*x_3^3+4*x_1^3*x_3^2*x_4-12*x_1*x_2*x_3^3*x_4+4*x_3^4*x_4^2-x_1^4+6*x_1^2*x_2*x_3+3*x_2^2*x_3^2-8*x_1*x_3^2*x_4)
      	       L = getNoetherianOperatorsHilb(Q)
	       Q' = getIdealFromNoetherianOperators(L.Ops, P);
	       Q == Q'	  
///

-------------- Noetherian operators documentation

doc ///
Key
    (noetherianOperators, Ideal)
Headline
    Noetherian operators of a primary ideal
Usage
    noetherianOperators Q
    noetherianOperators (Q, Strategy => "MacaulayMatrix")
Inputs
    Q:Ideal
        assumed to be primary
--Outputs
--Consequences
--    Item
Description
    Text
        Compute a set of Noetherian operators
    Example
        R = QQ[x,y,t];
        I = ideal(x^2, y^2-x*t);
        noetherianOperators I
    Text
        The optional argument {\tt Strategy} can be used to choose different algorithms.
        The following algorithms are supported:
        {\tt "MacaulayMatrix"}, {\tt "PunctualHilbert"}, {\tt "Hybrid"}

        @UL{
            TO2 {"MacaulayMatrix", "\"MacaulayMatrix\""},
            TO2 {"PunctualHilbert", "\"PunctualHilbert\""},
            TO2 {"Hybrid", "\"Hybrid\""},
        }@

--    CannedExample
--    Code
--    Pre
--ExampleFiles
--Contributors
--References
Caveat
    The behavior is undefined if {\tt Q} is not primary.
    For non-primary ideals, use @TO (noetherianOperators, Ideal, Ideal)@
--SeeAlso
///

doc ///
Key
    noetherianOperators
Headline
    Noetherian operators
///

doc ///
Key
    "MacaulayMatrix"
Headline
    strategy for computing Noetherian operators
///

doc ///
Key
    "Hybrid"
Headline
    strategy for computing Noetherian operators
///

doc ///
Key
    "PunctualHilbert"
Headline
    strategy for computing Noetherian operators
///
-------------- Noetherian operators tests



TEST ///
R = QQ[x,y,z]
I = ideal(x^2 - y, y^2)
nops = noetherianOperatorsViaMacaulayMatrix(I, DegreeLimit => 10)
assert(sanityCheck(nops, I))
///

TEST ///
R = QQ[x_0..x_3]
S = QQ[s,t]
I0 = ker map(S,R,{s^5,s^3*t^2, s^2*t^3, t^5})
nops = noetherianOperatorsViaMacaulayMatrix(I0, DegreeLimit => 10)
assert(sanityCheck(nops, I0))
I1 = ideal(x_0^2, x_1^2, x_2^2)
nops = noetherianOperatorsViaMacaulayMatrix(I1, DegreeLimit => 10)
assert(sanityCheck(nops,I1))
///


TEST ///
R = QQ[x,y]
I = ideal((x-1)^2,(x-1)*(y+1),(y+1)^3)
J = ideal((x)^2,(x)*(y),(y)^3)
Ps = associatedPrimes I
nopsI = noetherianOperatorsViaMacaulayMatrix(I, first Ps)
W = ring nopsI
assert(set flatten entries gens nopsI === set{W_1^2, W_0, W_1, 1_W})
nopsJ = noetherianOperatorsViaMacaulayMatrix(J, ideal gens R)
W = ring nopsJ
assert((sort flatten entries gens nopsJ) == sort{W_1^2, W_0, W_1, 1_W})
///


TEST /// -- Linear coordinate change test
R = QQ[x,y]
I = ideal(x^2*(y-x))
f = map(R,R,{2*x+y,x+y})
J = f I
NI = noetherianOperatorsViaMacaulayMatrix I
NJ = noetherianOperatorsViaMacaulayMatrix J
WI = ring first NI
WJ = ring first NJ
convertedNI = NI / (i-> sub(coordinateChangeOps(i,f), WJ))
assert sanityCheck(convertedNI,J)
assert sanityCheck(NJ,J)
assert sanityCheck(NI,I)
assert(set NI === set{1_WI, WI_0*WI_2 - WI_1*WI_2})
assert(set NJ === set{1_WJ, WJ_0*WJ_2})
assert(set convertedNI === set{1_WJ, WJ_0*WJ_2 - WJ_0*WJ_3})
///

TEST /// -- numNoethOpsAtPoint test
R = CC[x,y,t]
I = intersect(ideal(x^2-t*y, y^2), ideal(x+y+1))
nv = numericalIrreducibleDecomposition(I, Software => BERTINI)
Wsets = select(flatten values nv, x -> class x === WitnessSet )
pt = first (Wsets / sample / matrix)
l = numNoethOpsAtPoint(I, pt, Tolerance => 1e-2, DependentSet => {x,y})
assert(toString(l/terms/(m -> m/exponents)) === "{{{{0, 0, 0, 0, 0, 0}}}, {{{0, 0, 0, 1, 0, 0}}}, {{{0, 0, 0, 2, 0, 0}}, {{0, 0, 0, 0, 1, 0}}}, {{{0, 0, 0, 3, 0, 0}}, {{0, 0, 0, 1, 1, 0}}}}")
///


TEST /// -- rationalInterpolation test
R = CC[x,y]
num = (x^2 - 2*y*x) 
den = (y*x + y^2)
pts = {matrix {{.922548+.569867*ii, .668231+.918485*ii}}, matrix {{.413667+.0631326*ii,.210225+.688382*ii}}, matrix {{.129853+.649565*ii, .526889+.519065*ii}}, matrix{{.460057+.733113*ii, .642288+.571532*ii}}, matrix {{.268148+.34963*ii, .47612+.77208*ii}},matrix {{.237741+.937067*ii, .902133+.97911*ii}}, matrix {{.58456+.563844*ii,.877568+.129457*ii}}, matrix {{.323687+.586776*ii, .161867+.591524*ii}}, matrix{{.91034+.171779*ii, .913587+.517224*ii}}, matrix {{.963887+.704832*ii,.642633+.397505*ii}}, matrix {{.196713+.843673*ii, .0568273+.621807*ii}}, matrix{{.263865+.9356*ii, .3981+.858609*ii}}}
vals = pts / (p -> (evaluate(matrix num, p))_(0,0) / (evaluate(matrix den, p))_(0,0))
numBasis = sub(basis(0,2,R), R)
denBasis = sub(basis(0,2,R), R)
rationalInterpolation(pts, vals, numBasis, denBasis, Tolerance => 0.0001)
///


end


restart
debugLevel = 1
debug loadPackage("NoetherianOperators", Reload => true)
needsPackage "NumericalAlgebraicGeometry"
needsPackage "Bertini"
--installPackage("NoetherianOperators", RemakeAllDocumentation => true)
--loadPackage "NoetherianOperators"
R = QQ[x_0..x_5]
P = minors(2,matrix{{x_0,x_1,x_3,x_4},{x_1,x_2,x_4,x_5}});
f1 = x_1^4 - 2*x_0*x_1^2*x_2 + x_0^2*x_2^2 + x_1*x_2*x_3*x_4 - x_0*x_2*x_4^2 - x_1^2*x_3*x_5 + x_0*x_1*x_4*x_5
f2 = x_1^4 - 2*x_0*x_1^2*x_2 + x_0^2*x_2^2 + x_1*x_2*x_3*x_4 - x_1^2*x_4^2 - x_0*x_2*x_3*x_5 + x_0*x_1*x_4*x_5
f3 = x_2^2*x_3*x_4 - x_1*x_2*x_4^2 + x_4^4 - x_1*x_2*x_3*x_5 + x_1^2*x_4*x_5 - 2*x_3*x_4^2*x_5 + x_3^2*x_5^2
I = ideal(f1,f2,f3)
primes = minimalPrimes I
P = primes#0

apply(primes,P->elapsedTime hybridNoetherianOperators(I, P))

primes / (P -> elapsedTime hybridNoetherianOperators(I,P))

elapsedTime noetherianOperatorsViaMacaulayMatrix(I, primes#0, DegreeLimit=>4)


needsPackage "NumericalAlgebraicGeometry"


R = CC[x,y]
M = matrix {{x^2-x*y^2,x^3}}
--M = matrix {{x*y}}
--M = matrix {{x^9 - y}}
p = point matrix{{0_CC,0_CC}}
q = point matrix{{1_CC,0_CC}}
L = reduceSpace truncatedDual(M,p,6,Strategy=>DZ)
L = reduceSpace truncatedDual(M,p,6,Strategy=>BM)
--shiftDual(L,q,8)
G = matrix{{x^2,x*y^2,y^4}}
socles G

hilbertFunction(toList(0..8),L)
dualInfo(M,p,Truncate=>8)
standardBasis(M)
dualHilbert(M,Truncate=>25)
dualBasis(M)
dualInfo(M)
dualInfo(M, Strategy=>DZ)
dualInfo(M,Point=>{0.01,0.01})

-- small example
restart
loadPackage ("NoetherianOperators", Reload => true)
R = QQ[x,y,z, MonomialOrder => {Weights=>{-1,-1,-1}}, Global => false]
M = matrix {{x-y^2, y-z^2}}
RH = QQ[t,x,y,z,MonomialOrder => {Weights=>{-1,-1,-1,-1}}, Global => false]
MH = homogenize(sub(M,RH),t)	  
DH = flatten entries gens dualBasis(MH,4)
DHhat = sub(matrix{select(DH, Q->first first last listForm Q >= 1)}, {t=>1})
(
     sort flatten entries gens dualBasis(M,3)
     ) == (
     sort unique flatten entries sub(DHhat,R)
     )










 ----------------------------------------------------
-- SOME EXAMPLES -----------------------------------
----------------------------------------------------
restart
needsPackage "NoetherianOperators"

----------------------------------------------------
----------------------------------------------------
-- Example 0: Running example throughout the paper
-- We compute the ideal as explained in the introduction.
U= QQ[x_1,x_2,x_3,x_4,u_1,u_2,u_3,u_4,y_1,y_2];
A = matrix {{u_3,u_1,u_2},{u_1,u_2,u_4}};
PP = minors(2,A);
JJ=ideal{PP,x_1-u_1-y_1,x_2-u_2-y_2,x_3-u_3,x_4-u_4,y_1^3,y_2+x_2*y_1^2};
J=ideal{eliminate(JJ,{u_1,u_2,u_3,u_4,y_1,y_2})};
R=QQ[x_1,x_2,x_3,x_4];
F=map(R,U);
Q=F(J);
assert( Q == ideal(3*x_1^2*x_2^2-x_2^3*x_3-x_1^3*x_4-3*x_1*x_2*x_3*x_4+2*x_3^2*x_4^2,3*x_1^3*x_2*x_4-3*x
      _1*x_2^2*x_3*x_4-3*x_1^2*x_3*x_4^2+3*x_2*x_3^2*x_4^2+2*x_2^3-2*x_3*x_4^2,3*x_2^4*x_3-6*x_1*
      x_2^2*x_3*x_4+3*x_1^2*x_3*x_4^2+x_2^3-x_3*x_4^2,4*x_1*x_2^3*x_3+x_1^4*x_4-6*x_1^2*x_2*x_3*x
      _4-3*x_2^2*x_3^2*x_4+4*x_1*x_3^2*x_4^2,x_2^5-x_1*x_2^3*x_4-x_2^2*x_3*x_4^2+x_1*x_3*x_4^3,x_
      1*x_2^4-x_2^3*x_3*x_4-x_1*x_2*x_3*x_4^2+x_3^2*x_4^3,x_1^4*x_2-x_2^3*x_3^2-2*x_1^3*x_3*x_4+2
      *x_1*x_2*x_3^2*x_4,x_1^5-4*x_1^3*x_2*x_3+3*x_1*x_2^2*x_3^2+2*x_1^2*x_3^2*x_4-2*x_2*x_3^3*x_
      4,3*x_1^4*x_3*x_4-6*x_1^2*x_2*x_3^2*x_4+3*x_2^2*x_3^3*x_4+2*x_1^3*x_2+6*x_1*x_2^2*x_3-6*x_1
      ^2*x_3*x_4-2*x_2*x_3^2*x_4,4*x_2^3*x_3^3+4*x_1^3*x_3^2*x_4-12*x_1*x_2*x_3^3*x_4+4*x_3^4*x_4
      ^2-x_1^4+6*x_1^2*x_2*x_3+3*x_2^2*x_3^2-8*x_1*x_3^2*x_4) );
elapsedTime noetherianOperators(Q, Strategy => "MacaulayMatrix")
L = elapsedTime noetherianOperators(Q, Strategy => "PunctualHilbert")
Q' = getIdealFromNoetherianOperators(L, radical Q)
Q == Q'
----------------------------------------------------
----------------------------------------------------

----------------------------------------------------
----------------------------------------------------
-- Example 1 : Contains the computations in Example 3.10
R=QQ[x_1,x_2,x_3,x_4];
Q=ideal{x_1^2,x_1*x_2,x_1*x_3,x_1*x_4-x_3^2+x_1,x_3^2*x_4-x_2^2,x_3^2*x_4-x_3^2-x_2*x_3+2*x_1};
elapsedTime noetherianOperators(Q, Strategy => "MacaulayMatrix")
L = elapsedTime noetherianOperators(Q, Strategy => "PunctualHilbert")
Q' = getIdealFromNoetherianOperators(L, radical Q)
Q == Q'
----------------------------------------------------
----------------------------------------------------

----------------------------------------------------
----------------------------------------------------
-- Example 2 : This the Example 7.8 regarding the join construction.
R=QQ[x_1,x_2,x_3,x_4];
MM = matrix {{x_3,x_1,x_2},{x_1,x_2,x_4}};
P = minors(2,MM);
M=ideal{x_1^2,x_2^2,x_3^2,x_4^2};
--- Computes the join of two ideals
Q=joinIdeals(P,M)
elapsedTime noetherianOperators(Q, Strategy => "MacaulayMatrix")
L = elapsedTime noetherianOperators(Q, Strategy => "PunctualHilbert")
Q' = getIdealFromNoetherianOperators(L, radical Q)
Q == Q'
----------------------------------------------------
----------------------------------------------------

----------------------------------------------------
----------------------------------------------------
-- Example 3: Palamodov's example
---------------------------------------------------
R = QQ[x_1, x_2, x_3]
Q = ideal(x_1^2, x_2^2, x_1-x_2*x_3)
elapsedTime noetherianOperators(Q, Strategy => "MacaulayMatrix")
L = elapsedTime noetherianOperators(Q, Strategy => "PunctualHilbert")
Q' = getIdealFromNoetherianOperators(L, radical Q)
Q == Q'
----------------------------------------------------
----------------------------------------------------

----------------------------------------------------
----------------------------------------------------
-- Example 4: taken from page 143 of "Solving Systems of Polynomial Equations"
---------------------------------------------------
R = QQ[x_1, x_2, x_3, x_4]
Q = ideal(x_1^3*x_4^2-x_2^5, x_1^2*x_4^3-x_3^5, x_1*x_3^2-x_2^3, x_2^2*x_4 - x_3^3)
Q1 = ideal(x_1*x_4-x_2*x_3, x_1*x_3^2-x_2^3, x_2^2*x_4-x_3^3)
Q2 = ideal(x_1^2, x_2^2, x_3^2)
Q3 = ideal(x_2^2, x_3^2, x_4^2)
Q4 = ideal(x_1^3, x_2^3, x_3^3, x_4^3, x_1*x_3^2, x_2^2*x_4)
assert(Q == intersect(Q1, Q2, Q3, Q4)) -- check that we copied correctly
---- the Noetherian operators of Q1
isPrime Q1
-- since it is prime we can choose 1 as the Noetherian operator
---- the Noetherian operators of Q2 
isPrime Q2
P2 = radical Q2 -- it is equal to (x_1, x_2, x_3)
elapsedTime noetherianOperators(Q2, Strategy => "MacaulayMatrix")
L = elapsedTime noetherianOperators(Q2, Strategy => "PunctualHilbert")
Q2' = getIdealFromNoetherianOperators(L, Q2)
Q2 == Q2'
---- the Noetherian operators of Q3
isPrime Q3
P3 = radical Q3 -- it is equal to (x2, x3, x4)
elapsedTime noetherianOperators(Q3, Strategy => "MacaulayMatrix")
L = elapsedTime noetherianOperators(Q3, Strategy => "PunctualHilbert")
Q3' = getIdealFromNoetherianOperators(L, Q2)
Q3 == Q3'
---- the Noetherian operators of Q4
isPrime Q4
P4 = radical Q4 -- it is equal to (x1, x2, x3, x4)
elapsedTime noetherianOperators(Q4, Strategy => "MacaulayMatrix")
elapsedTime noetherianOperators(Q4, Strategy => "Hybrid")
L = elapsedTime noetherianOperators(Q4, Strategy => "PunctualHilbert")
Q4' = getIdealFromNoetherianOperators(L, Q2)
Q4 == Q4'
----------------------------------------------------
----------------------------------------------------

----------------------------------------------------
----------------------------------------------------
-- Example 5: some random primary ideal
---------------------------------------------------
R = QQ[x_1,x_2,x_3]
Q = ideal(random(3, R), random(2, R), random(2, R), random(4, R))
assert(dim Q == 0)
elapsedTime noetherianOperators(Q, Strategy => "MacaulayMatrix")
L = elapsedTime noetherianOperators(Q, Strategy => "PunctualHilbert")
Q' = getIdealFromNoetherianOperators(L, radical Q)
Q == Q'
----------------------------------------------------
----------------------------------------------------

----------------------------------------------------
----------------------------------------------------
-- Example 6 : a small example 
---------------------------------------------------
R = QQ[x_1,x_2,x_3]
Q = ideal(x_1^2, x_2^2, x_3^2, x_1*x_2 + x_1*x_3 +x_2*x_3)
elapsedTime noetherianOperators(Q, Strategy => "MacaulayMatrix")
L = elapsedTime noetherianOperators(Q, Strategy => "PunctualHilbert")
Q' = getIdealFromNoetherianOperators(L, radical Q)
Q == Q'
----------------------------------------------------
----------------------------------------------------

----------------------------------------------------
----------------------------------------------------
-- Example 7:
---------------------------------------------------
R = QQ[x_1,x_2,x_3,x_4]
J = ideal(x_1^4 + x_2*x_3*x_4, x_2^4 + x_1*x_3*x_4, x_3^4 + x_1*x_2*x_4)
dim J
primDec = primaryDecomposition J
-- here we will only take care of the first primary component...
Q = primDec_0
elapsedTime noetherianOperators(Q, Strategy => "MacaulayMatrix")
L = elapsedTime noetherianOperators(Q, Strategy => "PunctualHilbert")
Q' = getIdealFromNoetherianOperators(L, radical Q)
Q == Q'
----------------------------------------------------
----------------------------------------------------

----------------------------------------------------
----------------------------------------------------
-- Example 8: powers of the maximal irrelevant ideal 
---------------------------------------------------
R = QQ[x_1,x_2,x_3]
mm= ideal vars R
n=6
Q=mm^n
elapsedTime noetherianOperators(Q, Strategy => "MacaulayMatrix")
L = elapsedTime noetherianOperators(Q, Strategy => "PunctualHilbert")
Q' = getIdealFromNoetherianOperators(L, radical Q)
Q == Q'
----------------------------------------------------
----------------------------------------------------

----------------------------------------------------
----------------------------------------------------
-- Example 9:
---------------------------------------------------
R = QQ[x_1,x_2,x_3]
Q = ideal(x_1^2,x_2^2,x_3^2)
elapsedTime noetherianOperators(Q, Strategy => "MacaulayMatrix")
L = elapsedTime noetherianOperators(Q, Strategy => "PunctualHilbert")
Q' = getIdealFromNoetherianOperators(L, radical Q)
Q == Q'
----------------------------------------------------
----------------------------------------------------


----------------------------------------------------
----------------------------------------------------
-- Example 10:
---------------------------------------------------
R = QQ[x_1..x_4]
I = minors(2, matrix{{x_1..x_3},{x_2..x_4}})
k = 6
J = I_* / (f -> f^k) // ideal
L' = elapsedTime noetherianOperators(J,I, Strategy => "MacaulayMatrix")
elapsedTime Q = first select(primaryDecomposition J, q -> radical q == I)
L = elapsedTime noetherianOperators(Q, Strategy => "PunctualHilbert")
Q' = getIdealFromNoetherianOperators(L, radical Q)
Q == Q'
----------------------------------------------------
----------------------------------------------------
