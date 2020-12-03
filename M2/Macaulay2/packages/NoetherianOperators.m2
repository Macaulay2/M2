-- -*- coding: utf-8 -*-
newPackage(
    "NoetherianOperators",
    Version => "1.0",
    Date => "Nov 25, 2020",
    Authors => {
        {Name => "Robert Krone", 
        Email => "krone@math.gatech.edu"},
        {Name => "Justin Chen", 
        Email => "justin.chen@math.gatech.edu"},
        {Name => "Marc Harkonen", 
        Email => "harkonen@gatech.edu"},
        {Name => "Yairon Cid-Ruiz",
        Email => "Yairon.CidRuiz@UGent.be"},
        {Name => "Anton Leykin",
        Email => "anton.leykin@gmail.com"}
    },
    Headline => "numerically compute local dual spaces, Hilbert functions, and Noetherian operators",
    PackageExports => {"Bertini", "NumericalLinearAlgebra", "NAGtypes"},
    PackageImports => {"Dmodules", "PrimaryDecomposition"},
    AuxiliaryFiles => false,
    DebuggingMode => false,
    Keywords => {"Numerical Algebraic Geometry", "Commutative Algebra"}
)

debug NumericalLinearAlgebra

export {
    "truncatedDual",
    "zeroDimensionalDual",
    "gCorners",
    "localHilbertRegularity",
    "eliminatingDual",
    "innerProduct",
    "orthogonalInSubspace",
    "Rational",
    "ProduceSB",

    "DiffOp",
    "ZeroDiffOp",
    "InterpolatedDiffOp",
    "diffOp",
    "normalize",

    "noetherianOperators",
    "specializedNoetherianOperators",
    "numericalNoetherianOperators",
    "noethOpsFromComponents",
    "coordinateChangeOps",
    "rationalInterpolation",
    "InterpolationTolerance",
    "InterpolationDegreeLimit",
    "NoetherianDegreeLimit",
    "DependentSet",
    "KernelStrategy",
    "IntegralStrategy",

    "Sampler",
    "TrustedPoint",

    --functions from punctual Hilb approach
    "getIdealFromNoetherianOperators",
    "joinIdeals",
    "mapToPunctualHilbertScheme" 

}

export { "isPointEmbedded", "isPointEmbeddedInCurve", "AllVisible" }

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
-- Create a dual space from a list of Noetherian operators
-- Caveat: if Noetherian operators have non-constant coefficients,
--          behavior is undefined.
dualSpace (List, Point) := (L, p) -> (
    if #L == 0 then error"expected nonempty list";
    L' := select(L, op -> not instance(op, ZeroDiffOp));
    gens := matrix{ L' / (op -> keys op / (k -> op#k * k)) / sum };
    if #L' == 0 then gens = sub(gens, ring first L);
    dualSpace(gens, p)
)
TEST ///
R = CC[x,y]
foo1 = new DiffOp from {x => 12, x^2*y => 3}
foo2 = new DiffOp from {1_R => 4, x*y => 4}
pt = point{{1_CC,2}}
a = dualSpace({foo1,foo2}, pt)
b = dualSpace(matrix{{12*x + 3*x^2*y, 4*x*y + 4}}, pt)
assert(gens a.Space == gens b.Space)
assert(point a == point b)
///


shiftGens := (p,Igens) -> (
    R := ring Igens;
    sub(Igens, matrix{gens R + apply(p.Coordinates,c->sub(c,R))})
    )

listFactorial = L -> product(L, l->l!)

-- Legacy methods
----------------------------------
truncatedDual = method(Options => {Tolerance => null})
truncatedDual (Point,Ideal,ZZ) := o -> (p,I,d) -> (
    depVars := gens (ring I);
    t := getTolerance(ring first coordinates p,o);
    L := numNoethOpsAtPoint(I,p, DependentSet => depVars, DegreeLimit => d, Tolerance => t);
    dualSpace(L, p)
)
truncatedDual (Matrix, Ideal, ZZ) := o -> (p, I, d) -> truncatedDual(point p, I, d, o)
truncatedDual (Point, Matrix, ZZ) := o -> (p, I, d) -> truncatedDual(p, ideal I, d, o)
truncatedDual (Matrix, Matrix, ZZ) := o -> (p, I, d) -> truncatedDual(point p, ideal I, d, o)

zeroDimensionalDual = method(TypicalValue => DualSpace, Options => {Tolerance => null})
zeroDimensionalDual (Point,Ideal) := o -> (p,I) -> (
    depVars := gens (ring I);
    t := getTolerance(ring first coordinates p,o);
    L := numNoethOpsAtPoint(I,p, DependentSet => depVars, Tolerance => t);
    dualSpace(L, p)
)
zeroDimensionalDual (Matrix,Ideal) := o -> (p,I) -> zeroDimensionalDual(point p, I, o)
zeroDimensionalDual (Point,Matrix) := o -> (p,I) -> zeroDimensionalDual(p, ideal I, o)
zeroDimensionalDual (Matrix,Matrix) := o -> (p,I) -> zeroDimensionalDual(point p, ideal I, o)
----------------------------------

--An object that stores the data for an ongoing iterative tuncated dual space computation
TruncDualData = new Type of MutableHashTable
initializeDualData = method(Options => {KernelStrategy => "Default"})
initializeDualData (Matrix,Boolean,Number) := opts -> (Igens,syl,t) -> (
    H := new MutableHashTable;
    R := ring Igens;
    F := coefficientRing R;
    H.Igens = Igens;
    H.syl = syl;
    H.deg = 0;
    h := symbol h;
    S := F(monoid[{h}|gens R, MonomialOrder => {Weights => (numgens R+1):1, 1, (options R).MonomialOrder}]); --projectivization of R
    h = S_0;
    H.hIgens = homogenize(sub(Igens,S), h); 
    T := if syl then S else R;
    H.Seeds = dualSpace(matrix{{1_T}},origin(T));
    H.BMmatrix = innerProduct(polySpace if syl then H.hIgens else H.Igens, H.Seeds);
    H.BMmatrix = sub(H.BMmatrix,F);
    H.BMintegrals = gens H.Seeds;
    H.BMcoefs = myKernel(H.BMmatrix, opts, Tolerance=>t);
    H.BMbasis = H.BMcoefs;
    --print(H.BMmatrix,H.BMcoefs);
    H.dBasis = H.BMintegrals * H.BMcoefs;
    if H.syl then H.dBasis = (map(R,S,{1_R} | gens R)) H.dBasis;
    new TruncDualData from H
    )
truncDualData = initializeDualData

-- advances the truncated dual computation from whatever was stored in parameters up to degree d
nextTDD = method(Options => {KernelStrategy => "Default"})
nextTDD (TruncDualData,Number) := opts -> (H,t) -> nextTDD(H.deg + 1,H,t, opts)
nextTDD (ZZ,TruncDualData,Number) := opts -> (d,H,t) -> (
    R := ring H.Igens;
    S := ring H.hIgens;
    dehomog := map(R,S,{1_R} | gens R);
    for e from H.deg+1 to d do (
        (M,E) := BMmatrix H;
        H.BMmatrix = M; H.BMintegrals = E;
        H.BMcoefs = myKernel(M, opts, Tolerance=>t);
        --print(M,H.BMcoefs);
        I := basisIndices(last coefficients E*H.BMcoefs, t);
        H.BMbasis = submatrix(H.BMcoefs, I);
        H.dBasis = if H.syl then dehomog(E*H.BMcoefs) else H.dBasis | E*H.BMcoefs;
        if numcols H.BMcoefs == 0 then break;
        --print (e, numrows M, numcols M, numcols H.dBasis, dim reduceSpace polySpace H.dBasis);
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

gCorners = method(TypicalValue => Matrix, Options => {Tolerance => null, ProduceSB => false})
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
    TDD := initializeDualData(Igens,true,t); -- initial parameters for computing truncated duals
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
socles MonomialIdeal := I -> mingens((I : ideal gens ring I)/I)
socles Matrix := M -> socles monomialIdeal M
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
    F := coefficientRing R;
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
    M = sub(M | Mnew, F);
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
    V := if opts.?DependentSet then opts.DependentSet else gens R;
    matrix{ unique flatten entries (gens I ** basis(0,d-1,R, Variables => V))}
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


-------------------------------------------------------
-- functions related to numerical primary decomposition
-- (previously residing in NumericalAlgebraicGeometry)
-------------------------------------------------------

isPointEmbedded = method(Options=>{AllVisible=>false})
isPointEmbedded(Point, Ideal, List) := o -> (p,I,C) -> ( -- C is a list of witness sets for irreducible components
    R := ring I;
    time gCs := gCorners(p,I); -- assume Robert's
	       	              -- algorithm a DualSpace D,
			      -- and g-corners
    d := 0;
    l := random(1,ring I); -- a generic linear form 
    while true do (
	-- FIRST PART: returns true if embeddedness is certified
	if o.AllVisible then (
    	    Jd := interpolate(p,I,C,d);  -- a function that returns 
	    -- a list of polynomials forming a basis of J_d (vector
	    -- space), where J is the "part" of the decomposition of I
	    -- corresponding to the components given in C.
	    if dim R_d - dim Jd != hilbertFunction(d, monomialIdeal gCs) then return true; -- is there a better way?
	    )
	else (
	    if debugLevel>0 then print "-- double truncation...";
	    time Jdd := doubleTruncation(I,C,d,d);
	    if debugLevel>0 then print (d,dim Jdd);
	    for d' from 1 to d do (
		g := random(d', Jdd);
		if debugLevel>0 then << "-- witness poly: (d',d) = " << (d',d) << endl;
		time if isWitnessPolynomial(p,I,g,d)     
		then (
		    if debugLevel>0 then print toString g;
		    return true;
		    )
		)
	    );
        --SECOND PART: returns false if deemed not embedded
	if debugLevel>0 then print "-- colon(truncated dual)...";
	time Sd := colon(truncatedDual(p,I,d), l);
	sCs := flatten entries sCorners gCs;
	colonLMs := leadMonomial \ flatten entries gens reduceSpace Sd;
    	if debugLevel>0 then << "-- s-corners: " << sCs << endl << "-- LM(dual of colon ideal): " << colonLMs << endl;	
	if isSubset(sCs, colonLMs) then return false; 
    	d = d+1;
	)
    )

colon = method(TypicalValue => DualSpace, Options => {Tolerance=>1e-6})
colon (DualSpace, RingElement) := o-> (L,g) -> (
    (gmons,gcoefs) := coefficients g;
    (Lmons,Lcoefs) := coefficients gens L;
    M := matrix apply(flatten entries gmons, gm->(
	    apply(flatten entries Lmons, Lm->(
		    d := diff(gm,Lm);
		    if d == 0 then d else leadMonomial d
		    ))
	    ));
    if numcols M == 0 then M = map((ring L)^1,(ring L)^0,0);
    M = (transpose gcoefs)*M*Lcoefs;
    (Mmons,Mcoefs) := coefficients M;
    M = Mmons*sub(numericalImage(Mcoefs,o.Tolerance),ring Mmons);
    dualSpace(polySpace M, L.BasePoint)
    )
colon (DualSpace, Ideal) := (L,J) -> error "not implemented"



interpolate = method()
interpolate (Point,Ideal,List,ZZ) := (p,I,C,d) -> error "not implemented"

TEST ///
setRandomSeed 0
needsPackage "NumericalAlgebraicGeometry"
-- NPD2.8: pseudo-component at the origin
RQQ = QQ[x_1..x_3]
M = matrix{{x_1^2,x_1*x_2*x_3}}

-- NPD3.10: all components are embedded
RQQ = QQ[x_1..x_3]
M = matrix{{x_1^2,x_1*x_2^2*x_3,x_1*x_2*x_3^3}}

I = ideal M
RCC = CC[x_1..x_3]
C = drop(flatten flatten (ass I / values@@numericalIrreducibleDecomposition),-1)
O = origin RCC
assert isPointEmbedded(O,sub(I,RCC),C)
///

doubleTruncation = method()
doubleTruncation (Ideal,List,ZZ,ZZ) := (I,C,d,e) -> (
    R := ring I;
    S := polySpace basis(0,d,R);
    orthogonalInSubspace(I,C,e,S)
    ) 

orthogonalInSubspace (Ideal,List,ZZ,PolySpace) := (I,C,e,S) -> (
    t := 1e-6;
    done := false;
    while not done do done = all(C, V->(
	    p := random V; 
	    D := truncatedDual(p,I,e); -- D_p^e[I]
	    S' := orthogonalInSubspace(D,S,t);
	    nothing'new := (dim S' == dim S);
	    S = S';
	    nothing'new
	    ));
    S
    )

isWitnessPolynomial = method()
isWitnessPolynomial (Point, Ideal, RingElement, ZZ) := (p,I,g,dStop) -> (
    t := 1e-6;
    R := ring g;
    n := numgens R;
    if g == 0 then return false;
    Igens := sub(gens I, matrix{gens R + apply(p.Coordinates,c->sub(c,R))});
    g = sub(g, matrix{gens R + apply(p.Coordinates,c->sub(c,R))});
    e := first degree g;
    
    Iplusg := Igens|matrix{{g}};
    IP := truncDualData(Igens,false,t);
    IplusgP := truncDualData(Iplusg,false,t);
    IhP := truncDualData(Igens,true,t);
    ID := IhD := IplusgD := polySpace map(R^1,R^0,0);
    S := ring homogPolySpace IhP;
    dehomog := map(R,S,{1_R} | gens R);
    gh := homogenize(sub(g,S),S_0);
    d := 0;
    GCs := {};
    varList := new MutableList from n:false;
    ginI := true;
    ginJ := false;
    while d <= dStop do (
	IP = nextTDD(d,IP,t);
	IplusgP = nextTDD(d,IplusgP,t);
	if dim polySpace IP != dim polySpace IplusgP then ginI = false;
	IhP = nextTDD(d+e,IhP,t);
	IhD = dualSpace(homogPolySpace IhP, point {toList (numgens S:0)});
	IhcolonghD := reduceSpace polySpace dehomog gens colon(IhD,gh);
	newGCs := newGCorners(IhcolonghD,GCs,d,d);
	if any(newGCs, g->(g#0 == 1_R)) then return false;
	for g in newGCs do (
	    l := (listForm(g#0))#0#0;
	    ls := select(n, i->(l#i != 0));
	    if #ls == 1 then varList#(first ls) = true;
	);
	if all(varList, v->v) then ginJ = true;
      	GCs = GCs|newGCs;
	d = d+1;
	if ginJ and not ginI then return true;
	);
    false
    )

TEST ///
debug needsPackage "NoetherianOperators"
R = CC[x,y]
p = point {{0,0}}
I = ideal {x^2,y*x}
g = x^2
assert(not isWitnessPolynomial(p,I,g,10))
h = x
assert(isWitnessPolynomial(p,I,h,10))
///


isPointEmbeddedInCurve = method(Options=>{"regularity"=>-1})
isPointEmbeddedInCurve (Point,Ideal) := o-> (p,I) -> (
    R := ring I;
    I' := ideal sub(gens I, matrix{gens R + apply(p.Coordinates,c->sub(c,R))});
    p' := origin(R);
    m := matrix{apply(gens R, v->random(1,R))}; -- matrix for random linear change of coordinates
    I' = (map(R,R,m)) I'; -- I with new coordinates
    r := o#"regularity";
    if r == -1 then (
	r1 := localHilbertRegularity(p',I');
	r2 := dim truncatedDual(p',I',r);
	r = max{r1,r2-1};
	);
    E := eliminatingDual(p',I',{0},r); -- assume I is in general position w.r.t. x = R_0
    E1 := truncate(E,{0},r-1);
    E2 := colon(E,R_0);
    print (dim E1, dim E2, areEqual(E1,E2));
    dim E1 != dim E2 -- "truncate" extracts a lesser truncated eliminating dual from E
    -- can we make a less expensive test than above? look at the leading terms of the dual basis (and the quotient)?
    )



--
-----  Noetherian operator code
--

-- DiffOp is a type of hash table, which contains one key, Op. This is to make inheritance with NoethOp work.
-- The value of Op is a HashTable, with keys corresponding to partial monomials, 
--  and values corresponding to coefficients.
-- Constructors
DiffOp = new Type of HashTable
DiffOp.synonym = "differential operator"
new DiffOp from HashTable := (DD,H) -> (
    if #set(keys H / ring) > 1 then error"expected all elements in same ring";
    if not all(keys H, m -> monomials m == m) then error"keys must be pure monomials";
    R := ring first keys H;
    applyValues(H, v -> sub(v,R))
)
new DiffOp from List := (DD,L) -> new DiffOp from hashTable L
diffOp = method()
diffOp HashTable := H -> (
    if #keys H == 0 then error "expected non-empty hash table";
    H' := select(H, f -> f!= 0);
    if #keys H' == 0 then new ZeroDiffOp from ring first keys H
    else new DiffOp from H'
)
diffOp List := L -> diffOp hashTable L
-- Create DiffOp from Weyl algebra element. 
-- Output will be in ring R and R must contain the non
diffOp (Ring, RingElement) := (R,f) -> diffOp(f,R)
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
-- other useful functions
ring DiffOp := D -> ring first keys D
substitute (DiffOp, Ring) := (D,R) -> applyPairs(D, (k,v) -> sub(k, R) => sub(v,R))
normalize = method();
normalize DiffOp := D -> 1/(sub( leadCoefficient D#(first rsort keys D), coefficientRing ring D)) * D
DiffOp == ZZ := (D, z) -> if z == 0 then false else error"cannot compare DiffOp to nonzero integer"
ZZ == DiffOp := (z, D) -> D == z
evaluate (DiffOp, Matrix) := (D,p) -> applyValues(D, v -> promote((evaluate(matrix{{v}}, p))_(0,0), ring D))
evaluate (DiffOp, Point) := (D,p) -> evaluate(D, matrix p)


-- instances of ZeroDiffOp are differential operators that
-- act as the zero operator. They have exactly one key with value zero
ZeroDiffOp = new Type of DiffOp
new ZeroDiffOp from Ring := (DD, R) -> hashTable{1_R => 0_R}
toExternalString ZeroDiffOp := D -> "new ZeroDiffOp from " | toExternalString(ring D)
ZeroDiffOp == ZZ := (D, z) -> if z == 0 then true else error"cannot compare DiffOp to nonzero integer"
ZZ == ZeroDiffOp := (z, D) -> (D == z)

-- Type used for interpolated differential operators
-- As in DiffOp, each key corresponds to a monomial,
-- but each value is the numerator and denominator of a rational function
InterpolatedDiffOp = new Type of DiffOp
new InterpolatedDiffOp from List := (TT, L) -> hashTable L
new InterpolatedDiffOp from HashTable := (TT, H) -> H
expression InterpolatedDiffOp := D -> 
    rsort(keys D, MonomialOrder => Lex) / 
    (k -> ((expression D#k#0)/(expression D#k#1)) * if k == 1 then expression(1) else addDsymbol(k)) //
    sum
net InterpolatedDiffOp := D -> net expression D
evaluate (InterpolatedDiffOp, Matrix) := (D, p) -> (
    if any(splice values D, x -> x === "?") then error "cannot evaluate an incomplete interpolated differential operator"
    else diffOp(applyValues(D, (n,d) -> 
        promote((evaluate(matrix{{n}},p))_(0,0)/(evaluate(matrix{{d}},p))_(0,0), ring D))))
evaluate (InterpolatedDiffOp, Point) := (D, p) -> evaluate(D, matrix p)


TEST ///
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
assert(foo2 - foo2 == 0)
assert(not foo == 0)
needsPackage "Dmodules"
R' = makeWA(R)
wa = diffOp(x^2*dx - dx^2 + dy^3 + (x-3)*dx)
use ring wa
dop = diffOp({x => x^2+x-3, x^2 => -1, y^3 => 1})
assert(dop == wa)
-- InterpolatedDiffOp
a = new InterpolatedDiffOp from {x => (x^2+y, y^2+x), y^2*x => (z+2, x^2+z^3*x)}
assert((evaluate(a, point{{1,2,3}}))(x) == 3/5)
///

sanityCheck = (nops, I) -> (
    all(flatten table(nops, I_*, (N,i) -> (N i)%(radical I) == 0), identity)
)

myKernel = method(Options => {Tolerance => null, KernelStrategy => "Default"})
myKernel Matrix := Matrix => opts -> MM -> (
    if precision MM < infinity then return colReduce(numericalKernel(MM,Tolerance => opts.Tolerance),Tolerance => opts.Tolerance);
    if opts.KernelStrategy == "Default" then return colReduce gens kernel MM;

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

TEST ///
debug NoetherianOperators
M = random(QQ^4,QQ^2) * random(QQ^2,QQ^4)
assert((myKernel M - gens kernel M) == 0)
///

-- dispatcher method
noetherianOperators = method(Options => true)
noetherianOperators (Ideal) := List => true >> opts -> I -> (
    strats := new HashTable from {
        "Hybrid" => hybridNoetherianOperators,
        "MacaulayMatrix" => noetherianOperatorsViaMacaulayMatrix,
        "PunctualHilbert" => getNoetherianOperatorsHilb,
    };
    strat := if opts.?Strategy then opts.Strategy else "PunctualHilbert";
    if strats#?strat then strats#strat(I, opts) 
    else error ("expected Strategy to be one of: \"" | demark("\", \"", sort keys strats) | "\"")
)

noetherianOperators (Ideal, Ideal) := List => true >> opts -> (I,P) -> (
    strats := new HashTable from {
        "Hybrid" => hybridNoetherianOperators,
        "MacaulayMatrix" => noetherianOperatorsViaMacaulayMatrix,
        "PunctualHilbert" => getNoetherianOperatorsHilb,
    };
    strat := if opts.?Strategy then opts.Strategy else "MacaulayMatrix";
    if strats#?strat then strats#strat(I, P, opts) 
    else error ("expected Strategy to be one of: \"" | demark("\", \"", sort keys strats) | "\"")
)
-- End dispatcher method


macaulayMatrixKernel := true >> opts -> (I, kP) -> (
    S := ring I;
    -- option handling
    if debugLevel > 1 then <<opts<<endl;
    tol := if not opts.?Tolerance then getTolerance(S,opts) else opts.Tolerance;
    degLim := if not opts.?DegreeLimit then -1 else opts.DegreeLimit;
    rat := if not opts.?Rational then false else opts.Rational;
    kerStrat := if not opts.?KernelStrategy then "Default" else opts.KernelStrategy;
    useBM := if not opts.?IntegralStrategy then null else opts.IntegralStrategy;

    rat = rat or all(gens S, v -> isConstant sub(sub(v, kP), S));
    if not rat or useBM === false then (
        if debugLevel > 1 then <<"macaulayMatrixKernel: using DZ strategy"<<endl;
        L := (map(S^1,S^1,0), map(kP^1,kP^0,0));
        d := max(degLim, 1);
        while true do (
            Ldim := numcols last L;
            dBasis := basis(0,d,S);
            polys := transpose idealBasis(I,d);
            M' := diff(dBasis, polys);
            M := (map(kP,S)) M';
            if debugLevel >= 1 then  <<"Cols: "<<numColumns M<<", rows: "<<numRows M<<endl;
            K := myKernel(M,Tolerance => tol, KernelStrategy => kerStrat);
            L = (dBasis, K);
            if degLim >=0 or Ldim == numcols last L then break;
            d = d+1;
        );
        L
    ) 
    else if not rat and useBM === true then error"expected rational point when IntegralStrategy => true"
    else (
        if debugLevel > 1 then <<"macaulayMatrixKernel: using BM strategy"<<endl;
        pt := sub(sub(vars S, kP), S);
        if degLim < 0 then degLim = infinity;
        igens := sub(gens I, vars S + pt);
        H := truncDualData(igens,false,tol);
        DB := map(S^1,S^0,0);
        d = -1;
        DBdim := -1;
        while DBdim != numcols DB and d < degLim do (
            d = d+1;
            DBdim = numcols DB;
            H = nextTDD(d,H,tol, KernelStrategy => kerStrat);
            DB = H.dBasis;
        );
        (M,L) = coefficients DB;
        L = apply(flatten entries M / exponents / first / listFactorial, entries L, (c, l) -> 1/c * l);
        (M, if #L == 0 then map(kP^0, kP^0, 0) else sub(matrix L, kP))
    )
)


-- returns a list of Diff ops based on matrices M, dBasis
matrixToDiffOps = (M, dBasis) -> (
    if M == 0 then {new ZeroDiffOp from ring M}
    else transpose entries M / 
        (c -> apply(flatten entries dBasis, c, identity)) /
        diffOp //
        sort
)


noetherianOperatorsViaMacaulayMatrix = method(Options => true) 
noetherianOperatorsViaMacaulayMatrix (Ideal, Ideal) := List => true >> opts -> (I, P) -> (
    R := ring I;
    m := if opts.?DegreeLimit then opts.DegreeLimit else -1;
    rat := if opts.?Rational then opts.Rational else false;

    t := getTolerance(R,opts);
    (depVars,indVars) := getDepIndVars(P, opts);
    -- use the original coefficient field if appropriate, else a rational function field
    F := if #indVars == 0 then coefficientRing R else frac((coefficientRing R)(monoid[indVars]));
    S := F(monoid[depVars]);
    PS := sub(P,S); IS := sub(I,S);
    -- extend the field only if the point is not specified to be rational
    kP := if rat then F else toField(S/PS);

    L := macaulayMatrixKernel(IS, kP, opts, Tolerance => t, DegreeLimit => m, Rational => rat);
    -- Clear denominators, create list of DiffOps
    matrixToDiffOps(liftColumns(lift(last L,S), R), sub(first L,R))
)

TEST ///
debug NoetherianOperators
R = QQ[x,y,t]
I = ideal(x^2, y^2 - t*x)
nops = noetherianOperatorsViaMacaulayMatrix(I) / normalize
correct = {diffOp{1_R => 1}, diffOp{y => 1}, diffOp{y^2 => t, x => 2}, diffOp{y^3 => t, x*y => 6}}
assert(all(nops, correct, (i,j) -> i == j))

S = QQ[x,y]
J = ideal(x^3, y^4, x*y^2)

correct = sort {diffOp{1_S => 1},diffOp{x => 1},diffOp{y => 1},diffOp{x^2 => 1},diffOp{x*y => 1},diffOp{y^2 => 1},diffOp{x^2*y => 1},diffOp{y^3 => 1}}
nops = noetherianOperatorsViaMacaulayMatrix(J) / normalize
assert(all(nops, correct, (i,j) -> i == j))
///





getDepIndVars = true >> opts -> P -> (
    depVars := if not opts.?DependentSet then (
	   gens(ring P) - set support first independentSets P
	) else opts.DependentSet;
    indVars := gens(ring P) - set depVars;
    (depVars,indVars)
)

noetherianOperatorsViaMacaulayMatrix (Ideal) := List => true >> opts -> (I) -> noetherianOperatorsViaMacaulayMatrix(I, ideal gens radical I, opts)

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
        if member(R_i,depVars) then R_i+p_(0,i) else p_(0,i)
        ))};
    RtoS := map(S,R,sub(subs,S));

    L := macaulayMatrixKernel(RtoS I, coefficientRing S, opts, DegreeLimit => degLim, Tolerance => tol, Rational => true);
    matrixToDiffOps(promote(last L, R), sub(first L, R))
)

TEST ///
debug NoetherianOperators
R = CC[x,y,t]
I = ideal(x^2, y^2 - t*x)
p = point{{0_CC,0,3}}
nops = numNoethOpsAtPoint(I, p, DependentSet => {x,y})
assert(all(nops, op -> abs((evaluate(matrix{{op(-t*x^3)}}, p))_(0,0)) < 1e-6))
///

TEST ///
debug NoetherianOperators
R = CC[x,y]
J = ideal(y^2-4*y+4,-x^2+y)
p = point{{1.41421,2}}
L = numNoethOpsAtPoint(J,matrix p, DependentSet => {x,y}, Tolerance => 1e-3)
assert(#L == 2)
assert(all(values(L#0 - diffOp{1_R => 1}), v -> abs(sub(v,CC)) < 1e-6))
assert(all(values(normalize(L#1) - normalize(diffOp{x => 1, y => 2*sqrt(2)})), v -> abs(sub(v,CC)) < 1e-3))
///

hybridNoetherianOperators = method(Options => true)
hybridNoetherianOperators (Ideal, Ideal, Matrix) := List => true >> opts -> (I,P, pt) -> (
    R := ring I;
    (depVars,indVars) := getDepIndVars(P,opts);
    S := (frac((coefficientRing R)(monoid[indVars])))(monoid[depVars]);
    PS := sub(P, S);
    IS := sub(I,S);
    kP := toField(S/PS);
    RCC := (ring pt) monoid R;
    nopsAtPoint := numNoethOpsAtPoint(sub(I,RCC), pt, opts, DependentSet => depVars / (i->sub(i,RCC)));
    sort flatten for op in nopsAtPoint list (
        dBasis := sub(matrix{keys op / (m -> R_(first exponents m))}, S);
        maxdeg := flatten entries dBasis / sum @@ degree // max;
        K := dBasis;
        for d from 0 to maxdeg - 1 do (
            if debugLevel >= 1 then <<"hybridNoetherianOperators: trying degree "<<d<<" multiples of generators"<<endl;
            G := transpose (gens IS ** basis(0,d,S));
            M := sub(diff(dBasis, G), kP);
            K = myKernel(M, KernelStrategy => if opts.?KernelStrategy then opts.KernelStrategy else "Default");
            if numColumns K == 1 then break;
        );
        -- Clear denominators and return a DiffOp
        first matrixToDiffOps(liftColumns(lift(K, S), R), sub(dBasis, R))
    )
)
hybridNoetherianOperators (Ideal, Ideal, Point) := List => true >> opts -> (I,P, pt) -> hybridNoetherianOperators(I,P, matrix pt, opts)

hybridNoetherianOperators (Ideal, Ideal) := List => true >> opts -> (I,P) -> (
    f := if opts.?Sampler then opts.Sampler else J -> first bertiniSample(1,first components bertiniPosDimSolve(J));
    hybridNoetherianOperators(I,P,f P, opts)
)

hybridNoetherianOperators (Ideal) := List => true >> opts -> I -> hybridNoetherianOperators(I, radical I, opts)

TEST ///
debug NoetherianOperators
R = QQ[x,y,t]
I = ideal(x^2, y^2 - t*x)
a = hybridNoetherianOperators(I, Sampler => i -> point{{0_CC,0,3}}) / normalize
b = noetherianOperators(I, Strategy => "PunctualHilbert") / normalize
c = hybridNoetherianOperators(I, radical I, point{{0_QQ,0,3}}) / normalize
assert(all(a,b, (a,b) -> a==b))
assert(all(a,c, (a,b) -> a==b))
///


numericalNoetherianOperators = method(Options => true)
-- option TrustedPoint is a point with the "correct" dx-support
-- option Sampler is a function f(n, I) which computes a list of n distinct points on the variety of I
-- other valid options: InterpolationDegreeLimit, InterpolationTolerance
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
    noethDegLim := if not opts.?NoetherianDegreeLimit then infinity else opts.NoetherianDegreeLimit;
    R := (ultimate(coefficientRing, ring matrix goodPoint)) monoid S;
    J := sub(I,R);



    nopsTemplate := numNoethOpsAtPoint(J, goodPoint, opts, DependentSet => depSet / (i -> sub(i,R)), Tolerance => tol, DegreeLimit => noethDegLim);
    if not S.?cache then S.cache = new CacheTable;
    S.cache#"interp point list" = new List;
    nopsTemplate / (tmpl -> interpolateFromTemplate(I, tmpl, opts, Tolerance => tol, Sampler => sampler))
)

-- computes specialized Nops at rational points
specializedNoetherianOperators = method(Options => true)
specializedNoetherianOperators(Ideal, Matrix) := List => true >> opts -> (I,pt) -> (
    S := ring I;
    if precision ring pt == infinity and precision S == infinity then (
        depVars := if opts.?DependentSet then opts.DependentSet 
            else if isPrimary I then getDepIndVars(I, opts)
            else error"expected option DependentSet";
        numNoethOpsAtPoint(I, pt, opts, DependentSet => depVars)
    )
    else if coefficientRing S === QQ then (
        T := (ultimate(coefficientRing, ring pt)) monoid S;
        depVars = if opts.?DependentSet then opts.DependentSet 
            else if isPrimary I then getDepIndVars(I, opts)
            else error"expected option DependentSet";
        numNoethOpsAtPoint(sub(I,T), pt, opts, DependentSet => depVars / (x -> sub(x, T)))
    )
    else (
        try promote(pt, coefficientRing S) then numNoethOpsAtPoint(I,pt,opts) else (
                if not liftable(pt, S) then error"point is not liftable/promotable to ring of ideal";
                if not opts.?DependentSet then error "expected option DependentSet";
                numNoethOpsAtPoint(I, pt, opts)
        )
    )
)
specializedNoetherianOperators(Ideal, Point) := List => true >> opts -> (I,pt) -> specializedNoetherianOperators(I, matrix pt, opts)

TEST ///
debug NoetherianOperators
R = QQ[x,y,t]
I = ideal(x^2, y^2 - t*x)
p = point{{0_CC,0, 3}}
ptList = new MutableHashTable from {i => 0, pts => {4,7,-12} / (i -> point{{0_CC,0,i}})}
gen = I -> (ptList.i = (ptList.i+1)%3; ptList.pts#(ptList.i))
sampler = (n,I) -> apply(n, i -> gen(I))
nnops = numericalNoetherianOperators(I, DependentSet => {x,y}, TrustedPoint => {{0_CC,0,12}}, Sampler => sampler)
enops = nnops / (op -> evaluate(op, point{{0,0,3_CC}}))
snops = specializedNoetherianOperators(I, point{{0,0,3_CC}}, DependentSet => {x,y})

S = ring first snops

enops = enops / (op -> 1/lift(op#(first sort keys op), coefficientRing ring op) * op)
enops = enops / (i -> sub(i, S))
snops = snops / (op -> 1/lift(op#(first sort keys op), coefficientRing ring op) * op)

assert(all(snops - enops, i -> all(values i, j -> abs(lift(j,coefficientRing ring first snops)) < 1e-6)))

numericalNoetherianOperators(I, DependentSet => {x,y}, TrustedPoint => {{0_CC,0,12}}, Sampler => sampler, InterpolationDegreeLimit => 0)
///


interpolateFromTemplate = true >> opts -> (I, tmpl) -> (
    oldPtList := (ring I).cache#"interp point list";
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
            -- generate as many new points and new specialized nops as necessary
            while (neededPoints := numColumns numBasis + numColumns denBasis + 1 - #ptList) > 0 do (
                if neededPoints > 0 and debugLevel > 0 then 
                    <<"Computing "<<neededPoints<<" new specialized NOps"<<endl;
                newPoints := max(neededPoints - #oldPtList, 0);
                newPtList := take(oldPtList, neededPoints);
                oldPtList = drop(oldPtList, neededPoints);
                if newPoints > 0 and debugLevel > 0 then 
                    <<"Generating "<<newPoints<<" new points"<<endl;
                if newPoints > 0 then newPtList = newPtList | sampler(newPoints, I);
                nopList := for p in newPtList list (
                    nop := specializedNopsFromTemplate(I, p, tmpl, opts, Tolerance => opts.Tolerance);
                    if nop === null then continue else {nop, p}
                );
                opList = opList | first transpose nopList;
                ptList = ptList | last transpose nopList;
            );
            
            interpPoints := numColumns numBasis + numColumns denBasis + 1;
            liftedCoeffs := take(opList, interpPoints)  /
                (op -> lift(op#m, ultimate(coefficientRing, ring op)));
            neededPtList := take(ptList, interpPoints);
            try result = rationalInterpolation(neededPtList, liftedCoeffs, numBasis, denBasis, Tolerance => interpTol) then break
                else d = d+1;
        );
        result = result / (j -> cleanPoly(opts.Tolerance, j));
        (m => result)
    ));
    if debugLevel > 0 then <<"Done interpolating from template "<<tmpl<<endl;
    (ring I).cache#"interp point list" = ptList | oldPtList;
    new InterpolatedDiffOp from nops
)

-- Create new specialized Noeth op at a random point using tmpl as a template
-- sampler(n,I) is a function that generates a list of n points on the variety of I
specializedNopsFromTemplate = true >> opts -> (I, pt, tmpl) -> (
    R := ring I;
    R' := ring tmpl;
    bd := keys tmpl;
    maxdeg := bd / sum @@ degree // max;
    bx := basis(0, maxdeg-1, R');
    M := diff(matrix{bd}, transpose (sub(gens I, R') ** bx));

    M' := evaluate(M, pt);
    K := numericalKernel(M', Tolerance=>opts.Tolerance);
    -- If we don't get one kernel element, try again
    if numColumns K != 1 then (
        if debugLevel > 0 then <<"specializedNopsFromTemplate: bad point, trying again"<<endl;
        return null;
    );
    first matrixToDiffOps(promote(colReduce(K, Tolerance => opts.Tolerance), ring tmpl), matrix{bd})

)



cleanComplex = (tol, x) -> clean(tol,realPart x) + ii*clean(tol, imaginaryPart x)
cleanPoly = (tol, x) -> (
    if x === "?" then return "?";
    (mon,coef) := coefficients x;
    coef = matrix applyTable(entries coef, f -> cleanComplex(tol,sub(f,CC)));
    (mon * coef)_(0,0)
)

coordinateChangeOps = method()
coordinateChangeOps(Matrix, DiffOp) := DiffOp => (A, D) -> (
    R := ring D;
    A' := inverse A;
    b := pairs D / ((m,c) -> (sub(m, vars R * A'), sub(c, vars R * transpose A)));
    b / (p -> last p * (first matrixToDiffOps reverse coefficients first p)) // sum
)
coordinateChangeOps(Matrix, List) := coordinateChangeOps(RingMap, List) := List => (A, L) -> L/(D -> coordinateChangeOps(A, D))
coordinateChangeOps(RingMap, DiffOp) := DiffOp => (phi, D) -> coordinateChangeOps(transpose(matrix phi // vars ring D), D)


TEST ///
R = QQ[x,y,t]
n = numgens R
I = ideal(x^2, y^2 - x*t)
A = random(R^n, R^n)
B = random(R^n, R^n)
nops = noetherianOperators I
comp = nops / (op -> coordinateChangeOps_A op) / (op -> coordinateChangeOps_B op)
prod = nops / (op -> coordinateChangeOps_(A*B) op)
assert(all(comp - prod, D -> D == 0))
phi = map(R,R, vars R * transpose (A*B))
assert(coordinateChangeOps_phi last nops - last comp == 0)
///


noethOpsFromComponents = method()
-- List of ordered pairs (P, N), where P is a minimal prime of I, N a list of nops for the P-primary component.
-- Output is a list of operators, which satisfy the Noetherian operator condition for I and radical I 
noethOpsFromComponents(List) := List => L -> (
    nops := unique flatten (L / last);
    primes := L / first;
    R := ring first primes;
    primesContainingNops := apply(nops, D -> select(L, p -> member(D, p#1)) / first);
    mults := primesContainingNops / (primeList -> (
        if #primeList == #primes then 1_R else (
            J := intersect(primes - set primeList);
            primeList / (P -> J_(position(J_*, f -> f%P != 0))) // lcm
        )
    ));
    apply(mults, nops, (i,j) -> i*j)
)

TEST ///
R = QQ[x,y,t]
I = ideal(x^2, y^2 - t*x)
J = ideal((y+t)^2)
K = intersect(I, J)
primes = associatedPrimes K
L = primes / (P -> (P, noetherianOperators(K, P)))
ops = noethOpsFromComponents L
radK = radical K

assert(all(flatten table(ops, K_*, (D, f) -> (D f) % radK == 0), identity))
///

-- Inputs
-- pts: list of points (each point as a row matrix)
-- vals: list of values (in CC)
-- numBasis: basis for numerator (row matrix)
-- denBasis: basis for denominator (row matrix)
-- Outputs a sequence (numerator, denominator)
rationalInterpolation = method(Options => {Tolerance => 1e-6})
rationalInterpolation(List, List, Matrix, Matrix) := Sequence => opts -> (pts, vals, numBasis, denBasis) -> (
    if numColumns numBasis + numColumns denBasis > #pts - 1 then error "Rational interpolation needs more points";
    if numColumns numBasis < numColumns denBasis then error"expected numerator monomial support to be at least as large as the denominator monomial support";
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
        );
    if idx === {} then error "No fitting rational function found";
    norms := apply(idx, i -> entries K_i / abs // sum);
    K = unmingleVector(K_(idx#(minPosition(norms))), nn, nd);

    ((numBasis * K^{0..(nn - 1)})_(0,0), (denBasis * K^{nn .. (nn+nd-1)})_(0,0))
)
rationalInterpolation(List, List, Matrix) := (RingElement, RingElement) => opts -> (pts, vals, bas) -> (
    rationalInterpolation(pts,vals,bas,bas,opts)
)
rationalInterpolation(List,List,Ring) := opts -> (pts, vals,R) -> (
    d := 0;
    local i; local b;
    while (try (b = rsort basis(0,d,R); i = rationalInterpolation(pts, vals, b, opts)) then false else true) do (
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
joinIdeals = method()
joinIdeals(Ideal,Ideal) := (I, J) -> (
    v := symbol v; 
    w := symbol w;
    R := ring I;
    n := numgens R;
    T := (coefficientRing R)[v_1..v_n, w_1..w_n];
    Q := ((map(T, R, toList(v_1..v_n))) I) + ((map(T, R, toList(w_1..w_n))) J);
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

mapToPunctualHilbertScheme = method()
mapToPunctualHilbertScheme(Ideal) := (Q) -> (
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
invSystemFromHilbToNoethOps = true >> opts -> (I, R, S, depVars) -> (
    

    mm := ideal vars S; -- maximal irrelevant ideal of S
    m := 0; -- compute the exponent that determines the order of the diff ops
    if debugLevel > 0 then <<"Precomputing Noetherian operator degree limit: ";
    while (I : mm^m) != ideal(1_S) do m = m + 1;  
    if debugLevel > 0 then <<m-1<<endl;
    FF := coefficientRing S;
    L := macaulayMatrixKernel(I,FF, opts, DegreeLimit => m-1);
    StoR := map(R, S, apply(#depVars, i -> R_(index depVars#i)));
    matrixToDiffOps(liftColumnsPunctualHilbert(last L, R), StoR first L)
)
   
-- This function can compute the Noetherian operators of a primary ideal Q.
-- Here we pass first through the punctual Hilbert scheme 
getNoetherianOperatorsHilb = method(Options => true)
getNoetherianOperatorsHilb Ideal := List => true >> opts -> Q -> (
    kerStrat := if not opts.?KernelStrategy then "Default" else "Gaussian";

    R := ring Q;
    P := radical Q;
    indVars := support first independentSets P;
    depVars := gens R - set indVars;	
    S := getHilb(P, depVars);
    I := mapRtoHilb(Q, P, S, depVars, indVars);
    invSystemFromHilbToNoethOps(I, R, S, depVars, opts, KernelStrategy => kerStrat)
)

getNoetherianOperatorsHilb (Ideal, Ideal) := List => true >> opts -> (Q,P) -> (
    if P != radical Q then error "expected second argument to be the radical of the first"
    else getNoetherianOperatorsHilb(Q,opts)
)
TEST ///
debug NoetherianOperators
R = QQ[x,y,t]
I = ideal(x^2, y^2 - t*x)
hilb = getNoetherianOperatorsHilb(I) / normalize
maca = noetherianOperatorsViaMacaulayMatrix(I) / normalize
assert(all(hilb, maca, (i,j) -> i == j))
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
    for v in indVars do -- heuristic for faster computation 
      Q = saturate(Q, ideal(v));
    first select(primaryDecomposition(Q), K -> radical(K) == P)    
)

TEST ///
debug NoetherianOperators
R = QQ[x,y,t]
I = ideal(x^2, y^2 - t*x)
L = getNoetherianOperatorsHilb(I)
P = radical I
Q = getIdealFromNoetherianOperators(L,P)
assert(Q == I)
///
----------------------------------------------------------

undocumented {
    (expression, DiffOp),
    (expression, InterpolatedDiffOp),
    (net, DiffOp),
    (net, InterpolatedDiffOp),
    (toExternalString, ZeroDiffOp),
    (toString, DiffOp)
}

beginDocumentation()
refKroneLeykin := "R. Krone and A. Leykin, \"Numerical algorithms for detecting embedded components.\", arXiv:1405.7871"

doc ///
     Key
       NoetherianOperators
     Headline
       algorithms for computing local dual spaces and sets of Noetherian operators 
     Description
       Text
	   The @EM "NoetherianOperators"@ package includes algorithms for computing Noetherian operators and local dual 
	   spaces of polynomial ideals, and related local combinatorial data about its scheme structure.
    	    	
	   The problem of characterizing ideal membership with differential conditions was first addressed by 
	   Gr\"obner ("Uber eine neue idealtheoretische Grundlegung der algebraischen Geometrie", Math. Ann. 115 (1938), no. 1, 333–358).
	   Despite this early algebraic interest by Gr\"obner, a complete description of primary ideals in terms of differential operators was first obtained by analysts in the Fundamental Principle of Ehrenpreis and Palamodov.
	   At the core of the Fundamental Principle, one has the notion of Noetherian operators to describe a primary ideal. 
	   
	   In case of an ideal supported at one point a set of Noetherian operators forms a Macaulay inverse system that spans the dual space of the ideal. 
	   These notions relate to the work of 
	   Macaulay ("The algebraic theory of modular systems", Cambridge Press, (1916)). 
	    
	   In this package, we implement several (exact symbolic and approximate numerical) algorithms for the computation of a set of Noetherian operators describing a primary ideal. 
    	           	   
           Methods and types for computing and manipulating Noetherian operators:

           @UL {
               {TO noetherianOperators, UL {
                 {TO "Strategy => \"PunctualHilbert\""},
                 {TO "Strategy => \"MacaulayMatrix\""},
                 {TO "Strategy => \"Hybrid\""}
                }},
               {TO specializedNoetherianOperators},
               {TO numericalNoetherianOperators},
               {TO getIdealFromNoetherianOperators},
               {TO DiffOp},
               {TO coordinateChangeOps},
               {TO noethOpsFromComponents}
           }@

	   Methods for computing and manipulating local dual spaces:

	   @UL {
    		   {TO truncatedDual},
    		   {TO zeroDimensionalDual},
       		   {TO eliminatingDual},
    		   {TO localHilbertRegularity},
    		   {TO gCorners},
    		   {TO innerProduct},
	 	   	   {TO isPointEmbedded},
	 	   	   {TO isPointEmbeddedInCurve},
	 		   {TO colon},
               {TO reduceSpace}
	   }@

	   Auxiliary numerical linear algebra methods:

	   @UL {
               {TO rationalInterpolation}
	   }@

	   For the task of computing Noetherian operators, here we implement the algorithms developed in the papers 
	   @ HREF("https://arxiv.org/abs/2006.13881", "Noetherian Operators and Primary Decomposition")@ and  
	   @ HREF("https://arxiv.org/abs/2001.04700", "Primary ideals and their differential equations")@.
           These include both symbolic and numerical algorithms, and a hybrid algorithm, where numerical data is used to
           speed up the symbolic algorithm.
           
           To compute the initial ideal and Hilbert regularity of positive dimensional
           ideals we use the algorithm of R. Krone ("Numerical algorithms for dual bases of positive-dimensional ideals." Journal of
           Algebra and Its Applications, 12(06):1350018, 2013.). 
           These techniques are numerically stable, and can be used with floating point arithmetic over the complex numbers.  
           They provide a viable alternative in this setting to purely symbolic methods such as standard bases.  
	   

///


doc ///
Key
    truncatedDual
    (truncatedDual, Point, Ideal, ZZ)
    (truncatedDual, Point, Matrix, ZZ)
    (truncatedDual, Matrix, Ideal, ZZ)
    (truncatedDual, Matrix, Matrix, ZZ)
Headline
    truncated dual space of a polynomial ideal
Usage
    S = truncatedDual(p, I, d)
Inputs
    p:Point
        or a row @TO Matrix@
    I:Ideal
        or a row @TO Matrix@ of generators
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
        The optional argument @TO "Tolerance (NoetherianOperators)"@ can be set to adjust the tolerance of the numerical computations.
        Higher degree dual computations generally require higher accuracy in the input and larger tolerance value to complete correctly.

        In this example, the point q is slightly away from the variety of I, but an appropriate @TT "Tolerance"@ value can overcome the error. 
    Example
        q = point matrix{{0_CC + 1e-10, 1_CC}}
        tol = 1e-6;
        S = truncatedDual(q,I,3, Tolerance => tol)
        (m,c) = coefficients gens S;
        m*clean(tol, c)
SeeAlso
    zeroDimensionalDual
///

TEST ///
R = CC[x,y]
I1 = ideal{x^2,x*y}
D1 = truncatedDual(origin R, I1, 4)
assert(hilbertFunction({0,1,2,3,4}, D1) == {1,2,1,1,1})
///

doc ///
Key
    zeroDimensionalDual
    (zeroDimensionalDual,Point,Ideal)
    (zeroDimensionalDual, Point, Matrix)
    (zeroDimensionalDual, Matrix, Ideal)
    (zeroDimensionalDual, Matrix, Matrix)
Headline
    dual space of a zero-dimensional polynomial ideal
Usage
    S = zeroDimensionalDual(p, I)
Inputs
    p:Point
        or a row @TO Matrix@
    I:Ideal
        or a row @TO Matrix@ of generators
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

Caveat
    The computation will not terminate if I is not locally zero-dimensional at the chosen point.  This is not checked.
SeeAlso
    truncatedDual
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


TEST ///
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
	       gCorners(p, I)
	  Text
	       If the optional argument @TT "ProduceSB"@ is set to true, the output is instead a matrix of elements of the ideal
	       with the p translated to the origin such that the lead terms generate the inital ideal, i.e. a standard basis.
	       Note that the coordinates of the standard basis elements are translated to be centered at the point p.
	  Example
	       S = gCorners(p, I, ProduceSB=>true)
	       R = CC[x,y,z];
	       J = ideal{z*(x*y-4), x-y}
	       q = point matrix{{1.4142136, 1.4142136, 0}};
	       gCorners(q, J, Tolerance=>1e-5)
	       gCorners(q, J, ProduceSB=>true)
///

TEST ///
R = CC[x,y]
M = matrix {{x^2-x*y^2,x^3}}
--M = matrix {{x*y, y^2}}
p = point matrix{{0_CC,0_CC}}
q = point matrix{{0_CC,1_CC}}
assert(numcols gCorners(p,M) == 2)
assert(numcols gCorners(q,M) == 1)
LDZ = reduceSpace truncatedDual(p,M,5)
LBM = reduceSpace truncatedDual(p,M,5)
assert(dim LDZ == dim LBM)
///


TEST ///
debug NoetherianOperators
R = CC[x,y]
G1 = matrix{{x^2,x*y^2,y^4}}
assert(socles G1 == matrix {{x*y, y^3}})
G2 = matrix{{x*y^2,x^2*y^2,y^4}}
assert(socles G2 == matrix {{y^3}})
G3 = matrix{{x*y^2,x^2*y^2}}
assert(socles G3 == 0)
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
	       d = localHilbertRegularity(origin R, I)
	       D = truncatedDual(origin R, I, 3)
	       L = hilbertFunction({0,1,2,3}, D)
	  Text
	       See also @TO gCorners@.
///

TEST ///
R = QQ[x,y,z]
I = ideal {x*y*z}
assert(localHilbertRegularity(origin R, I) == 1)
assert(localHilbertRegularity(point matrix {{0,0,1}}, I) == 0)
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
	       eliminatingDual(origin R, I, {0}, 2)
	  Text
	       This function generalizes @TO truncatedDual@ in that if v includes all the variables
	       in the ring, then its behavior is the same.
	  Example
	       eliminatingDual(origin R, I, {0,1}, 2)
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

undocumented {  (orthogonalInSubspace,Ideal,List,ZZ,PolySpace) } 
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
      [rationalInterpolation, Tolerance]
      [truncatedDual, Tolerance]
      [zeroDimensionalDual, Tolerance]
     Headline
          optional argument for numerical tolernace
     Description
          Text
	       Many of the numerical operations require a tolerance value, below which numbers are considered to be numerically zero.
	       {\tt Tolerance} should be given a non-negative real number.
	       Over exact fields the default value is zero, while for inexact fields the default is 1e-6.
	       
	       
	       
	       See also @TO Tolerance@.
///

document {
    Key => {
	(isPointEmbedded,Point,Ideal,List), isPointEmbedded,
	AllVisible, [isPointEmbedded,AllVisible],
	},
    Headline => "determine if the point is an embedded component of the scheme",
    Usage => "B = isPointEmbedded(P,I,C)",
    Inputs => { 
	"P", 
	"I",
	"C"=>{" witness sets representing components of ", TT "Spec(I)", " containing ", TT "P"} 
	},
    Outputs => { "B"=>Boolean },
    PARA {"Runs an embedded component test described in "},
    refKroneLeykin,
    SeeAlso=>{isPointEmbeddedInCurve}
    }

document {
    Key => {
	(isPointEmbeddedInCurve,Point,Ideal), isPointEmbeddedInCurve
	},
    Headline => "determine if the point is an embedded component of a 1-dimensional scheme",
    Usage => "B = isPointEmbeddedInCurve(P,I)",
    Inputs => { 
	"P", 
	"I"
	},
    Outputs => { "B"=>Boolean },
    PARA {"Runs an embedded component test described in "},
    refKroneLeykin,
    SeeAlso=>{isPointEmbeddedInCurve}
    }

document {
    Key => {colon, (colon,DualSpace,RingElement), (colon,DualSpace,Ideal), [colon,Tolerance]},
    Headline => "colon of a (truncated) dual space",
    Usage => "Dg = colon(D,g)\nDJ = colon(D,J)",
    Inputs => { "D"=>DualSpace, "g"=>RingElement, "J"=>Ideal },
    Outputs => { "Dg, DJ"=>DualSpace },
    "Computes (a part of) the dual space of the dual. See",
    PARA { refKroneLeykin },
    "for a description."
    }

-- Numerical LA
doc ///
Key
    joinIdeals
    (joinIdeals, Ideal, Ideal)
Headline
    Computes the join of two ideals
Usage
    K = joinIdeals(I, J)
Inputs
    I : Ideal
        an ideal
    J : Ideal
        an ideal
Outputs
    K : Ideal
        the join of the ideals I and J
Description
    Text
        This method computes the join of two given ideals.
        The join can be used to describe very interesting types of 
        primary ideals that include the symbolic powers of prime ideals.
        For more details reader is referred to Section 7 of the paper    
        @ HREF("https://arxiv.org/abs/2001.04700", "Primary ideals and their differential equations")@. 

    Example
        R = QQ[x_1..x_9]
        MM = genericMatrix(R, 3, 3)
        P = minors(2, MM)
        M = ideal(x_1^2, x_2^2, x_3^2, x_4, x_5, x_6, x_7, x_8, x_9)
        Q = joinIdeals(P, M)
        isPrimary Q  
///


-------------- Noetherian operators documentation

------- DiffOp documentation
doc ///
Key
    DiffOp
Headline
    differential operator
Description
    Text
        A differential operator of the ring $R = \mathbb{K}[x_1,\dots,x_n]$ can be thought of as a polynomial
        with coefficients in $R$, and monomials in variables $dx_1, \dots, dx_n$, where $dx_i$ corresponds to the
        partial derivative with respect to $x_i$. These operators form an $R$-vector space, and act naturally on elements of $R$.

    Example
        R = QQ[x,y]
        D = diffOp {x => x+y, x*y^2 => 3+x}
        (x^2+3) * D
        D + D
        D(x^5*y^2)
    Text
        Instances of {\tt DiffOp} are @TO2 {HashTable, "hash tables"}@, where keys are differential monomials
        (represented as monomials in $R$), and values are the corresponding coefficients. A useful shortcut for
        creating instances of {\tt DiffOp} is to use a @TO WeylAlgebra@.

    Example
        needsPackage "Dmodules"
        S = makeWA R
        E = diffOp(y*dx - x*dy^2)
SeeAlso
    (diffOp, HashTable)
    (diffOp, RingElement)
///

doc ///
Key
    diffOp
Headline
    create a differential operator
///

doc ///
Key
    (diffOp, HashTable)
    (diffOp, List)
    (NewFromMethod, DiffOp, HashTable)
    (NewFromMethod, DiffOp, List)
Headline
    create a differential operator
Usage
    diffOp H
Inputs
    H:HashTable
        or a @TO2 {List, "list"}@ of pairs {\tt mon => coef}
Outputs
    :DiffOp
Description
    Text
        The @TO HashTable@ {\tt H} should contain monomials as keys and polynomials as values, all
        of which should lie in the same ring. The keys represent monomials of each term (in $dx$ variables),
        and the value represent the coefficient.
    Example
        R = QQ[x,y]
        H = new HashTable from {x^2 => x+y+3, y^2*x^5 => 2*x}
        D1 = diffOp H
    Text
        Alternatively, {\tt diffOp} can also create differential operators from lists of {\tt key => value} pairs
    Example
        D2 = diffOp {x^2 => x+y+3, y^2*x^5 => 2*x}
        D1 == D2
    Text
        For a simpler way of creating differential operators, see @TO (diffOp, RingElement)@.
Caveat
    The constructors @TO (NewFromMethod, DiffOp, HashTable)@ and @TO (NewFromMethod, DiffOp, List)@ are for internal use only. Use @TO (diffOp, HashTable)@ and @TO (diffOp, List)@ instead.

SeeAlso
    DiffOp
    (diffOp, RingElement)
///

doc ///
Key
    (diffOp, Ring, RingElement)
    (diffOp, RingElement, Ring)
    (diffOp, RingElement)
Headline
    create a differential operator from a Weyl algebra element
Usage
    diffOp_R f
    diffOp(f, R)
    diffOp f
Inputs
    R:Ring
    f:RingElement
        of a @TO WeylAlgebra@ of {\tt R}
Outputs
    :DiffOp
Consequences
    Item
        if called without a specified ring (i.e. {\tt diffOp f}), creates a new ring by discarding the $dx$-variables and caches it in the Weyl algebra
    Item
        if such a ring had been previously cached, the cached ring will be used.
Description
    Text
        Creates a differential operator of the ring {\tt R} from an element {\tt f} of a Weyl algebra of {\tt R}
    Example
        needsPackage "Dmodules"
        R = QQ[x,y]
        S = makeWA R
        D = diffOp_R(x^2 * dx + y^2 * dy^2*dx)
        ring D === R
    Text
        The ring does not have to be specified. Note that in this case, the resulting operator will not be a differential
        operator of {\tt R}, but that of a new ring. This ring is cached, so subsequent calls will result in operators of the same ring.
    Example
        E = diffOp(x^2* dx)
        ring E === R
        F = diffOp(dy^2)
        ring E === ring F
SeeAlso
    DiffOp
    (diffOp, HashTable)

///

doc ///
Key
    (symbol +, DiffOp, DiffOp)
Headline
    addition of differential operators
Usage
    D + E
Inputs
    D:DiffOp
    E:DiffOp
Outputs
    :DiffOp
Description
    Text
        Adds two differential operators. The ring of both operators must match
    Example
        R = QQ[x,y];
        D = diffOp{x => x^2+ y}
        E = diffOp{x => -y, x^2*y^2 => 2}
        D + E
///

doc ///
Key
    (symbol -, DiffOp, DiffOp)
Headline
    subtraction of differential operators
Usage
    D - E
Inputs
    D:DiffOp
    E:DiffOp
Outputs
    :DiffOp
Description
    Text
        Subtracts two differential operators. The ring of both operators must match
    Example
        R = QQ[x,y];
        D = diffOp{x => x^2+ y}
        E = diffOp{x => -y, x^2*y^2 => 2}
        D - E
///


doc ///
Key
    (symbol -, DiffOp)
Headline
    negation of differential operators
Usage
    - D
Inputs
    D:DiffOp
Outputs
    :DiffOp
Description
    Text
        Corresponds to $(-1)*D$
    Example
        R = QQ[x,y];
        D = diffOp{x => x^2+ y}
        - D
///

doc ///
Key
    (symbol *, RingElement, DiffOp)
    (symbol *, Number, DiffOp)
Headline
    scaling of differential operators
Usage
    c*D
Inputs
    c:RingElement
        in the same ring as {\tt D}, or a @TO Number@

    D:DiffOp
Outputs
    :DiffOp
Description
    Text
        Multiplies every coefficient of {\tt D} by {\tt c}
    Example
        R = QQ[x,y];
        D = diffOp{x => x^2+ y, y^2 => 1}
        y*D
        2*D
///

doc ///
Key
    (symbol ?, DiffOp, DiffOp)
    (symbol ==, DiffOp, DiffOp)
    (symbol ==, DiffOp, ZZ)
    (symbol ==, ZZ, DiffOp)
Headline
    comparison of differential operators
Usage
    D ? E
    D == E
    D == 0
Inputs
    D:DiffOp
    E:DiffOp
Description
    Text
        The ordering of DiffOps a product ordering of the undelying ring, where the $dx$ monomoial are compared
        first, and ties are broken with coefficients.
    Example
        R = QQ[x,y, MonomialOrder => Lex]
        D1 = diffOp{x^2 => y, y => x^2}
        D2 = diffOp{y => y^2}
        D3 = diffOp{x^2 => y, y => x^2 + y^2}
        D1 ? D1
        D1 ? D2
        D1 ? D3
        D1 + D2 == D3
        D1 + D2 - D3 == 0
///

doc ///
Key
    (ring, DiffOp)
Headline
    get the ring associated to a differential operator
Usage
    ring D
Inputs
    D:DiffOp
Outputs
    :Ring
Description
    Text
        Returns the ring in which the coefficients lie
    Example
        R = QQ[x]
        D = diffOp{x => 2*x^2}
        ring D
Caveat
    If the @TO DiffOp@ was created using @TO (diffOp, RingElement)@ (e.g. after running @TO makeWA@),
    the ring
///

doc ///
Key
    (symbol SPACE, DiffOp, RingElement)
Headline
    apply a differential operator
Usage
    D f
Inputs
    D:DiffOp
    f:RingElement
        in the same ring as {\tt D}
Outputs
    :RingElement
        in the same ring as {\tt f}
Description
    Text
        The differential operators of the ring $R = \mathbb{F}[x_1,\dots,x_n]$ act naturally on elements of $R$.
        The operator $dx_i$ acts as a partial derivarive with respect to $x_i$, and a polynomial acts by multiplication.
    Example
        R = QQ[x,y]
        dx = diffOp{x^2 => 1}
        D = diffOp{1_R => x^2 + y^2}
        dx(x^4 + x^3 + y)
        D(x^2 - y^2)
///

doc ///
Key
    (substitute, DiffOp, Ring)
Headline
    change the ring of a differential operator
Usage
    substitute(D, R)
    sub(D, R)
Inputs
    D: DiffOp
    R: Ring
Outputs
    :DiffOp
Description
    Text
        Attempts to change the underlying ring of the differential operator by calling @TO substitute@
        for each monomial and coefficient of {\tt D}
    Example
        R = QQ[x,y]
        D = diffOp{1_R => x^2, x^2 => y^2}
        S = QQ[x,y,z]
        DS = sub(D,S)
        ring DS === S
///


doc ///
Key
    normalize
    (normalize, DiffOp)
Headline
    rescale a differential operator to a canonical form
Usage
    normalize D
Inputs
    D:DiffOp
Outputs
    :DiffOp
Description
    Text
        Rescales a differential operator so that the leading term of the leading coefficient is 1.
    Example
        R = QQ[x,y,t];
        D = diffOp{x^2*t => 3*x^3 + 2*y, t^2 => x+y}
        normalize D
    Text
        This can be useful when computing "canonical" sets of Noetherian operators,
        as a valid set of Noetherian operators stays valid even after rescaling.
    Example
        I = ideal(x^2,y^2 - x*t);
        nops = noetherianOperators(I, Strategy => "MacaulayMatrix");
        nops // sort / normalize == {diffOp{1_R => 1}, diffOp{y => 1}, diffOp{y^2 => t, x => 2}, diffOp{y^3 => t, x*y => 6}}
///


doc ///
Key
    ZeroDiffOp
    (NewFromMethod, ZeroDiffOp, Ring)
    (symbol ==, ZZ, ZeroDiffOp)
    (symbol ==, ZeroDiffOp, ZZ)
Headline
    the zero differential operator of a ring
Description
    Text
        For internal use. A type of @TO DiffOp@ with a single key {\tt 1} and value 0.
        Users are not expected to create instances of {\tt ZeroDiffOp}, they are created automatically by @TO diffOp@ when necessary.
    Example
        R = QQ[x,y]
        D = diffOp{x => 0};
        instance(D, ZeroDiffOp)
        peek D
    Text
        Comparison to the integer 0 works as expected
    Example
        E = diffOp{1_R => 0}
        E == 0
SeeAlso
    diffOp

///

doc ///
Key
    InterpolatedDiffOp
    (NewFromMethod, InterpolatedDiffOp, HashTable)
    (NewFromMethod, InterpolatedDiffOp, List)
Headline
    differential operator with interpolated coefficients
Description
    Text
        A type of @TO DiffOp@ returned by interpolation based methods, such as @TO numericalNoetherianOperators@.
        Because of this, users are not expected to create instances of this type.

        If the interpolation of any coefficient fails, the numerator and denominator will be replaced by the @TO2 {String, "string"}@ {\tt "?"}.

        Assuming the interpolation was successful, the method @TO (evaluate, InterpolatedDiffOp, Point)@ will convert
        an @TO InterpolatedDiffOp@ to a specialized differential operators of type @TO DiffOp@ by evaluating each numerator and denominator at a point.
    Example
        R = CC[x,y]
        D = new InterpolatedDiffOp from {x => (x+y, pi*ii*x^2*y), y => (x, 1.3_R)}
        D' = evaluate(D, point{{1.2, 2+ii}})
SeeAlso
    (evaluate, InterpolatedDiffOp, Point)

///



doc ///
Key
    (noetherianOperators, Ideal)
Headline
    Noetherian operators of a primary ideal
Usage
    noetherianOperators Q
    noetherianOperators (Q, Strategy => "PunctualHilbert")
Inputs
    Q:Ideal
        assumed to be primary
Outputs
    :List
        of @TO2{DiffOp, "differential operators"}@
Description
    Text
        Compute a set of Noetherian operators for the primary ideal I.
    Example
        R = QQ[x,y,t];
        I = ideal(x^2, y^2-x*t);
        noetherianOperators I
    Text
        The optional argument {\tt Strategy} can be used to choose different algorithms. Each strategy may accept additional optional arguments, see the documentation page for each strategy for details.

        @UL{
            TO2 {"Strategy => \"PunctualHilbert\"", "\"PunctualHilbert\" (default)"},
            TO2 {"Strategy => \"MacaulayMatrix\"", "\"MacaulayMatrix\""},
            TO2 {"Strategy => \"Hybrid\"", "\"Hybrid\""},
        }@
Caveat
    The behavior is undefined if {\tt Q} is not primary.
    For non-primary ideals, use @TO (noetherianOperators, Ideal, Ideal)@
///

doc ///
Key
    (noetherianOperators, Ideal, Ideal)
    Rational
Headline
    Noetherian operators of a primary component
Usage
    noetherianOperators (I, P)
    noetherianOperators (I, P, Strategy => "MacaulayMatrix")
    noetherianOperators (I, P, Rational => false)
Inputs
    I:Ideal
        assumed to be unmixed
    P:Ideal
        a minimal prime of $I$
Outputs
    :List
        of @TO2{DiffOp, "differential operators"}@
Description
    Text
        Compute a set of Noetherian operators for $P$-primary component of $I$.
    Example
        R = QQ[x,y,t];
        I1 = ideal(x^2, y^2-x*t);
        I2 = ideal((x-t)^2);
        I = intersect(I1, I2);
        noetherianOperators(I, radical I1)
        noetherianOperators(I, radical I2) == noetherianOperators(I2)
    Text
        The optional argument {\tt Strategy} can be used to choose different algorithms. Each strategy may accept additional optional arguments, see the documentation page for each strategy for details.

        @UL{
            TO2 {"Strategy => \"MacaulayMatrix\"", "\"MacaulayMatrix\" (default)"},
            TO2 {"Strategy => \"Hybrid\"", "\"Hybrid\""},
        }@
    Text
        If the prime $P$ is known to be a ration point, the optional argument {\tt Rational} can be set to true.  This may offer a speed up in the computation.
SeeAlso
    noetherianOperators
    (noetherianOperators, Ideal)
///


doc ///
Key
    noetherianOperators
Headline
    Noetherian operators
Description
    Text
        Let $R$ be a polynomial ring $R = K[x_1,\ldots,x_n]$ over a field $K$ of characteristic zero. 
        Consider the Weyl algebra $D = R<dx_1,\ldots,dx_n>$, 
        a prime ideal $P \subset R$ and a $P$-primary ideal.
        When this method is applied we obtain a finite list of 
        differential operators $L_1,\ldots,L_m \in D$ such that 
        $$
        Q = \{ f \,\in\, R\, \mid\, L_i\, \bullet\, f\, \in P \, \forall  1 \le i \le m \}.
        $$
        We say that $\{L_1,\ldots,L_m\}$ is a set Noetherian operators for the primary ideal $Q$.
        In the output of the algorithm we always have that $m$ (the number of Noetherian operators) is equal to the 
        multiplicity of $Q$ over the prime ideal $P$.

    Example
        R=QQ[x_1,x_2,x_3,x_4]
        Q = ideal(x_1^2,x_1*x_2,x_1*x_3,x_1*x_4-x_3^2+x_1,x_3^2*x_4-x_2^2,x_3^2*x_4-x_3^2-x_2*x_3+2*x_1)
        isPrimary Q
        noetherianOperators(Q, Strategy => "PunctualHilbert")
///

doc ///
Key
    "Strategy => \"MacaulayMatrix\""
    IntegralStrategy
    KernelStrategy
Headline
    strategy for computing Noetherian operators
Description
    Text
        This strategy implements Algorithm 2 in @ HREF("https://arxiv.org/abs/2006.13881", "Noetherian Operators and Primary Decomposition")@,
        and supports computing Noetherian operators of either primary ideals (@TO (noetherianOperators, Ideal)@), or primary components
        of unmixed ideals (@TO (noetherianOperators, Ideal, Ideal)@).

        The strategy relies on computing the kernel of successively larger Macaulay matrices. 
        The behavior can be controlled with optional arguments:

        {\tt DegreeLimit => ...}: takes an integer $d$, and stops computation at degree $d$. Note that if $d$ is set too low, this may lead to an incomplete answer. If unset, stops computation when the dimension of the kernel stabilizes.

    Example
        R = QQ[x,y,z];
        I = (ideal(x,y,z))^3;
        noetherianOperators(I, Strategy => "MacaulayMatrix")
        noetherianOperators(I, Strategy => "MacaulayMatrix", DegreeLimit => 1)

    Text
        {\tt KernelStrategy => ...}: takes a string {\tt "Default"} or {\tt "Gaussian"}. The {\tt "Default"} strategy uses the Macaulay2 builtin function @TO kernel@
        to compute kernels (via Grobner bases). The strategy {\tt "Gaussian"} computes kernels directly via a Gaussian reduction, and may offer performance improvements compared to {\tt "Default"}.

        {\tt IntegralStrategy => ...}: takes a boolean value. If {\tt true}, uses the Mourrain algorithm to compute the kernel of the MacaulayMatrix, which constructs columns
        of the Macaulay matrix by taking integrals of the columns in the previous step. If {\tt false}, uses the method outlined in Algorithm 1 in @ HREF("https://arxiv.org/abs/2006.13881", "Noetherian Operators and Primary Decomposition")@.
        If unset, will choose automatically. See: B. Mourrain. Isolated points, duality and residues. @EM "J. Pure Appl. Algebra"@, 117/118:469-493, 1997. 
           Algorithms for algebra (Eindhoven, 1996).

        {\tt DependentSet => ...}: takes a list of variables. For details, see @TO DependentSet@.

SeeAlso
    "Strategy => \"Hybrid\""
    "Strategy => \"PunctualHilbert\""
    DependentSet
///

doc ///
Key
    "Strategy => \"Hybrid\""
Headline
    strategy for computing Noetherian operators
Description
    Text
        This strategy implements a numerical-symbolic hybrid algorithm for computing Noetherian operators. The output is symbolic.
        {\tt "Hybrid"} supports computing Noetherian operators of either primary ideals (@TO (noetherianOperators, Ideal)@), or primary components
        of unmixed ideals (@TO (noetherianOperators, Ideal, Ideal)@).

        The {\tt "Hybrid"} strategy finds a point on the variety of the component of interest, and computes a set of
        specialized Noetherian operators (see @TO specializedNoetherianOperators@). Using this numerical data is then used
        as a starting point for the symbolic computation of Noetherian operators, which in many cases lead to significant performance
        improvements over the fully symbolic methods.

        The strategy accepts the following optional arguments:

        {\tt Sampler => f}, where {\tt f} is a function taking a primary ideal and returning a single point on the variety.
        The default sampler uses a combination of @TO bertiniSample@ and @TO bertiniPosDimSolve@.
        The user can supply a point to used by using a dummy sampler, as in the example below:
    Example
        R = QQ[x,y,t];
        I = ideal(x^2, y^2-x*t);
        p = point{{0_CC,0, 3}};
        noetherianOperators(I, Strategy => "Hybrid", Sampler => I -> p)

    Text
        {\tt Tolerance =>} a positive real number. This specifies the numerical precision when computing the
        specialized Noetherian operators. The default value is {\tt 1e-6}. See See @TO "Tolerance (NoetherianOperators)"@.

        {\tt DependentSet =>} a list of variables. For details, see @TO DependentSet@.
SeeAlso
    "Strategy => \"PunctualHilbert\""
    "Strategy => \"MacaulayMatrix\""
///

doc ///
Key
    "Strategy => \"PunctualHilbert\""
Headline
    strategy for computing Noetherian operators
Description
    Text
        This strategy implements Algorithm 3.8 in @ HREF("https://arxiv.org/abs/2001.04700", "Primary ideals and their differential equations")@.

        The following example deals with a rather non-trivial primary ideal to show the capabilities
        of this strategy.

    Example
        R = QQ[x_1,x_2,x_3,x_4]
        k = 3
        J = ideal((x_1^2-x_2*x_3)^k,(x_1*x_2-x_3*x_4)^k,(x_2^2-x_1*x_4)^k)
        Q = saturate(J,ideal(x_1*x_2*x_3*x_4))
        isPrimary Q
        elapsedTime noetherianOperators(Q, Strategy => "PunctualHilbert")
SeeAlso
    mapToPunctualHilbertScheme
    "Strategy => \"MacaulayMatrix\""
    "Strategy => \"Hybrid\""
///



doc ///
Key
    mapToPunctualHilbertScheme
    (mapToPunctualHilbertScheme, Ideal)
Headline
    maps an ideal into a point in a certain punctual Hilbert scheme
Usage
    I = mapToPunctualHilbertScheme(Q)
Inputs
    Q : Ideal
        a primary ideal
Outputs
    I : Ideal
        an ideal that parametrizes Q in a punctual Hilbert scheme
Description
    Text
        This method maps a P-primary ideal Q into a point in a punctual Hilbert scheme.
	Let $\mathbb{K}$ be a characteristic zero and a prime ideal $P$ of codimension $c$ in the polynomial ring $R = \mathbb{K}[x_1,\ldots,x_n]$.
	We write $\mathbb{F}$ for the field of fractions of the integral domain $R/P$. 
	To simplify our notation, perhaps after a linear change of coordinates, we assume that $\{ x_{c+1}, \ldots, x_n \}$ is a maximal independent set of variables module $P$.
    	
	The main purpose of this method is to reduce the study of arbitrary $P$-primary ideals in $R = \mathbb{K}[x_1,\ldots,x_n]$ to a zero-dimensional setting over the function field $\mathbb{F}$.
	This reduction is made by parametrizing $P$-primary ideals with the punctual Hilbert scheme 
	$
 	{\rm Hilb}^m ( \,\mathbb{F}[[y_1,\ldots,y_c]] \,). 
	$
	This is a quasiprojective scheme over the function field $\mathbb{F}$.
	Its classical points are   ideals of colength $m$ in the local ring $\mathbb{F}[[y_1,\ldots,y_c]]$.
	
    	
	This method maps a P-primary ideal Q into a unique point in ${\rm Hilb}^m ( \,\mathbb{F}[[y_1,\ldots,y_c]] \,)$ that corresponds with Q.
	This method can be seen as an implementation of the map $\gamma$ described in Section 2 of @ HREF("https://arxiv.org/abs/2001.04700", "Primary ideals and their differential equations")@. 

    Example
    	R = QQ[x_1, x_2, x_3]
        Q = ideal(x_1^2, x_2^2, x_1-x_2*x_3)
	mapToPunctualHilbertScheme Q
SeeAlso
    "Strategy => \"PunctualHilbert\""
///



doc ///
Key
    specializedNoetherianOperators
    (specializedNoetherianOperators, Ideal, Point)
    (specializedNoetherianOperators, Ideal, Matrix)
Headline
    Noetherian operators evaluated at a point
Usage
    specializedNoetherianOperators(I, pt)
Inputs
    I:Ideal
        unmixed
    pt:Point
        or a row @TO2 {Matrix, "matrix"}@
Outputs
    :List
        of @TO2 {DiffOp, "differential operators"}@
Description
    Text
        Numerically computes evaluations of Noetherian operators. If the point {\tt p} lies on the variety of the
        minimal prime $P$, the function returns a set of specialized Noetherian operators of the $P$-primary component.
        The option @TO DependentSet@ is required when dealing with ideals over numerical fields, or when dealing with non-primary ideals
    Example
        R = QQ[x,y,t];
        Q1 = ideal(x^2, y^2 + x*t);
        Q2 = ideal((x+t)^2);
        I = intersect(Q1, Q2);
        P = radical Q1;
        pt = point{{0,0,2}};
        A = specializedNoetherianOperators(I, pt, DependentSet => {x,y}) / normalize
        B = noetherianOperators(I, P) / (D -> evaluate(D, pt)) / normalize
        A == B
    Text
        Over a non-exact field, the output will be non-exact
    Example
        S = CC[x,y,t]
        pt = point{{0,0,2.1}}
        specializedNoetherianOperators(sub(I, S), pt, DependentSet => {x,y})

Caveat
    It is assumed that the point lies on the variety of {\tt I}
///


doc ///
Key
    numericalNoetherianOperators
    (numericalNoetherianOperators, Ideal)
    InterpolationDegreeLimit
    InterpolationTolerance
    NoetherianDegreeLimit
    TrustedPoint
Headline
    Noetherian operators via numerical interpolation
Usage
    numericalNoetherianOperators(I)
Inputs
    I:Ideal
        unmixed
Outputs
    :List
        of @TO2 {InterpolatedDiffOp, "interpolated differential operators"}@
Description
    Text
        The method computes specialized Noetherian operators from many sampled points, and attempts to find fitting rational functions
        using rational function interpolation.
    CannedExample
        i1 : R = CC[x,y,t];

        i2 : I = ideal(x^2, y^2 - x*t);

        o2 : Ideal of R

        i3 : numericalNoetherianOperators(I, DependentSet => {x,y})

                         2    1      t   3
        o3 = {1, 1*dy, dy  + ---*dx, -*dy  + 1*dx*dy}
                             .5t     6

        o3 : List


    Text
        The behavior of the function can be adjusted using options. Currently only the option
        @TO DependentSet@ is required. The following are supported:
        

        {\tt TrustedPoint =>} a @TO Point@. The function does not compute specialized Noetherian operators from scratch for each point.
        Instead, it computes it for a "trusted" point on the variety, and uses the obtained Noetherian operators
        as a template for the rest of the computation. If {\tt TrustedPoint} is unset, the first point returned by the sampler
        will be used as the trusted point.

        {\tt NoetherianDegreeLimit =>} a non-negative @TO2 {ZZ, "integer"}@. Limits the degrees of the Noetherian operators (with respect to the $dx$ variables).
            If unset, will compute the full Noetherian operators of the "trusted" point. Can introduce speedups when the maxmial degree of the Noetherian operators
            is known in advance.

        {\tt Tolerance =>} a positive real number. This specifies the numerical precision when computing the
        specialized Noetherian operators. The default value is {\tt 1e-6}. See @TO "Tolerance (NoetherianOperators)"@.
        
        {\tt Sampler =>} a function, taking inputs \{tt (I,n)}, where {\tt I} is an @TO2{Ideal, "ideal"}@, and {\tt n} is an integer.
        The sampler function returns a list of {\tt n} @TO2 {Point, "points"}@ on the component of interest of {\tt I}. If unset, the default sampler
        uses @TO Bertini@, and assumes that {\tt I} is primary.

        {\tt DependentSet =>} a @TO2 {List, "list"}@ of variables that are algebraically dependent. See @TO DependentSet@ for details.
        
        {\tt InterpolationTolerance =>} a positive real number. This specifies the numerical precision for the interpolation routines.
        The default value is {\tt 1e-6}. See @TO "Tolerance (NoetherianOperators)"@.

        {\tt InterpolationDegreeLimit =>} a non-negative @TO2 {ZZ, "integer"}@. Limits the degree of the interpolated rational function coefficients.
        If no rational functions are found within the degree limit, outputs an incomplete differential operator
    CannedExample
        i1 : R = CC[x,y,t];

        i2 : I = ideal(x^2, y^2 - x*t);

        o2 : Ideal of R

        i3 : numericalNoetherianOperators(I, DependentSet => {x,y}, InterpolationDegreeLimit => 0)
        
                           2   ?     ?   3
        o3 = {1, 1*dy, 1*dy  + -*dx, -*dy  + 1*dx*dy}
                               ?     ?

        o3 : List

SeeAlso
    rationalInterpolation
///


doc ///
Key
    rationalInterpolation
    (rationalInterpolation, List, List, Matrix, Matrix)
    (rationalInterpolation, List, List, Matrix)
Headline
    numerically interpolate rational functions
Usage
    (n,d) = rationalInterpolation(pts, vals, numBasis, denBasis)
    (n,d) = rationalInterpolation(pts, vals, numDenBasis)
Inputs
    pts:List
        of row matrices corresponding to points at which the rational function was evaluated
    vals:List
        of @TO2 {Number, "numbers"}@ corresponding to evaluations of the rational function
    numBasis:Matrix
        a row of monomials, the monomial support of the numerator
    denBasis:Matrix
        a row of monomials, the monomial support of the numerator
    numDenBasis:Matrix
        a row of monomials, the monomial support of both the numerator and denominator
    Tolerance => RR
        default value {\tt 1e-6}
Outputs
    n:RingElement
        the numerator of the rational function
    d:RingElement
        the denominator of the rational function
Description
    Text
        Given a set of points $pts = \{p_1,\dots,p_k\}$ and values $vals = \{v_1,\dots,v_k\}$, attempts to find a rational function
        $f = g/h$, such that $f(p_i) = v_i$. The polynomials $g$ and $h$ have monomial support {\tt numBasis} and {\tt denBasis} respectively.
    Example
        R = CC[x,y]

        pts = {point{{1,0}}, point{{0,1}}, point{{1,1}}, point{{-1,-1}}, point{{-1,0}}}
        vals = {1, 0, 1/2, -1/2, -1}
        numBasis = matrix{{x,y}}
        denBasis = matrix{{x^2,y^2}}
        rationalInterpolation(pts, vals, numBasis, denBasis)
    Text
        The output corresponds to the function $x / (x^2 + y^2)$. If no fitting rational function is found, the method returns an error.

        The method @TO (rationalInterpolation, List, List, Ring)@ can be used to choose monomial supports automatically.
Caveat
    The method uses the first point to remove $0/0$ rational functions. Because of this, the first entry of {\tt val} should be non-zero.
SeeAlso
    (rationalInterpolation, List, List, Ring)
///

doc ///
Key
    (rationalInterpolation, List, List, Ring)
Headline
    numerically interpolate rational functions
Usage
    (n,d) = rationalInterpolation(pts, vals, R)
Inputs
    pts:List
        of row matrices corresponding to points at which the rational function was evaluated
    vals:List
        of @TO2 {Number, "numbers"}@ corresponding to evaluations of the rational function
    R:Ring
        the polynomial ring in which the numerator and denominator are sought
Outputs
    n:RingElement
        the numerator of the rational function
    d:RingElement
        the denominator of the rational function
Description
    Text
        Given a set of points $pts = \{p_1,\dots,p_k\}$ and values $vals = \{v_1,\dots,v_k\}$, attempts to find a rational function
        $f = g/h$, such that $f(p_i) = v_i$. The method first tries to find polynomials $g,h$ of degree 0;
        if this fails, it tries to find $g,h$ of degree 1 and so on. This procedure stops when there are not
        enough points to compute the next degree, in which case an error will be thrown.
    Example
        R = CC[x]
        pts = {point{{0}},point{{1}},point{{2}}, point{{3}}, point{{4}}}
        vals = {-1, 1/2, 1, 5/4, 7/5}
        rationalInterpolation(pts, vals, R)
SeeAlso
    (rationalInterpolation, List, List, Matrix, Matrix)
///

doc ///
Key
    (evaluate, DiffOp, Point)
    (evaluate, DiffOp, Matrix)
    (evaluate, InterpolatedDiffOp, Point)
    (evaluate, InterpolatedDiffOp, Matrix)
Headline
    evaluate coefficients of a differential operator
Usage
    evaluate(D, p)
Inputs
    D:DiffOp
    p:Point
        or a row @TO2 {Matrix, "matrix"}@
Outputs
    :DiffOp
Description
    Text
        Evaluates a @TO2 {DiffOp, "differential operator"}@ at a point. This can be used to obtain a set of specialized Noetherian operators.
    Example
        R = QQ[x,y];
        D = diffOp{x => x, y => y}
        evaluate(D, point{{1,2}})
    Text
        The method supports @TO2 {InterpolatedDiffOp, "interpolated differential operators"}@ as well, assuming that the denominator does not vanish.
        The resulting operator is a @TO DiffOp@, not an @TO InterpolatedDiffOp@.
    Example
        E = new InterpolatedDiffOp from {x => (x, x^2 + y^2)}
        evaluate(E, point{{1,2}})
///

doc ///
Key
    DependentSet
Headline
    option for computing Noetherian operators
Description
    Text
        Let $Q \subseteq R := \mathbb{K}[x_1,\dots,x_n]$ be an $d$-dimensional primary ideal.
        Then there exists a set of $d$ variables in $R$ which is algebraically independent in $R/I$.
        We refer to these as the independent variables, and the remaining variables are the dependent variables.
        The function @TO independentSets@ can compute sets of independent variables for symbolic ideals.

        The functions computing Noetherian operators, namely

        @UL { TO noetherianOperators, TO specializedNoetherianOperators, TO numericalNoetherianOperators} @

        pass to a polynomial ring in the dependent variables, with the coefficient field being the fraction field
        of a polynomial ring in the independent varaibles. Because of this, computing Noetherian operators requires
        a knowledge of a dependent set of variables, which can be set using the option {\tt DependentSet}. Note that
        the $dx$-monomials will only involve dependent variables.
    Example
        R = QQ[x,y];
        I = ideal((x+y)^2);
        P = radical I;
        A = noetherianOperators(I, P, DependentSet => {x})
        B = noetherianOperators(I, P, DependentSet => {y})
        getIdealFromNoetherianOperators(A, P) == getIdealFromNoetherianOperators(B, P)

    Text
        The symbolic method @TO noetherianOperators@ will usually be able to figure out a dependent set of variables
        automatically. On the other hand, numerical computations using @TO specializedNoetherianOperators@ and 
        @TO numericalNoetherianOperators@ will usually require the user to set the option {\tt DependentSet}.

Caveat
    The option {\tt DependentSet} is ignored when calling @TO noetherianOperators@ with @TO "Strategy => \"PunctualHilbert\""@.
    Note that this is the default strategy for @TO (noetherianOperators, Ideal)@.

SeeAlso
    noetherianOperators
    specializedNoetherianOperators
    numericalNoetherianOperators
///

doc ///
Key
    Sampler
Headline
    optional sampler function
Description
    Text
        There are currently two methods that accept a user-specified sampler function

        @UL {
            {TO "Strategy => \"Hybrid\"", " used with ", TO noetherianOperators},
            {TO numericalNoetherianOperators}
        }@

        See the respective documentation nodes for details. If the option {\tt Sampler} is unset,
        the default sampler will use @TO Bertini@.
SeeAlso
    "Strategy => \"Hybrid\""
    numericalNoetherianOperators
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
        of @TO2 {DiffOp, "differential operators"}@
    P : Ideal
        a prime ideal
Outputs
    Q : Ideal
        the primary ideal corresponding to L and P
Description
    Text
        This method contains an implementation of Algorithm 3.9 in @ HREF("https://arxiv.org/abs/2001.04700", "Primary ideals and their differential equations")@. 
        Applying this algorithm can be seen as making the backward process of computing a set of Noetherian operators.  

        Let $R$ be a polynomial ring $R = K[x_1,\ldots,x_n]$ over a field $K$ of characteristic zero. 
        Consider the Weyl algebra $D = R<dx_1,\ldots,dx_n>$ and a list of differential operators $L = \{L_1,\ldots,L_m\} \,\subset\, D$.
        Denote by $\mathcal{E} \,\subset\, D$ the $R$-bisubmodule of $D$ that is generated by $L_1,\ldots,L_m$.
        For a given prime ideal $P \,\subset\, R$, this method computes the $P$-primary ideal given as 
        $$
        Q = \{ f \,\in\, R\, \mid\, \delta\, \bullet\, f\, \in P \, \forall  \delta \in \mathcal{E} \}.
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
        L1 = noetherianOperators(Q) -- A set of Noetherian operators
        Q1 = getIdealFromNoetherianOperators(L1, P);
        Q == Q1
        L2 = noetherianOperators(M) -- Another set of Noetherian operators
        Q2 = getIdealFromNoetherianOperators(L2, P);
        Q == Q2
    Text
        The following example was given as the running example in the Introduction of @ HREF("https://arxiv.org/abs/2001.04700", "Primary ideals and their differential equations")@.
    Example
        Q = ideal(3*x_1^2*x_2^2-x_2^3*x_3-x_1^3*x_4-3*x_1*x_2*x_3*x_4+2*x_3^2*x_4^2,3*x_1^3*x_2*x_4-3*x_1*x_2^2*x_3*x_4-3*x_1^2*x_3*x_4^2+3*x_2*x_3^2*x_4^2+2*x_2^3-2*x_3*x_4^2,3*x_2^4*x_3-6*x_1*x_2^2*x_3*x_4+3*x_1^2*x_3*x_4^2+x_2^3-x_3*x_4^2,4*x_1*x_2^3*x_3+x_1^4*x_4-6*x_1^2*x_2*x_3*x_4-3*x_2^2*x_3^2*x_4+4*x_1*x_3^2*x_4^2,x_2^5-x_1*x_2^3*x_4-x_2^2*x_3*x_4^2+x_1*x_3*x_4^3,x_1*x_2^4-x_2^3*x_3*x_4-x_1*x_2*x_3*x_4^2+x_3^2*x_4^3,x_1^4*x_2-x_2^3*x_3^2-2*x_1^3*x_3*x_4+2*x_1*x_2*x_3^2*x_4,x_1^5-4*x_1^3*x_2*x_3+3*x_1*x_2^2*x_3^2+2*x_1^2*x_3^2*x_4-2*x_2*x_3^3*x_4,3*x_1^4*x_3*x_4-6*x_1^2*x_2*x_3^2*x_4+3*x_2^2*x_3^3*x_4+2*x_1^3*x_2+6*x_1*x_2^2*x_3-6*x_1^2*x_3*x_4-2*x_2*x_3^2*x_4,4*x_2^3*x_3^3+4*x_1^3*x_3^2*x_4-12*x_1*x_2*x_3^3*x_4+4*x_3^4*x_4^2-x_1^4+6*x_1^2*x_2*x_3+3*x_2^2*x_3^2-8*x_1*x_3^2*x_4)
        L = noetherianOperators(Q)
        Q' = getIdealFromNoetherianOperators(L, P);
        Q == Q'  
    Text
        The next example was given by Palamodov to show that there exists primary ideals that cannot be described by using differential operators with constant coefficients.       
    Example
        R = QQ[x_1, x_2, x_3]
        Q = ideal(x_1^2, x_2^2, x_1-x_2*x_3)
        L = noetherianOperators(Q)
        Q' = getIdealFromNoetherianOperators(L,radical Q)
        Q == Q'
    Text 
        For the last example we consider an ideal defined by using the join construction.
    Example
        R = QQ[x_1..x_9]
        MM = genericMatrix(R, 3, 3)
        P = minors(2, MM)
        M = ideal(x_1^2, x_5^2, x_9^2, x_2, x_3, x_4, x_6, x_7, x_8)
        Q = joinIdeals(P, M)
        L = noetherianOperators(Q) 
        Q' = getIdealFromNoetherianOperators(L, radical Q) 
        Q == Q'
///

doc ///
Key
    coordinateChangeOps
    (coordinateChangeOps, Matrix, DiffOp)
    (coordinateChangeOps, Matrix, List)
    (coordinateChangeOps, RingMap, DiffOp)
    (coordinateChangeOps, RingMap, List)
Headline
    induced Noetherian operators under coordinate change
Usage
    coordinateChangeOps(phi, D)
    coordinateChangeOps(phi, L)
Inputs
    phi:Matrix
        or @TO RingMap@
    D:DiffOp
    L:List
        of @TO DiffOp@s
Outputs
    :DiffOp
        (resp. a list of differential operators)
Description
    Text
        Let $I$ be an ideal in a polynomial ring $K[x_1, ..., x_n]$, and $\phi \in GL_n(K)$ a 
        matrix representing a $K$-linear automorphism of $R$. Then there is an automorphism 
        $\psi$ of the Weyl algebra $K[x_i, dx_i]$ such that if $D_1, ..., D_r$ is a set of Noetherian
        operators for $I$ then $\psi(D_1), ..., \psi(D_r)$ is a set of Noetherian operators for 
        $\phi(I)$. This function computes the induced operators for a given $\phi$. The action 
        of $\psi$ on polynomial variables $x_i$ is given by $\phi$, while the action of $\psi$ on
        differential variables $dx_i$ is given by the inverse transpose of $\phi$.
    Example
        R = QQ[x,y,t]
        I = ideal(x^2, y^2 - x*t)
        P = radical I
        N = noetherianOperators I
        phi = map(R, R, diagonalMatrix apply(numgens R, i -> random QQ))
        N' = coordinateChangeOps_phi N
        I' = phi I
        P' = phi P
        I' == getIdealFromNoetherianOperators(N', P')
SeeAlso
    noetherianOperators
///

doc ///
Key
    noethOpsFromComponents
    (noethOpsFromComponents, List)
Headline
    merge Noetherian operators for non-primary ideals
Usage
    noethOpsFromComponents L
Inputs
    L:List
        of ordered pairs (P, N) where P is a minimal prime of I, and
        N is a set of Noetherian operators for the P-primary component of I
Outputs
    :List
        of @TO DiffOp@s
Description
    Text
        Let $I$ be an unmixed ideal in a polynomial ring $R = K[x_1, ..., x_n]$, with primary 
        decomposition $I = Q_1 \cap ... \cap Q_s$, where $Q_i$ is $P_i$-primary. 
        If $N_i$ is a set of Noetherian operators for $Q_i$, then one can construct a 
        set of differential operators $N$ for $I$ which satisfies the Noetherian operator 
        condition: given $f \in R$, one has $f \in I$ iff $D(f) \in\sqrt{I}$ for all $D \in N$.
    Example
        R = QQ[x,y,t]
        I = intersect(ideal((y+t)^2), ideal(x^2, y^2 - t*x))
        radI = radical I
        primes = associatedPrimes I
        L = primes / (P -> (P, noetherianOperators(I, P)))
        N = noethOpsFromComponents L
        all(flatten table(N, I_*, (D, f) -> (D f) % radI == 0), identity)
    Text
        Note that this construction justifies the focus of Noetherian operators on the case
        that the ideal I is primary: in order to get a useful membership test for a non-primary
        (but still unmixed) ideal, it suffices to compute Noetherian operators on each primary 
        component, and then combine them in the way given above.
SeeAlso
    (noetherianOperators, Ideal, Ideal)
///

-------------- Noetherian operators tests



TEST ///
debug NoetherianOperators
R = QQ[x_0..x_3]
S = QQ[s,t]
I0 = ker map(S,R,{s^5,s^3*t^2, s^2*t^3, t^5})
nops = noetherianOperators(I0)
assert(sanityCheck(nops, I0))
I1 = ideal(x_0^2, x_1^2, x_2^2)
nops = noetherianOperators(I1)
assert(sanityCheck(nops,I1))
///

TEST ///
debug NoetherianOperators
R = QQ[x,y,z]
I = ideal(x^2 - y, y^2)
nops = noetherianOperators(I)
assert(sanityCheck(nops, I))
///


TEST ///
R = QQ[x,y]
I = ideal((x-1)^2,(x-1)*(y+1),(y+1)^3)
J = ideal((x)^2,(x)*(y),(y)^3)
Ps = associatedPrimes I
nopsI = noetherianOperators(I, first Ps)
assert(#(set nopsI - ({1_R, R_0, R_1, R_1^2} / (x -> diffOp{x=>1}))) == 0)
nopsJ = noetherianOperators(J, ideal gens R)
assert(#(set nopsJ - ({1_R, R_0, R_1, R_1^2} / (x -> diffOp{x=>1}))) == 0)
///


TEST /// -- Linear coordinate change test
R = QQ[x,y]
I = ideal(x^2,(y-x))
f = map(R,R,{2*x+y,x+y})
J = f I
NI = noetherianOperators I
NJ = noetherianOperators J
fNI = NI / coordinateChangeOps_f
assert(all(fNI - NJ, D -> D == 0))
///

TEST /// -- numNoethOpsAtPoint test
debug NoetherianOperators
R = CC[x,y,t]
I = intersect(ideal(x^2-t*y, y^2), ideal(x+y+1))
pt = point{{0,0,12}}
l = numNoethOpsAtPoint(I, pt, Tolerance => 1e-6, DependentSet => {x,y})
dif = l / normalize - {diffOp{1_R => 1}, diffOp{x => 1}, diffOp{x^2 => 1, y => 1/6}, diffOp{x^3 => 1, x*y => 1/2}}
assert(all(dif, nop -> all(values nop, v -> abs sub(v,CC) < 1e-6)))
///


TEST /// -- rationalInterpolation test
R = CC[x,y]
num = (x^2 - 2*y*x) 
den = (y*x + y^2)
pts = {matrix {{.922548+.569867*ii, .668231+.918485*ii}}, matrix {{.413667+.0631326*ii,.210225+.688382*ii}}, matrix {{.129853+.649565*ii, .526889+.519065*ii}}, matrix{{.460057+.733113*ii, .642288+.571532*ii}}, matrix {{.268148+.34963*ii, .47612+.77208*ii}},matrix {{.237741+.937067*ii, .902133+.97911*ii}}, matrix {{.58456+.563844*ii,.877568+.129457*ii}}, matrix {{.323687+.586776*ii, .161867+.591524*ii}}, matrix{{.91034+.171779*ii, .913587+.517224*ii}}, matrix {{.963887+.704832*ii,.642633+.397505*ii}}, matrix {{.196713+.843673*ii, .0568273+.621807*ii}}, matrix{{.263865+.9356*ii, .3981+.858609*ii}}, matrix {{.0425473+.770241*ii, .706036+.370498*ii}}}
vals = pts / (p -> (evaluate(matrix num, p))_(0,0) / (evaluate(matrix den, p))_(0,0))
numBasis = sub(basis(0,2,R), R)
denBasis = sub(basis(0,2,R), R)
(n, d) = rationalInterpolation(pts, vals, numBasis, denBasis, Tolerance => 0.0001)
assert(norm(n - num) < 1e-6)
assert(norm(d - den) < 1e-6)
///


end


restart
debugLevel = 1
debug loadPackage("NoetherianOperators", Reload => true)
needsPackage "NumericalAlgebraicGeometry"
needsPackage "Bertini"
installPackage("NoetherianOperators", RemakeAllDocumentation => true)
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
L = reduceSpace truncatedDual(M,p,6)
--shiftDual(L,q,8)
G = matrix{{x^2,x*y^2,y^4}}
--socles G

hilbertFunction(toList(0..8),L)
dualInfo(M,p,Truncate=>8)
standardBasis(M)
dualHilbert(M,Truncate=>25)
dualBasis(M)
dualInfo(M)
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
installPackage "NoetherianOperators"
viewHelp "NoetherianOperators"


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
L' = elapsedTime noetherianOperators(J,I, Strategy => "MacaulayMatrix", KernelStrategy => "Default")
elapsedTime Q = first select(primaryDecomposition J, q -> radical q == I)
L = elapsedTime noetherianOperators(Q, Strategy => "PunctualHilbert")
elapsedTime Q' = getIdealFromNoetherianOperators(L, radical Q)
Q == Q'
----------------------------------------------------
----------------------------------------------------


-----------------------------------------------------
-- Example 11: timing example
-- This example can be used to compare some timings
----------------------------------------------------
debugLevel = 2
R = QQ[x_1,x_2,x_3,x_4]
k=6
J = ideal((x_1^2-x_2*x_3)^k,(x_1*x_2-x_3*x_4)^k,(x_2^2-x_1*x_4)^k)
Q = saturate(J,ideal(x_1*x_2*x_3*x_4))
isPrimary Q
elapsedTime noetherianOperators(Q, Strategy => "PunctualHilbert", KernelStrategy => "Default", IntegralStrategy => true)
elapsedTime noetherianOperators(Q, Strategy => "PunctualHilbert", KernelStrategy => "Gaussian", IntegralStrategy => true)
elapsedTime noetherianOperators(Q, Strategy => "PunctualHilbert", KernelStrategy => "Default", IntegralStrategy => false)
elapsedTime noetherianOperators(Q, Strategy => "PunctualHilbert", KernelStrategy => "Gaussian", IntegralStrategy => false)
elapsedTime noetherianOperators(Q, Strategy => "MacaulayMatrix", KernelStrategy => "Default", IntegralStrategy => true)
elapsedTime noetherianOperators(Q, Strategy => "MacaulayMatrix", KernelStrategy => "Gaussian", IntegralStrategy => true)
elapsedTime noetherianOperators(Q, Strategy => "MacaulayMatrix", KernelStrategy => "Default", IntegralStrategy => false)
elapsedTime noetherianOperators(Q, Strategy => "MacaulayMatrix", KernelStrategy => "Gaussian", IntegralStrategy => false)
elapsedTime noetherianOperators(Q, Strategy => "Hybrid", KernelStrategy => "Default")
elapsedTime noetherianOperators(Q, Strategy => "Hybrid", KernelStrategy => "Gaussian")
