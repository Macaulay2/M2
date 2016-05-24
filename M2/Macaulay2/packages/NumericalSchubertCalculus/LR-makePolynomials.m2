if version#"VERSION" >= "1.8.2.1" then needsPackage "SLPexpressions" else GateMatrix = List
needsPackage "NAGtypes"
---------------------------------
-- squareUpPolynomials
---------------------------------
-- m random linear combinations of generators of the ideal
squareUpPolynomials = method()
squareUpPolynomials(ZZ,Matrix) := (m,eqs) ->  eqs
squareUpPolynomials(ZZ,Ideal) := (m,eqs) ->  if numgens eqs == m then gens eqs else gens eqs * random(FFF^(numgens eqs), FFF^m)  

----------------------------------------------
-- Optimization of makePolynomials

-- #0: get Pluecker coords set
getB = method()
getB (ZZ,ZZ) := memoize(
    (k,n) -> subsets(n,k) 
    )

-- #1: get all partitions not above the current condition lambda (in the Bruhat order)
getA = method()
getA (ZZ,ZZ,List) := memoize(
    -- bracket are 1-based... subtracting 1 from everything
    (k,n,lambda)->apply(notAboveLambda(lambda,k,n),a->apply(a,b->b-1))
    ) 

-- #2: called once per level
makeG = method()
makeG (ZZ,ZZ,List, Matrix) := (k,n,lambda,F) -> (
    Finv := solve(F,id_(FFF^n));
    A := getA(k,n,lambda);
    B := getB(k,n);
    matrix apply(A, a->apply(B,b->det submatrix(Finv,a,b)))
    ) 
makeG (ZZ,ZZ,List, GateMatrix) := (k,n,lambda,F) -> (
    Finv := F; -- assumption: we supply the dense expression for the inverse 
    A := getA(k,n,lambda);
    B := getB(k,n);
    matrix apply(A, a->apply(B,b->det submatrix(Finv,a,b)))
    ) 

-- #3: called once per level
-- IN: k,n = Grassmannian size
--     schubertProblem = list of pairs (partition,flag)
-- OUT: "squared up" matrix RG 
makeGG = method()
makeGG (ZZ,ZZ,List) := (k,n,schubertProblem) -> (
    B := getB(k,n);
    GG := null;
    scan(schubertProblem, lambdaF->(
	    (lambda, F) := lambdaF;
	    c := sum lambda;
    	    G := makeG(k,n,lambda,F);
    	    R := random(FFF^c,FFF^(numRows G));
	    if GG === null then GG = R*G
	    else GG = GG || (R*G)
	    ));
    GG
    ) 

-- #4: called once per checker move
makeSquareSystem = method()
GGstash := new MutableHashTable
resetGGstash = () -> (GGstash = new MutableHashTable;)
makeSquareSystemGGMX := (GG,MX) -> (
    k := numColumns MX; 
    n := numRows MX; 
    B := getB(k,n);
    plueckerCoords := transpose matrix {apply(B,b->det MX^b)};
    (if instance(GG,GateMatrix) or instance(plueckerCoords,GateMatrix) then GG else promote(GG,ring plueckerCoords))*plueckerCoords  
    ) 
makeSquareSystem'Matrix'GateMatrix := (MX,remaining'conditions'flags) -> (
    r := #remaining'conditions'flags;
    k := numColumns MX; 
    n := numRows MX; 
    GG := if GGstash#?r then GGstash#r else (
	GGstash#r = makeGG(k,n,remaining'conditions'flags)
	);
    makeSquareSystemGGMX(GG,MX)  	  
    )
makeSquareSystem (Matrix,List) := makeSquareSystem'Matrix'GateMatrix
makeSquareSystem (GateMatrix,List) := makeSquareSystem'Matrix'GateMatrix

--##########################################
-----------------
-- makePolynomials
--
-- creates a square zero dimensional system
-- that corresponds to a localization pattern
-- and the list of Schubert conditions
-- together with specified flags.
----------------
-- input:
--     	   MX = global coordinates for an open subset
--     	        of a checkerboard variety (or MX' in the homotopy (in Ravi's notes))
--
--     	    conds = list of pairs (l,F) where l is a Schubert condition and F is a flag
--
-- output:  a matrix of polynomials
-----------------

makePolynomials = method(TypicalValue => Ideal, Options=>{Strategy=>"Cauchy-Binet"})

makePolynomials(Matrix, List, List) := o -> (MX, conds, sols) -> if o.Strategy == "good old way" then (
    -- **************** original (less efficient) way ***********
    R := ring MX;
    k := numgens source MX;
    n := numgens target MX;
    eqs := sum(conds, lF ->(
	    (l,F) := lF;
	    MXF:=MX|sub(F,R);
	    b := partition2bracket(l,k,n);
	    sum(#b, r->( 
		    c := b#r;
		    minors(k+c-(r+1)+1, MXF_{0..k+c-1})
		    ))
	    ));    
    (eqs,sols) 
    ) else if o.Strategy == "Cauchy-Binet" then (
    -- ********* new (more efficient) way ***************************
    if DBG>0 then start := cpuTime(); 
    I := ideal makeSquareSystem (MX,conds);
    if DBG>0 then << "time(makePolynomials) = " << cpuTime()-start << endl;
    (I,sols)
    ) else if o.Strategy == "deflation" then ( 
    -- ********* extra variables introduced *************************
    R = ring MX;
    C := coefficientRing R;
    k = numColumns MX; 
    n = numRows MX;   
    assert all(conds, c->#first c==1); -- assuming all partitions are {a}  
    sols = sols / (s->matrix {s});
    sols' := sols;
    L := symbol L; 
    Ilist := apply(#conds, i ->( 
	    F := last conds#i;
	    Finv := solve(F,id_(FFF^n));
	    m := k; -- number of new variables
    	    R = C monoid([gens R, L_(i,1)..L_(i,m)]);
	    colL := transpose matrix{take(gens R,-m)}; 	    
    	    --MM := MX|F_{0..k-1}; for this m := 2*k above
	    a := first first conds#i;
	    FinvMX := Finv*MX;
	    MM := FinvMX^{n-k-a+1..n-1};
	    slice := random(C^1,C^m);
	    sols' = apply(#sols, i->sols'#i|transpose solve(
		    evaluate(MM,matrix{{0}}|sols#i)||slice,
		    map(C^(k+a-1),C^1,0)||matrix{{1}},
		    ClosestFit=>true
		    ));
	    ideal (sub(MM,R)*colL) + ideal ((slice*colL)_(0,0) - 1)
	    ));
    (sum(Ilist,I->sub(I,R)),sols'/entries//flatten) 
    ) else error "unknown Strategy"

-- ******* old way (used in changeFlags) ********************************
makePolynomialsGivenConditionsFlags = method()
makePolynomialsGivenConditionsFlags(Matrix, List, List) := (MX, conds, flagsHomotopy)->(
    R := ring MX;
    k := numcols MX;
    n := numrows MX;
    eqs := sum(#conds, i->(
	    MXF := MX|sub(flagsHomotopy#i,R);
	    b:= partition2bracket(conds#i,k,n);
	    sum(#b,r->(
		    c := b#r;
		    minors(k+c-(r+1)+1, MXF_{0..k+c-1})
		    ))
	    ));
    eqs
    )


