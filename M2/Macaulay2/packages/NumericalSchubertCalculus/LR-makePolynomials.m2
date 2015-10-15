if version#"VERSION" == "1.8.2.1" then needsPackage "SLPexpressions" else GateMatrix = List
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
makePolynomials = method(TypicalValue => Ideal)
makePolynomials(Matrix, List) := (MX, conds) ->(
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
     eqs 
)
makePolynomials(Matrix, List, List) := (MX, conds, flagsHomotopy)->(
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

---------------------------------
-- squareUpPolynomials
---------------------------------
-- m random linear combinations of generators of the ideal
squareUpPolynomials = method()
squareUpPolynomials(ZZ,Matrix) := (m,eqs) ->  eqs
squareUpPolynomials(ZZ,Ideal) := (m,eqs) ->  gens eqs * random(FFF^(numgens eqs), FFF^m)  


----------------------------------------------
-- Optimization of makePolynomials

-- #0: get Pluecker coords set
getB = method()
getB (ZZ,ZZ) := memoize(
    (k,n) -> subsets(n,k) 
    )

-- #1: get all partitions not above the current condition lambda
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

-- SUBSTITUTING makePolynomials !!!
makePolynomials(Matrix, List) := (MX,conds) -> (
    if DBG>0 then start := cpuTime(); 
    I := ideal makeSquareSystem (MX,conds);
    if DBG>0 then << "time(makePolynomials) = " << cpuTime()-start << endl;
    I
    )
