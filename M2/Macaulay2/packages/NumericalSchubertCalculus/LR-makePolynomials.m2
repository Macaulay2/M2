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
squareUpPolynomials(ZZ,Ideal) := (m,eqs) ->  gens eqs * random(FFF^(numgens eqs), FFF^m)  


----------------------------------------------
-- Optimization of makePolynomials

-- #0: get Pluecker coords set
getB = method()
getB (ZZ,ZZ) := memoize(
    (k,n) -> subsets(n,k) 
    )

-- #1:  
getA = method()
getA (ZZ,ZZ,List) := memoize(
    -- bracket are 1-based... subtracting 1 from everything
    (k,n,lambda)->apply(notAboveLambda(lambda,k,n),a->apply(a,b->b-1))
    ) 

-- #2: called once per level
makeG = method()
makeG (ZZ,ZZ,List, Matrix) := (k,n,lambda,F) -> (
    A := getA(k,n,lambda);
    B := getB(k,n);
    matrix apply(A, a->apply(B,b->det submatrix(G,a,b)))
    ) 

-- #3: called once per level
-- IN: k,n = Grassmannian size
--     schubertProblem = list of pairs (partition,flag)
-- OUT: "squared up" matrix RG 
makeGG = method()
makeGG (ZZ,ZZ,Matrix) := (k,n,schubertProblem) -> (
    B := getB(k,n);
    GG := matrix(FFF^0,FFF^(#B)); 
    scan(schubertProblem, lambdaF->(
	    lambdaF := (lambda, F);
	    c := sum lambda;
    	    G := makeG(k,n,lambda,F);
    	    R := random(FFF^c,FFF^(numRows G));
	    GG || G
	    ));
    GG
    ) 

-- #4: called once per checker move
makeSquareSystem = method()
makeSquareSystem (Matrix, Matrix) := (GG,MX) -> (
    k := numColumns MX; 
    n := numRows MX; 
    B := getB(k,n);
    plueckerCoords := transpose matrix apply(B,b->MX^b);
    GG*plueckerCoords  
    ) 

