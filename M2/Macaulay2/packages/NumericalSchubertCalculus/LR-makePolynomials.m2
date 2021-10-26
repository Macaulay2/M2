---------------------------------
-- squareUpPolynomials
---------------------------------
-- m random linear combinations of generators of the ideal
squareUpPolynomials = method()
squareUpPolynomials(ZZ,Matrix) := (m,eqs) ->  eqs
-- seems that if you give a matrix, then it won't do anything...
squareUpPolynomials(ZZ,Ideal) := (m,eqs) ->  if numgens eqs == m then gens eqs else gens eqs * random(FFF^(numgens eqs), FFF^m)  


-- -----------------------------
-- Naive way to makePolynomials 
-- (take all minors)
--------------------------
-- Input: 
--     MX, local coordinates
--     conds, list of partitions
--     flags, list of flags (given by square matrices)
-- Output:
--     polynomial equations for the given Schubert problem
-- Note: 
--     if flags involve any variables 
--     (e.g., if flags depend on the continuation parameter t)
--     MX should be over the same ring as flags 
naivePolynomials = method()
naivePolynomials(Matrix, List, List) := (MX, conds, flags)->(
    R := ring MX;
    k := numcols MX;
    n := numrows MX;
    eqs := sum(#conds, i->(
	    MXF := MX|sub(flags#i,R);
	    b:= partition2bracket(conds#i,k,n);
	    sum(#b,r->(
		    c := b#r;
		    minors(k+c-(r+1)+1, MXF_{0..k+c-1})
		    ))
	    ));
    eqs
    )

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
plueckerSystem = method()
GGstash := new MutableHashTable
resetGGstash = () -> (GGstash = new MutableHashTable;)
plueckerSystemGGMX := (GG,MX) -> (
    k := numColumns MX; 
    n := numRows MX; 
    B := getB(k,n);
    plueckerCoords := transpose matrix {apply(B,b->det MX^b)};
    (if instance(GG,GateMatrix) or instance(plueckerCoords,GateMatrix) then GG else promote(GG,ring plueckerCoords))*plueckerCoords  
    ) 
plueckerSystem'Matrix'GateMatrix := (MX,remaining'conditions'flags) -> (
    r := #remaining'conditions'flags;
    k := numColumns MX; 
    n := numRows MX; 
    GG := if GGstash#?r then GGstash#r else (
	GGstash#r = makeGG(k,n,remaining'conditions'flags)
	);
    plueckerSystemGGMX(GG,MX)  	  
    )
plueckerSystem (Matrix,List) := plueckerSystem'Matrix'GateMatrix
plueckerSystem (GateMatrix,List) := plueckerSystem'Matrix'GateMatrix

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
--     	   conds = list of pairs (l,F) where l is a Schubert condition and F is a flag
--
--         sols = list of solutions (used only by "lifting" strategy)  
-- output:  (Ideal, List) = (system, solutions)
-- note: solutions = sols (or new lifted solutions) 
-----------------

makePolynomials = method(TypicalValue => Ideal, Options=>{Strategy=>"Pluecker"})

makePolynomials(Matrix, List, List) := o -> (MX, conds, sols) -> if o.Strategy == "naive" then (
    -- **************** original (less efficient) way ***********
    (naivePolynomials(MX,conds/first,conds/last), sols) 
    ) else if o.Strategy == "Pluecker" then (
    -- ********* new (more efficient) way ***************************
    if DBG>0 then start := cpuTime(); 
    I := ideal plueckerSystem (MX,conds);
    if DBG>0 then << "time(makePolynomials) = " << cpuTime()-start << endl;
    (I,sols)
    ) else if o.Strategy == "lifting" then ( 
    -- ********* extra variables introduced *************************
    R := ring MX;
    C := coefficientRing R;
    k := numColumns MX; 
    n := numRows MX;   
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

---------------------------------
--- changeFlags
---------------------------------
---
-- changeFlags is a function to
-- write solutions written w.r.t. flagsA
-- to solutions written w.r.t flagsB
--
-- Input:
--    MX -- X -> A localization pattern, M -> flag (Information about the 
--    	      first two flags determines the localization pattern X)
--    solutionsA -> solutions to the problem specialized to flagsA
--    conds'A'B -> sequence with conditions and flags as follows: 
--    	  conditions = list of partitions (L3,..., Lm), _not_pairs (partition, flag)
--    	  flagsA = (A3,...,Am)
--    	  flagsB = (B3,...,Bm)
-- Output:
--    List of solutions written w.r.t flags B
---------------------------------
changeFlags = method(Options=>{OneHomotopy=>true})
changeFlags(List, Sequence) := o->(solutionsA, conds'A'B)->( -- solutionsA is a list of matrices
   if #solutionsA == 0 then return {};
   (conditions,flagsA,flagsB) := conds'A'B; 
   SchA := apply(#conditions, i->(conditions#i,flagsA#i));
   SchB := apply(#conditions, i->(conditions#i,flagsB#i));
   -- August 20, 2013:
   -------------------
   -- commenting th checkIncidenceSolutions check as we discovered
   -- this is a test that is numerical unstable!
   --assert all(solutionsA, s->checkIncidenceSolution(s,SchA));
   s := first solutionsA;
   n := numrows s;
   k := numcols s;
   x := symbol x;
   R := FFF[x_(1,1)..x_(k,n-k)];
   MX := sub(random(FFF^n,FFF^n),R)*(transpose genericMatrix(R,k,n-k)||id_(FFF^k)); -- random chart on G(k,n)
   -- THE SOLUTIONS MIGHT NOT FIT MX (that's why I have an error for some problems)
   
   -- NEW: solutionsB := changeFlags'OneHomotopy(MX,solutionsA/(s->solutionToChart(s,MX)),conds'A'B);
   -- OLD: 
   solutionsB := changeFlags(MX,solutionsA/(s->solutionToChart(s,MX)),conds'A'B,o);
   
   -- the following clean is a hack, instead, we need to do a newton step check
   -- when we changeFlags as there is a numerical check in there... 
   -- the following is a hack
   ret := apply(solutionsB, s-> clean(ERROR'TOLERANCE^2,sub(MX, matrix{s})));
   --print(" changing solutions from flagsA to FlagsB using parameter homotopy\n we verify the returned solutions using newton Iteration");
   --print(checkNewtonIteration(ret,SchB,(k,n))); --this is not working with the new way to create eqns
   --assert all(ret, s->checkIncidenceSolution(s,SchB));
   ret
   )

---------------------------------
-- changeFlags (recursive call!!!)
--
-- This function is doing a parameter homotopy
-- change one column at a time to move solutions
-- w.r.t. flags A to solutions w.r.t. flags B
--
-- CAVEAT:
--     it generates the polynomial equations using
--     all minors of the incidence conditions
--     (not the efficient way later implemented)
---------------------------------
-- Input:
--    MX --> matrix of local coordinates
--    solutionsA -> solutions to the problem specialized to flagsA
--    conds'A'B -> sequence with conditions and flags as follows: 
--    	  conditions = list of partitions (L3,..., Lm), _not_pairs (partition, flag)
--    	  flagsA = (A3,...,Am)
--    	  flagsB = (B3,...,Bm)
-- Output:
--    List of solutions to the problem specialized to flagsB
-- Option:
--     "one homotopy"=>true [default]       
--        there are two strategies, "one homotopy"=>true assumes flagsA are generic
--        "one homotopy"=>false make a gradual random change of flags (one column at a time)   
--        and uses only linear homotopies.
----------------------------------
changeFlags(Matrix, List, Sequence) := o -> (MX, solutionsA, conds'A'B) ->
if o#OneHomotopy then changeFlags'OneHomotopy(MX, solutionsA, conds'A'B) else ( 
   -- solutionsA is a list of lists (of values for the parameters)
   (conditions,flagsA,flagsB) := conds'A'B; 
   solutionsS := solutionsA;
   if solutionsA!={} then(
       t:= symbol t;
       n := numcols last flagsA;
       R := ring last flagsA;
       R1 := R[t];
       Mt := matrix{{t}};
       Mt1 := matrix{{1-t}};
       scan(n, i->(
	       -- start when t = 0, target when t = 1
	       flagsHomot := apply(#flagsA, f-> (
		       FlagBB := sub(flagsB#f,R1);
		       FlagAA := sub(flagsA#f,R1);
		       FlagBB_{0..i-1}|
		       (FlagBB_{i}*Mt - FlagAA_{i}*Mt1)|
		       FlagAA_{i+1..n-1}
		       ));
	       RMx := ring MX;
	       m := numgens RMx;
       	       R2 := (coefficientRing RMx)[t,gens RMx];
	       Polys := flatten entries squareUpPolynomials(m, 
		   naivePolynomials(sub(MX,R2),conditions,flagsHomot));
	       A0 := map(RMx,R2,prepend(0_RMx,gens RMx));
	       A1 := map(RMx,R2,prepend(1_RMx, gens RMx));
	       solutionsT:=track(Polys/A0, Polys/A1, solutionsS, 
		   NumericalAlgebraicGeometry$gamma=>exp(2*pi*ii*random RR));
      	       solutionsS = solutionsT/coordinates;
	       ));
       );
   solutionsS
   )

changeFlags'OneHomotopy = method()
changeFlags'OneHomotopy(Matrix, List, Sequence) := (MX, solutionsA, conds'A'B)->( -- solutionsA is a list of lists (of values for the parameters)
   (conditions,flagsA,flagsB) := conds'A'B; 
   if #solutionsA == 0 then return {};
   t:= symbol t;
   n := numcols last flagsA;
   R := ring last flagsA;
   R1 := R[t];
   toR1 := map(R1,R);
   flagsHomot := apply(flagsA, flagsB, (A,B)->toR1 A * (1-t) + toR1 B * t); 
   RMx := ring MX;
   m := numgens RMx;
   R2 := (coefficientRing RMx)[gens RMx,t];
   Polys := flatten entries squareUpPolynomials(m, 
       naivePolynomials(sub(MX,R2),conditions,flagsHomot)
       );
   solutionsB := trackHomotopy(transpose matrix {Polys}, apply(solutionsA,s->point{s}));      	  
   solutionsB/coordinates
   )


TEST ///
---------
-- Test the function changeFlags that
-- moves solutions wrt flags A
-- to solutions wrt flags B
--
restart
needsPackage "NumericalAlgebraicGeometry"
debug needsPackage "NumericalSchubertCalculus"
FFF = CC_53
Rng = FFF[x_{1,1}, x_{1,2}];
MX = matrix{{x_{1,1}, x_{1,2}}, {1,0}, {0,1}, {0,0}};
conds = {{1},{1}};
Flags1 = {random(FFF^4,FFF^4), random(FFF^4,FFF^4)};
-- Flags1 = {rsort id_(FFF^4), id_(FFF^4)_{1,3,0,2}};
sols = solveSystem (
    first makePolynomials(MX, apply(#conds,i->(conds#i,Flags1#i)),{})
    )_*
Flags2 = {id_(FFF^4)_{1,3,0,2}, rsort id_(FFF^4)} --we should get (0,0) as solution
solsT = changeFlags(MX, sols/coordinates, (conds, Flags1, Flags2))
assert(clean_0.0001 matrix solsT == 0) -- check that the solutions are actually (0,0)
solsT = changeFlags'OneHomotopy(MX, sols, (conds, Flags1, Flags2))
assert(clean_0.0001 matrix solsT == 0) -- check that the solutions are actually (0,0)

/// --end of TEST



