export {
    --"skewSchubertVariety", -- for Pieri Homotopies
    "checkIncidenceSolution", --this is only for our tests... shouldn't be used by the user
    "solutionsToAffineCoords", --Temporary! User shouldn't use this function
    "partition2bracket",
    "bracket2partition",
    "randomSchubertProblemInstance"
    }
----------------
--Functions contained here but not exported:
----------------
-- verifyLength(List, ZZ)
-- partition2bracket(List,ZZ,ZZ)
-- output2partition(List) --input redcheckers
-- output2bracket(List)
-- bracket2partition(List,ZZ)
-- printTree(MutableHashTable)--input node
-- checkNewtonIteration -- this is for testing only should be removed from the final version (test if it works with the new way to create eqns)
-- moveFlags2Flags (List, List) --input two list of flags (F's, G's)
-- MovingFlag'at'Root ZZ
-- notAboveLambda(List,ZZ,ZZ) -- input(lambda, k,n)
-- checkSchubertProblem(conds,k,n)  
-- verifyInput

---------------------
--   verifyLength  --
--
-- makes sure a partition l of k parts
-- has length k and it add zeroes at the
-- end if not
--
verifyLength = method(TypicalValue => List)
verifyLength(VisibleList, ZZ) := (l,k) ->(
     x:=new List;
     if #l <= k then (
	  x = for i to k-#l-1 list 0;
	  l | x
     ) else print("partition has too many parts")
)
--------------------------
-- Dictionaries of different notations
--------------------------

--
--  This converts a partition to a bracket
--
partition2bracket = method(TypicalValue => List)
partition2bracket(List,ZZ,ZZ) := (l, k, n) -> (
     l = verifyLength(l, k);
     brackt := for i to #l-1 list (n-k)+(i+1)-l#i
)

--
--  Frank asks: What does this do?
--
output2partition = method(TypicalValue => List)
output2partition(List) := redpos ->(
		n:= #redpos;
		posn := select( redpos, x->x!= NC);
		k:= #posn;
		partitn := new MutableList from k:0;
		apply(#posn, j->(
			partitn#j = n-k+j-posn#j;
		));
		reverse sort toList partitn
)

-- *** not using this function (we use redChkrPos instead)
bracket2input = method(TypicalValue => List)
bracket2input(List,ZZ) := (br,n) ->(
     inp := for i to n-1 list NC;
     inp = new MutableList from inp;
     apply(br, b-> inp#(b-1) = b-1);
     toList inp
)

-- not using this function in general, but when debugging,
-- we might use this function with the columnReduce sometimes
--
-- input: redcheckers
-- output: the bracket condition (read from redCheckers)
--     	   that affects the ID flag
output2bracket = method(TypicalValue=>List)
output2bracket List := outp -> (
     br := select(outp, x-> x!=NC);
     apply(br, x-> x=x+1)
) 

--
--  Converts a bracket into a partition.  This is useful.
--
bracket2partition = method(TypicalValue => List)
bracket2partition(List,ZZ) := (l, n) -> (
--     l = reverse sort l;
     partitn := for i to #l-1 list (n-#l)+(i+1)-l#i 
)

---------

-------------------------
-- printTree (useful when debugging)
-------------------------
-- print Tree is a recursive call of peek 
-- to print all the children of a node
printTree = method()
printTree MutableHashTable := node ->(
	print peek node;
	scan(node.Children, c-> printTree c);
)



----------------------
-- checkNewtonIteration
----------------------
-- Function that given a proposed
-- solution to a Schubert Problem 
-- it creates a Newton step to compare
-- the convergence of the approximated solution
----------------------
-- Input: 
--    Solns - list of solutions (in local coordinates?)
--    Pblm - list of (li,Fi) representing the SchubertProblem
--    (k,n) - sequence with (k,n) that indicate the Grassmannian G(k,n)
-- Output:
--    Newt - List where each entry is the l_1 norm of (s-NewtStep(s)) for s\in Solns
--------------------------------------------------------------------
--       ?? This is the info necessary to create a system of eqns
--------------------------------------------------------------------
--    *** NOT USING THIS FUNCTION ***
--
--   Frank thinks that this might be a useful alternative to checkIncidenceSolution.
--    However, he has yet to find an instance where checkIncidenceSolution fails (but it should sometime)
--
checkNewtonIteration = method()
checkNewtonIteration (List,List,Sequence) := (Solns, Pblm, kn)->(
    (k,n):= kn;
    -- we create a random change of coordinates:
    A:= random(FFF^n,FFF^n);
    SolTransformed := Solns/(i->A*i);
    PblmTransf := apply(Pblm, CF->(
	    (c,f):=CF;
	    (c,A*f)
	    ));
    -- we choose coordinates for the Grassmannian
    X := symbol X;
    RX:=FFF[X_{0,0}..X_{(n-k)-1,k-1}];
    Vs := first entries vars RX;
    coordX := matrix pack(first entries vars RX,k)||id_(FFF^k);
    polySyst := makePolynomials(coordX,PblmTransf);
    solsInCoordsX := solutionsToAffineCoords SolTransformed;
    solutions := apply(solsInCoordsX, X-> toRawSolutions(coordX,X));
    squareSyst:=first entries squareUpPolynomials(numgens ring polySyst, polySyst);
    -- we compute the Jacobian of the system
    JacSystem := for i in squareSyst list for j in Vs list diff(j,i);
    ID:=id_(FFF^(#Vs));
    apply(solutions, newt->(
	    Mp := (map(FFF,RX,matrix{newt}));
    	    JacEval := Mp matrix JacSystem;
	    Jinv := solve(JacEval, ID);
	    Jinv*transpose(Mp matrix{squareSyst})
	    ))
    )


------------------------
-- solutionsToAffineCoords
------------------------
-- writing the solutions in global coords
-- as a set of solutions in terms of my
-- favorite coordinate chart:
-- s = [**||id] the identity on bottom
-------------------------
-- Caveat!!!
-------------------------
-- THIS MAY FAIL if the solutions are
-- not in general position (if they cannot fit this local coords)
-- i.e. when T cannot be computed
--
-- One way to avoid this is taking a random linear 
-- transformation of the solutions (and flags) before calling
-- this function
--------------------------------------------------
-- *** ONLY used in checkNewtonIteration which is no longer used
--------------------------------------------------
solutionsToAffineCoords = method()
solutionsToAffineCoords List := Solutions ->(
    apply(Solutions, s->(
	    k:=numColumns(s);
	    n:=numRows(s);
    	    b:= id_(FFF^k);
    	    T:= solve(submatrix(s,n-k..n-1,0..k-1),b);
	    clean(0.001,s*T)
	    ))
    )


----------------------
-- checkIncidenceSolution
----------------------
-- August 20,2013
-- THIS FUNCTION NEEDS TO BE DELETED??  (Does it?)
-- it was for testing solutions of Schubert varieties
-- but this is not numerical stable... we replace this
-- with a Newton step check
----------------------
-- Function that given a proposed 
-- n by k matrix, and a list of Schubert conditions and flags,
--  it checks if the matrix satisfies the incidence conditions
--  by computing the corresponding minors and see if they vanish
----------------------
-- Input:
--    H -- n by k matrix (representing an element of G(k,n)
--        (make it so that H can have matrices with variables)
--    SchbPrblm -- Schubert problem given as
--    	           list of sequences of the form
--    	      	   {(l1,F1),...,(lm,Fm)}
-- Output:
--    True or False
-----------------------
checkIncidenceSolution = method()
checkIncidenceSolution(Matrix, List) := (H, SchbPrblm) ->(
  n:= numRows H;
  k:= numColumns H;
  verif:= true;
  scan(SchbPrblm, T->(
    (l,F) := T; -- (partition, flag)
    b:=partition2bracket(l,k,n);
    HXF:=promote(H|F,ring H);
    scan(#b, r->( 
       c := b#r;
       rnk := k+c-(r+1)+1;
       if(rnk<= n) then(
         chooseCols:= subsets(k+c,rnk);
	 chooseRows:= subsets(n,rnk);
	 scan(chooseRows, rws->(
	   scan(chooseCols, cls->(
		   n := norm det submatrix(HXF_{0..k+c-1},rws,cls);
		   if n>ERROR'TOLERANCE then(
	               verif=false;
		       print("These are the NONZERO residuals");
 		       print(n);
	               );
	      	   ));
             ));
         );
      ));
    ));
  verif
  )

--
-- TEST
-- H = promote(matrix{{1,0},{0,0},{0,1},{0,0}},FFF)
-- SchbPrblm = {({2,1},id_(FFF^4)),({1,0}, rsort id_(FFF^4))}
-- checkIncidenceSolution(H, SchbPrblm)
-- 
-- SchbPrblm = {({2,1},id_(FFF^4)),({1,0}, random(FFF^4,FFF^4))}
-- checkIncidenceSolution(H, SchbPrblm)


-------------------------------
-- moveFlags2Flags
--
-- function that finds linear transformations
-- to send two flags (F1, F2) to other two
-- flags (G1,G2). The idea is based on the fact
-- that any two flags can be send to the standard
-- flag and the opposite standard flag.
--
-- The idea is to get a matrix A in GL(n)
-- and two triangular matrices T1, T2 such that
--
--   A*F1*T1^-1 = G1
--   A*F2*T2^-1 = G2
---------------------------------
-- Input:
--    {F1,F2} - list of two start flags
--    {G1,G2} - list of two target flags
-- Output:
--    (A,T1,T2) - sequence of three matrices
--         A - invertible nxn matrix
--         T1 - upper triangular matrix
--              with 1's in the diagonal
--         T2 - upper triangular matrix with nonzero
--              entries in the diagonal
---------------------------------
moveFlags2Flags = method()
moveFlags2Flags (List, List) := (F's, G's)->(
    F1:=first F's;
    F2:=last F's;
    G1:=first G's;
    G2:=last G's;
    n:=numColumns F1;
    a := symbol a;
    x := symbol x;
    y := symbol y;
    indx:= subsets(0..n-1,2)/toSequence;
    Vars := sort(flatten apply(indx, i-> {x_(i),y_(i)})|apply(n,i-> y_(i,i)));
    R:= (ring F1)[a_(0,0)..a_(n-1,n-1),Vars];
    A := genericMatrix(R,n,n);
    T1 := mutableIdentity(R,n);
    T2 := mutableIdentity(R,n);
    scan(indx, i-> (T1_i=x_i; T2_i=y_i));
    apply(n,i-> T2_(i,i)=y_(i,i));
    T1 = matrix T1;
    T2 = matrix T2;
    p1:=flatten entries(A*F1-G1*T1);
    p2:=flatten entries(A*F2-G2*T2);
    Eqs:= p1|p2; 
    A1 := map(FFF^(2*n^2),FFF^(2*n^2),(i,j)->(Eqs#i)_(R_j));
    b1 := map(FFF^(2*n^2),FFF^1,(i,j)->-(Eqs#i)_(1_R));
    X := transpose solve(A1,b1);
    (sub(A, X), sub(T1, X), sub(T2, X))
    )
--------------------
--  Example:
--------------------
-- F1 = id_(FFF^4)
-- F2 = rsort id_(FFF^4)
-- G1 = random(FFF^4,FFF^4)
-- G2 = random(FFF^4,FFF^4)
-- 
-- moveFlags2Flags({F1,F2},{G1,G2})
-- o =(| .942485+.841897i -.453529+.038107i  -1.17023-1.18863i -.0211864+.39196i |,
--     | .033580+.866902i -.0844235-.340175i 1.06488-.119899i  .0185506+.411333i | 
--     | .725934+.355448i .229312+.274355i   -.467423+1.25503i -.195849+.736079i | 
--     | .542686+.238398i -.150172+.371548i  -1.36279+.224972i -.055652+.890821i | 
--    ------------------------------------------------------------------------------
--     | 1 -.584783+.236584i -2.07229-1.56411i -.628292-.794925i |, 
--     | 0 1                 1.4547+3.57843i   -.791082+1.93156i |  
--     | 0 0                 1                 .686317+.55597i   |  
--     | 0 0                 0                 1                 |  
--      ------------------------------------------------------------------------------
--     | .461031+.52935i .674426+3.90896i  .957904-.133557i  -1.95461-.07167i  |)
--     | 0               -1.98146-3.05206i -.619948+.386562i 3.38618+.656677i  |
--     | 0               0                 -.387966+.161397i -.064671+1.15621i |
--     | 0               0                 0                 .038901-4.54283i  |
--
--------------------

-------------------------
-- MovingFlag'at'Root 
-------------------------
-- function to create the moving flag node.FlagM
-- observed in the root of a Dag, after the specialization
-- in the checkerboard game.
-- this will be the same for every checkerboard Tree
-- and it is used to solve Internal Problem
------------------------
-- Example:
--
-- MovingFlag'at'Root 4
--  o =  | 1  1  1  1 |
--       | -1 -1 -1 0 |
--       | 1  1  0  0 |
--       | -1 0  0  0 |
------------------------
MovingFlag'at'Root = method(TypicalValue => Matrix)
MovingFlag'at'Root ZZ := n -> (
    M := mutableMatrix rsort id_(FFF^n);
    apply(0..n-1, i->(
	    apply(0..n-1-i, j->(
		    M_(i,j) = (-1)^i;
		    ));
	    ));
    matrix M
    )


-- NotAboveLambda
--
-- Function that, given a partition
-- Lambda, it computes the partitions
-- that are not above lambda in the Bruhat order
-------------------------------
-- Input:
--    l -- (List) the partition Lambda
--    k,n --ZZ,ZZ the Grassmannian Gr(k,n)
--
-- Output:
--    NotAbove  -- List of brackets b such that the 
--                 corresponding partition m is not above l 
--
----------------------------- 
-- Example:
-- 
-- the Bruhat order near the partition {2,1} in G(3,6) is:
--
--        /  \  |  / \ 
--    {1,1,1} {2,1}  {3}
--         \   /  \  /
--         {1,1}   {2}
--             \   /
--              {1}
--               |
--              { }
---------
-- notAboveLambda({2,1},3,6)
--  o = {{4, 5, 6}, {3, 5, 6}, {3, 4, 6}, 
--       {2, 5, 6}, {3, 4, 5}, {1, 5, 6}}
--
--  and these brackets corresponds to the partitions:
--      {{0, 0, 0}, {1, 0, 0}, {1, 1, 0}, 
--       {2, 0, 0}, {1, 1, 1}, {3, 0, 0}}
--
------- 
--  More examples in TST/tstNotAbove.m2
-----------------------------
notAboveLambda = method()
notAboveLambda(List,ZZ,ZZ) := (lambda,k,n) ->(
  -- We Assume that lambda is not the zero partition {0,0,...,0}
  -- k-#lambda is how many zeroes we have
  L':=unique(lambda);
  pos'corners:= apply(L', l->position(lambda, i->i==l, Reverse=>true));
  maxElements:=apply(pos'corners, i->(  --the maximal elements of the ordered set notAbove
	  toList(i:(n-k))|toList(k-i:lambda_i-1)
	  ));
  notAbove := new MutableHashTable;
  addPartition := la -> (
      b := sum la;
      if not notAbove#?b then notAbove#b = {la} 
      else if not member(la, notAbove#b)
      then notAbove#b = notAbove#b | {la}; 
      );
  scan(apply(maxElements, la->select(la,t->t!=0)), addPartition);
  i := max keys notAbove;
  while i>=0 do (
      scan(notAbove#i, la->
	  for j in 0..<#la do
	  if j==#la-1 or la#j>la#(j+1) 
	  then if j==#la-1 and la#j == 1 then addPartition drop(la,-1) 
	  else addPartition replace(j,la#j-1,la) 
	  );
      i = i - 1;
      );
  apply(flatten values notAbove, la->partition2bracket(la,k,n))
  )

---******************************---
------ For Pieri Homotopies ---------
---******************************----
---------------------------
--  skewSchubertVariety  --
-- Creates Matrix E_{m,l} --
---------------------------
-- option: Inputs, an integer or symbol; if integer then 

-- this line of code makes no sense and doesn't work:
-- if version#"VERSION" =!= "1.8.2.1" then inputGate = "inputGate"

skewSchubertVariety = method(TypicalValue=>Matrix, Options=>{Inputs=>53})
skewSchubertVariety(Sequence,List,List) := o->(kn,l,m)->(
     -- k and n are the integers defining the Grassmanian G(k,n)
     -- l and m are partitions of n
     (k,n):=kn;
     l = verifyLength(l, k);
     m = verifyLength(m, k);
     d := (k*(n-k)-sum(l)-sum(m));
     if instance(o.Inputs,ZZ) then R := FFF[vars(o.Inputs..d+o.Inputs-1)]; -- ring where the variables for the matrix lie
     r := 0;
     M := matrix (
	 for i from 1 to k list (
	     for j from 1 to n list (
		 if j==i+l_(k-i) then 1
		 else if j>i+l_(k-i) and j<=(n-k+i-m_(i-1)) then ( 
		     r=r+1; 
		     if instance(o.Inputs,ZZ) 
		     then R_(r-1) 
		     else inputGate (o.Inputs)_(r-1)
		     )
		 else 0
		 ) 
	     )    
	 );
     if instance(o.Inputs,ZZ) then M else 
     (M,toList apply(d,i->inputGate (o.Inputs)_i))
     )



----------------------
-- checkSchubertProblem
----------------------
-- Function that checks if a list of partitions or brackets
-- impose a feasible Schubert problem
----------------------
-- Input: 
--    conds - list of partitions or brackets
--    k,n   - integers that indicate the Grassmannian G(k,n)
-- Output:
--    none - if the partitions are good for a Schubert problem
--      or ERROR otherwise
--------------------------------------------------------------------
-- sanity check
--------------------------------------------------------------------
-- CAVEAT: if the user entered the conditions as brackets
--         it changes them into partitions and do the check
--------------------------------------------------------------------
checkSchubertProblem = method()
checkSchubertProblem (List,ZZ,ZZ) := (conds,k,n) -> (
    -- detect if conds are partitions or brackets and transforms them in partitions if not
    areAllBrackets:=apply(conds, c->(
	    if #c == k and c == sort unique c then true else false
	    ));
    if #unique(areAllBrackets) > 1 then (
	   error "verify your conditions: some seemed partitions some brackets"
    	)else if areAllBrackets_0 == true then(
	   conds = conds/(i-> bracket2partition(i,n)); -- we transform them into partitions
	);
    --------------
    scan(conds, c -> if #c > k or 
	c != rsort c or
	first c > n-k then error ("wrong partition: "|toString c|" verify your input")
	);
    if sum flatten conds != k*(n-k) then 
    error "sum of codimensions of partitions should equal the dimension of the Grassmannian";
    ) 

-----------------------------
--  checkSimpleSchubertProblem
-----------------------------
-- verify if the user gave a Simple Schubert Problem
-- the first two non-simple condition and the rest
-- should be the condition {1} and must add to dim Gr(k,n)
----------------------------
-- Input:
--    conds    - list of partitions {l,m,{1},...,{1}}
--    k,n      - integers specifying Gr(k,n)
-- Output:
--    none - if the Schubert problem is Simple
--    or ERROR otherwise
---------------------------
checkSimpleSchubertProblem = method()
checkSimpleSchubertProblem(List,ZZ,ZZ) := (conds,k,n) ->(    
    checkSchubertProblem(conds,k,n);
    simpleconds:= drop(conds,2);
    scan(simpleconds, c->(
	    if sum c != 1 then error (toString c| " is not a codimension one condition");
	    )); 
    )


--------------------------
-- verifyInput
-------------------------
-- Detects if the user gave partitions
-- or brackets to define Schubert conditions
-- checks if the input makes a feasible Schubert pblm
-- and if the flags are general
-------------------------
-- Input:
--    conds'flags - List of Schubert conditions with flags
--                  {(cond_List, flag_Matrix),...}
--
--    k,n         - integers defining Gr(k,n)
--
-- Output:
--    Pblm - List of Schubert conditions with flags
--            {(partition_List, flag_Matrix),...}
--    or ERROR - if the input was not correct
--------------------------
-- Note: Our methods assume you give Schubert
--       conditions as partitions and flags
--       as square invertible matrices
-------------------------
---------------
-- Examples in TST/tstVerifyInput.m2 
---------------

verifyInput = method()
verifyInput(List,ZZ,ZZ) := (conds'flags, k,n) ->(
    conds := conds'flags/first; -- list of schubert conditions
    flags := conds'flags/last; -- list of flags
    -- check if these conditions impose a 0-dimensional Schubert Problem
    checkSchubertProblem(conds,k,n);
    --- Verify that the flags are square matrices of full rank
    scan(flags, F->(
	    if not instance(F,Matrix) or numColumns F != numRows F or det F < ERROR'TOLERANCE  then error(toString F|" should be an invertible square matrix of size "| toString n)	    
	    ));
    apply(#conds, i-> (conds_i, flags_i))
    )

----------------------
-- randomSchubertProblemInstance
----------------------
-- Creates a random instance of a Schubert problem 
-- by computing random matrices to specify the flags
----------------------
-- Input: 
--    conds - list of partitions
--    k,n   - integers that indicate the Grassmannian G(k,n)
--
--    Options:  Strategy => "unitary" : uses the Random Unitary Matrix from NAG4M2.
--                          "unit circle" : [default] creates a matrix whose entries are 
--                                           random complex numbers in the unit circle
-- Output:
--    a list of sequences of the from (cond_List,flag_Matrix)
--      where flag_Matrix is a random nxn matrix
------------------------
-- create an instance of a Schubert problem with random matrices specifying flags
randomSchubertProblemInstance = method(Options=>{Strategy=>"unit circle"})
randomSchubertProblemInstance (List,ZZ,ZZ) := o -> (conds,k,n) -> (
    checkSchubertProblem(conds,k,n);
    apply(conds, c->(c, 
	    if o.Strategy == "unitary" then randomUnitaryMatrix n else
	    if o.Strategy == "unit circle" then matrix table(n,n,(i,j)->exp(2*pi*ii*random RR))
	    else error "unknown strategy for random matrix"   
	    ))
    )  
