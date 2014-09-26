export {
   --dist, --temporary 08,20,2013
   checkIncidenceSolution, 
   moveFlags2Flags, 
   solsToFavCoords, --temporary 08.20.13
   checkNewtonIteration,
    partition2bracket,
    bracket2input,
    output2partition,
    output2bracket,
    bracket2partition,
    redcheckers2partitions
    }

---------------------
--	verifyLength	--
--								--
-- makes sure a partition l
-- that is supposed to impose
-- conditions on Gr(k,n)
-- is in fact a partition 
-- of length k (add 0s if not)
--
verifyLength = method(TypicalValue => List)
verifyLength(VisibleList, ZZ) := (l,k) ->(
     x:=new List;
     if #l <= k then (
	  x = for i to k-#l-1 list 0;
	  l | x
     ) else print("wrong size of partition")
)
--------------------------
-- Dictionaries of different notations
--------------------------

partition2bracket = method(TypicalValue => List)
partition2bracket(List,ZZ,ZZ) := (l, k, n) -> (
     l = verifyLength(l, k);
     brackt := for i to #l-1 list (n-k)+(i+1)-l#i
)

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

-- not using this function
bracket2input = method(TypicalValue => List)
bracket2input(List,ZZ) := (br,n) ->(
     inp := for i to n-1 list NC;
     inp = new MutableList from inp;
     apply(br, b-> inp#(b-1) = b-1);
     toList inp
)

-- not using this function either
-- we might use this in the column reduce
--
output2bracket = method(TypicalValue=>List)
output2bracket List := outp -> (
     br := select(outp, x-> x!=NC);
     apply(br, x-> x=x+1)
) 

-- the next function is not completely right:
-- it gives two partitions but only one is correct
---- input: redcheckers
---- output: list with the two partitions that generate
----         that redchecker board
redcheckers2partitions= method(TypicalValue=>List)
redcheckers2partitions List := redchckrs ->(
     br := sort select(redchckrs, x-> x!=NC);     
     part1:=apply(#br, i-> br#i-i);
     br2:=select(#redchckrs, i-> redchckrs#i != NC);
     part2 := apply(#br2, i-> br2#i-i);
     {rsort part1, rsort part2}
     )
----

bracket2partition = method(TypicalValue => List)
bracket2partition(List,ZZ) := (l, n) -> (
--     l = reverse sort l;
     partitn := for i to #l-1 list (n-#l)+(i+1)-l#i 
)

---------

----------------------
-- checkNewtonIteration
----------------------
-- Function that given a proposed
-- solution to a Schubert Problem 
-- it creates a Newton step to compare
-- the convergence of the approximated solution
----------------------
-- Input: 
--    Solns - list of solutions 
--    Pblm - list of (li,Fi) representing the SchubertProblem
--    (k,n) - sequence with (k,n) that indicate the Grassmannian G(k,n)
-- Output:
--    Newt - List where each entry is the l_1 norm of (s-NewtStep(s)) for s\in Solns
--------------------------------------------------------------------
--        These is the info necessary to create a system of eqns
--------------------------------------------------------------------
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
    solsInCoordsX := solsToFavCoords SolTransformed;
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
--checkNewtonIteration(List,Matrix,List) := (Solns,coordX,remaining'conditions'flags) -> (
--    polySyst := makePolynomials(coordX,remaining'conditions'flags);
--    solutions := apply(Solns, X-> toRawSolutions(coordX,X));
--    NewtStep:=checkNewtonIteration(solutions,polySyst);
--    apply(NewtStep, n-> map(FFF,ring coordX, matrix{n}) coordX)
--    )



--squareSyst := flatten entries gens makePolynomials(coordX, remaining'conditions'flags);
--polysquares := squareUpPolynomials(numgens ring coordX, ideal(squareSyst));
--Sols:=  apply(node.Solutions, X->toRawSolutions(coordX,X));
--NewtonStep1 := refine(squareSyst, Sols, Software=>M2, Iterations=>1);
--NewtonStep2 := refine(squareSyst, NewtonStep1, Software=>M2, Iterations=>1);
--print(dist(NewtonStep1,Sols));
--print("distance between two newton steps:");
--print(dist(NewtonStep2,NewtonStep1));

------------------------
-- dist
------------------------
-- function to measure the 
-- euclidean distance between
-- two points: (x1,...,xn) and (y1,...,yn)
-- it computes:
--    sqrt(sum( (xi-yi)^2 ))
-- 
-- Input: two Lists representing two set of solutions to a system
-- Output: list of distances between the elements of the list
--   
--- Notice that solns1 could come from a numerical Newton step,
-- so the function first check if the solution is of type List or 
-- of type Point
--
--------- FOR DAN AND MIKE -------
-- Notice we had to do this function because
-- M2 function norm(2,_) does not give the 2-norm
-- of the complex vector (x1,...,xn)
----------------------- 
dist = method()
dist(List,List) := (solns1,solns2) -> (
    apply(#solns1, i->(
	    v1 := solns1#i;
	    v2 := solns2#i;
	    if instance(v1,Point) then v1 = coordinates(v1);
	    if instance(v2,Point) then v2 = coordinates(v2);
	    sqrt(sum(apply(v2-v1, i->i^2)))
   ))
)


------------------------
-- solsToFavCoords
------------------------
-- writting the solutions in global coords
-- as a set of solutions in terms of my
-- favorite coordinate chart:
-- s = [**||id] the identity on top
-------------------------
-- Caveat!!!
-------------------------
-- THIS MAY FAIL if the solutions are
-- not in general position (if they cannot fit this local coords)
-- i.e. when T cannot be computed
--------------------------------------------------
solsToFavCoords = method()
solsToFavCoords List := Solutions ->(
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
-- THIS FUNCTION NEEDS TO BE DELETED
-- it was for testing solutions of Schubert varieties
-- but this is not numerical stable... we replace this
-- with a Newton step check
----------------------
-- Function that given a proposed
-- n by k matrix, it checks
-- if it satisfies incidence conditions
----------------------
-- Input:
--    M -- n by k matrix (representing an element of G(k,n)
--        (make it so that M can have matrices with variables)
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
    (l,F) := T;
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
--         T1 - upper triangular matrix with 1's in
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


-------------------------
-- MovingFlag'at'Root 
-------------------------
-- function to create the moving flag node.FlagM
-- that will be the same for every checkerboard Tree
-- this is used to solve Internal Problem
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

