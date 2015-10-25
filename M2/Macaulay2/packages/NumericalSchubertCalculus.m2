newPackage(
        "NumericalSchubertCalculus",
        Version => "0.3", 
        Date => "October 29, 2009",
        Authors => {{Name => "Anton Leykin", 
                  Email => "leykin@math.gatech.edu", 
                  HomePage => "http://people.math.gatech.edu/~aleykin3"},
	          {Name => "Abraham Martin del Campo", 
                  Email => "asanchez@math.tamu.edu", 
                  HomePage => "www.math.tamu.edu/~asanchez"}},
        Headline => "a Macaulay2 package for using numerical methods in Schubert Calculus",
	PackageImports => {"NumericalAlgebraicGeometry"},
        DebuggingMode => false
        )

export {   
   "StartSolutions",
   "skewSchubertVariety",
   "createRandomFlagsForSimpleSchubert",
   "solveSimpleSchubert",
   "trackSimpleSchubert",
   "findGaloisElement",
   "isFullSymmetric",
   "isGaloisFullSymmetric",
   "Memoize",
-----------------------
-- The following are functions for
-- LR-Homotopies
-----------------------
   "trackHomotopy",
   "partition2bracket",
   "bracket2input",
   "output2partition",
   "output2bracket",
   "bracket2partition",
   "redcheckers2partitions",
   "redChkrPos",
   "moveRed",
   "moveCheckers",
   "playCheckers",
   "NC",
   "FFF",
   "Board",
   "IsResolved",
   "Fathers",
   "Children",
   "printTree",
   "makeLocalCoordinates",
   "resolveNode",
   "FlagM",
   "CriticalRow",
   "Polynomials",
   "Solutions",
   "solveSchubertProblem",
   "changeFlags", -- temporary
   "makePolynomials", -- temporary
   "SolutionsSuperset", -- temporary
   "checkIncidenceSolution", --temporary
   "moveFlags2Flags", --temporary
   "dist" --temporary Aug,20,2013
   }

-------------------------
-- Pieri Homotopy Code --
--------------------------
-- Authors: Anton Leykin
--          Abraham Martin del Campo
--
-- Date:  October 29, 2009
--
-- Last Update: August 11, 2010
------------------------------------
-- Littlewood-Richardson Homotopy --
------------------------------------
-- Authors: Anton Leykin
--          Abraham Martin del Campo
--          Frank Sottile
--
-- Date: April 5, 2012
--
-- Last Update: August 19, 2013
------------------------------------

-- needsPackage "NumericalAlgebraicGeometry"
-- we need to use GAP's package

-- NC means no checker in that column
NC = infinity

-- OUR FIELD
FFF = QQ
FFF = RR
FFF = CC
FFF = CC_53
ERROR'TOLERANCE := 0.001
------------------
-- Debug Level --
------------------
-- 0 = no debug mode
-- 1 = timing main processes
-- 2 = verify solutions against blackbox solver (no timing)
-- 3 = time processes and blackbox solver 
-- 4 = new experimental stuff kicks in
DEBUG'LEVEL = 1


solutionsHash := new MutableHashTable;

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
output2bracket = method(TypicalValue=>List)
output2bracket List := outp -> (
     br := select(outp, x-> x!=NC);
     apply(br, x-> x=x+1)
) 

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

--------------------------------
-- Numerical Pieri Homotopies --
--------------------------------

---------------------------
--  skewSchubertVariety  --
-- Creates Matrix E_{m,l} --
---------------------------
skewSchubertVariety = method(TypicalValue=>Matrix)
skewSchubertVariety(Sequence,List,List) := (kn,l,m)->(
     -- k and n are the integers defining the Grassmanian G(k,n)
     -- l and m are partitions of n
     (k,n):=kn;
     l = verifyLength(l, k);
     m = verifyLength(m, k);
     d := (k*(n-k)-sum(l)-sum(m));
     R := FFF[vars(53..d+52)]; -- ring where the variables for the matrix lie
     r := 0;
     matrix (
     	  for i from 1 to k list (
               for j from 1 to n list (
               	    if j==i+l_(k-i) then 1
		    else if j>i+l_(k-i) and j<=(n-k+i-m_(i-1)) then ( 
		     	          r=r+1; 
		     	          R_(r-1) 
		     	         )
            	    else 0
         	     ) 
      	       )
      	  )
     )

---------------------
-- Generate partitions for 
-- Children problems of a 
-- partition 'm' 
---------------------
generateChildren = method(TypicalValue=>List)
generateChildren(Sequence, List, List) := (kn, l, m) -> (
     (k,n):=kn;
     L := apply(#m, i->if n-k-l#(k-i-1)>=m#i+1 then take(m,i)|{m#i+1}|drop(m,i+1));
     select(L, a-> a=!=null and a==reverse sort a)
)
-------------------
-- find the position where you need
-- to add a solution of the children problem
-- to have a solution of the parent problem
------------------
positionVariableChildren = method(TypicalValue=>ZZ)
positionVariableChildren(Sequence,List,List,List):=(kn,l,m,v)->(
   -- kn is a sequence (k,n)
   -- l, m are partitions
   -- v is a children partition of m 
   (k,n) := kn;
   i := maxPosition(v-m);
   t := apply(i+1, j->plus(n-k-m_(j)-l_(k-j-1)));
   sum(t)
)

-----------------------
-- precookPieri
--
-- creates a special matrix G_\mu
-- and attach it to E_{\mu\lambda}
----------------------- 
precookPieriHomotopy = method(TypicalValue=>List)
precookPieriHomotopy(Sequence,List,List) := (kn,l,m)->(
     -- k and n are the integers defining the Grassmanian G(k,n)
     -- l and m are partitions of n
     (k,n) := kn;
     l = verifyLength(l, k);
     m = verifyLength(m, k);
     E := skewSchubertVariety(kn,l,m);
     ------------
     -- d is the number of variables i.e. the codimension of the Schubert variety E_{l,m}
     ------------
     d := (k*(n-k)-sum(l)-sum(m));
     S := FFF[vars(53..d+52)];
     T:= apply(#m, i->n-k+i-m#i);
     -- P is a list with the indeces where the special flag has ones
     P:=toList(set toList(0..n-1)-T);
     G:=mutableMatrix(S,n-k,n);
     apply(#P, j->G_(j,P#j)=1);
     F:=matrix E || sub(matrix G, ring E);
     return F;
)

--------------------------
-- Given the partitions l, and m for the Grassmannian Gr(k,n)
-- it creates a flags as random numeric matrices G_1,...,G_m
-- for solving the system defined by these random matrices
-- using Homotopy Continuation
--------------------------
createRandomFlagsForSimpleSchubert = method( )
createRandomFlagsForSimpleSchubert(Sequence, List, List) := (kn,l,m)->(
	 (k,n) := kn;
	 l = verifyLength(l, k);
	 m = verifyLength(m, k);
   d := k*(n-k)-sum(l)-sum(m);
   apply(d, i->matrix apply(n-k,i->apply(n,j->random FFF)))
   )

     
solveSimpleSchubert = method(TypicalValue=>List)
solveSimpleSchubert(Sequence,List,List,List) := (kn,l,m,G)->(
   -- l and m are partitions of n
   -- G is a flag
   (k,n) := kn;
   l = verifyLength(l, k);
   m = verifyLength(m, k);
   d := k*(n-k)-sum(l)-sum(m);
   E := skewSchubertVariety(kn,l,m);
   if solutionsHash#?{l,m,G} then(
   	 solutionsHash # {l,m,G}
   )
   else if d == 1 then (
      -- solve linear equation
      solutionsHash#{l,m,G} = solveEasy det (matrix E || sub(G#0, ring E), Strategy=>Cofactor)
   )
   else(
      -- generate the children problems
      L:=generateChildren(kn, l,m);
      
      -- once the children problems are solved
      -- store solutions in "start" 
      start := flatten(apply(L, p->(
         C := solveSimpleSchubert(kn,l,p,G);
         i := positionVariableChildren((k,n),l,m,p);
         apply(C,c->insert(i-1,0,c))
         )
      ));
      ---- Create the start system S  ----
      S := apply(take(G,d-1), g->det( matrix E || sub(g, ring E),Strategy=>Cofactor)) | {det(precookPieriHomotopy(kn,l,m), Strategy=>Cofactor)};
      ---- Create the target system T ----
      T := apply(take(G,d), g->det( matrix E || sub(g, ring E), Strategy=>Cofactor)); 
      newR := FFF(monoid[gens ring first S]);
      S = S/(s->sub(s,newR));
      T = T/(t->sub(t,newR));
      ------------------------
      -- make sure your starting set of solutions are in fact solutions
      -- of the Starting system S
      ------------------------
      assert all(start, s->norm sub(matrix{S},matrix{s}) < 1e-3);
      solutionsHash#{l,m,G} = track(S,T,start,gamma=>exp(2*pi*ii*random RR)) / coordinates;
      ---------------------
      ---- make sure that you got solutions of the Target System --
      ---------------------
      assert all(solutionsHash#{l,m,G}, s->norm sub(matrix{T},matrix{s}) < 1e-3);
      solutionsHash#{l,m,G}
   )
)


-----------------------------
---- function written to solve
---- a simple linear equation
solveEasy = method(TypicalValue=>CC)
solveEasy(RingElement) := (p)->(
   R:=ring p;
   var:=support p;
   b:=part(0,p);
   a:=p_(var_(0));
   -- print(p,a,b);
   {{toCC sub((-b)/a, coefficientRing R)}}
)


--------------------------------------
--- trackSimpleSchubert
--------------------------------------
---
--- A function to find solution from a specific instance 
--- of a Schubert problem using homotopy 
--- continuation starting from solving
--- another instance (hopefully easier) of
--- the Schubert problem, but with respect 
--- to a different flag
--------------------------------------

trackSimpleSchubert = method(TypicalValue=>List, Options=>{Memoize => false, StartSolutions=>null})
trackSimpleSchubert(Sequence, Sequence, List, List) := o->(kn,cond,G,F) ->(
   -- G is the start flag and F the target flag
   -- k and n are integers defining the Grassmannian G(k,n)
   (k,n) := kn;
   -- l and m are partitions of n
   (l,m) := cond;
   Sols:= (if o.StartSolutions === null then solveSimpleSchubert(kn,l,m,G) else o.StartSolutions);
   E := skewSchubertVariety(kn,l,m);
   Start:=apply(G, g->det( matrix E || sub(g, ring E),Strategy=>Cofactor));
   Target:=apply(F,f->det( matrix E || sub(f, ring E),Strategy=>Cofactor));
   Ret:=track(Start,Target,Sols,gamma=>exp(2*pi*ii*random RR)) / coordinates;
   if o.Memoize then solutionsHash#{l,m,F} = Ret;  
   return Ret;
)

-----------------------------
-- Numerical LR-Homotopies
-----------------------------

---------------------
-- input: two schubert conditions l and m
--			entered as brackets
--		  the Grassmannian G(k,n)
--
-- Output: checkboard coordinates for the 
--         red checkers
---------------------
-- example: for {2,1}*{2} in G(3,6)
--
--partition2bracket({2,1},3,6)
--partition2bracket({2},3,6)
--redChkrPos({2,4,6},{2,5,6},3,6)
--------------------
redChkrPos = method(TypicalValue => List)
redChkrPos(List,List,ZZ,ZZ) := (l,m,k,n) -> (
     -- input the Schubert conditions l and m
     -- as bracket
     -- input the Grassmannian G(k,n)
     m = reverse m;
     board := for i to n-1 list NC;
     redPos := new MutableList from board;
     apply(#l, j -> redPos#(l#j-1) = m#j-1);
     toList redPos
)
------- TEST -----
-- first, given the partitions 
-- {2,1}*{2} in G(3,6)
-- we test if the positions of the 
-- redcheckers is {NC,5,NC,4,NC,1}
------------------
-- partition2bracket({2,1},3,6)
-- partition2bracket({2},3,6)
-- redChkrPos({2,4,6},{2,5,6},3,6)
-- redChkrPos(partition2bracket({2,1},3,6),partition2bracket({2},3,6),3,6)

--####################
-- "moveRed" moves the red checkers
--
-- input:
--       blackup - Coordinates of the ascending black checker
--       blackdown - Coordinates of the descending black checker
--       redpos - List of red checker positions
--
--	output: {(repos,typeofmove,critrow)} or {(repos1,typeofmove1,critrow),(repos2,typeofmove2,critrow)}
--       redpos - Updated list (of lists) of red checker positions
--       typeofmove - {row,column,split}
--                    a tuple which tells the type of the move we had to perform from
--                    the 3x3 table of moves. This is given as a 
--                    tuple {row,column,split} where split says
--                    if you moved or not the red checkers
--                    (by 0 and 1 respectively) when there was a split
--        critrow - the critical row
moveRed = method(TypicalValue => List)
moveRed(List,List,List) := (blackup, blackdown, redposition) -> (
    ------------------------------------------------
    -- We need to check first if it is a valid configuration
    -- that is why I have been having errors
    ------------------------------------------------
    n := #redposition; -- n is the size of the checkboard
    split:=0;
    critrow := 0;
    critdiag := 0;
    g:=2; -- These are two flags to indicate in which situation we are 
    r:=2;
    indx := new List;
    redpos := new MutableList from redposition;
    -- find the "critical row"
    indx = for i to n-blackdown#0-1 list n-1-i;
    apply(indx, j -> (
	    if redpos#j === blackdown#1 then (
	       	critrow = j;
	       	if j == blackdown#0 then g=0 else g=1;
	  	) 	
     	    ));    
    -- find the "critical diagonal"
    indx= for i to blackdown#0-1 list i;
    indx = reverse indx;
    apply(indx, j->(
	    if blackdown#0-j+redpos#j == n then(
	       	critdiag = j;
	       	if blackup === {j,redpos#j} then r=0 else r=1;
	  	)
     	    ));
    if r == 0 then (
	redpos#(blackup#0)=redpos#(blackup#0)-1;
	if g == 0 then redpos#(blackdown#0) = redpos#(blackdown#0)+1;
	if g == 1 then redpos#critrow = redpos#critrow + 1;
     	) else if r == 1 then (
	if g == 0 then(
	    redpos#critrow = redpos#critdiag;
	    redpos#critdiag = NC;
	    redpos#(blackup#0) = blackdown#1;
	    ) else if g == 1 then(
	    block := 0;
	    blockindx := for i to critrow-1-critdiag-1 list critrow-1-i;
	    apply(blockindx, b -> if redpos#critrow < redpos#b and redpos#b < redpos#critdiag then block = 1);
	    if block != 1 then (
		-- switch the rows of the red checkers in the critical diagonal and row
		-- then, move the left one over to the column of the ascending black
		redpos#critrow = redpos#critdiag;
		redpos#critdiag = NC;
		redpos#(blackup#0) = blackdown#1;
		split = 1;
	       	);
	    );
     	) else if r == 2 and g == 0 then (
	redpos#(blackup#0)=redpos#critrow;
	redpos#critrow = NC;
     	);
    if split == 0 then {(toList redpos,{r,g,split})} else {(redposition,{r,g,0}), (toList redpos,{r,g,split})}
    )
-- TEST THE FUNCTION HERE!!

--moveRed({0,2},{2,1},{NC,3,NC,1})
-- The output must be
-- {{NC, 3, NC, 1, 1, NC, NC, 3},1}

--moveRed({0, 3}, {1, 2}, {NC, 3, NC, 1})
--moveRed({0, 2}, {2, 1}, {NC, 3, NC, 1})
--moveRed( {1, 3}, {2, 2}, {NC, 3, NC, 1, 1, NC, NC, 3})
--moveRed({0, 1}, {3, 0}, {NC, 2, NC, 1, 1, NC, NC, 3})
--moveRed({ 1, 2}, {3, 1}, {NC, 2, NC, 1, 1, NC, NC, 3})
--moveRed({2, 3}, {3, 2}, {NC, 1, NC, 2, 1, NC, NC, 3})
--moveRed({1, 3}, {2, 2}, {1, NC, NC, 3})
--moveRed( {0, 1}, {3, 0}, {1, NC, NC, 3})
--moveRed({1, 2}, {3, 1}, {0, NC, NC, 3})
--moveRed({2, 3}, {3, 2}, {0, NC, NC, 3})

moveCheckers = method(TypicalValue => List)
moveCheckers Array := blackred -> (
     blackposition := first blackred;
     redposition := last blackred;
     n := #redposition; -- n is the size of the board
     splitcount:=0;
     copies:=0;
     -- determine the columns of the descending and ascending black checkers
     -- blackdown1 is the column to the right of the column of the lowest black checker
	 -- blackup1 is the column of the checker that is one row lower than the checker 
	 --        in blackdown1 
     blackdown1 := position(blackposition, x->x == n-1) + 1;
     if blackdown1 == n then return ({},"leaf");
     blackup1 := position(blackposition, x-> x == 1+blackposition#blackdown1);
     -- The column of the right black checker to be sorted goes from desccol 
     -- to the end of the board.
     -- Determine the rows of the next pair of black checkers to be sorted.
	 blackup2 := n-blackdown1+blackup1;
	 blackdown2 := blackup2-1; -- this is the critical row
	 listofredpositions := moveRed({blackup1,blackup2},{blackdown1,blackdown2}, redposition);
	 blackposition = new MutableList from blackposition;
	 blackposition#blackup1 = blackposition#blackup1 - 1;
	 blackposition#blackdown1 = blackposition#blackdown1 + 1;
	 (
	      apply(listofredpositions, r-> [toList blackposition, 
		   	first r, -- new redposition
		   	last r -- new type of move
		   	]), 
	      blackdown2 --return also the critical row
	      )
)
--------------
-- TEST
--moveCheckers({3,5,4,2,1,0},{3,NC,NC,5,NC,1})

-----------------
-- playCheckers
-----------------
-- Function that gets a specific node and plays
-- a checkerboard game between two varieties X1 and X2
--
-- It sets up the game, and then it uses
-- the combinatorial LR-rule to make deformations
-- between the Schubert variety X2 to X1
-- It stores all the information in a HashTable
-------------------
-- If we want to compute X1\cap X2 \cap X3 \cap...\cap Xn
-- we first play the checkers with X1 and X2
--
-- input1:
--         l1, l2, two partitions (representing X1 and X2)
--         k,n the Grassmannian where they live
-- Output1:
--         Dag - a Hashtable with all the checkermoves played
--
-------------------
playCheckers = method(TypicalValue => MutableHashTable)
playCheckers(List,List,ZZ,ZZ) := (partn1,partn2,k,n) -> (
     all'nodes := new MutableHashTable;
     redChkrs := 
     if partn1 > partn2 then
     	  redChkrPos(partition2bracket(partn2,k,n),partition2bracket(partn1,k,n),k,n)
      else 
          redChkrPos(partition2bracket(partn1,k,n),partition2bracket(partn2,k,n),k,n)
     ;
     blackChkrs := reverse toList (0..(n-1)); --initial black positions
     --///////////////////////////////
     -- If two checkers produce no solutions, we can uncomment the code bellow
     -- and it will check if it is an "invalid checkerboard" then it won't try
     -- to play checkers but to output no solutions
     -- ////////////////////////////
     --if select(#redChkrs, i-> (redChkrs)_i < #redChkrs - i -1)!={} then(
	--  self := new MutableHashTable from{
	--  Board => [blackChkrs, redChkrs], 
	--  Fathers => {},
	--  Children => {},
	--  Solutions => {},
	--  CriticalRow => "Schubert Problem with no solutions"
	--  };
        --  self
	--  )
     --else(
     print "this is the root";
     print([blackChkrs, redChkrs]);
     root := playCheckers ([blackChkrs, redChkrs], null, {}, all'nodes);  -- returns the root of the tree
     if DEBUG'LEVEL>1 then print VerticalList keys all'nodes;
     root
    -- )
)

------------------------
-- PlayCheckers will also play the next checkerboard game
-- in the Tournament.
-- Input: 
--       board 
--       father (the checkergame this game came from)
--       typeofmove?
--       all'nodes - the list of games played already
--
-- THIS IS THE RECURSIVE CALL OF PLAYCHECKERS
----------------------------
playCheckers (Array,Thing,List,MutableHashTable) := (board,father,typeofmove,all'nodes) ->(
    -- Document this function!! it is not understandable
    -- all'nodes is a HashTable whose keys are boards,
    -- and this is where we store all nodes that we have
    -- already visited.
    --
    -- Abr started the documentation of this function on Feb 6, 2013
    --------------------------------------------
     node'exists := all'nodes#?board; -- check if we already played this game
     self := if node'exists  
     then all'nodes#board  -- if so, then glue solutions, otherwise, start a new hashtable
     else new MutableHashTable from {
	  Board => board, 
	  IsResolved => false,
	  Fathers => {}
	  };
     if father=!=null then self.Fathers = self.Fathers | {(father,typeofmove)};
     if not node'exists then ( --add the ultimate node part here...
--<< "this is node'exists "<< node'exists<<endl;
	 coordX := makeLocalCoordinates board; -- local coordinates X = (x_(i,j))
     	 if numgens ring coordX > 0 then ( 
     	     (children,c) := moveCheckers board;
     	     self.CriticalRow = c;
     	     self.Children = apply(children, b -> playCheckers (take(b,2),self,last b,all'nodes));
	     );
	 all'nodes#board = self;
	 );
     self
)

printTree = method()
printTree MutableHashTable := node ->(
	print peek node;
	scan(node.Children, c-> printTree c);
)

--Examples:
--verifyLength({2,1,1,1},3)
--partition2bracket({2,1},3,7)
--partition2bracket({1,1},3,7)
--partition2bracket({1},3,7)
--bracket2input({4,6,7},7)
--output2bracket({NC, NC, NC, 3, NC, 5, 6})
--bracket2partition({2,4,6},6)

--redChkrPos(partition2bracket({2,1},3,6),partition2bracket({1,1},3,6),3,6)


--playCheckers({1,1},{2,1},3,6)
--playCheckers({1,1},{2,0},2,4)
--playCheckers({1},{1},2,4)
--playCheckers({1,1},{2,1},3,6)
--playCheckers({2,1},{1,1},3,6)


-----------------
--- makeLocalCoordinates
--
-- This procedure will translate a checker 
-- board configuration into a matrix with
-- 0's, 1's and variables
-----------------
-- input: an array of black and red checkers
--        in the form ( ListofPositionsBlack, ListofPositionsRed)
-- output: a matrix with local coordinates
-----------------
-- example:
-----------------
makeLocalCoordinates = method(TypicalValue => MutableMatrix)
makeLocalCoordinates Array := blackred ->(
  blackposition := first blackred;
  redposition := last blackred;
  VAR := symbol VAR;
  n := #redposition; -- n is the size of the board
  -- we find how many black checkers are in northwest to a given red
  rowsred := sort select(redposition, r->r=!=NC);
  colsred := apply(rowsred, r -> position(redposition, j-> j == r));
  E := new MutableHashTable;
    for r to #rowsred-1 do(
      E#(rowsred#r,r) = 1;
      variablerows := take(blackposition,colsred#r+1);
      variablerows = select(variablerows, b-> b< rowsred#r);
      scan(variablerows, j->(
        if member(j,rowsred) and position(redposition, i-> i == j) < colsred#r then
	  variablerows = delete(j,variablerows);
      ));
      scan(variablerows, col-> (
        E#(col,r)=VAR;
      ));
   );
   x:= symbol x;
   R:=FFF[apply(select(sort keys E, k-> E#k===VAR), k-> x_k)];
   X := mutableMatrix(R,n,#rowsred);
   scan(keys E, k-> X_k = if E#k === 1 then 1 else x_k);
   matrix X
)

------------------
-- resolveNode
------------------
-- A function that will be the skeleton
-- of the homotopy.
-- It first transform the Flag of the child Sch. Var
-- into a generalized flag for the parent
------------------
resolveNode = method()
resolveNode(MutableHashTable,List) := (node,remaining'conditions'and'flags) ->  
if not node.IsResolved then (
   n := #node.Board#0;
   coordX := makeLocalCoordinates node.Board; -- local coordinates X = (x_(i,j))
   if numgens ring coordX == 0 then ( -- We need to move this block to playcheckers
	     print "great success: we hit the ULTIMATE LEAF";
	     node.Solutions = {lift(coordX,FFF)};
	     node.IsResolved = true;
	     node.FlagM= rsort id_(FFF^n);
--	  if #remaining'conditions'and'flags > 0
--	  then error "invalid Schubert problem"
--	  else node.Solutions = {lift(coordX,FFF)};
	  )
   else ( -- coordX has variables
     black := first node.Board;
          
     if node.Children == {} then node.FlagM = matrix mutableIdentity(FFF,n) --change here
     else scan(node.Children, c->resolveNode(c,remaining'conditions'and'flags));
     
     if DEBUG'LEVEL >= 2 then (
	 -- temporary: creates a superset of solutions via a blackbox solver
	 all'polynomials := makePolynomials(node.FlagM * coordX, remaining'conditions'and'flags);
	 polynomials := squareUpPolynomials(numgens ring coordX, all'polynomials);
	 ---* this part is to time and keep track of what is the expensive part of the computation
	 if DEBUG'LEVEL == 3 then blckbxtime1 := cpuTime();
	 ---*
	 Soluciones:=solveSystem flatten entries polynomials;
	 ---*
	 if DEBUG'LEVEL == 3 then (
	     blckbxtime2 := cpuTime();
	     <<"Blackbox solving cpuTime:"<<(blckbxtime2 - blckbxtime1)<<endl;
	     );
	 ---*
	 node.SolutionsSuperset = apply(
	     select(
		 --// After finish with the timing, remove the previous part and the next line
		 --// and uncomment the following line (deleting the line after that)
		 Soluciones,
		 --time solveSystem flatten entries polynomials, 
		 s-> norm sub(gens all'polynomials,matrix s) <= ERROR'TOLERANCE * 
		 norm matrix s * 
		 norm sub(last coefficients gens all'polynomials,FFF)
		 ), 
	     ss-> (map(FFF,ring coordX,matrix ss)) coordX
	     );
	 ); -- close if DEBUG'LEVEL>= 2 HERE
     if node.Children == {} then ( 
	 lambda := output2partition(last node.Board);
	 ------------------------
	 -- THIS WAS PRECONDITIONED WITH DEBUG'LEVEL BUT NOW IT'S NOT
	 --if DEBUG'LEVEL >= 4 then (
	 print "calling solveSchubertProblem from resolveNode ";
	 print(node.Board);
	 print(lambda);
	 print("remaning Conditions:");
	 print(remaining'conditions'and'flags);
	 print("---------");
	 ---------------------------
	 k:=#lambda;
	 validpartition := true;
	 scan(lambda, i-> if i>n-k then validpartition = false) ;
	 if validpartition then(
	     S := solveSchubertProblem(
		 prepend(
		     (lambda,id_(FFF^n)), -- check that this is the correct flag!!! 
		     remaining'conditions'and'flags
		     ),
		 k,n);
	     ---------------------
	     -- April 12:
	     ---------------------
	     -- Had to clear zeroes in the matrix
	     -- after transforming the matrix to be 
	     -- with respect to the local coordinates
	     node.Solutions = apply(S,s->sub(coordX, clean(ERROR'TOLERANCE,matrix{solutionToChart(s,coordX)})));
	     -- need to make this better (need to clean only those small entries below the pivots of the chart)
	     ---------------------------
	     -- Newton iteration test!!
	     ---------------------------
	     --  Aug 20,2013
	     ---------------------------
	     --squareSyst := flatten entries gens makePolynomials(coordX, remaining'conditions'and'flags);
	     --polysquares := squareUpPolynomials(numgens ring coordX, ideal(squareSyst));
	     --Sols:=  apply(node.Solutions, X->toRawSolutions(coordX,X));
	     --NewtonStep1 := refine(squareSyst, Sols, Software=>M2, Iterations=>1);
	     --NewtonStep2 := refine(squareSyst, NewtonStep1, Software=>M2, Iterations=>1);
	     print "And These are the solutions obtained:";
	     print(node.Solutions);
	     --print("The first Newton step:");
	     --print(NewtonStep1);
	     --print("distance between solutions and newton1");
	     --print(dist(NewtonStep1,Sols));
	     --print("distance between two newton steps:");
	     --print(dist(NewtonStep2,NewtonStep1));
	     --dist(NewtonStep2,Sols);
	     node.IsResolved = true;
	     -- assert(???); --verify that the solutions fit the localization pattern 
	     )else(   
	     << "-- partition is not valid: " << lambda << endl;
	     node.Solutions = {}; 
	     );
	 --) -- closes if DEBUG'LEVEL == 4
	 --else node.Solutions = node.SolutionsSuperset; -- should change!!! 
	 ); 
     scan(node.Fathers, father'movetype->(
     	  (father,movetype) := father'movetype; 
     	  if DEBUG'LEVEL > 0 then << "-- FROM " << node.Board << " TO " << father.Board << endl;
     	  if DEBUG'LEVEL == 1 or DEBUG'LEVEL == 3 then(
     	       tparents1:=cpuTime();
     	       );
     	  r := father.CriticalRow; -- critical row: rows r and r+1 are the most important ones
          red := last father.Board;     
     	  red'sorted := sort delete(NC, red);
	  M := node.FlagM;
	  M'':= M_{0..(r-1)} | M_{r} - M_{r+1} | M_{r}| M_{(r+2)..(n-1)};
	  if not father.?FlagM then father.FlagM = M'' 
	  else if DEBUG'LEVEL>0 then assert (father.FlagM == M'');
	  
	  --if movetype=={2,2,0} and r == 1 then 1/0;
	  parent'solutions :=  -- THIS IS WHERE THE MAIN ACTION HAPPENS
	  if node.Solutions == {} then {} -- means: not implemented
	  else if movetype#1 == 2 then (-- case (_,2)
     	       apply(node.Solutions, X->(
		    	 X'' := (X^{0..r-1}) || (-X^{r+1}) || (X^{r}+X^{r+1}) ||( X^{r+2..n-1});
			 j := position(red'sorted, i-> i==r+1);-- the column we need to normalize
		    	 if j=!=null then redCheckersColumnReduce2(normalizeColumn(X'',r+1,j),father)
			 else X''
	       	    	 ))
	       )
	  else ( -- cases STAY and SWAP require homotopy 
	       R := ring coordX;
	       t := symbol t;
	       Rt := (coefficientRing R)[t,gens R]; -- homotopy ring
	       mapRtoRt := map(Rt,R,drop(gens Rt,1));
	       Xt := mapRtoRt coordX; --  "homotopy" X 
	       local M'X'; -- homotopy in global coordinates (produced by each case)
	       
	       -- these are used only in SWAP cases
	       s := position(red'sorted, i->i==r); -- number of the first moving red checker
	       VwrtM := map(Rt^n,Rt^0,{}); -- an empty column vector

	       if member(movetype,{{2,0,0},{2,1,0},{1,1,0}}) then (-- case "STAY"
		    -- V(t) = M'(t) X'(t) .......... we write everything in terms of M
		    scan(#red'sorted, j-> VwrtM = VwrtM |
		    	 if isRedCheckerInRegionE(
			      position(red,i->i==red'sorted#j), -- column of the j-th red checker on the board
			      father) 
			 then (
		    	      --submatrix(Xt,{0..r-1},{j}) 
		    	      --|| submatrix(Xt,{r},{j}) + submatrix(Xt,{r+1},{j})
		    	      submatrix(Xt,{0..r},{j}) 
		    	      || matrix{{0_FFF}}
		    	      || submatrix(Xt, {r+2..n-1}, {j})
			      ) else (
		    	      submatrix(Xt,{0..r},{j}) 
		    	      || submatrix(Xt,{r+1},{j})-t*submatrix(Xt,{r},{j})
		    	      || submatrix(Xt, {r+2..n-1}, {j})	    
			      )
		    	 );
	       	    M'X' = promote(M,Rt) * VwrtM;
		    ) 
	       else if member(movetype,{{1,0,0},{1,1,1},{0,0,0},{0,1,0}}) then (-- case SWAP(middle row)		    
	       	    bigR := red'sorted#(s+1); -- row of the second moving red checker
		    rightmost'col'B := position(black, j->j==r);
		    leftmost'col'A := position(black, j->j==r+1)+1;
 		    
		    -- check if the black checker in the i'th row is in region A
		    isRegionA := i -> position(black, i'->i'==i) >= leftmost'col'A;	     
		    -- check if the black checker in the i'th row is in region B
		    isRegionB := i -> position(black, i'->i'==i) <= rightmost'col'B;
		    	       	    
		    -- V(t) = M'(t) X'(t) .......... we write everything in terms of M
		    scan(#red'sorted, j-> VwrtM = VwrtM |
		    	 if j == s then ( -- note: this part can be optimized for speed
			      transpose matrix { apply(n,i->(
					if i==r then Xt_(r+1,s+1)
					else if i==r+1 then -t*Xt_(r+1,s+1)
					else if isRegionA i then -t*Xt_(i,s+1)
					else if isRegionB i then Xt_(r+1,s+1)*Xt_(i,s)
					else 0
					)) }
			      )
			 else if j == s+1 then (
			      transpose matrix { apply(n,i->(
					if i==bigR then 1
					else if i==r+1 then Xt_(r+1,s+1)
					else if i==r then 0
					else Xt_(i,s+1)
					)) }
			      )
			 else if isRedCheckerInRegionE(
			      position(red,i->i==red'sorted#j), -- column of the j-th red checker on the board
			      father
			      ) 
			 then (
		    	      submatrix(Xt,{0..r-1},{j}) 
		    	      || submatrix(Xt,{r},{j}) + submatrix(Xt,{r+1},{j})
		    	      || matrix{{0_FFF}}
		    	      || submatrix(Xt, {r+2..n-1}, {j})
			      ) else (
		    	      submatrix(Xt,{0..r},{j}) 
		    	      || submatrix(Xt,{r+1},{j})-t*submatrix(Xt,{r},{j})
		    	      || submatrix(Xt, {r+2..n-1}, {j})	    
			      )
		    	 );
	       	    M'X' = promote(M,Rt) * VwrtM;
		    )
	       -- implementing this case separately gives lower degree polynomials
	       --else if member(movetype,{{0,0,0},{0,1,0}}) then (-- case SWAP(top row)		    
	       --    )
	       else error "an unaccounted case";

	       if DEBUG'LEVEL == 1 or DEBUG'LEVEL == 3 then timemakePolys1 := cpuTime();
	       all'polys := makePolynomials(M'X',remaining'conditions'and'flags);
	       if DEBUG'LEVEL == 1 or DEBUG'LEVEL == 3 then(
		    timemakePolys2 := cpuTime();
		    << "-- time to make equations:  "<< (timemakePolys2-timemakePolys1)<<endl;
		    );
    
	       polys := squareUpPolynomials(numgens R, all'polys);
	       startSolutions := apply(node.Solutions, X->toRawSolutions(coordX,X));
	       
	       -- track homotopy and plug in the solution together with t=1 into Xt
	       scan(startSolutions,  
		    s->assert(norm sub(polys,matrix{{0_FFF}|s}) < ERROR'TOLERANCE * 
			 norm matrix{s} * 
			 norm sub(last coefficients polys,FFF)));
	       if DEBUG'LEVEL == 1 or DEBUG'LEVEL == 3 then( 
	       	    t1:= cpuTime();
		    );
	       targetSolutions := trackHomotopy(polys,startSolutions);
	       if DEBUG'LEVEL == 1 or DEBUG'LEVEL == 3 then(
	       	    t2:= cpuTime();
	       	    << node.Board << " -- trackHomotopy time: " << (t2-t1) << endl;
	       	    );
	       apply(targetSolutions, sln->( 
		    M''X'' := (map(FFF,Rt,matrix{{1}}|matrix sln)) M'X';
		    X'' := inverse M'' * M''X'';
		    if not member(movetype,{ {2,0,0},{2,1,0},{1,1,0} }) -- SWAP CASE
		    then (
     			 k := numgens source X'';
			 X'' = X''_{0..s}| X''_{s}+X''_{s+1}| X''_{s+2..k-1}; -- we substitute the s+1 column for the vector w_{s+1}
		    	 redCheckersColumnReduce2(normalizeColumn(X'',r,s),father)
			 ) 
		    else --redCheckersColumnReduce2(
		    normalizeColumn(X'',r,s)
		    --,father) -- !!!
		    ))
	       );
     	  -- else {}; -- means: not implemented
	  if DEBUG'LEVEL >= 2 then (
	       -- verify solutions
	       parentX := makeLocalCoordinates father.Board;
	       parentXlist := flatten entries parentX;
	       scan(parent'solutions, X'''->( 
			 -- check that solutions fit the parent's pattern
		    	 a := flatten entries X''';
		    	 scan(#a, i->assert(
			      	   (abs a#i < ERROR'TOLERANCE and parentXlist#i == 0)
			      	   or (abs(a#i-1) < ERROR'TOLERANCE and parentXlist#i == 1)
			      	   or (parentXlist#i != 0 and parentXlist#i != 1)
			      	   ));
		    	 ));
	       );
	  if not father.?Solutions then father.Solutions = {};  
	  father.Solutions = father.Solutions | parent'solutions;
     	  
	  if DEBUG'LEVEL == 1 or DEBUG'LEVEL == 3 then(
     	       tparents2:=cpuTime();
     	       << "time of computing one edge: "<< (tparents2 - tparents1) << endl;
     	       );
     	  ));
     if DEBUG'LEVEL >= 2 then(
     	  -- check against the blackbox solutions
     	  scan(node.Solutions, X->
	       assert(position(node.SolutionsSuperset, Y->norm(Y-X)<ERROR'TOLERANCE) =!= null)); 
     	  );
     ); -- END coordX has variables
     node.IsResolved = true;
   ) -- END resolveNode


---------------
-- solveSchubertProblem
---------------
-- Function that solves a Schubert problem
-- by first taking two of the conditions,
-- then create a tree (with nodes) by playing a 
-- checker game, then resolve the node numerically
-- using homotopies, and gluing the solutions to each
-- node
---------------
-- input:
--    SchPblm := list of Schubert conditions with flags
--    k,n := the Grassmannian G(k,n)
-- output:
--    list of solutions
---------------
solveSchubertProblem = method()
solveSchubertProblem(List,ZZ,ZZ) := (SchPblm,k,n) ->(
    -- SchPblm is a list of sequences with two entries
    -- a partition and a flag
    twoconds := take(SchPblm,2);
    remaining'conditions'and'flags := drop(SchPblm,2);
    -- take the first two conditions
    l1:=verifyLength(first first twoconds,k);
    l2:=verifyLength(first last twoconds,k);
    F1:=promote(last first twoconds,FFF);
    F2:=promote(last last twoconds,FFF);
    
    Slns:={};
    checkOrthogonal := l1+reverse l2;
    if sum(checkOrthogonal) == k*(n-k) and #(unique checkOrthogonal)>1 then 
       Slns
    else(
	newDag := playCheckers(l1,l2,k,n);
	resolveNode(newDag, remaining'conditions'and'flags);
	conds := {l1,l2};
	--Changed here March 22:
	--
	-- resolveNode gives a solution in local coords
	-- of {l1}*{l2} w.r.t {FlagM, Id}
	-- we multiply the solution S by FlagM
	-- and we obtain a solution of the Sch. problem
	-- {l1,...,lm} with respect to
	-- {FlagM, Id, F3,...,Fm}
	-- 
	-- we need to make a change of flags
	-- to send (FlagM,Id)-->(Id,F2)
	-------------------------------
	localFlags := {
	    newDag.FlagM,
	    id_(FFF^n)
	    };
	localFlags1:= {id_(FFF^n),F2}; --
	Transf := moveFlags2Flags(localFlags,localFlags1);
	-- Transf gives three matrices
	-- A,T1,T2, such that
	--          A*FlagM = Id*T1 (representing the standard flag)
	--          A*Id = F2*T2 (representing the same flag as F2)
	GL := first Transf;
	T1:=Transf#1;
	Flags1 := {id_(FFF^n),F2}; 
	Flags2:= {F1,F2};
	scan(remaining'conditions'and'flags, c-> (
		conds = append(conds, first c);
		Flags1 = append(Flags1, GL*(last c));
		Flags2 = append(Flags2, last c);
		));
	-----------------------------
	-- August 20, 2013:
	-----------------------------
	-- AFter the transformation (GL*newDag.FlagM*newDag.Solutions)
	-- the solutions obtained are ALMOST with respect to the local chart
	-- corresponding to the Schubert Variety (l1, Id)... but we need
	-- to clear the entries that are below the pivots first...
		
	-- doing cleanSolutions := apply(GL*newDag.FlagM*newDag.Solutions, s->clean(ERROR'TOLERANCE^2, s));
	-- is not the right way to clean...need to clean zeroes below the pivots only, this is
	-- just a hack
	
	----------------------------
	-- IMPORTANT!
	-----------------------------
	-- We need to check if the solutions newDag.Solutions
	-- after the change of coordinates and flags, are actual
	-- solutions to our original problem... we need to do
	-- another Newton step here!
	changeFlags(GL*newDag.FlagM*newDag.Solutions, -- these are matrices in absolute coordinates
	    (conds, Flags1, Flags2))
	)
    )-- end of solveSchubertProblem


----------------------------
-- changeCoordsSolutions
----------------------------
-- !!! this function should be removed!!!
changeCoordsSolutions = method()
changeCoordsSolutions Matrix := MX ->(
    k := numcols MX;
    n := numrows MX;
    a := symbol a;
    RMX := ring MX;
    indx:= subsets(0..n-1,k)/toSequence;
    Vars := apply(indx, i-> a_(i));
    R:= (coefficientRing RMX)[Vars, gens RMX];
    G := mutableIdentity(R,n);
    scan(indx, i->G_i=a_i);
    --s:= mutableMatrix random(FFF^n,FFF^k);
    Temp:=entries transpose MX;
    zeroes:=apply(Temp, t->position(t, i-> i==1));
    s := transpose matrix apply(zeroes, i->(
	    cl:=for j from 0 to i list 1;
	    cl2:=for j from i to n-1 list 0;
	    cl|drop(cl2,1)	    
	    ));
    f:=flatten entries(matrix G*sub(s,R)-sub(MX,R));
    nk := n*k;
    numParameters := #Vars+#gens RMX;
    A:= map(FFF^nk, FFF^numParameters, (i,j)-> (f#i)_(R_j));
    b := map(FFF^nk, FFF^1, (i,j)-> -(f#i)_(1_R));
    X := solve(A,b, ClosestFit=>true);
    Vals:=take(flatten entries X, #Vars); -- take a_(i,j) coordinates
    scan(#indx, i->G_(indx#i) = Vals#i);
    sub(matrix G, coefficientRing RMX)
    )

--------- March 24, 2013
-- created a linear homotopy
-- from one set of flags to another
-- by changing column by column
-- for each of the flags 
--
-- Later, we can speed up a little
-- by just creating the homotopy
-- between the flags, by changing
-- only the relevant parts of the 
-- flag...
--------------------------
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
solutionToChart = method() -- writes s (a matrix solution) in terms the chart MX (as a list of values of the parameters)
solutionToChart(Matrix, Matrix) := (s,MX) -> (
    k := numcols s;
    n := numrows s;
    a := symbol a;
    RMX := ring MX;
    R := (coefficientRing RMX)[a_(1,1)..a_(k,k),gens RMX];
    G := genericMatrix(R,k,k);
    f := flatten entries(s*G - sub(MX,R)); -- linear system in nk vars 
    nk := n*k;
    nParameters := k^2+#gens RMX; -- number of parameters in f
    A := map(FFF^nk,FFF^nParameters,(i,j)->(f#i)_(R_j));
    b := map(FFF^nk,FFF^1,(i,j)->-(f#i)_(1_R));
    X := solve(A,b, ClosestFit=>true);
    drop(flatten entries X, k*k) -- drop a_(i,j) coordinates      
    )
changeFlags = method()
changeFlags(List, Sequence) := (solutionsA, conds'A'B)->( -- solutionsA is a list of matrices
   if #solutionsA == 0 then return {};
   (conditions,flagsA,flagsB) := conds'A'B; 
   SchA := apply(#conditions, i->(conditions#i,flagsA#i));
   SchB := apply(#conditions, i->(conditions#i,flagsB#i));
   -- August 20, 2013:
   -------------------
   -- commenting th checkIncidenceSolutions check as we discovered
   -- this is a test that is numerical unstable!
   assert all(solutionsA, s->checkIncidenceSolution(s,SchA));
   s := first solutionsA;
   n := numrows s;
   k := numcols s;
   x := symbol x;
   R := FFF[x_(1,1)..x_(k,n-k)];
   MX := sub(random(FFF^n,FFF^n),R)*(transpose genericMatrix(R,k,n-k)||id_(FFF^k)); -- random chart on G(k,n)
   solutionsB := changeFlags(MX,solutionsA/(s->solutionToChart(s,MX)),conds'A'B);
   -- the following clean is a hack, instead, we need to do a newton step check
   -- when we all changeFlags as there is a numerical check in there... 
   -- the following is a hack
   ret := apply(solutionsB, s->clean(ERROR'TOLERANCE^2,sub(MX, matrix{s})));
   assert all(ret, s->checkIncidenceSolution(s,SchB));
   ret
   )

---------------------------------
-- DOCUMENT THIS FUNCTION!!!
--
-- This function is doing a parameter homotopy
-- change one column at a time to move solutions
-- w.r.t. flags A to solutions w.r.t. flags B
----------------------------------
changeFlags(Matrix, List, Sequence) := (MX, solutionsA, conds'A'B)->( -- solutionsA is a list of lists (of values for the parameters)
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
	       Polys := flatten entries squareUpPolynomials(m, makePolynomials(sub(MX,R2),conditions,flagsHomot));
	       A0 := map(RMx,R2,prepend(0_RMx,gens RMx));
	       A1 := map(RMx,R2,prepend(1_RMx, gens RMx));
	       solutionsT:=track(Polys/A0, Polys/A1, solutionsS, gamma=>exp(2*pi*ii*random RR));
      	       solutionsS = solutionsT/coordinates;
	       ));
       );
   -- August 20,2013:
   ---------------------------
   -- Newton refinement here!!
   --   SquareSyst := flatten entries squareUpPolynomials(nVars, makePolynomials(MX, apply(#conditionss,i->(conditions#i,FlagsB#i))));
   --   print("Newton refinement");
   --   print(refine(SquareSyst, solutionsS, Software=>M2, Iterations=>1));
   solutionsS
   )


TEST ///
---------
-- Test the function changeFlags that
-- moves solutions wrt flags A
-- to solutions wrt flags B
--
-- This function doesn't really test if they are actual solutions
-----------------
-- If you want to run this example:
-- 1.- you need to uncomment it AFTER loading NumericalSchubertCalculus package
-- 2.- you need to load the NumercialAlgebraicGeometry package
-- 3.- you need to manually load the function makePolynomials
-- 4.- you need to manually load the function changeFlags
-- 5.- you need to manually load the function changeflagsLinear
--
-- You could avoid all this maybe if you can run this example at the end of
-- the code, where the other examples are.
-----------------
--
debug needsPackage "NumericalSchubertCalculus"
needsPackage "NumericalAlgebraicGeometry"
Rng = FFF[x_{1,1}, x_{1,2}];
MX = matrix{{x_{1,1}, x_{1,2}}, {1,0}, {0,1}, {0,0}};
conds = {{1},{1}};
Flags1 = {random(FFF^4,FFF^4), random(FFF^4,FFF^4)};
sols = solveSystem (makePolynomials(MX, apply(#conds,i->(conds#i,Flags1#i))))_*
Flags2 = {id_(FFF^4)_{1,3,0,2}, rsort id_(FFF^4)} --we should get (0,0) as solution
solsT = changeFlags(MX, sols/coordinates, (conds, Flags1, Flags2))
assert(clean_0.0001 matrix solsT == 0) -- check that the solutions are actually (0,0)
/// --end of TEST


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
--    {A,T1,T2} - list of three matrices
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
    {sub(A, X), sub(T1, X), sub(T2, X)}    
    )


--------------------------
-- toRawSolutions
--
-- Need to document this function
-------------------------
toRawSolutions = method()
toRawSolutions(Matrix,Matrix) := (coordX,X) -> (
     a := flatten entries coordX;
     b := flatten entries X;
     delete(null, apply(#a, i->if a#i == 1 or a#i == 0 then null else b#i))
     )

------------------
-- normalizeColumn
------------------
--  this function divide a specific column of a matrix
--  by a specific value
------------------
-- Input:
--     X'' - the matrix to be normalized
--     r   - the row of the elt that becomes 1
--     j - the column to be normalized
-----------------
normalizeColumn = method(TypicalValue => Matrix)
normalizeColumn(Matrix,ZZ,ZZ) := (X,r,j) -> (  
     k := numgens source X;
     if j=!=null then(
	  X = X_{0..j-1} | (1/X_(r,j))*X_{j}  | X_{j+1..k-1}; 
	  --X''_(r,j) =-1/(1+X_(r,j)); -- error in Ravi's notes: should be -X_(r+1,j)/(1+X_(r,j))
	  --X''_(r+1,j) = 1; -- this is correct, but is also already taken care of 
	  );
     matrix X
     )

-----------------
-- redCheckersColumnReduce
-----------------
-- This function reduce specific column
-- using elementary column operations
-----------------
redCheckersColumnReduce = method(TypicalValue => Matrix)
redCheckersColumnReduce(Matrix, MutableHashTable) := (X'', father) -> (
     k := numgens source X'';
     n := numgens target X'';
     red := delete(NC,last father.Board);
     redSorted := sort red;
     r := father.CriticalRow;
     j := position(redSorted, i-> i==r+1);
     if j=!=null then(
     	  X''  = mutableMatrix X'';
	  crit'col := position(red, i->i==r+1);
     	  for jj from j+1 to k-1 do 
	  -- reduce the columns for red checkers that have higher number and "see" the red checker in the row r+1
     	  if position(red, i->red#jj == i) > crit'col then ( 
	       c := X''_(r+1,jj)/X''_(r+1,j); 
	       scan(n, i->X''_(i,jj) = X''_(i,jj) - c*X''_(i,j))
	       )
	  );
     matrix X''
     )

redCheckersColumnReduceSwap = method(TypicalValue => Matrix)
redCheckersColumnReduceSwap(Matrix, MutableHashTable) := (X'', father) -> (
     k := numgens source X'';
     n := numgens target X'';
     red := delete(NC,last father.Board);
     redSorted := sort red;
     r := father.CriticalRow;
     j := position(redSorted, i-> i>=r+1);
     rowj := redSorted#j; -- the row of the lower swapped red checker (in the father)
     if j=!=null then(
     	  X''  = mutableMatrix X'';
	  for jj from j+1 to k-1 do(
	       -- reduce the columns for red checkers that have higher number and "see" the red checker in the row r+1
	       scan(n, i-> (
	       		 X''_(i,jj) = X''_(i,jj) - X''_(rowj,jj)*X''_(i,j);
	       		 ));
	       )
     	  );
     matrix X''
     )

redCheckersColumnReduce2 = method(TypicalValue => Matrix)
redCheckersColumnReduce2(Matrix, MutableHashTable) := (X'', father) -> (
     k := numgens source X'';
     n := numgens target X'';
     X''  = mutableMatrix X'';
     red := delete(NC,last father.Board);
     redSorted := sort red; -- numbers of the rows where red checkers are
     apply(#redSorted, r->( -- column r is to be reduced 
--	 -- find the redcheckers bellow that can see the current redChecker
--	 witnessReds:=select(drop(red,r), i->i>red#r);
--	 j:={};
--         scan(witnessReds, i-> j=append(j,position(redSorted, l-> l==i)));
     	 col'of'r'on'board := position(last father.Board, i->i==redSorted#r); 
	 reducers := select(0..r-1, 
	      j->position(last father.Board, i->i==redSorted#j)<col'of'r'on'board
	      );  
	 scan(reducers, j->(
	      		-- reduce the columns for red checkers that have higher number and "see" the red checker in the row r+1
			scan(n, i-> (
			     	  X''_(i,r) = X''_(i,r) - X''_(redSorted#j,r)*X''_(i,j);
			     	  ));
		   	));
	 ));
     matrix X''
     )
 
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
	       ------------------
	       -- March 23:
	       --
	       -- we need to make sure that F is
	       -- a flag in the working field FFF
	       --F = promote(F,FFF);
	       ----------------
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

-- Document the Following function
---------------------------------
-- squareUpPolynomials
---------------------------------
-- m random linear combinations of generators of the ideal
squareUpPolynomials = method()
squareUpPolynomials(ZZ,Ideal) := (m,eqs) ->  gens eqs * random(FFF^(numgens eqs), FFF^m)  
--squareUpPolynomials(ZZ,Ideal,Ideal) := (m,eqs1,eqs2) ->  (
--    G := random(FFF^(numgens eqs1), FFF^m);
--    (gens eqs1 * G, gens eqs2 * G)
--    )

-----------------------------
-- Tracks a homotopy
-----------------------------
-- Input: 
--    H -- a list of polynomials in k[xx,t];
--    S = {{...},...,{...}} -- a list of solutions to H at t=0
-- Output: 
--    T - a list of Points that are solutions to H at t=1
trackHomotopy = method(TypicalValue=>List)
trackHomotopy (Matrix,List) := (H,S) -> (
     Rt := ring H;
     R := (coefficientRing Rt)[drop(gens Rt,1)];
     map't'0 := map(R, Rt, matrix{{0_FFF}}|vars R);
     map't'1 := map(R, Rt, matrix{{1_FFF}}|vars R);
     track(first entries map't'0 H, first entries map't'1 H, S)
     )

------------------------
-- isRedCheckerInRegionE
------------------------
-- NEEDS to be documentted!
----------------------------
-- Input: 
--     j = number of a red checker
--     r = critical row
--     black = black checkers on the board
isRedCheckerInRegionE = method()
isRedCheckerInRegionE(ZZ,MutableHashTable) := (i,node) -> (
     r := node.CriticalRow;
     black := first node.Board;
     e0 := position(black, b->b==r+1);
     e1 := position(black, b->b==r);
     i < e1 and i >= e0
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


----------------------
-- checkNewtonIteration
----------------------
-- Function that given a proposed
-- solution to a Schubert Problem 
-- it creates a Newton step to compare
-- the convergence of the approximated solution
----------------------
-- Input:
--    M -- n by k matrix (representing an element of G(k,n))
--    MX -- local coordinates for the solution
-- Output:
--    NewtonStep
-----------------------
-- This function NEEDS to be created!!

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


-----------------------------
-- end Numerical LR-Homotopies
-----------------------------

-------------------------------
---  findGaloisElement
-------------------------------
--- A function that computes
--- another instances of a
--- given Schubert problem to
--- create a short loop
--- and track the solutions
--- using homotopy continuation,
--- then extracts the created
--- permutation of the solutions

findGaloisElement = method(TypicalValue => List)
findGaloisElement(Sequence, List, List) :=(prblm, flgs, solns) ->(
    ---------------------------------
     -- prblm 
     -- is a List that contains partitions 
     -- l and m, and integers k,n 
     -- that define the simple 
     -- Schubert problem in Gr(k,n)
     (l,m,k,n):=prblm;
     l = verifyLength(l, k);
     m = verifyLength(m, k);
     d := k*(n-k)-sum(l)-sum(m);
     -- create a random flag to start a loop
     -- We will work only from a short loop
     -- so we need only the first two rows
     -- of a random flag
     F := matrix apply(2, i->apply(n,j->random FFF));
     swaps := {0,1,0,1};
     tmpMtrx := mutableMatrix(flgs#(d-1) || F);
     tempSlns := solns;
     apply(swaps, j->(
	       M1 := submatrix'(matrix tmpMtrx, {n-k, n-k+1},);
	       rowSwap(tmpMtrx, j, n-k+j);
	       M2 := submatrix'(matrix tmpMtrx, {n-k,n-k+1},);
	       tempSlns = trackSimpleSchubert((k,n), (l,m), drop(flgs, -1) | {M1}, drop(flgs, -1) | {M2}, StartSolutions=>tempSlns);
	       ));
     apply(solns, s->positions(tempSlns, j->areEqual(j,s))) / first
)

------------------
-- isFullSymmetric
-- is a function that
-- takes a list of permutations
-- creates a file and run
-- GAP to test if the list
-- generates the full symmetric group
--
-- CAVIAT: it assumes that GAP runs
-- 	   when you type "gap" in a 
--	   terminal
------------------

getFileName = () -> (
	filename := temporaryFileName();
	while fileExists(filename) or fileExists(filename|".mat") or fileExists(filename|".lat") do filename = temporaryFileName();
	filename
	)

--GAPexe ="/Applications/gap4r4/bin/./gap.sh";	
--GAPexe := "gap";
-- With GAP's workspace (ultrafast)
GAPexe := "gap -L /Applications/gap4r4/bin/wsgap4";

isFullSymmetric = method(TypicalValue => Boolean)
isFullSymmetric(List) := (perms)->(
	--
	-- perms is a list of permutations
	-- of [n] = {1,2,...,n}
	--
	F := getFileName();
	file := openOut(F|".gapjob");
	file << "u := Group(" << endl;
	scan(#perms, i->
		(
			p := perms#i;
			file << "PermList([" ;
			scan(p, j->( 
				file << j+1; 
				if j=!= last p then file << ", " ;
			));
			file <<"])";
			if i=!=#perms-1 then file << ", "<<endl; 
		)
	);
	file <<endl << ");"<<endl;

	n := max perms#0;
	file <<"if NrMovedPoints(u)="<< n+1 << " and IsNaturalSymmetricGroup(u) then RemoveFile(\""<< toString(file) <<"\"); fi;\n";
	file << "QUIT;\n";
	close file;
  	--------------
	--
	-- Running GAP
	--
	--------------
	run(GAPexe|" -q "|toString(file));
	if fileExists toString(file) then (
		removeFile toString(file); 
		return false;
	)else(
		return true;
	)
)

-------------------
--
--	isGaloisFullSymmetric
--
-------------------
-- function that find Galois
-- elements of a Schubert Problem
-- until it gets the full symmetric
-- group
--
-- CAVIAT: this assumes that we
-- know that Gal(Prblm) = symm_n
--
-------------------
isGaloisFullSymmetric = method(TypicalValue => Boolean)
isGaloisFullSymmetric(Sequence, List, List, ZZ) := (prblm, flgs, solns, mx) ->(
	-- mx is the maximal number of loops we want to run
 	(l,m,k,n) := prblm;
	permuts := new List;
	cntr:= 0;
	tempOut := false;
	for i from 1 to mx when (tempOut===false) do (
	     permuts = permuts | {findGaloisElement(prblm,flgs,solns)};
	     cntr = i;
	     tempOut = isFullSymmetric(permuts);
	);
	if tempOut then (
		(tempOut, cntr) 
	)else (
		(tempOut, permuts)
	)
)

-------------------
-- Documentation --
-------------------

beginDocumentation()

doc ///
   Key
      skewSchubertVariety
   Headline
      skew Schubert variety (or Richardson variety) from partitions $l$ and $m$
   Usage
      skewSchubertVariety(kn,l,m)
   Inputs
      kn:Sequence
         two integers denoting the Grassmannian Gr(k,n)
      l:List
      m:List
         partitions of n
   Outputs
      :Matrix
   Description
      Text
         Creates the matrix $E_{l,m}$ that parametrizes the skew Schubert variety $Y_{l,m} = Y_l \cap Y_m$.
      Example
         -- for l = 2,1 and m = 1,1
       	 -- in Gr(3,7)
      	 skewSchubertVariety( (3,7),{2,1},{1,1} )
   SeeAlso
         solveSimpleSchubert
///;

doc ///
   Key
      createRandomFlagsForSimpleSchubert
   Headline
      Create a list of flags with random numbers to solve a simple Schubert problem
   Usage
      createRandomFlagsForSimpleSchubert(kn,l,m)
   Inputs
      kn:Sequence
         two integers denoting the Grassmannian Gr(k,n)
      l:List
      m:List
         partitions of n
   Outputs
      :List
         random fixed flags
   Description
      Text
         Creates a list of d matrices with random numbers, where $d = k*(n-k)-|m|-|l|$.
      Example
         -- for l = 2,1 and m = 1,1
      	 -- in Gr(3,7)
      	 createRandomFlagsForSimpleSchubert((3,7),{2,1,0},{1,1,0})
   SeeAlso
         solveSimpleSchubert
///;

doc ///
   Key
      solveSimpleSchubert
   Headline
      Uses Pieri Homotopy continuation to solve simple Schubert problems
   Usage
      solveSimpleSchubert(kn,l,m,G)
   Inputs
      kn:Sequence
         two integers denoting the Grassmannian Gr(k,n)
      l:List
      m:List
         partitions of n
      G:List
         of fixed Flags G_1,...,G_d
   Outputs
      :List
         solutions of the simple Schubert Problem defined by l and m with respect to the flags G_1,...,G_d
   Description
      Text
         Given partitions $l$ and $m$ in the Grassmannian $Gr(k,n)$, and a set of fixed flags $G_1,...,G_d$, where $d=k*(k-n) - |l| - |m|$. The function solves the system taking the first $d-1$ flags, and replacing the last one for a simpler one $G_m$. Then it uses homotopy continuation to track the solutions of this simpler system to solutions of the original system.         
    	 This function is used to solve Simple Schubert Problems, as described in the paper:          
    	 Leykin and Sottile, "Galois groups of Schubert problems via homotopy continuation", Mathematics of Computation, 78 (2009) 1749--1765.
      Example
         ---- Simple Schubert Problem
       	 k = 3
	 n = 7
       	 l = {2,1,0}
       	 m = {1,1,0}
       	 ----  Generate random flags G----
       	 d = k*(n-k)-sum(l)-sum(m);
       	 G = apply(d, i->matrix apply(n-k,i->apply(n,j->random FFF)));
       	 ---------------------------------
       	 solveSimpleSchubert((k,n),l,m,G)
   SeeAlso
         createRandomFlagsForSimpleSchubert 
         skewSchubertVariety
///;

doc ///
    Key
       trackSimpleSchubert
    Headline
       Uses Homotopy continuation to solve a Schubert problem
    Usage
       trackSimpleSchubert(kn,cond, G, F)
    Inputs
       kn:Sequence
          two integers (k,n) denoting the Grassmannian Gr(k,n)
       cond:Sequence
          of two partitions of n
       G:List
          of starting Flags G_1,..., G_d
       F:List
          of target Flags F_1,...,F_d
    Outputs
       :List
          solutions of the Schubert problem defined by l and m with respect to the flags F_1,...,F_d
    Description
       Text
          Given partitions $l$ and $m$ in the Grassmannian $Gr(k,n)$, and two sets of fixed flags $G_1,...,G_d$, and $F_1,...,F_d$; where $d=k*(k-n) - |l| - |m|$. The function tracks the solutions of the system defined by $G_1,...,G_d$ (if the solutions are not given, it computes them using {\tt solveSimpleSchubert}) to find solutions for the system defined by $F_1,...,F_d$. 
       Example
          ---- Simple Schubert Problem
   	  (k,n) = (3,7)
   	  l = {2,1,0}
   	  m = {1,1,0}
   	  ----  Generate random flags G and F----
   	  d = k*(n-k)-sum(l)-sum(m);
   	  G = apply(d, i->matrix apply(n-k,i->apply(n,j->random FFF)));
   	  F = apply(d, i->matrix apply(n-k,i->apply(n,j->random FFF)));
   	  ---------------------------------
   	  trackSimpleSchubert((k,n),(l,m),G,F)
       Text
          If the solutions of the system defined by $G_1,...,G_d$ are given, they can be given in the function to avoid unnecessary computations
       Example
          ---- Simple Schubert Problem
   	  (k,n) = (3,7)
   	  l = {2,1,0}
   	  m = {1,1,0}
   	  ----  Generate random flags G and F----
   	  d = k*(n-k)-sum(l)-sum(m);
   	  G = apply(d, i->matrix apply(n-k,i->apply(n,j->random FFF)));
   	  F = apply(d, i->matrix apply(n-k,i->apply(n,j->random FFF)));
   	  ---------------------------------
   	  Solns = solveSimpleSchubert((k,n),l,m,G);
          trackSimpleSchubert((k,n),(l,m),G,F, StartSolutions=>Solns)
   SeeAlso
      solveSimpleSchubert
      createRandomFlagsForSimpleSchubert
///;

doc ///
   Key
      findGaloisElement
   Headline
      computes a permutation from a loop of an instance of a simple Schubert problem.
   Usage
      findGaloisElement(pblm, flag, solns)
   Inputs
      pblm:Sequence
         a sequence (l,m,k,n) that contains two partitions l,m and two integers k,n that define the simple Schubert problem l,m in the Grassmannian Gr(k,n)
      flag:List
         a list of numerical matrices that define an instance of the simple Schubert Problem
      solns:List
         solutions of the specific instance
   Outputs
      :List
         a permutation that lie in the Galois group
   Description
      Text
         Given a simple Schubert problem $(l,m)$ in $Gr(k,n)$. Fix a 
	 set of flags $F_1,...,F_d$ and let $S$ be the set of solutions of
	 the intance of the Schubert problem given by the flags $\{F_i\}$.
	 We compute a loop in the problem space based on the solution $S$
	 by deforming one of the flags $F_i$ using Homotopy continuation. 
	 This  generates a loop in the problem space, which corresponds to 
	 a permutation in the Galois group.
      Example
         l={1,1}
	 m={2,1}
	 (k,n) = (3,7)
      Text
	 Generate a random set of flags to compute an instance of the problem	 
      Example
	 G = createRandomFlagsForSimpleSchubert((k,n),l,m)	 
      Text
         Solve the problem
      Example
	 S = solveSimpleSchubert((k,n),l,m,G);
      Text
         This is a problem with 77 solutions
      Example
	 #S
      Text
	 an element of the Galois group is:
      Example
	 findGaloisElement((l,m,k,n), G, S)
   SeeAlso
      isFullSymmetric
      isGaloisFullSymmetric 
      solveSimpleSchubert
      createRandomFlagsForSimpleSchubert
///;

doc ///
   Key
      isFullSymmetric
   Headline
      Check if a list of permutations generate the full symmetric group.
   Usage
      isFullSymmetric(P)
   Inputs
      P:List
         of Lists of permutations
   Outputs
      :Boolean
   Description
      Text
         Takes a list of permutations of {1,...,n} and uses GAP to check if those generate the symmetric group $S_n$.
   Caveat
      It assumes that GAP runs when you type {\tt gap} in a terminal      
///;

doc ///
   Key
      isGaloisFullSymmetric
   Headline
      find Galois elements of a simple Schubert Problem until they generate the full symmetric group
   Usage
      isGaloisFullSymmetric(pblm, flag, solns, mx)
   Inputs
      pblm:Sequence
         a sequence (l,m,k,n) that contains two partitions l,m and two integers k,n that define the simple Schubert problem l,m in the Grassmannian Gr(k,n)
      flag:List
         a list of numerical matrices that define an instance of the simple Schubert Problem
      solns:List
         solutions of the specific instance
      mx:ZZ
         the maximum number of loops you want to run
   Outputs
      :Boolean
			   If the answer is true, it will output the number of loops it needed; if false it will output the permutations it tried
   Description
      Text
         It runs a loop to find elements of the Galois group until it find a generating set or die after {\tt mx} tries.
      Example
         l={1,1}
         m={2,1}
         (k,n) = (3,7)
      Text
         Generate a random set of flags to compute an instance of the problem	 
      Example
         G = createRandomFlagsForSimpleSchubert((k,n),l,m)	 
      Text
         Solve the Schubert problem
      Example
         S = solveSimpleSchubert((k,n),l,m,G);
      Text
         Check if the Galois group is the symmetric group
      Example
         isGaloisFullSymmetric((l,m,k,n), G, S, 5)
      Text 
          one permutation is not enough
      Example
          isGaloisFullSymmetric((l,m,k,n), G, S, 1)
   SeeAlso
      isFullSymmetric
      findGaloisElement
   Caveat
      This assumes that GAP runs when you type in the terminal {\tt gap} and that we already know that the Galois group is the full symmetric group, otherwise it will output {\tt false} after {\tt mx} repetitions.
///;

TEST ///
restart 
debug needsPackage "NumericalSchubertCalculus"
setRandomSeed 0
-----------------------
-- 4 lines in P^3
SchPblm = {({1},id_(FFF^4)), 
    ({1},random(FFF^4,FFF^4)),
    ({1},random(FFF^4,FFF^4)), 
    ({1},random(FFF^4,FFF^4))};

solveSchubertProblem(SchPblm,2,4)

restart 
debug needsPackage "NumericalSchubertCalculus"
-- setRandomSeed 2

Pblm = {({1},id_(FFF^4)),
    ({1},rsort id_(FFF^4)), 
    ({1},transpose matrix {{1,1,1,1},{0,1,2,3},{0,0,2,6},{0,0,0,1}}),
    ({1},transpose matrix {{1,2,4,8}, {0,1,4,12}, {0,0,1,6}, {0,0,0,1}})}

solveSchubertProblem(Pblm, 2,4)
-- we need to make a column reduction
-- to see if these solutions are real
S = oo;
Sreduced = apply(S, s->(
	M1:= matrix{
	    {s_(0,0)^(-1), -s_(0,1)*s_(0,0)^-1},
	    {0, 1}};
	s1 := clean_0.001 s*M1;
	M2 := matrix{
	    {1,0},
	    {-s1_(3,0)*s1_(3,1)^-1 ,s1_(3,1)^-1 }
	    };
	s2 := clean(0.001, s1*M2);
	s2
	));
Sreduced


-- Problem (2,1)^3=2 in G(3,6)
SchPblm = {({2,1},random(FFF^6,FFF^6)), ({2,1},random(FFF^6,FFF^6)),({2,1},random(FFF^6,FFF^6))}
solveSchubertProblem(SchPblm,3,6)
-- not a simple tree

-- Problem (2,1)*(2)^3 = 2 in G(3,6) 
-- This problem has a non-trivial tree (not like the problem of 4 lines)
Pblm={({2},random(FFF^6,FFF^6)),
    ({2}, random(FFF^6,FFF^6)),
    ({2},random(FFF^6,FFF^6)), 
    ({2,1},random(FFF^6,FFF^6))}
solveSchubertProblem(Pblm,3,6)

-- Problem (1)*(2)*(2,1)^2 = 3 in G(3,6)
Pblm={({1},random(FFF^6,FFF^6)),
    ({2}, random(FFF^6,FFF^6)),
    ({2,1},random(FFF^6,FFF^6))}
solveSchubertProblem(Pblm,3,6)
-- code breaks here
----------------------


root = playCheckers({1},{1},2,4)
resolveNode(root, {({1},random(FFF^4,FFF^4)), ({1},random(FFF^4,FFF^4))})
assert(#root.Solutions==2)

root = playCheckers({2,1,0},{2,1,0},3,6)
time resolveNode(root, {({2,1,0},random(FFF^6,FFF^6))})
assert(#root.Solutions==2)
peek root
-- test code and assertions here
-- may have as many TEST sections as needed

-- Problem (2,1)^2*(1)*(2) = 3 in G(3,6)
root = playCheckers({2,1},{2,1},3,6)
time resolveNode(root, {({2},random(FFF^6,FFF^6)), ({1},random(FFF^6,FFF^6))})
assert(#root.Solutions==3)
peek root
printTree root
-- Problem (2,1)*(2)^3 = 2 in G(3,6) 
-- This problem has a non-trivial tree (not like the problem of 4 lines)
root = playCheckers({2},{2},3,6)
time resolveNode(root, {({2},random(FFF^6,FFF^6)), ({2,1},random(FFF^6,FFF^6))})
assert(#root.Solutions == 2)
peek root
printTree root
///

end
check "NumericalSchubertCalculus"

-- EXAMPLES (see TEST section for more simple examples)
restart
setRandomSeed 0
--debug 
needsPackage "NumericalSchubertCalculus";

root = playCheckers({2,1},{2,1},3,6)
time resolveNode(root, {({2},random(FFF^6,FFF^6)), ({1},random(FFF^6,FFF^6))})
peek root

DEBUG'LEVEL = 1 --check that DebugLevel = 1 should do the black box solving only at the leaves
n=7; K'n=FFF^n; -- takes about 10 minutes!
root = playCheckers({2,1,0},{2,1,0},3,n)
time resolveNode(root, {({2,1,0},random(K'n,K'n)),({2,1,0},random(K'n,K'n))})
peek root
printTree root

root = playCheckers({2,1}, {2}, 3,6)
time resolveNode(root, {({2},random(FFF^6,FFF^6)), ({2},random(FFF^6,FFF^6))})
peek root
printTree root

---- there is something wrong, this problem gives an error
root = playCheckers({3,2,2},{2}, 3,6)
peek root
resolveNode(root, {})


restart
setRandomSeed 0
debug needsPackage "LRcheckergame";

-- we test if the resolveNode function
-- can just solve the problem when 
-- the Schubert problem consist of two
-- complementary partitions only
root = playCheckers({3,3,1},{2},3,6)
resolveNode(root,{})
peek root

-- this problem should give empty solutions as
-- the two partition are not complementary
root = playCheckers({3,3,1}, {1,1}, 3,6)
resolveNode(root, {({2,1,0},random(FFF^6,FFF^6))})


------------------- WHAT IS THIS STUFF? CAN IT BE DELETED??? ---------------------------
