phcPresent := run ("type phc >/dev/null 2>&1") === 0
phcVersion := if phcPresent then replace("PHCv([0-9.]+) .*\n","\\1",get "! phc --version")
phcVersionNeeded := "2.3.80"
phcPresentAndModern := phcPresent and match("^[0-9.]+$",phcVersion) and phcVersion >= phcVersionNeeded

newPackage(
    "NumericalSchubertCalculus",
    Version => "1.17", 
    Date => "Sep 2020",
    Authors => {
	{Name => "Anton Leykin", 
	    Email => "leykin@math.gatech.edu", 
	    HomePage => "http://people.math.gatech.edu/~aleykin3"},
	{Name => "Abraham Martin del Campo", 
	    Email => "abraham.mc@cimat.mx", 
	    HomePage => "http://www.cimat.mx/~abraham.mc"},
	{Name => "Frank Sottile", 
	    Email => "sottile@math.tamu.edu", 
	    HomePage => "http://www.math.tamu.edu/~sottile"},
	{Name => "Ravi Vakil",
		Email => "vakil@math.stanford.edu",
		HomePage => "http://math.stanford.edu/~vakil"},
	{Name => "Jan Verschelde",
		Email => "jan@math.uic.edu",
		HomePage => "http://www.math.uic.edu/~jan/"}
	},
    Headline => "numerical methods in Schubert Calculus",
    PackageImports => {
	"PHCpack",
	"NumericalAlgebraicGeometry",
	"MonodromySolver",
        "Schubert2"
	},
    AuxiliaryFiles => true,
    CacheExampleOutput => true,
    OptionalComponentsPresent => phcPresentAndModern,
    DebuggingMode => false
    )
debug NumericalAlgebraicGeometry
export { 
   "changeFlags",
   "resetStatistics",
   "printStatistics",
   "setVerboseLevel", 
   "solveSchubertProblem",
   "OneHomotopy"
   }
protect Board
protect IsResolved
protect Fathers
protect Children
protect FlagM
protect CriticalRow
protect Solutions
protect SolutionsSuperset -- temporary

-- NC means no checker in that column
--  16.09.29:  Frank thinks we should choose one or the other, but not both
NC = infinity

-- OUR FIELD
FFF = QQ
FFF = RR
FFF = CC
--FFF = CC_53
ERROR'TOLERANCE = 0.001
NEWTON'TOLERANCE = 10^-10
------------------
-- Debug Level --
------------------
-- 0 = no debug mode (default)
-- 1 = print progress info and time main processes
-- 2 = ... + checkerboard steps info
-- >2 = new experimental stuff kicks in
DBG = 0

---------------------
-- setVerboseLevel --
---------------------
--
-- Function to change different levels of 
-- information printed while running
---------------------
-- input: integer number between 0,1,2, or greater
--
setVerboseLevel = method()
setVerboseLevel ZZ := i->DBG=i
--
VERIFY'SOLUTIONS = true
BLACKBOX = false

--INITIALIZING THE KEYS OF NODE
--Board= symbol Board


setDebugOptions = method(Options=>{"debug"=>null,"verify solutions"=>null,"blackbox"=>null})
installMethod(setDebugOptions, o -> () -> scan(keys o, k->if o#k=!=null then
	if k == "debug" then DBG = o#k
	else if k === "verify solutions" then VERIFY'SOLUTIONS = o#k
	else if k === "blackbox" then BLACKBOX = o#k
	))
 
load "NumericalSchubertCalculus/PHCpack-LRhomotopies.m2"
load "NumericalSchubertCalculus/pieri.m2"
load "NumericalSchubertCalculus/service-functions.m2"
--load "NumericalSchubertCalculus/UnderDevelopment/galois.m2"

--------------------------------------
-- produces a matrix that parametrizes
-- the "big cell" of Gr(k,n)
------------------------------
bigCellLocalCoordinates = method()
bigCellLocalCoordinates(ZZ,ZZ) := (k,n) -> (
    x := symbol x;
    R := FFF(monoid[x_(1,1)..x_(n-k,k)]);
    transpose genericMatrix(R,k,n-k) || map(R^k)
    )

-----------------------------
-- Numerical LR-Homotopies
-----------------------------

---------------------
-- redChkrPos 
--
-- given two brackets, computes the positions of the 
-- red checkers at the start of a checkerboard game
---------------------
-- input: two Schubert conditions l and m written 
--	   as brackets the Grassmannian G(k,n)
--
-- Output: checkboard coordinates for the 
--         red checkers
---------------------
-- example: for {2,1}*{2} in G(3,6)
--
--partition2bracket({2,1},3,6)
--     o = {2, 4, 6}
--partition2bracket({2},3,6)
--     o = {2, 5, 6}
--redChkrPos({2,4,6},{2,5,6},3,6)  
--     o = {infinity, 5, infinity, 4, infinity, 1}
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

----------------------------------------------------
-- moveRed
--
-- makes the move of the red checkers during a game
---------------------------------------------------
-- input: {(blackup, blackdown, redposition)}
--       blackup - Coordinates of the ascending black checker
--       blackdown - Coordinates of the descending black checker
--       redpos - List of red checker positions
--
-- output: {(repos,typeofmove,critrow)} or {(repos1,typeofmove1,critrow),(repos2,typeofmove2,critrow)}
--       redpos - Updated list (of lists) of red checker positions
--       typeofmove - {row,column,split}
--                    a triple which tells the type of the move we had to perform from
--                    the 3x3 table of moves (critical row/diagonal). This is given as a 
--                    tuple {row,column,split} where split says
--                    if you moved or not the red checkers
--                    (by 0 and 1 respectively) when there was a split
--       critrow - the critical row
moveRed = method(TypicalValue => List)
moveRed(List,List,List) := (blackup, blackdown, redposition) -> (
    ------------------------------------------------
    -- We need to check first if it is a valid configuration
    --
    -- (no longer need as it is checked before calling playCheckers) -- Abr. 15.0ct.15 
    ------------------------------------------------
    n := #redposition; -- n is the size of the checkboard
    split:=0;
    critrow := 0;
    critdiag := 0;
    g:=2; -- g answers where is the red checker in the critical row
    r:=2; -- r answers where is the red checker in the critical diagonal
    -- r,g is the coordinate of the moving situation in the 3x3 table of moves
    indx := new List;
    redpos := new MutableList from redposition;
    -- find the critical row, and how the red checkers sit with respect to it
    indx = for i to n-blackdown#0-1 list n-1-i;
    apply(indx, j -> (
	    if redpos#j === blackdown#1 then (
	       	critrow = j;
	       	if j == blackdown#0 then g=0 else g=1;
	  	) 	
     	    ));    
    -- find the critical diagonal, and how the red checkers sit with respect to it
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
		-- switch the rows of the red checkers in the critical diagonal and critical row
		-- then, move the left one over to the column of the ascending black checker
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

---------------------------------------------------------------------------------
-- moveCheckers
-----------------
-- makes the next move of black and 
-- red checkers during a game
-----------------
-- Input:
--    blackred -> array of black and redchecker positions [blackPositions, redPositions]
--
-- Output:
--         a Sequence containing:
--    board --> the new checkerboard [blackCheckers, redCheckers]
--    move --> the type of move that we realized: {i,j,splt} (3x3 table from Ravi's notes) 
--    critrow --> the critical row
-----------------------------------
-- Example:
--
-- blackCheckersPosition = {0,1,3,4,5,2};
-- redCheckersPosition = {0, NC, NC, 4, NC, NC};
--
-- moveCheckers [blackCheckersPosition, redCheckersPosition];
--    o =  ({[{0, 1, 2, 4, 5, 3}, {0, infinity, infinity, 4, infinity, infinity}, {1, 2, 0}]}, 2)
-------------------------------------
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
	 --  in blackdown1 
     blackdown1 := position(blackposition, x->x == n-1) + 1;
     if blackdown1 == n then return ({},"leaf");
     blackup1 := position(blackposition, x-> x == 1+blackposition#blackdown1);
     -- Determine the rows of the pair of black checkers that will be sorted.  They are row r and 
     --    r+1 in the paper with r the critical row of the falling checker.
     --  n-blackdown1 is one more than the number of checkers in the upper right corner 
     --     (region A in paper)
     --  blackup1 is the number of checkers above and to the left of rising checker (as we are 0-based)
     --  Their sum is one more than the number of checkers above the moving pair = row of rising checker
     blackup2 := n-blackdown1+blackup1;
     blackdown2 := blackup2-1; -- this is the critical row 
     -- Now we figure out how to move the red checkers
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

----------------------------------------------------
-- Statistics
---------------
-- this function displays some information about
-- the type of moves that are performed in each
-- checkerboard game. These are the redcheker moves
-- encoded in the 9x9 table in the paper, where we
-- denote them by a triplet {i,j,k} where i is the
-- row (0,1, or 2), j is the column (0,1,2) and
-- k is 0 or 1 depending if we have a swap or not
----------
-- NOTE: the move {} indicates to be in the top
--       of a dag, i.e. the beginning of a game
--
-- NOTE: the tracking time we report is the whole
--       time used when tracking 
--
-- CAVEAT: printStatistics will display the information
--        about moves performed every time you run
--        a checker board game. Thus, if you use the
--        function solveSchubertProblem twice, the function 
--        will report the information of both Tournaments,
--        to avoid that, you need to export the following:
--            resetStatistics()
---------------------------------
resetStatistics = () -> (
    stats =  new MutableHashTable from 
    flatten flatten (apply(3,i->apply(3,j->{i,j,0}=>0)) | {{1,1,1}=>0, {}=>0}) | 
    { "tracking time" => 0 };
    )  
resetStatistics()

statsIncrementMove = m -> stats#m = stats#m + 1;
statsIncrementTrackingTime = t -> stats#"tracking time" = stats#"tracking time" + t
printStatistics = () -> (
    scan(sort select(keys stats, k->class k === List), k-> 
    	<< "# moves of type " << k << " = " << stats#k << endl
	);
    scan(select(keys stats, k->class k =!= List), k-> 
	<< k << " = " << stats#k << endl
	)
    )

--------------------------------------------------------
-- playCheckers
-----------------
-- This function takes as input a specific node and plays
-- a checkerboard game between two varieties X1 and X2
--
-- It sets up the game, and then it uses
-- the combinatorial Littlewood Richardson rule to make deformations
-- between the Schubert variety X2 to X1
-- It stores all the information in a HashTable
-------------------
-- To compute X1\cap X2 \cap X3 \cap...\cap Xn
-- we first play the checkers with X1 and X2
--
-- input1:
--         partn1, partn2, two partitions (representing X1 and X2)
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
          redChkrPos(partition2bracket(partn1,k,n),partition2bracket(partn2,k,n),k,n);
     blackChkrs := reverse toList (0..(n-1)); --initial black positions
     if DBG>0 then print "-- playCheckers";
     if DBG>1 then print(partn1,partn2);
     if DBG>1 then print([blackChkrs, redChkrs]);
     if DBG>0 then cpu0 := cpuTime();
     -- we call playCheckers recursively
     root :=playCheckers ([blackChkrs, redChkrs], null, {}, all'nodes);  -- returns the root of the tree
     if DBG>0 then << "-- cpu time = " << cpuTime()-cpu0 << endl;
     if DBG>1 then print VerticalList keys all'nodes;
     root
)

------------------------
-- PlayCheckers will also play the next checkerboard game
-- in the Tournament.
-- Input: 
--       board 
--       father (the checkergame this game came from)
--       typeofmove - the type of move in the 3x3 table we perform to go to the father
--       all'nodes - the list of games played already
--
-- THIS IS THE RECURSIVE CALL OF PLAYCHECKERS
--
-- Output: 
--      Dag --> a HashTable with the following information:
--           	     Board
--           	     IsResolved
--           	     Fathers
--           	     Children (a HashTable if the condition is not 0-dimensional)
-- 
----------------------------
playCheckers (Array,Thing,List,MutableHashTable) := (board,father,typeofmove,all'nodes) ->(
    -- all'nodes is a HashTable whose keys are boards,
    -- and this is where we store all nodes that we have
    -- already visited.
    --
    --------------------------------------------
     node'exists := all'nodes#?board; -- check if we already played this game
     self := if node'exists  
     then all'nodes#board  -- if so, then glue solutions, otherwise, start a new hashtable
     else new MutableHashTable from {
	  Board => board, 
	  IsResolved => false,
	  Fathers => {}
	  };
     statsIncrementMove typeofmove; -- here we are collecting statistics for the number of times we see each type of move
     if father=!=null then self.Fathers = self.Fathers | {(father,typeofmove)}; -- add the new way to get to this node
     if not node'exists then (
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

-----------------------------------
-- Example:
--
-- We play the game {2,1} vs {1,1,1} in G36
-- Game = playCheckers({2,1},{1,1,1}, 3,6) -- plays the game {2,1} vs. {1,1,1} in G36
--    o =  MutableHashTable{Board => [{5, 4, 3, 2, 1, 0}, {infinity, infinity, 5, 3, 1, infinity}]}
--                      Children => {MutableHashTable{...5...}}
--                      CriticalRow => 4
--                      Fathers => {}
--                      IsResolved => false
--
-- G = first Game.Children -- this is a Hash table with the first node below the root from above
--              we can now test the recursive call of playCheckers 
--              NOTE: the key Fathers has value a list of sequences of fathers and movetypes 
--
-- These are the values in G
--    	  Board =  [{4, 5, 3, 2, 1, 0}, {infinity, infinity, 5, 3, 1, infinity}]
--    	  Children = {MutableHashTable{...5...}}
--    	  CriticalRow = 3
--        Fathers = {([{5, 4, 3, 2, 1, 0}, {infinity, infinity, 5, 3, 1, infinity}], {2, 2, 0})}
--    	  IsResolved = false
--
-- playCheckers(G.Board, first first G.Fathers, last first G.Fathers, first Q.Children)
--    o =  MutableHashTable{Board => [{4, 5, 3, 2, 1, 0}, {infinity, infinity, 5, 3, 1, infinity}]}
--                       Children => {MutableHashTable{...5...}}
--                       CriticalRow => 3
--                       Fathers => {(MutableHashTable{...5...}, {2, 2, 0})}
--                       IsResolved => false
-------------------------------------


-----------------
--- makeLocalCoordinates
--
-- Translates a checkerboard
-- configuration into a matrix with
-- 0's, 1's and variables
-----------------
-- input: an array of black and red checkers
--        in the form [ ListofPositionsBlack, ListofPositionsRed ]
-- output: a matrix with local coordinates
-----------------
-- example:
--
-- blackCheckersPosition = {0,1,3,4,5,2};
-- redCheckersPosition = {0, NC, NC, 4, NC, NC};
--
-- makeLocalCoordinates [blackCheckersPosition, redCheckersPosition]
--   o = | 1 0       |
--       | 0 x_(1,1) |
--       | 0 0       |
--       | 0 x_(3,1) |
--       | 0 1       |
--       | 0 0       |
-- 
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

load "NumericalSchubertCalculus/LR-resolveNode.m2"

---------------
-- solveSchubertProblem
---------------
-- Function that solves a Schubert problem
-- by first taking two of the conditions,
-- then create a tree/Dag (with nodes) by playing a 
-- checker game, then resolve the node numerically
-- using homotopies, and gluing the solutions to each
-- node
---------------
-- input:
--    SchPblm := list of Schubert conditions with general flags
--               {(partition_List, flag_Matrix),...}
--    k,n := the Grassmannian G(k,n)
--    (option) LinAlgebra [default = true] 
--             move to user flags via Linear Algebra (if false via homotopy continuation)
-- output:
--    list of solutions
---------------
solveSchubertProblem = method(Options=>{LinearAlgebra=>true})
solveSchubertProblem(List,ZZ,ZZ) := o -> (SchPblm,k,n) ->(
    -- SchPblm is a list of sequences with two entries  a condition and a flag
    -- Check that it does indeed form a Schubert problem, and convert the consitions to partitions (if they were brackes)
    SchPblm = ensurePartitions(SchPblm,k,n);

    twoconds := take(SchPblm,2);
    remaining'conditions'flags := drop(SchPblm,2);
    -- take the first two conditions
    l1:=verifyLength(first first twoconds,k);
    l2:=verifyLength(first last twoconds,k);
    F1:=promote(last first twoconds,FFF);
    F2:=promote(last last twoconds,FFF);
    resetGGstash(); -- resets GGstash in LR-makePolynomials.m2    
    Slns:={};
    checkPartitionsOverlap := (l1+reverse l2)/(i->n-k-i);
    if min(checkPartitionsOverlap) < 0 then
       Slns
    else(
	if DBG>1 then print "solveSchubertProblem: transforming flags to (M,Id,...)";
	-- resolveNode expects flags to be the following list:
	--  flagM, Id, F3'....  
	--
	-- we compute the linear transformations s.t.
	-- A*F1 = FlagM*T1
	-- A*F2 = ID * T2
	ID := id_(FFF^n);
	-- 
	-- There is a fundamental difference between the case
	-- with only two conditions and the one with 3 or more Schubert conditions
	--
	LocalFlags1 := {F1,F2};
	flgM := matrix;
	local LocalFlags2;
	if #remaining'conditions'flags == 0 then (
	    flgM = ID;
	    LocalFlags2 = {flgM, rsort ID};
	) else (
	    flgM = MovingFlag'at'Root n;
	    LocalFlags2 = {flgM, ID};
	    );
	At1t2 := moveFlags2Flags(LocalFlags1,LocalFlags2); --Gets the transformations A, T1, T2
	A := first At1t2;	    
	-- we update the given flags F3 ... Fm
	-- to F3' .. Fm' where Fi' = A*Fi
	new'remaining'conditions'flags := apply(
	    remaining'conditions'flags, CF->(
		(C,F):=CF;
		(C,A*F)
	    ));
	newDag := playCheckers(l1,l2,k,n);
	resolveNode(newDag, new'remaining'conditions'flags);
	conds := {l1,l2};
	-- resolveNode gives a solution in local coords
	-- of {l1}*{l2} w.r.t {FlagM, Id}
	-- we multiply the solution S by FlagM
	-- and we obtain a solution of the Sch. problem
	-- {l1,...,lm} with respect to
	-- {FlagM, Id, F3',...,Fm'}
	--
	--  we need to make a change of coordinates back to the user-defined flags
	-- that is, send (FlagM,Id)-->(F1,F2), which is done by A^(-1)	
	-------------------------------
	--############ Fork to decide if you want to do this
	-- change of flags via homotopy or via Linear Algebra
	-- ########################################
	if o.LinearAlgebra then(
	    Ainv := solve(A,ID);
	    Ainv*flgM*newDag.Solutions
	    )else(
	    -- #### NOW THIS IS BROKEN!! because is was based on wrong math
	    -- we need to use homotopy to transform the solutions to the
	    -- user defined flags.
	    --
	    -- A is the matrix above such that
	    --          A*F1 = FlagM*T1 (representing the same flag as F1)
	    --          A*F2 = ID*T2 (representing the same flag as F2)
	    --LocalFlags1 := {F1,F2}; 
	    --LocalFlags2:= {flgM,ID};
	    T1 := At1t2_1;    
	    T2 := At1t2_2;    
	    scan(remaining'conditions'flags, c-> (
		    conds = append(conds, first c);
		    LocalFlags2 = append(LocalFlags2, A*(last c));
		    LocalFlags1 = append(LocalFlags1, last c);
		    ));
    	    if DBG>1 then (
	    	print "solutions obtained at the root of a node";
	    	print newDag.Solutions;    	    	
	    	print "this are the transformations that we apply";
		print "before calling changeFlags:";
		print(flgM);
	    	print(flgM*newDag.Solutions);
	    	);
	    changeFlags(flgM*newDag.Solutions, -- these are matrices in absolute coordinates
	    	(conds, LocalFlags2, LocalFlags1), OneHomotopy=>false
		)
	    ) --
	)
    )-- end of solveSchubertProblem

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
--- solutionToChart
---------------------------------
-- takes a solution matrix in global coordinates
-- and converts it into local coordinates to know
-- what the values of the variables of the local 
-- coordinates are, i.e., 
-- writes a solution Matrix in terms 
-- of the chart MX  (as a list of values 
-- of the parameters)
---
-- Input:
--    s -> a nxk matrix representing the 
--    	   solutions of the problem (in global coordinates ?)
--    MX -> the local coordinates of the checkerboard variety
--
-- Output: List of values for the variables in MX
--
---------------------------------
-- Example:
--
-- R = FFF[x_(1,1),x_(3,1)]
-- MX = matrix {{1,    0}, 
--              {0, x_(1,1)}, 
--              {0,    0}, 
--              {0, x_(3,1)}, 
--              {0,    1}, 
--              {0,    0}};
-- s =  promote(transpose matrix{
--                        {1,0,0,0,0,0},
--                        {1,3,5,7,1,0}},FFF);
--
-- solutionToChart(s,MX)
--         o = {.115385, .269231}
------
-- s2 = promote(transpose matrix{
--     	       	       	   {2,3,5,7,11,13},
--     	       	       	   {1,4,9,25,49,-1}},FFF);
-- solutionToChart(s2,MX)
--
--    	  o = {.0755344, .491155}
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
--------------------------
-- toRawSolutions
--
-- Function that takes solutions (in local coordinates) as nxk matrices
-- and writes them into a list of values corresponding to
-- the variables in the local coordinates coordX of the 
-- checkerboard variety 
--
-- !! This functions is used to express the solutions from
-- matrix form to list form when using homotopies !!
--------------------------
-- Input:
--    coordX -- matrix of 0s,1s, and variables representing
--              the local coordinates of the checkerboard variety
--    X -- an nxk matrix that is a solution of the current incidence problem
--
-- Output:
--    List of values that correspond to the variables in the local coordinates
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
--  this function multiplies a column C of 
--  a matrix by the multiplicative inverse of 
--  the element C_r at row r.
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
--
-- These reductions are necessary for the
-- change of coordinates in Ravi's notes
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
--	 -- find the redcheckers below that can see the current redChecker
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


-------------------
-- columnReduce
-------------------
-- Given a matrix of solutions in checkerboard 
-- coordinates (w.r.t lambda1,lambda2) we do
-- column row reduction to the solutions:
--
-- Given a solution matrix and the bracket (where 
-- the pivots are) makes 0's in the columns right to the pivots
-------------------
-- input:     S -- matrix of solutions
--    	      	    assumes the matrix lives in the Schubert cell for l with
--                  the standard flag, but not all pivots are 1.
--    	      b -- the bracket corresponding to the standard flag
--    	      	    this is just a list of the parts of the flag that are afected by a partition lambda
--    	      	    (equivalent to a partition with k parts of size <= n-k)
--            (the default bracket is  
-- output:  Sred  --matrix reduced
-------------------
columnReduce=method(TypicalValue=> Matrix )
columnReduce(Matrix,List) := (S,b)->(
    k := numColumns S;
    n := numRows S;
    -- we use the bracket instead of the partition
    -- b := output2bracket (redcheckers);
    -- b := partition2bracket(l,k,n);
    M:= S;
    apply(k-1, col->(
	    -- editing with respect to the pivot of the (col)th column
	    r := b_(col)-1; --row where the ith pivot is
	    a := S_(r,col); --most likely, this will be 1 (in our application because we always have the standard flag)
	    --rescale the column
	    N := M_{0..col-1}|a^(-1)*M_{col};
	    scan(col+1..k-1, j->(
		    a2 := S_(r,j);
		    N=N|(-a2*N_{col}+M_{j});
		    ));
	   M = N; 
	    ));
    return M
    )

TEST ///
load "NumericalSchubertCalculus/TST/columnReduce.m2"
///    

load "NumericalSchubertCalculus/LR-makePolynomials.m2"
load "NumericalSchubertCalculus/LR-ParameterHomotopy.m2"

-----------------------------
-- Tracks a homotopy
-----------------------------
-- Input: 
--    H -- a list of polynomials in k[xx,t];
--    S = {{...},...,{...}} -- a list of solutions to H at t=0
-- Output: 
--    T - a list of Points that are solutions to H at t=1
-- Caveat:
--    H should be _linear_ in t
------------------------------
trackHomotopyNSC = method(TypicalValue=>List)
trackHomotopyNSC (Matrix,List) := (H,S) -> (
     Rt := ring H;
     t := Rt_0;
     R := (coefficientRing Rt)[drop(gens Rt,1)];
     map't'0 := map(R, Rt, matrix{{0_FFF}}|vars R);
     map't'1 := map(R, Rt, matrix{{1_FFF}}|vars R);
     correctorTolerance := 0.1*getDefault NumericalAlgebraicGeometry$CorrectorTolerance;
     all'sols := select(
	 track(first entries map't'0 H, first entries map't'1 H, S,
	     NumericalAlgebraicGeometry$CorrectorTolerance=>correctorTolerance
	     ),
	 s->status s === Regular
	 );
     nAttempts := 3;
     while nAttempts > 0 and #all'sols < #S do (
     	 sols := track(first entries map't'0 H, first entries map't'1 H, S,
	     NumericalAlgebraicGeometry$CorrectorTolerance=>correctorTolerance
	     );
     	 all'sols = solutionsWithMultiplicity(all'sols|select(sols, s->status s===Regular));
	 nAttempts = nAttempts - 1;
	 correctorTolerance = 0.1 * correctorTolerance;
	 -* -- alternative rerun strategy: piecewise linear path 
   	    -- (gets other solutions though... need to sync between undegenerations?) 
     	 t' := exp(2*pi*ii*random RR);
	 H1 := sub(H,matrix{{t'*t}|drop(gens Rt,1)});
	 sols' := select(track(
		 first entries map't'0 H1, 
	     	 first entries map't'1 H1, 
	     	 S,
	     	 NumericalAlgebraicGeometry$CorrectorTolerance=>correctorTolerance
	     	 ), s->status s === Regular);
	 H2 := sub(H,matrix{{t+(1-t)*t'}|drop(gens Rt,1)});
     	 sols := select(track(
		 first entries map't'0 H2, 
	     	 first entries map't'1 H2, sols',
	     	 NumericalAlgebraicGeometry$CorrectorTolerance=>correctorTolerance
	     	 ), s->status s === Regular);
     	 all'sols = solutionsWithMultiplicity(all'sols|sols);
	 nAttempts = nAttempts - 1;
	 *-
	 );
     if #all'sols < #S then error "trackHomotopy: singularity encountered";
     if #all'sols > #S then error "trackHomotopy: more solutions found than expected";
     if VERIFY'SOLUTIONS then verifyTarget(H, all'sols);
     all'sols 
     )

------------------------
-- isRedCheckerInRegionE
------------------------
-- Binary function that tells if a given red checker, indicated by its row number,
--  lies in the `critical diagonal', also referred to as `Region E'.  This is explained in 
--  the paper.   This is equivalent to the column C of this red checker satisfying
--   a \leq C < b, where a is the column of the rising black checker and b that of 
--   the falling (these are in rows r and r+1, where r is the `critical row'
--
-- This function is needed to set up the homotopy in (at least) case II (again, see the paper).
----------------------------
-- Input: 
--     i = coordinates of a red checker
--     r = critical row
--     black = black checkers on the board
---------------------------
-- Example:
--
-- !! Show a better example!!
--
--blackCheckersPosition = {0,1,3,4,5,2};
--redCheckersPosition = {0, NC, NC, 4, NC, NC};
--
-- simNode = new MutableHashTable
-- simNode.Board =  [{0, 1, 2, 4, 5, 3}, {0, infinity, infinity, 4, infinity, infinity}]
-- simNode.CriticalRow = 2;
-- isRedCheckerInRegionE(1,simNode)
---------------------------
isRedCheckerInRegionE = method()
isRedCheckerInRegionE(ZZ,MutableHashTable) := (i,node) -> (
     r := node.CriticalRow;
     black := first node.Board;
     e0 := position(black, b->b==r+1);
     e1 := position(black, b->b==r);
     i < e1 and i >= e0
     )

-----------------------------
-- end Numerical LR-Homotopies
-----------------------------

-------------------
-- Documentation --
-------------------
beginDocumentation()
load "NumericalSchubertCalculus/doc.m2"
load "NumericalSchubertCalculus/PHCpack-LRhomotopies-doc.m2"

-------------------
-- Tests         --
-------------------
TEST ///
load "NumericalSchubertCalculus/TST/4lines.m2"
///
TEST ///
load "NumericalSchubertCalculus/TST/2e4-G26.m2"
///
TEST ///
load "NumericalSchubertCalculus/TST/21e3-G36.m2"
///
TEST ///
load "NumericalSchubertCalculus/TST/4LinesOsculating_changeFlags.m2"
///
end ---------------------------------------------------------------------
-- END OF THE PACKAGE
---------------------------------------------------------------------------
restart
check "NumericalSchubertCalculus"
installPackage "NumericalSchubertCalculus"
installPackage ("NumericalSchubertCalculus", RerunExamples=>true)
installPackage ("NumericalSchubertCalculus", RunExamples=>false)

--n = 6;
--SchubProb =  matrix{{3, 2,4,6}};
--(f, p, s) := LRtriple(n,SchubProb);
--(R, pols, sols, fixedFlags, movedFlag, solutionPlanes) = parseTriplet(f, p, s)
viewHelp NumericalSchubertCalculus
--first PieriHomotopies(2,2)
