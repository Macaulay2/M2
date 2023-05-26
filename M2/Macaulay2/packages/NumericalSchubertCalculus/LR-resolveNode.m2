----------------
--Functions contained here but not exported:
----------------
-- blackBoxSolve
-- verifyTarget
-- verifyStart
-- globalStayCoords
-- globalSwapCoords
-- caseSwapStay
-- verifyParent
-- solveCases
-- resolveNode
----------------
blackBoxSolve = method();
blackBoxSolve(MutableHashTable,List,Matrix) := (node,
   remaining'conditions'flags,coordX) -> (
--
-- DESCRIPTION :
--   If resolveNode runs with the options VERIFY'SOLUTIONS and BLACKBOX 
--   then the problem is solved by the blackbox solver.
--
-- IN :
--   node : 
--      node.flagM
--   remaining'conditions'flags
--   coordX : a matrix of zeros, ones, and variables for the local coordinates,
--      computed as the output of makeLocalCoordinates.
--
-- OUT :
--   node.SolutionsSuperSet are the solutions computed by the blackbox solver.
--
   all'polynomials := makePolynomials(node.FlagM * coordX,
      remaining'conditions'flags);
   polynomials := squareUpPolynomials(numgens ring coordX, all'polynomials);
   -- time and keep track of the expensive part of the computation
   blckbxtime1 := cpuTime();
   Soluciones:=solveSystem flatten entries polynomials;
   blckbxtime2 := cpuTime();
   << "-- blackbox solving cpuTime: " << (blckbxtime2 - blckbxtime1) << endl;
   node.SolutionsSuperset = apply(
      select(
      -- After finish with the timing, remove the previous part
      -- and the next line; and uncomment the following line
      -- (deleting the line after that)
         Soluciones,
          -- time solveSystem flatten entries polynomials, 
         s-> norm sub(gens all'polynomials,matrix s)
                <= ERROR'TOLERANCE * norm matrix s * 
                   norm sub(last coefficients gens all'polynomials,FFF)
      ), -- end of select
      ss-> (map(FFF,ring coordX,matrix ss)) coordX
   ); -- end of apply
);

verifyTarget = method();
verifyTarget(Matrix,List) := (polys, targetSolutions) -> (
--
-- DESCRIPTION :
--   Verifies the solutions at the end of the homotopy.
--
-- IN :
--   polys : matrix of polynomials;
--   startSolutions : solutions that should vanish as polys.
--
   scan(targetSolutions, p->assert (
      s := matrix p;
      norm sub(polys,matrix{{1_FFF}}| s)
      < ERROR'TOLERANCE * max{1,norm s}
      * norm sub(last coefficients polys,FFF)
      ) -- end assert
   ) -- end scan 
);

verifyStart = method();
verifyStart(Matrix,List) := (polys, startSolutions) -> (
--
-- DESCRIPTION :
--   Verifies the solutions at the start of the homotopy.
--
-- IN :
--   polys : matrix of polynomials;
--   startSolutions : solutions that should vanish as polys.
--
   scan(startSolutions, s->assert (
      norm sub(polys,matrix{{0_FFF}|s})
      < ERROR'TOLERANCE * max{1,norm matrix{s}}
      * norm sub(last coefficients polys,FFF)
      ) -- end assert
   ) -- end scan startSolutions
);

globalStayCoords = method();
globalStayCoords(MutableHashTable,Sequence,Sequence,Sequence) := (father,
   rings,redchk,rnM) -> (
--
-- DESCRIPTION :
--    Returns the global coordinates for the homotopy in the stay case, which is 
--       case II in the paper.  By global coordinates, we mean the Steifel coordinates MX.
--
-- IN :
--    father : the current father to the node,
--    rings : the homotopy ring Rt, the Xt, and the symbol t,
--    redchk : the checkers red and red'sorted,
--                red'sorted is just the rows containing red checkers
--    rnM : the critical row r, the dimension n, and flag M.
--
-- OUT :
--    returns the M YtCoords(t) as needed in the stay homotopy.
--     YtCoords are the Stiefel Coordinates used in the homotopy
--     Note: Xt is used for the Stiefel coordinates Y_{\cbd} in the ring [t]
--     Also, Xt = YtCoords(t=0)
--
   (Rt, Xt, t) := rings;
   (red, red'sorted) := redchk;
   (r, n, M) := rnM;
   YtCoords := map(Rt^n,Rt^0,{}); -- an empty column vector
   -- V(t) = M YtCoords(t) ... we write everything in terms of M
   scan(#red'sorted, j-> YtCoords = YtCoords |
      if isRedCheckerInRegionE(position(red, i->i==red'sorted#j),father)
         -- column of the j-th red checker on the board
      then (
	submatrix(Xt,{0..n-1},{j})
      ) else (
         submatrix(Xt,{0..r},{j}) 
         || submatrix(Xt,{r+1},{j})-t*submatrix(Xt,{r},{j})
         || submatrix(Xt, {r+2..n-1}, {j})	    
      )
   );
   result := promote(M,Rt) * YtCoords;
   if DBG>1 then (
      << "via M*" << YtCoords << " = " << result
      << " where M = " << promote(M,Rt) << endl;
   );
   result
);

globalSwapCoords = method();
globalSwapCoords(MutableHashTable,Sequence,Sequence,Sequence) := (father,
   rings,checkers,rsnM) -> (
--
-- DESCRIPTION :
--    Returns the global coordinates for the homotopy in the swap case, case III in paper
--
-- IN :
--    father : the current father to the node,
--    rings : the homotopy ring Rt, the Xt, and the symbol t,
--    checkers : the checkers black, red and red'sorted,
--    rsnM : the critical row r, the index s, the dimension n, and flag M.
--
-- OUT :
--    returns the M YtCoords as needed in the swap homotopy.
--      YtCoords are the Stiefel coordinates used in the homotopy, depends on t
--
   (Rt, Xt, t) := rings;
   (black, red, red'sorted) := checkers;
   (r, s, n, M) := rsnM;
   YtCoords := map(Rt^n,Rt^0,{}); -- an empty column vector
   -- V(t) = M YtCoords(t) ... we write everything in terms of M
   bigR := red'sorted#(s+1);   -- row of the second moving red checker
   rightmost'col'B := position(black, j->j==r);
   leftmost'col'A  := position(black, j->j==r+1)+1;
   -- check if the black checker in the i'th row is in region A
   isRegionA := i -> position(black, i'->i'==i) >= leftmost'col'A;
   -- check if the black checker in the i'th row is in region B
   isRegionB := i -> position(black, i'->i'==i) <= rightmost'col'B;
--------  Frank/Abr. revision Oct. 2016
   scan(#red'sorted, j -> YtCoords = YtCoords |
      if j == s then 
      (
      -- note: this part can be optimized for speed
         transpose matrix { apply(n, i -> (
            if i==r then Xt_(r+1,s+1)
            else if i==r+1 then -t*Xt_(r+1,s+1)
            else if isRegionA i then -t*Xt_(i,s+1)
            else if isRegionB i then Xt_(r+1,s+1)*Xt_(i,s)
            else 0)) }
      ) else
      (
	  submatrix(Xt, {0..n-1}, {j})
       )
   );  -- end scan red'sorted
----------------
   result := promote(M,Rt) * YtCoords;
   if DBG>1 then (
      << "via M*" << YtCoords << " = " << result
      << " where M = " << promote(M,Rt) << endl;
   );
   result
);

caseSwapStay = method();
caseSwapStay(MutableHashTable,List,Matrix,Sequence) := (node,
   remaining'conditions'flags,coordX,
   r'Mdprime'father'movetype'black'red'red'sorted) -> (
--
-- DESCRIPTION :
--   Applies a homotopy in the cases of swap or stay. II, III(a), and III(b) from the paper.
--    In all of these, there is a red checker in the critical row, r
--
-- IN :
--    node : see the resolveNode documentation for all items
--    remaining'conditions'flags : pairs of conditions and flags
--    coordX : local coordinates
--    movetype'red'red'sorted is a sequence that contains
--     (0) r : index of the CriticalRow
--     (1) Mdprime : M'' in the solveCases below
--     (2) father : the current father node
--     (3) movetype : to decide which case applies (position in 3x3 array of questions
--                      where red checker is in critical diagonal, critical row
--     (4) black : black checkers
--     (5) red : the red checkers
--     (6) red'sorted : sorted red checkers
--    stored into a sequence to bypass the annoying limitation that
--    Macaulay2 methods can have no more than 4 arguments.
--
-- OUT :
--    returns solutions for assignment to father'Solutions.
--
   (r, M'', father, movetype, black, red, red'sorted)
      := r'Mdprime'father'movetype'black'red'red'sorted;
   n := #node.Board#0;
   M := node.FlagM;
   R := ring coordX;
   t := symbol t;
   Rt := (coefficientRing R)[t,gens R]; -- homotopy ring
   mapRtoRt := map(Rt,R,drop(gens Rt,1));
   Xt := mapRtoRt coordX; --  "homotopy" X 
   s := position(red'sorted, i->i==r);
   -- Column of red checker in the critical row
   local M'X'; -- homotopy in global coordinates
   -- (produced by each case) these are used only in SWAP cases
   if member(movetype,{{2,0,0},{2,1,0},{1,1,0}}) then ( -- stay case, case II in paper
      M'X' = globalStayCoords(father,(Rt,Xt,t),(red,red'sorted),(r,n,M))
   ) -- end case "STAY" 
   else
      if member(movetype, {{1,0,0},{1,1,1},{0,0,0},{0,1,0}}) then
      ( -- case SWAP(middle row)
         M'X' = globalSwapCoords(father,(Rt,Xt,t),(black,red,red'sorted),
                   (r,s,n,M))
      ) -- end case SWAP(middle row)
      -- implementing this case separately 
      -- gives lower degree polynomials
      -- else if member(movetype,{{0,0,0},{0,1,0}}) 
      -- then (-- case SWAP(top row)		    
      -- )
      else
         error "an unaccounted case";
   if DBG>0 then timemakePolys1 := cpuTime();
   strategy := 
     if all(remaining'conditions'flags/first, c->#c==1) and 
        #remaining'conditions'flags*numrows M'X' <= 30 
     then "lifting" else "Pluecker";
   (all'polys,startSolutions) := makePolynomials(M'X', remaining'conditions'flags, 
       apply(node.Solutions, X->toRawSolutions(coordX,X)), --start solutions
       Strategy=>strategy
       );
   if DBG>0 then (
      timemakePolys2 := cpuTime();
      << "-- time to make equations: "
      << (timemakePolys2-timemakePolys1)<<endl;
   );
   polys := squareUpPolynomials(numgens ring all'polys-1, all'polys);
   -- check at t=0
   if VERIFY'SOLUTIONS then verifyStart(polys, startSolutions);
   -- track homotopy and plug in the solution together with t=1 into Xt
   (ti,targetSolutions) := toSequence elapsedTiming trackHomotopyNSC(polys,startSolutions);
   statsIncrementTrackingTime ti; 
   if DBG>0 then (
      << " -- trackHomotopy time = " << ti << " sec."
      << " for " << node.Board << endl;
   );
   apply(targetSolutions, sln -> (
      x'sln := if strategy != "lifting" 
               then matrix sln else (matrix sln)_{0..numgens R-1};
      M''X'' := (map(FFF,Rt,matrix{{1}}|x'sln)) M'X';
      X'' := inverse M'' * M''X'';
      if not member(movetype,{{2,0,0},{2,1,0},{1,1,0}}) then ( -- SWAP CASE
         k := numgens source X'';
         X'' = X''_{0..s}| X''_{s}+X''_{s+1}| X''_{s+2..k-1};
         -- we substitute the s+1 column for the vector w_{s+1}
         redCheckersColumnReduce2(normalizeColumn(X'',r,s),father)
      ) 
      else
         normalizeColumn(X'',r,s)
     ) -- end second argument of apply
   ) -- end apply targetSolutions
);

verifyParent = method();
verifyParent(MutableHashTable,List) := (father, parent'solutions) -> (
--
-- DESCRIPTION :
--   Verifies the solutions computed at the parent node.
--   This verification is called when the flag VERIFY'SOLUTIONS
--   is on when solveCases is running.
--
-- IN :
--   father : parent node of the current node in solveCases,
--   parent'solutions : solutions computed by solveCases.
--
-- OUT :
--   verifies that all solutions fits the pattern of the parent.
--
   parentX := makeLocalCoordinates father.Board;
   parentXlist := flatten entries parentX;
   scan(parent'solutions, X'''-> ( 
      -- check that solutions fit the parent's pattern
      a := flatten entries X''';
      scan(#a, i -> 
	  if not (
              (abs a#i < ERROR'TOLERANCE and parentXlist#i == 0)
              or (abs(a#i-1) < ERROR'TOLERANCE and parentXlist#i == 1)
              or (parentXlist#i != 0 and parentXlist#i != 1)
	      )
	  then error "a solution does not fit the expected pattern (numerical error occurred)"         	 
      	 ); -- end scan on #a
     ) -- end of second argument of scan
   ) -- end scan parent'solutions
);

solveCases = method();
solveCases(MutableHashTable,List,Matrix) := (node,
   remaining'conditions'flags,coordX) -> (
--
-- DESCRIPTION :
--   Solves the nine cases in the Littlewood-Richardson homotopies. 
--
-- IN :
--    node : see the resolveNode documentation for all items
--    remaining'conditions'flags : pairs of conditions and flags
--    coordX : local coordinates
--
-- OUT :
--    node : the Solutions are computed.
--
   n := #node.Board#0;
   black := first node.Board;
   scan(node.Fathers, father'movetype ->
   (
      (father, movetype) := father'movetype; 
      r := father.CriticalRow; 
      -- The critical row r and the next one r+1 are where the black checkers are moving.
      --  These are where all of the action in the homotopy is.
      red := last father.Board;     
      red'sorted := sort delete(NC, red);
      M := node.FlagM;
      --  The moving flag is modified.  In the paper, this is the flag M' defined on 
      --   page 12 (Reference needs to be put in)
      M'':= M_{0..(r-1)} | M_{r} - M_{r+1} | M_{r}| M_{(r+2)..(n-1)};
      -- Defined the flag for the next level if it is empty
      if not father.?FlagM then father.FlagM = M''; 
      -- If that flag exists, checks it equals this one, for consistency.
      assert (father.FlagM == M'');
      if DBG>1 then (
         << "-- FROM " << node.Board << " TO " << father.Board << endl;
         << "using this move: " << movetype<<endl;
         << "from "<< node.FlagM * makeLocalCoordinates(node.Board)
         << " to  "<< father.FlagM * makeLocalCoordinates(father.Board) << endl;
         << "starting with these solutions: " << node.Solutions << endl << endl;
      );
      if DBG>0 then tparents1:=cpuTime();
      parent'solutions :=  -- this is where the main action happens
         if node.Solutions == {} then
            {} -- means: not implemented
         else if movetype#1 == 2 then ( -- case movetype = (_,2).  The conditions on the k-plane do not change.
	     --  This is case I in Section 3.3.1 in the paper, and it is just a coordinate change,
	     --   see Equation (???) on page 13.
	     --  The paper uses Y for the Stiefel coordinates, and only a single '.  
            apply(node.Solutions, X -> (
               X'' := (X^{0..r-1}) || (-X^{r+1})
                                   || (X^{r}+X^{r+1}) ||( X^{r+2..n-1});
               -- These coordinates are not in echelon form if there is a red checker in row r+1.
	       --  We need to check for this and if so, get its column.
               j := position(red'sorted, i-> i == r+1);
               -- If so, then we need to divide the jth column by the entry in position (r+1,j) to put the
	       --  coordinates in echelon form
               if j =!= null then
                  redCheckersColumnReduce2(normalizeColumn(X'',r+1,j), father)
               else X'' )
            ) -- end apply to node.Solutions
         ) -- end case movetype = (_,2) 
         else -- The other cases require a homotopy, and were implemented in caseSwapStay
            caseSwapStay(node, remaining'conditions'flags, coordX,
               (r, M'',father, movetype, black, red, red'sorted));
         if VERIFY'SOLUTIONS then
            verifyParent(father, parent'solutions);
         if not father.?Solutions then father.Solutions = {};  
         father.Solutions = father.Solutions | parent'solutions;
         if DBG>0 then (
            tparents2:=cpuTime();
            << "-- time of performing one checker move: "
            << (tparents2 - tparents1) << endl;
         );
      )
   ) -- end scan node.Fathers
);

resolveNode = method();
resolveNode(MutableHashTable,List) := (node,remaining'conditions'flags) -> (
--
-- DESCRIPTION :
--    This method resolves a node in the Littlewood-Richardson homotopy.
--
-- IN :
--    node : the node contains nine items
--       (1) Board represents a checkerboard, stored as two vectors,
--           defining the location of the black and white checkers;
--       (2) CriticalRow is either a number or "leaf" if at the leaf,
--           the number is the position of the black checker that moves;
--       (3) flagM is the moving flag;
--       (4) IsResolved is a boolean;
--       (5) Solutions is a list of solutions in local coordinates;
--       (6) Children points to the children of the  node;
--       (7) Fathers contains the ancestors of the node;
--       (8) movetype is a list to connect the node to its fathers
--       (9) SolutionsSuperSet is made when the BLACKBOX option is on.
--    remaining'conditions'flags : the remaining pairs of conditions
--       and flags.
--
-- OUT :
--    node : the following items are modified
--       (3) flagM is the moving flag;
--       (4) IsResolved is the moving flag;
--       (5) Solutions is a list of solutions in local coordinates;
--       (8) SolutionsSuperSet is made when the BLACKBOX option is on.
--     remaining'conditions'flags : will be transformed.
--
   if not node.IsResolved then (
      n := #node.Board#0;
      coordX := makeLocalCoordinates node.Board;
      -- local coordinates X = (x_(i,j))
      if numgens ring coordX == 0 then ( -- need to move this to playcheckers
         assert(#remaining'conditions'flags == 0);
         if DBG>0 then 
            print "resolveNode reached node of no remaining conditions";
         node.Solutions = {lift(coordX,FFF)};
         node.IsResolved = true;
         node.FlagM= rsort id_(FFF^n);
      ) -- end if numgens ring coordX == 0
      else if #remaining'conditions'flags == 0 then ( -- check consistency???
         node.Solutions = {};
         node.IsResolved = true;
         node.FlagM = rsort id_(FFF^n);
      )
      else ( -- coordX has variables
         black := first node.Board;
         if node.Children == {} then
            node.FlagM = matrix mutableIdentity(FFF,n) -- change here
         else
            scan(node.Children, c->resolveNode(c,remaining'conditions'flags));
         if node.Children == {} then ( 
            lambda := output2partition(last node.Board);
            k := #lambda;
            validpartition := true;
            scan(lambda, i-> if i>n-k then validpartition = false);
            if not validpartition then (
               << lambda << endl;
               error( "partition above is not valid")
            ) else (
               -- we take the next flag at the bottom of the checkerboard tree
               (l3,F3) := first remaining'conditions'flags;
               MM := lift(MovingFlag'at'Root n,FFF);
               ID := id_(FFF^n);
               (A,T1,T2) := moveFlags2Flags({MM,ID},{ID,F3});
               Ainv := solve(A,ID);
               newRemainingFlags := drop(remaining'conditions'flags,1);
               newRemainingFlags = apply(newRemainingFlags,
                  CF->(
                         (C, F) := CF;
                         (C, Ainv*F)
                      )
               );
               if DBG>0 then print "-- making a recursive call to resolveNode";
               if DBG>1 then (
                  print(node.Board);
                  print(lambda);
                  print("remaining conditions:");
                  print(remaining'conditions'flags);
               );
               new'l := output2partition last node.Board;
	       -- Abraham 15-Oct-2015:
	       l3 = verifyLength(l3,k);
	       checkPartitionsOverlap := (new'l+reverse l3)/(i->n-k-i);
    	       if min(checkPartitionsOverlap) < 0 then (
		   node.Solutions = {};
         	   node.IsResolved = true;
	       )else(
	       --
               	  newDag := playCheckers(new'l,l3,k,n);
               	  resolveNode(newDag,newRemainingFlags); -- recursive call
               	  S := newDag.Solutions; 
               	  if DBG>1 then
                     << "the previous level gets solution: " << S << endl;
               	  brack := output2bracket last node.Board;
               	  -- compute the bracket affecting the standard flag
               	  -- we use the bracket for column reduction of the solutions;
               	  if DBG>1 then << "the bracket" << brack << endl;
                  node.Solutions = if #newRemainingFlags > 0 then (
                      assert(MM == newDag.FlagM);
                      apply(S, s->columnReduce(A*MM*s,brack)) -- A is M^{-1} ??? 
               	  ) else (
                     MM = newDag.FlagM;
                     (A,T1,T2) = moveFlags2Flags({MM,ID},{ID,F3});
                     apply(S, s->columnReduce(A*MM*s,brack))
                  );
	          if DBG>1 then (
                     print "... and the transformed solutions are:";
                     print(node.Solutions);
                     print "-- end (recursive call to resolveNode)"
               	  );
	          node.IsResolved = true
               );--end else of the if checkPartitionsOverlap
	   ); -- end else of the if not validpartition
         ); -- end if node.children == {}
         solveCases(node,remaining'conditions'flags,coordX);
         if VERIFY'SOLUTIONS and BLACKBOX then
            blackBoxSolve(node,remaining'conditions'flags,coordX);
         if VERIFY'SOLUTIONS and node.?SolutionsSuperset then (
            -- check against the blackbox solutions
            scan(node.Solutions, X->
               assert(position(node.SolutionsSuperset,
               Y->norm(Y-X)<ERROR'TOLERANCE) =!= null)
            ); -- end scan node.Solutions
         );
      ); -- end coordX has variables
      node.IsResolved = true;
   ); -- end if not node.IsResolve
);
