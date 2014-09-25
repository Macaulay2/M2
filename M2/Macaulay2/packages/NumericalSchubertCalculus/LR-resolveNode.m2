------------------
-- resolveNode
------------------
-- A function that will be the skeleton
-- of the homotopy.
-- It first transform the Flag of the child Sch. Var
-- into a generalized flag for the parent
------------------
resolveNode = method()
resolveNode(MutableHashTable,List) := (node,remaining'conditions'flags) ->  
if not node.IsResolved then (
    n := #node.Board#0;
    coordX := makeLocalCoordinates node.Board; -- local coordinates X = (x_(i,j))
    if numgens ring coordX == 0 then ( -- We need to move this block to playcheckers
    	assert(#remaining'conditions'flags==0);
	if DBG>0 then print "-- resolveNode: reached the node with NO REMAINING CONDITIONS";
	node.Solutions = {lift(coordX,FFF)};
	node.IsResolved = true;
	node.FlagM= rsort id_(FFF^n);
	) 
    else if #remaining'conditions'flags==0 then ( -- should we check for consistency???
	     node.Solutions = {};
	     node.IsResolved = true;
	     node.FlagM = rsort id_(FFF^n);
	     )
     else ( -- coordX has variables
     black := first node.Board;

     if node.Children == {} then node.FlagM = matrix mutableIdentity(FFF,n) --change here
     else scan(node.Children, c->resolveNode(c,remaining'conditions'flags));
     
     if VERIFY'SOLUTIONS and BLACKBOX then (
 	 -- temporary: creates a superset of solutions via a blackbox solver
	 all'polynomials := makePolynomials(node.FlagM * coordX, remaining'conditions'flags);
	 polynomials := squareUpPolynomials(numgens ring coordX, all'polynomials);
	 ---* this part is to time and keep track of what is the expensive part of the computation
	 blckbxtime1 := cpuTime();
	 Soluciones:=solveSystem flatten entries polynomials;
	 blckbxtime2 := cpuTime();
	 <<"-- blackbox solving cpuTime:"<<(blckbxtime2 - blckbxtime1)<<endl;
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
     	 ); -- end of VERIFY'SOLUTIONS and BLACKBOX
     if node.Children == {} then ( 
	 lambda := output2partition(last node.Board);
	 k:=#lambda;
	 validpartition := true;
	 scan(lambda, i-> if i>n-k then validpartition = false) ;
	 if validpartition then(
	     -- This is changed! 08.22.2013:
    	     --------------------------------
	     -- we take the next flag at the bottom of the checkerboard tree
	     (l3,F3) := first remaining'conditions'flags;
	     MM := lift(MovingFlag'at'Root n,FFF);
	     ID := id_(FFF^n);
	     (A,T1,T2) := moveFlags2Flags({MM,ID},{ID,F3});
	     T1inv := solve(T1,ID);
	     Ainv := solve(A,ID);
	     newRemainingFlags := drop(remaining'conditions'flags,1);
	     newRemainingFlags = apply(newRemainingFlags, CF->(
		     (C,F) := CF;
		     (C, Ainv*F)
		     ));
	     if DBG>0 then print "-- making a recursive call to resolveNode";
	     if DBG>1 then (
		 print(node.Board);
	     	 print(lambda);
	     	 print("remaining conditions:");
	     	 print(remaining'conditions'flags);
		 );
	     -- *************** main recursive call ********************************************
	     new'l := output2partition last node.Board;
	     newDag := playCheckers(new'l,l3,k,n);
	     resolveNode(newDag,newRemainingFlags);
	     S := newDag.Solutions; 
	     if DBG>1 then << "the previous level gets solution: " << S << endl;
	     
	     brack:=output2bracket last node.Board; --compute the bracket afecting the standard flag
	     -- we use the bracket for column reduction of the solutions;
	     if DBG>1 then << "the bracket" << brack << endl;
	     node.Solutions = if #newRemainingFlags > 0 then (
		 assert(MM==newDag.FlagM);
		 apply(S, s->columnReduce(A*MM*s,brack)) -- A is roughly M^{-1} ??? 
	         ) 
	     else (
		 MM = newDag.FlagM;
		 (A,T1,T2) = moveFlags2Flags({MM,ID},{ID,F3});
		 apply(S, s->columnReduce(A*MM*s,brack))
		 );
	     if DBG>1 then print "... and the transformed solutions are:";
	     if DBG>1 then print(node.Solutions);
	     if DBG>1 then print "-- end (recursive call to resolveNode)";	     
	     node.IsResolved = true;
	     ) else (   
	     << lambda << endl;
	     error( "partition above is not valid"); 
	     );
	 ); 
     scan(node.Fathers, father'movetype->(
     	  (father,movetype) := father'movetype; 
     	  r := father.CriticalRow; -- critical row: rows r and r+1 are the most important ones
          red := last father.Board;     
     	  red'sorted := sort delete(NC, red);
	  M := node.FlagM;
	  M'':= M_{0..(r-1)} | M_{r} - M_{r+1} | M_{r}| M_{(r+2)..(n-1)};
	  if not father.?FlagM then father.FlagM = M''; 
	  assert (father.FlagM == M'');
     	  if DBG>1 then << "-- FROM " << node.Board << " TO " << father.Board << endl;
	  if DBG>1 then (
	       << "using this move: " << movetype<<endl;
	       << "from "<< node.FlagM * makeLocalCoordinates(node.Board)
	       << " to  "<< father.FlagM * makeLocalCoordinates(father.Board) << endl; 
	       << "starting with these solutions: "<< node.Solutions<<endl<<endl;
     	       );
     	  if DBG>0 then tparents1:=cpuTime();
	  
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
    	    	
	       if DBG>1 then (
	       	   << "via M*" << VwrtM << " = " << M'X' 
	       	   << " where M = " << promote(M,Rt) << endl;
    		   );
	       if DBG>0 then timemakePolys1 := cpuTime();
	       all'polys := makePolynomials(M'X',remaining'conditions'flags);
	       if DBG>0 then (
		    timemakePolys2 := cpuTime();
		    << "-- time to make equations:  "<< (timemakePolys2-timemakePolys1)<<endl;
		    );
    
	       polys := squareUpPolynomials(numgens R, all'polys);
	       startSolutions := apply(node.Solutions, X->toRawSolutions(coordX,X));
	       
	       -- check at t=0
	       if VERIFY'SOLUTIONS then scan(startSolutions,  
		   s->assert(norm sub(polys,matrix{{0_FFF}|s}) < ERROR'TOLERANCE * 
		       norm matrix{s} * 
		       norm sub(last coefficients polys,FFF)));
	       if DBG>0 then t1:= cpuTime();
	       -- track homotopy and plug in the solution together with t=1 into Xt
	       targetSolutions := trackHomotopy(polys,startSolutions);
	       if DBG>0 then (
	       	    t2:= cpuTime();
	       	    << " -- trackHomotopy time = " << (t2-t1) << " for " << node.Board << endl;
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
		    else normalizeColumn(X'',r,s)
		    ))
	       );
	   
	  if VERIFY'SOLUTIONS then (
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
     	  
	  if DBG>0 then (
     	       tparents2:=cpuTime();
     	       << "-- time of performing one checker move: "<< (tparents2 - tparents1) << endl;
     	       );
     	  ));
     if VERIFY'SOLUTIONS and node.?SolutionsSuperset then(
     	  -- check against the blackbox solutions
     	  scan(node.Solutions, X->
	       assert(position(node.SolutionsSuperset, Y->norm(Y-X)<ERROR'TOLERANCE) =!= null)); 
     	  );
     ); -- END coordX has variables
     node.IsResolved = true;
   ) -- END resolveNode
