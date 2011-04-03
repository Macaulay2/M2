-- -*- coding: utf-8 -*-

newPackage(
     "GraphicalModels",
     Version => "0.1",
     Date => "October 10, 2010",
     Authors => {
	  {Name => "Luis Garcia-Puente",
	   Email => "lgarcia@shsu.edu",
	   HomePage => "http://www.shsu.edu/~ldg005"},
	  {Name => "Mike Stillman",
	   Email => "mike@math.cornell.edu",
	   HomePage => "http://www.math.cornell.edu/~mike/"} 
	  },
     Headline => "A package for discrete and Gaussian statistical graphical models",
     PackageExports => {"Graphs"},
     DebuggingMode => true
     )

------------------------------------------
-- Algebraic Statistics in Macaulay2
-- Authors: Luis Garcia-Puente and Mike Stillman
-- Collaborators: Alexander Diaz, Shaowei Lin and Sonja Petrović
-- 
-- Routines:
--  Markov relations:
--   pairMarkov (Digraph G)
--   localMarkov (Digraph G)
--   globalMarkov (Digraph G)
--   bayesBall (set A, set C, Digraph G)  [internal function used by globalMarkov]
--
--  Removing redundant statements: 
--   equivStmts (S,T) -- [internal routine used within Markov relation routines]
--   setit (d) -- [internal routine used within Markov relation routines] 
--   under (d) -- [internal routine used within Markov relation routines]
--   sortdeps (Ds) -- [internal routine used within Markov relation routines]
--   normalizeStmt (D) -- [internal routine used within Markov relation routines]
--   minimize (Ds) -- [internal routine used within Markov relation routines]
--   removeRedundants (Ds) -- [internal routine used within Markov relation routines]
--
--  Markov Rings: 
--   markovRing (sequence d)
--   marginMap(R,i) : R --> R
--   hiddenMap(R,i) : R --> S
--
--  Markov Ideals:
--   markovMatrices (Ring R, Digraph G, List S)  -- S is a list of independence statements
--   markovIdeal (Ring R, Digraph G, List S)  -- S is a list of independence statements
--   cartesian -- [internal routine used in MarkovMatrices and MarkovIdeal]
--   possibleValues -- [internal routine used in MarkovMatrices and MarkovIdeal]
--   prob -- [internal routine used in MarkovMatrices and MarkovIdeal]
--   getPositionofVertices (Digraph G, list D) -- [internal routine]
--   
--  Gaussian directed acyclic graphs:
--   gaussianRing (Integer n)
--   gaussianRing (Digraph G)
--   covarianceMatrix (Ring R)
--   covarianceMatrix (Ring R, Digraph G)
--   gaussianMinors (Digraph G, Matrix M, List S) -- [internal routine]
--   gaussianMatrix (Ring R, Digraph G, List S) -- [internal routine]
--   gaussianMatrices (Ring R, Digraph G, List S) 
--   gaussianIdeal (Ring R, Digraph G, List S) 
--   gaussianIdeal (Ring R, Digraph G) 
--   trekIdeal (Ring R, Digraph D)
--    
-- Gaussian mixed graphs (DAG + Bidirected)
--   pos -- [internal routine]
--   setToBinary -- [internal routine]
--   subsetsBetween -- [internal routine]
--   gaussianRing (MixedGraph G)
--   covarianceMatrix (Ring R, MixedGraph G)
--   directedEdgesMatrix (Ring R, MixedGraph G)
--   bidirectedEdgesMatrix (Ring R, MixedGraph G)
--   gaussianParametrization (Ring R, MixedGraph G)
--   identifyParameters (Ring R, MixedGraph G)
--   trekSeparation (MixedGraph G)
--   trekIdeal (Ring R, MixedGraph G, List L)
--   trekIdeal (Ring R, MixedGraph G)
--
------------------------------------------

export {bidirectedEdgesMatrix,
       Coefficients,
       covarianceMatrix,
       directedEdgesMatrix,
       gaussianIdeal, 
       gaussianMatrices,
       gaussianParametrization,
       SimpleTreks,
       gaussianRing, 
       globalMarkov,
       hiddenMap,
       identifyParameters, 
       localMarkov,
       markovIdeal,
       markovMatrices, 
       markovRing,        
       marginMap, 
       pairMarkov, 
       trekIdeal, 
       trekSeparation,
       VariableName
	} 

markov = local markov
markovVariables = local markovVariables
gaussianVariables = local gaussianVariables

--------------------------
--   Markov relations   --
--------------------------

--------------------------
--   pairMarkov         --
--------------------------

pairMarkov = method()
pairMarkov Digraph := List => (G) -> (
     -- given a digraph G, returns a list of triples {A,B,C}
     -- where A,B,C are disjoint sets, and for every vertex v
     -- and non-descendent w of v,
     -- {v, w, nondescendents(G,v) - w}
     removeRedundants flatten apply(sort vertices G, v -> (
	       ND := nondescendents(G,v);
	       W := ND - parents(G,v);
	       apply(toList W, w -> {set {v}, set{w}, ND - set{w}}))))

--------------------------
--   localMarkov        --
--------------------------

localMarkov = method()			 
localMarkov Digraph := List =>  (G) -> (
     -- Given a digraph G, return a list of triples {A,B,C}
     -- of the form {v, nondescendents - parents, parents}
     result := {};
     scan(sort vertices G, v -> (
	       ND := nondescendents(G,v);
	       P := parents(G,v);
	       if #(ND - P) > 0 then
	         result = append(result,{set{v}, ND - P, P})));
     removeRedundants result)

--------------------------
--   globalMarkov       --
--------------------------
 
globalMarkov = method()
globalMarkov Digraph := List => (G) -> (
     -- Given a graph G, return a complete list of triples {A,B,C}
     -- so that A and B are d-separated by C (in the graph G).
     -- If G is large, this should maybe be rewritten so that
     -- one huge list of subsets is not made all at once
     V := sort vertices G;
     result := {};
     AX := subsets V;
     AX = drop(AX,1); -- drop the empty set
     AX = drop(AX,-1); -- drop the entire set
     scan(AX, A -> (
	       A = set A;
	       Acomplement := toList(set V - A);
	       CX := subsets Acomplement;
	       CX = drop(CX,-1); -- we don't want C to be the entire complement
	       scan(CX, C -> (
			 C = set C;
			 B := bayesBall(A,C,G);
			 if #B > 0 then (
			      B1 := {A,B,C};
			      if all(result, B2 -> not equivStmts(B1,B2))
			      then 
			          result = append(result, {A,B,C});
	       )))));
     removeRedundants result
     )

--------------------------
-- Bayes ball algorithm --
--------------------------

bayesBall = (A,C,G) -> (
     -- A is a set in 1..n (n = #G)
     -- C is a set in 1..n (the "blocking set")
     -- G is a DAG
     -- Returns the subset B of 1..n which is
     --   independent of A given C.
     -- The algorithm is the Bayes Ball algorithm,
     -- as implemented by Luis Garcia-Puente, after
     -- the paper of Ross D. Shachter.
     --
     V := sort vertices G;
     -- DEVELOPMENT NOTES: 
     -- 
     visited := new MutableHashTable from apply(V, k-> k=>false);
     blocked :=  new MutableHashTable from apply(V, k-> k=>false);
     up :=  new MutableHashTable from apply(V, k-> k=>false);
     down := new MutableHashTable from apply(V, k-> k=>false);
     top :=  new MutableHashTable from apply(V, k-> k=>false);
     bottom := new MutableHashTable from apply(V, k-> k=>false);
     vqueue := toList A; -- sort toList A
     -- Now initialize vqueue, set blocked
     scan(vqueue, a -> up#a = true);
     scan(toList C, c -> blocked#c = true);
     local pa;
     local ch;
     while #vqueue > 0 do (
	  v := vqueue#-1;
	  vqueue = drop(vqueue,-1);
	  visited#v = true;
	  if not blocked#v and up#v
	  then (
	       if not top#v then (
		    top#v = true;
		    pa = toList parents(G,v);
		    scan(pa, i -> up#i = true);
		    vqueue = join(vqueue,pa);
		    );
	       if not bottom#v then (
		    bottom#v = true;
		    ch = toList children(G,v);
		    scan(ch, i -> down#i = true);
		    vqueue = join(vqueue,ch);
		    );
	       );
	  if down#v
	  then (
	       if blocked#v and not top#v then (
		    top#v = true;
		    pa = toList parents(G,v);
		    scan(pa, i -> up#i = true);
		    vqueue = join(vqueue,pa);
		    );
	       if not blocked#v and not bottom#v then (
		    bottom#v = true;
		    ch = toList children(G,v);
		    scan(ch, i -> down#i = true);
		    vqueue = join(vqueue,ch);
		    );
	       );
	  ); -- while loop
     set toList select(V, i -> not blocked#i and not bottom#i)     
     )


------------------------------------------------------------------
-- Removing redundant statements:                               --
-- called from local, global, and pairwise Markov methods.      --
------------------------------------------------------------------

-- An independent statement is a list {A,B,C}
-- where A,B,C are (disjoint) subsets of labels for nodes in the graph.
-- It should be interpreted as: A independent of B given C.
-- A dependency list is a list of dependencies.

-- We have several simple routines to remove
-- the most obvious redundant elements, 
-- but a more serious attempt to remove dependencies could be made.

-- If S and T represent exactly the same dependency, return true.
 
-- check for symmetry:
equivStmts = (S,T) -> S#2 === T#2 and set{S#0,S#1} === set{T#0,T#1} 

-- More serious removal of redundancies.  
setit = (d) -> {set{d#0,d#1},d#2}

under = (d) -> (
           d01 := toList d_0;
           d0 := toList d01_0;
           d1 := toList d01_1;
           d2 := toList d_1;
           e0 := subsets d0;
           e1 := subsets d1;
           z1 := flatten apply(e0, x -> apply(e1, y -> (
      		    {set{d01_0 - set x, d01_1 - set y}, set x + set y +  d_1})));-- see comment at removeRedundants
           z2 := flatten apply(e0, x -> apply(e1, y -> (
      		    {set{d01_0 - set x, d01_1 - set y},  d_1})));-- see comment at removeRedundants
           z := join(z1,z2);
           z = select(z, z0 -> not member(set{}, z0_0));
           set z
           )

sortdeps = Ds -> (
     -- input: ds
     -- first make list where each element is {-a*b, set{A,B}, set C}
     -- sort the list
     -- remove the first element
     i := 0;
     ds := apply(Ds, d -> (x := toList d#0; i=i+1; { - #x#0 * #x#1, i, d#0, d#1}));
     ds = sort ds;
     apply(ds, d -> {d#2, d#3})
     )

normalizeStmt = (D) -> (
     -- D has the form: {set{set{A},set{B}},set{C}}
     -- output is {A,B,C}, where A,B,C are sorted in increasing order
     --  and A#0 < B#0
     D0 := sort apply(toList(D#0), x -> sort toList x);
     D1 := toList(D#1);
     {D0#0, D0#1, D1}
     )

minimize = (Ds) -> (
     -- each element of Ds should be a list {A,B,C}
     answer := {};
     -- step 1: first make the first two elements of each set a set
     Ds = Ds/setit;
     while #Ds > 0 do (
	  Ds = sortdeps Ds;
	  f := Ds_0;
	  funder := under f;
	  answer = append(answer, f);
	  Ds = set Ds - funder;
	  Ds = toList Ds;
	  );
     apply(answer, normalizeStmt))

removeRedundants = (Ds) -> (
     -- Ds is a list of triples of sets {A,B,C}
     -- test1: returns true if D1 can be removed
     -- Return a sublist of Ds which removes any 
     -- that test1 declares not necessary.
     --**CAVEAT: this works just fine when used internally, e.g. from localMarkov. 
     --  However, if we export it and try to use it, there is a problem: we seem to be 
     --  attempting to add a List to a Set in 2 lines of "under".
     test1 := (D1,D2) -> (D1_2 === D2_2 and 
                          ((isSubset(D1_0, D2_0) and isSubset(D1_1, D2_1))
	               or (isSubset(D1_1, D2_0) and isSubset(D1_0, D2_1))));
     -- first remove non-unique elements, if any
     Ds = apply(Ds, d -> {set{d#0,d#1}, d#2});
     Ds = unique Ds;
     Ds = apply(Ds, d -> append(toList(d#0), d#1));
     c := toList select(0..#Ds-1, i -> (
	       a := Ds_i;
	       D0 := drop(Ds,{i,i});
	       all(D0, b -> not test1(a,b))));
     minimize(Ds_c))

-------------------
-- Markov rings ---
-------------------

markovRingList := new MutableHashTable;
-- the hashtable is indexed by the sequence d, the coefficient ring kk, and the variable name p.
markovRing = method(Dispatch=>Thing, Options=>{Coefficients=>QQ,VariableName=>getSymbol "p"})
markovRing Sequence := Ring => opts -> d -> (
     -- d should be a sequence of integers di >= 1
     if any(d, di -> not instance(di,ZZ) or di <= 0)
     then error "markovRing expected positive integers";
     kk := opts.Coefficients;
     p := opts.VariableName;
     if (not markovRingList#?(d,kk,toString p)) then (
     	  start := (#d):1;
	  vlist := start .. d;
	  R := kk(monoid [p_start .. p_d, MonomialSize=>16]);
	  markovRingList#(d,kk,toString p) = R;
	  H := new HashTable from apply(#vlist, i -> vlist#i => R_i);
	  R.markovVariables = H;
	  markovRingList#(d,kk,toString p).markov = d;);
     markovRingList#(d,kk,toString p))

 ----------------
 -- marginMap ---
 ----------------
 -- Return the ring map F : R --> R such that
 --   F p_(u1,u2,..., +, ,un) = p_(u1,u2,..., 1, ,un)
 -- and
 --   F p_(u1,u2,..., j, ,un) = p_(u1,u2,..., j, ,un), for j >= 2.

marginMap = method()
marginMap(ZZ,Ring) := RingMap => (v,R) -> (
     -- R should be a Markov ring
     v = v-1;
     d := R.markov;
     -- use R; -- Dan suggested to delete this line
     p := i -> R.markovVariables#i;
     F := toList apply(((#d):1) .. d, i -> (
	       if i#v > 1 then p i
	       else (
		    i0 := drop(i,1);
		    p i - sum(apply(toList(2..d#v), j -> (
			      newi := join(take(i,v), {j}, take(i,v-#d+1));
			      p newi))))));
     map(R,R,F))

----------------
-- hiddenMap ---
----------------

hiddenMap = method()
hiddenMap(ZZ,Ring) := RingMap => (v,A) -> (
     -- creates a ring map inclusion F : S --> A.
     v = v-1;
     -- R := ring presentation A;
     p := i -> A.markovVariables#i;
     d := A.markov;
     e := drop(d, {v,v});
     S := markovRing (e);
     dv := d#v;
     -- use A; -- Dan suggested to delete this line
     F := toList apply(((#e):1) .. e, i -> (
	       sum(apply(toList(1..dv), j -> (
			      newi := join(take(i,v), {j}, take(i,v-#d+1));
			      p newi)))));
     map(A,S,F))

-------------------
-- Markov ideals --
-------------------

-- returns the position in list h of  x
pos = (h, x) -> position(h, i->i===x)
-- the following function retrieves the position of the vertices 
-- in the graph G for all vertices contained in the list S
-- vertices G does not return a sorted list of the vertices 
getPositionOfVertices := (G,S) -> apply(S, w -> pos(sort vertices G, w))

--------------------
-- markovMatrices --
--------------------

markovMatrices = method()
markovMatrices(Ring,Digraph,List) := (R,G,Stmts) -> (
     -- R should be a markovRing, G a digraph 
     -- and Stmts is a list of
     -- independence statements
     d := R.markov;
     flatten apply(Stmts, stmt -> (
     	       Avals := possibleValues(d,getPositionOfVertices(G,stmt#0)); 
     	       Bvals := possibleValues(d,getPositionOfVertices(G,stmt#1)); 
     	       Cvals := possibleValues(d,getPositionOfVertices(G,stmt#2)); 
     	       apply(Cvals, c -> (
                  matrix apply(Avals, 
		       a -> apply(Bvals, b -> (
				 e := toSequence(toList a + toList b + toList c);
		      		 prob(R,e))))))))
     )

--------------------
-- markovIdeal    --
--------------------

markovIdeal = method()
markovIdeal(Ring,Digraph,List) := (R,G,Stmts) -> (
     -- R should be a markovRing, G a digraph
     -- and Stmts is a list of independent statements
     -- markovIdeal computes the ideal associated to the 
     -- list of independent statements Stmts
     M := markovMatrices(R,G,Stmts);
     sum apply(M, m -> minors(2,m))
     )

--------------------------------------------------------
-- Constructing the ideal of an independence relation --
--------------------------------------------------------

-- NOTE: ALL THE FUNCTIONS BELOW ARE DECLARED GLOBAL INSTEAD OF LOCAL
-- FOR THE REASON THAT LOCAL DEFINITIONS WOULD INEXPLICABLY 
-- CREATE ERRORS.
     
-- cartesian ({d_1,...,d_n}) returns the cartesian product 
-- of {0,...,d_1-1} x ... x {0,...,d_n-1}
cartesian = (L) -> (
     if #L == 1 then 
	return toList apply (L#0, e -> 1:e);
     L0 := L#0;
     Lrest := drop (L,1);
     C := cartesian Lrest;
     flatten apply (L0, s -> apply (C, c -> prepend (s,c))))

-- possibleValues ((d_1,...,d_n),A) returns the cartesian product 
-- of all d_i's such that the vertex i is a member of the list A
-- it assumes that the list A is a list of integers.
possibleValues = (d,A) ->
     cartesian (toList apply(0..#d-1, i -> 
	       if member(i,A) 
	       then toList(1..d#i) 
	       else {0}))
     
-- prob((d_1,...,d_n),(s_1,dots,s_n))
-- this function assumes that R is a markovRing
prob = (R,s) -> (
     d := R.markov;
     p := i -> R.markovVariables#i;
     L := cartesian toList apply (#d, i -> 
	   if s#i === 0 
	   then toList(1..d#i) 
	   else {s#i});
     sum apply (L, v -> p v))

-----------------------------------------
-- Gaussian directed acyclic graphs    --
-----------------------------------------

---------------------
-- gaussianRing    --
---------------------

-- gaussianRingList still not fully implemented
-- gaussianRingList := new MutableHashTable;

gaussianRing = method(Options=>{Coefficients=>QQ, VariableName=>{getSymbol "s",getSymbol "l",getSymbol "p"}})
gaussianRing ZZ :=  Ring => opts -> (n) -> (
     -- s_{1,2} is the (1,2) entry in the covariance matrix.
     -- this assumes r.v.'s are labeled by integers.
     s := if instance(opts.VariableName,Symbol) then opts.VariableName else opts.VariableName#0;
     kk := opts.Coefficients;
     w := flatten toList apply(1..n, i -> toList apply(i..n, j -> (i,j)));
     v := apply (w, ij -> s_ij);
     R := kk(monoid [v, MonomialSize=>16]);
     R#gaussianRing = n;
     H := new HashTable from apply(#w, i -> w#i => R_i); 
     R.gaussianVariables = H;
     R
     )
     
-- we want to be able to do s_{a,b} 
gaussianRing Digraph :=  Ring => opts -> (G) -> (
     -- Input is a Digraph G, 
     -- we read off the list of labels from the vertices.
     -- This is done to avoid any ordering confusion. 
     s := if instance(opts.VariableName,Symbol) then opts.VariableName else opts.VariableName#0;
     kk := opts.Coefficients;
     vv := sort vertices G; 
     w := delete(null, flatten apply(vv, i -> apply(vv, j -> if pos(vv,i)>pos(vv,j) then null else (i,j))));
     v := apply (w, ij -> s_ij);
     R := kk(monoid [v, MonomialSize=>16]);
     R#gaussianRing = #vv;
     H := new HashTable from apply(#w, i -> w#i => R_i); 
     R.gaussianVariables = H;
     R
     )

------------------------
--- covarianceMatrix ---
------------------------

covarianceMatrix = method()
covarianceMatrix(Ring) := Matrix => (R) -> (
       n := R#gaussianRing; 
       genericSymmetricMatrix(R,n))
covarianceMatrix(Ring,Digraph) := Matrix => (R,g) -> covarianceMatrix R

----------------------
--- gaussianMinors ---
----------------------

gaussianMinors = method()
gaussianMinors(Digraph,Matrix,List) :=  Ideal => (G,M,Stmt) -> (
     -- M should be an n by n symmetric matrix, Stmts mentions variables 1..n 
     -- the list Stmt is one statement {A,B,C}.
     -- this function is NOT exported; it is called from gaussianIdeal.
     -- This function does not work if called directly but it works within gaussianIdeal!!!
     rows := join(getPositionOfVertices(G,Stmt#0), getPositionOfVertices(G,Stmt#2)); 
     cols := join(getPositionOfVertices(G,Stmt#1), getPositionOfVertices(G,Stmt#2));  
     M1 := submatrix(M,rows,cols);
     minors(#Stmt#2+1,M1)     
     )
-- ///EXAMPLE:
-- G = digraph {{a,{b,c}}, {b,{c,d}}, {c,{}}, {d,{}}}
-- R = gaussianRing G
-- describe R 
-- M = covarianceMatrix R;
-- peek M
-- submatrix(M,{0},{1})
-- Stmts = pairMarkov G
-- D=Stmts_0
-- gaussianMinors(G,M,D)
-- ///

---------------------
--- gaussianIdeal ---
---------------------

gaussianIdeal = method()
gaussianIdeal(Ring, Digraph, List) := Ideal =>  (R,G,Stmts) -> (
     -- for each statement, we take a set of minors
     -- Stmts = global markov statements of G
     -- R = gaussianRing of G
     --NOTE we force the user to give us the digraph G due to flexibility in labeling!!
     if not R#?gaussianRing then error "expected a ring created with gaussianRing";
     M := covarianceMatrix R;
     sum apply(Stmts, D -> gaussianMinors(G,M,D))     
     )

--in case the global Stmts are not computed already 
gaussianIdeal(Ring,Digraph) := Ideal =>  (R,G) -> gaussianIdeal(R,G,globalMarkov G)

----------------------
--- gaussianMatrix ---
----------------------

--in case user just wants to see the matrix instead of the minors.
-- this function is not imported, it is called from gaussianMatrices
gaussianMatrix = method()
gaussianMatrix(Ring,Digraph,List) := List =>  (R,G,s) -> (
     -- the list s is a statement of the form {A,B,C}.
     M := covarianceMatrix R;
     rows := join(getPositionOfVertices(G,s#0), getPositionOfVertices(G,s#2));  
     cols := join(getPositionOfVertices(G,s#1), getPositionOfVertices(G,s#2));  
     submatrix(M,rows,cols)
     )

------------------------
--- gaussianMatrices ---
------------------------

gaussianMatrices = method()
gaussianMatrices(Ring,Digraph,List) := List =>  (R,G,S) -> (
     apply(S, s -> gaussianMatrix(R,G,s))
     )

gaussianMatrices(Ring,Digraph) := List =>  (R,G) -> gaussianMatrices(R,G,globalMarkov G)

-----------------
--- trekIdeal ---
-----------------

--for a Digraph, the method is faster--so we just need to overload it for a DAG. 
trekIdeal = method()
trekIdeal(Ring, Digraph) := Ideal => (R,G) -> (
     vv := sort vertices G;
     n := #vv;
     v := (topSort G)#map;
     v = hashTable apply(keys v, i->v#i=>i);
     v = apply(n,i->v#(i+1));

     P := toList apply(v, i -> toList parents(G,i));
     nx := # gens R;
     ny := max(P/(p->#p));
     x := local x;
     y := local y;
     S := (coefficientRing R)[x_0 .. x_(nx-1),y_0 .. y_(ny-1)];
     newvars := apply(ny, i -> y_i);
     L := keys R.gaussianVariables;
     s := hashTable apply(nx,i->L#i=>x_i);
     sp := (i,j) -> if pos(vv,i) > pos(vv,j) then s#(j,i) else s#(i,j);
     
     I := trim ideal(0_S);
     for i from 1 to n-1 do (
     	  J := ideal apply(i, j -> sp(v#j,v#i) - sum apply(#P#i, k ->y_k * sp(v#j,P#i#k)));
     	  I = eliminate(newvars, I + J););
     F := map(R,S,apply(nx,i->x_i=>R.gaussianVariables#(L_i))|apply(newvars,i->i=>0));
     F(I))


------------------------------
-- Gaussian mixed graphs    --
------------------------------

-------------------------
-- INTERNAL FUNCTIONS --
-------------------------

-- takes a list A, and a sublist B of A, and converts the membership sequence of 0's and 1's of elements of B in A to binary
setToBinary = (A,B) -> sum(toList apply(0..#A-1, i->2^i*(if (set B)#?(A#i) then 1 else 0)))

-- returns all subsets of B which contain A
subsetsBetween = (A,B) -> apply(subsets ((set B) - A), i->toList (i+set A))

------------------------------
-- RINGS, MATRICES AND MAPS --
------------------------------

--------------------
--- gaussianRing ---
--------------------

gaussianRing MixedGraph := Ring => opts -> (g) -> (
     G := graph collateVertices g;
     dd := graph G#Digraph;
     bb := graph G#Bigraph;
     vv := sort vertices g;
     s := opts.VariableName#0;
     l := opts.VariableName#1;
     p := opts.VariableName#2;
     kk := opts.Coefficients;
     sL := delete(null, flatten apply(vv, x-> apply(vv, y->if pos(vv,x)>pos(vv,y) then null else s_(x,y))));
     lL := delete(null, flatten apply(vv, x-> apply(toList dd#x, y->l_(x,y))));	 
     pL := join(apply(vv, i->p_(i,i)),delete(null, flatten apply(vv, x-> apply(toList bb#x, y->if pos(vv,x)>pos(vv,y) then null else p_(x,y)))));
     m := #lL+#pL;
     R := kk(monoid [lL,pL,sL,MonomialOrder => Eliminate m, MonomialSize=>16]);
     R#gaussianRing = {#vv,s,l,p};
     R)

------------------------
--- covarianceMatrix ---
------------------------

covarianceMatrix (Ring,MixedGraph) := (R,g) -> (
     vv := sort vertices g;
     n := R#gaussianRing#0;
     s := value R#gaussianRing#1;
     SM := mutableMatrix(R,n,n);
     scan(vv,i->scan(vv, j->SM_(pos(vv,i),pos(vv,j))=if pos(vv,i)<pos(vv,j) then s_(i,j) else s_(j,i)));
     matrix SM) 

---------------------------
--- directedEdgesMatrix ---
---------------------------

directedEdgesMatrix = method()
directedEdgesMatrix (Ring,MixedGraph) := Matrix =>  (R,g) -> (
     G := graph collateVertices g;
     dd := graph G#Digraph;
     vv := sort vertices g;
     n := R#gaussianRing#0;
     l := value R#gaussianRing#2;
     LM := mutableMatrix(R,n,n);
     scan(vv,i->scan(toList dd#i, j->LM_(pos(vv,i),pos(vv,j))=l_(i,j)));
     matrix LM) 

-----------------------------
--- bidirectedEdgesMatrix ---
-----------------------------

bidirectedEdgesMatrix = method()
bidirectedEdgesMatrix (Ring,MixedGraph) := Matrix =>  (R,g) -> (
     G := graph collateVertices g;
     bb := graph G#Bigraph;
     vv := sort vertices g;
     n := R#gaussianRing#0;
     p := value R#gaussianRing#3;
     PM := mutableMatrix(R,n,n);
     scan(vv,i->PM_(pos(vv,i),pos(vv,i))=p_(i,i));
     scan(vv,i->scan(toList bb#i, j->PM_(pos(vv,i),pos(vv,j))=if pos(vv,i)<pos(vv,j) then p_(i,j) else p_(j,i)));
     matrix PM) 
 
-------------------------------
--- gaussianParametrization ---
-------------------------------
 
gaussianParametrization = method(Options=>{SimpleTreks=>false})
gaussianParametrization (Ring,MixedGraph) := Matrix => opts -> (R,g) -> (
     S := covarianceMatrix(R,g);    
     W := bidirectedEdgesMatrix(R,g);     
     L := directedEdgesMatrix(R,g);
     Li := inverse(1-matrix(L));
     M := transpose(Li)*matrix(W)*Li;
     if opts.SimpleTreks then (
       n := R#gaussianRing#0;
       P := matrix {apply(n,i->W_(i,i)-M_(i,i)+1)};
       Q := apply(n,i->W_(i,i)=>P_(0,i));
       scan(n,i->P=sub(P,Q));
       sub(M,apply(n,i->W_(i,i)=>P_(0,i))))
     else
       M)

---------------------
-- IDENTIFIABILITY --
---------------------

-------------------------
-- identifyParameters ---
-------------------------

identifyParameters = method()
identifyParameters (Ring,MixedGraph) := HashTable => (R,g) -> (
     J := ideal unique flatten entries (covarianceMatrix(R,g)-gaussianParametrization(R,g));
     G := graph g;
     m := #edges(G#Digraph)+#edges(G#Bigraph)+#vertices(g);
     plvars := toList apply(0..m-1,i->(flatten entries vars R)#i);
     new HashTable from apply(plvars,t->{t,eliminate(delete(t,plvars),J)}))

---------------------
-- trekSeparation  --
---------------------

trekSeparation = method()
trekSeparation MixedGraph := List => (g) -> (
    G := graph collateVertices g;
    dd := graph G#Digraph;
    bb := graph G#Bigraph; 
    vv := sort vertices g;

    -- Construct canonical double DAG cdG associated to mixed graph G
    cdG:= digraph join(
      apply(vv,i->{(1,i),join(
        apply(toList parents(G#Digraph,i),j->(1,j)),
        {(2,i)}, apply(toList bb#i,j->(2,j)))}),
      apply(vv,i->{(2,i),apply(toList dd#i,j->(2,j))}));
    aVertices := apply(vv, i->(1,i));
    bVertices := apply(vv, i->(2,i));
    allVertices := aVertices|bVertices;
    
    statements := {};
    cdC0 := new MutableHashTable;
    cdC0#cache = new CacheTable from {};
    cdC0#graph = new MutableHashTable from apply(allVertices,i->{i,cdG#graph#i});
    cdC := new Digraph from cdC0;
    for CA in (subsets aVertices) do (
      for CB in (subsets bVertices) do (
	CAbin := setToBinary(aVertices,CA);
	CBbin := setToBinary(bVertices,CB);
	if CAbin <= CBbin then (
          C := CA|CB;
	  scan(allVertices,i->cdC#graph#i=cdG#graph#i);
          scan(C, i->scan(allVertices, j->(
	    cdC#graph#i=cdC#graph#i-{j};
	    cdC#graph#j=cdC#graph#j-{i};)));
	  Alist := delete({},subsetsBetween(CA,aVertices));
          while #Alist > 0 do (
	    minA := first Alist;
	    pC := reachable(cdC,set minA);
	    A := toList ((pC*(set aVertices)) + set CA);
	    Alist = Alist - (set subsetsBetween(minA,A));
	    B := toList ((set bVertices) - pC);
	    
	    -- remove redundant statements
	    if #CA+#CB < min{#A,#B} then (
	    if not ((CAbin==CBbin) and (setToBinary(aVertices,A) > setToBinary(bVertices,B))) then (
	      nS := {apply(A,i->i#1),apply(B,i->i#1),apply(CA,i->i#1),apply(CB,i->i#1)};
	      appendnS := true;
	      statements = select(statements, cS->
		if cS#0===nS#0 and cS#1===nS#1 then (
		  if isSubset(cS#2,nS#2) and isSubset(cS#3,nS#3) then 
		    (appendnS = false; true)
		  else if isSubset(nS#2,cS#2) and isSubset(nS#3,cS#3) then 
		    false
		  else
		    true)
		else if cS#2===nS#2 and cS#3===nS#3 then (
		  if isSubset(cS#0,nS#0) and isSubset(cS#1,nS#1) then 
		    false
		  else if isSubset(nS#0,cS#0) and isSubset(nS#1,cS#1) then 
		    (appendnS = false; true)
		  else
		    true)		  
		else true);
              if appendnS then statements = append(statements, nS);););););););
    statements)

-----------------
--  trekIdeal  --
-----------------

trekIdeal (Ring,MixedGraph,List) := Ideal => (R,g,Stmts) -> (
     vv := sort vertices g;
     SM := covarianceMatrix(R,g);	
     sum apply(Stmts,s->minors(#s#2+#s#3+1, submatrix(SM,apply(s#0,x->pos(vv,x)),apply(s#1,x->pos(vv,x))))))

trekIdeal (Ring,MixedGraph) := Ideal => (R,g) -> trekIdeal(R,g,trekSeparation g)



----------------------
-- Parameterization --
----------------------

---- We need this for both directed and undirected graphs. 

----  parameterizations and for toric varieties the corresponding matrix. 
----  In the case of toric varieties the matrix is easy.  Here is the code, 
----  commented out to be used later when we are ready. 
---- 
----  toAMatrix = method()
----  toAMatrix List := Matrix => (M) -> (
----      if any(M,isMonomial)
----         then error "this parameterization does not correspond to a toric ideal." 
----         else (
----              Mexp := apply(M, exponents);
----              transpose matrix apply(Mexp, flatten)))
----
---- isMonomial = method()
---- isMonomial RingElement := Boolean => (m) -> (
----      termList := terms m;
----      if #termList == 1 then true else false)

---- isMonomial works well as long as m is actually a polynomial or monomial and not 
---- an element of ZZ, QQ, RR, etc.

--------------------
-- Documentation  --
--------------------

beginDocumentation()

doc ///
  Key
    GraphicalModels
  Headline
    A package for discrete and Gaussian statistical graphical models 
  Description
    Text
      This package extends Markov.m2. It is used to construct ideals corresponding to discrete graphical models,
      as described in several places, including the paper: Luis David Garcia, Michael Stillman and Bernd Sturmfels,
      "The algebraic geometry of Bayesian networks", J. Symbolic Comput., 39(3-4):331–355, 2005.
  
      The package also constructs ideals of Gaussian Bayesian networks and Gaussian graphical models 
      (graphs containing both directed and bidirected edges), as described in the papers:
      Seth Sullivant, "Algebraic geometry of Gaussian Bayesian networks", Adv. in Appl. Math. 40 (2008), no. 4, 482--513;
      Seth Sullivant, Kelli Talaska and Jan Draisma, "Trek separation for Gaussian graphical models", 
      Annals of Statistics 38 no.3 (2010) 1665--1685. 
      
      Further, the package contains procedures to solve the identifiability problem for 
      Gaussian graphical models as described in the paper: 
      Luis D. Garcia-Puente, Sarah Spielvogel and Seth Sullivant, "Identifying causal effects with computer algebra", 
      Proceedings of the $26^{th}$ Conference of Uncertainty in Artificial Intelligence.
      
      Here is a typical use of this package.  We create the ideal in 16 variables whose zero set 
      represents the probability distributions on four binary random variables which satisfy the
      conditional independence statements coming from the "diamond" graph d --> c,b --> a.
    Example
       G = digraph  {{a,{}},{b,{a}},{c,{a}},{d,{b,c}}}
       R = markovRing (2,2,2,2)
       S = globalMarkov G 
       I = markovIdeal(R,G,S)
       netList pack(2,I_*)
    Text
      Sometimes an ideal can be simplified by changing variables.  Very often, 
      by using @TO marginMap@
      such ideals can be transformed to binomial ideals.  This is the case here.
    Example
       F = marginMap(1,R)
       I = F I
       netList pack(2,I_*)
    Text
      This ideal has 5 primary components.  The first component is the one that has statistical significance.
      It is the defining ideal of the variety parameterized by the 
      the factorization of the probability distributions 
      according to the graph G. The remaining components lie on the boundary of the simplex
      and are still poorly understood.
    Example  
      netList primaryDecomposition I 
    Text
      The following example illustrates the caveat below.
    Example
       H = digraph {{d,{b,a}},{c,{}},{b,{c}},{a,{c}}}
       T = globalMarkov H  
       J = markovIdeal(R,H,T);
       netList pack(2,J_*)
       F = marginMap(3,R);
       J = F J;
       netList pack(2,J_*)
    Text
      Note that the graph $H$ is isomorphic to $G$, we have just relabeled the vertices. 
      Observe that the vertices of $H$ are stored
      in lexicographic order. Also note that the this graph isomorphism lifts to an isomorphism of ideals.     
  Caveat
     This package requires Graphs.m2, as a consequence it can do computations with graphs
     whose vertices are not necessarily labeled by integers. This could potentially create some confusion about what does
     $p_{i_1i_2\cdots i_n}$ mean. The package orders the vertices lexicographically, so 
     $p_{i_1i_2\cdots i_n} = p(X_1 = i_1, X_2 = i_2, \dots, X_n = i_n)$ where the labels
     $X_1,X_2,\dots,X_n$ have been ordered lexicographically. Therefore, the user is encouraged
     to label the vertices in a consistent way (all numbers, or all letters, etc).
///;

--------------------------------
-- Documentation pairMarkov ----
--------------------------------

doc ///
  Key
    pairMarkov
    (pairMarkov,Digraph)
  Headline
    Pairwise Markov statements for a directed graph.
  Usage
    pairMarkov G
  Inputs
    G:Digraph 
  Outputs
    L:List
      whose entries are triples {A,B,C} representing pairwise Markov  conditional independence statements of the form
      ''A is independent of B given C'' that hold for G.
  Description
    Text
      Given a directed graph G, pairwise Markov statements are statements of the form \{v,w,nondescendents(G,v)-w\} 
      for each vertex v of G. In other words, for every vertex v of G and all nondescendents w of v, 
      v is independent of w given all other nondescendents. 
      
      For example, for the digraph D on $4$ vertices with edges a->b, a->c, b->c, and b->d, 
      we get the following pairwise Markov statements:
    Example
      D = digraph {{a,{b,c}}, {b,{c,d}}, {c,{}}, {d,{}}}
      L = pairMarkov D
    Text
      Note that the method displays only non-redundant statements.
  SeeAlso
    localMarkov 
    globalMarkov
///

--------------------------------
-- Documentation localMarkov ---
--------------------------------

doc ///
  Key
    localMarkov
    (localMarkov,Digraph)
  Headline
    Local Markov statements for a directed graph.
  Usage
    localMarkov G
  Inputs
    G:Digraph 
  Outputs
    L:List
      whose entries are triples {A,B,C} representing local Markov  conditional independence statements of the form
      ''A is independent of B given C'' that hold for G.
  Description
    Text
      Given a directed graph G, local Markov statements are of the form
      \{$v$, nondescendents($v$) - parents($v$), parents($v$)\} .
      That is, 
      every vertex $v$ of G is independent of its nondescendents (excluding parents) given the parents. 
      
      For example, for the digraph D on 4 vertices with edges a->b, a->c, b->c, and b->d, 
      we get the following local Markov statements:
    Example
      D = digraph {{a,{b,c}}, {b,{c,d}}, {c,{}}, {d,{}}}
      L = localMarkov D
    Text
      Note that the method displays only non-redundant statements.
  SeeAlso
    pairMarkov
    globalMarkov
///

--------------------------------
-- Documentation globalMarkov --
--------------------------------

doc ///
  Key
    globalMarkov
    (globalMarkov,Digraph)
  Headline
    Global Markov statements for a directed graph.
  Usage
    globalMarkov G
  Inputs
    G:Digraph 
  Outputs
    L:List
      whose entries are triples {A,B,C} representing global Markov  conditional independence statements of the form
      ''A is independent of B given C'' that hold for G.
  Description
    Text
      Given a directed graph G, global Markov states that      
      A is independent of B given C for every triple of sets of vertices A, B, and C, 
      such that A and B are $d$-separated by C (in the graph G).\break
       
      The global independent statements are computed using the Bayes ball algorithm,
      as described in the paper "Bayes-Ball: The Rational Pastime (for Determining Irrelevance and Requisite Information
      in Belief Networks and Influence Diagrams)" by Ross D. Shachter.
      
      For example, for the digraph D on 4 vertices with edges a->b, a->c, b->c, and b->d, 
      we get the following global Markov statements:
    Example
      D = digraph {{a,{b,c}}, {b,{c,d}}, {c,{}}, {d,{}}} 
      L = globalMarkov D 
    Text
      Note that the method displays only non-redundant statements.
  Caveat
    -- If G is large, this should maybe be rewritten so that
    -- one huge list of subsets is not made all at once
  SeeAlso
    localMarkov
    pairMarkov
///

--------------------------------
-- Documentation marginMap    --
--------------------------------

doc ///
  Key
    marginMap
    (marginMap,ZZ,Ring)
  Headline
    Generates a linear map on joint probabilities for discrete variables that replaces marginals for indeterminates.
  Usage
    phi = marginMap(i,R)
  Inputs
    i:ZZ
      the index of the variable to marginalize
    R:Ring
      a Markov ring
  Outputs
    phi:RingMap
  Description
    Text
      Returns the ring map F : R -> R such that
      $ F p_{u_1,u_2,..., +,...,u_n} = p_{u_1,u_2,...,1,...,u_n} $ and
      $ F p_{u1,u2,..., j,...,un} = p_{u1,u2,..., j,...,un} $, for $ j\geq 2 $.
    Example
      marginMap(1,markovRing(3,2)) 
    Text
      This linear transformation simplifies ideals and/or polynomials involving 
      $ p_{u_1,u_2,..., +,...,u_n} $. In some cases, the resulting ideals are toric 
      ideals as the example at the beginning of the documentation. For more details 
      see the paper "Algebraic Geometry of Bayesian Networks" by Garcia, Stillman, and
      Sturmfels. 
  SeeAlso
    hiddenMap
///

--------------------------------
-- Documentation hiddenMap    --
--------------------------------`

doc ///
  Key
    hiddenMap
    (hiddenMap,ZZ,Ring)
  Headline
    Creates a linear map from a ring of probability distributions among observed discrete random variables into a ring of probability distributions among observed variables and one hidden variable specified by the integer input.
  Usage
    phi = hiddenMap(i,R)
  Inputs
    i:ZZ
      the index corresponding to the hidden random variable
    R:Ring
      a Markov ring
  Outputs
    phi:RingMap
  Description
    Text
      A linear map from a ring of probability distributions among observed discrete random variables into 
      a ring of probability distributions among observed variables and one hidden variable specified by the integer input. 
      This method is used to work with Bayesian networks with hidden variables.
      For more details see the paper "Algebraic Geometry of Bayesian Networks"
      by Garcia, Stillman, and Sturmfels.
    Example
      hiddenMap(1,markovRing(2,3,2)) 
  SeeAlso
    marginMap
///

--------------------------------
-- Documentation markovRing   --
--------------------------------

doc ///
  Key
    markovRing
    (markovRing,Sequence)
    [markovRing, Coefficients]
    [markovRing, VariableName]
  Headline
    Ring of probability distributions on several discrete random variables.
  Usage
    markovRing(d) or markovRing(d,Coefficients=>Ring) or markovRing(d,Variable=>Symbol)
  Inputs
    d:Sequence
      with positive integer entries (d1,...,dr)
  Outputs
    R:Ring
      A polynomial ring with d1*d2*...*dr variables $p_{i_1,...,i_r}$,
      with each $i_j$ satisfying $1\leq i_j \leq d_j$.
  Consequences
    Item
      Information about this sequence of integers is placed into the ring, and is used 
      by other functions in this package.  Also, at most one ring for each such sequence
      is created: the results are cached.
  Description
    Text 
      The sequence $d$ represents the number of states each discrete random variable can take.
      For example, if there are four random variables with the following state space sizes
    Example
      d=(2,3,4,5)
    Text 
      the corresponding ring will have as variables all the possible joint 
      probability distributions for the four variables:
    Example
      R = markovRing d;
      numgens R
      R_0, R_1, R_119 --here are some of the variables in the ring
    Text
      If no coefficient choice is specified, the polynomial ring is created over the rationals. 
    Example
      coefficientRing R
    Text 
      If we prefer to have a different base field, the following command can be used:
    Example
      Rnew = markovRing (d,Coefficients=>CC); 
      coefficientRing Rnew
    Text
      We might prefer to give different names to our variables. The letter ''p'' suggests a joint probability, 
      but it might be useful to create a new ring where the variables have changed. This can easily be done
      with the following option:
    Example
      d=(1,2);
      markovRing (d,VariableName=>q);
      vars oo --here is the list of variables.
    Text
      -- The LIST OF FNS USING THIS FUNCTION SHOULD BE INSERTED AS WELL.
  SeeAlso
///

------------------------------------
-- Documentation Coefficients     --
------------------------------------

doc ///
  Key
    Coefficients
  Headline
    Optional input to choose the base field.
  Description
    Text
      Put {\tt Coefficients => r} for a choice of ring(field) r as an argument in 
      the function @TO markovRing@ or @TO gaussianRing@ 
  SeeAlso
    markovRing
    gaussianRing
///

------------------------------------
-- Documentation variableName     --
------------------------------------

doc ///
  Key
    VariableName
  Headline
    Optional input to choose the letter for the variable name.
  Description
    Text
      Put {\tt VariableName => s} for a choice of a symbol s as an argument in 
      the function @TO markovRing@ or @TO gaussianRing@ for digraphs, and 
      {\tt VariableName => \{s,l,w\}} in @TO gaussianRing@ for mixed graphs
  SeeAlso
    markovRing
    gaussianRing
///

------------------------------------
-- Documentation markovMatrices   --
------------------------------------

doc ///
  Key
    markovMatrices
    (markovMatrices,Ring,Digraph,List) 
  Headline
    The matrices whose minors form the ideal associated to the list of independence statements of the graph.
  Usage
    markovMatrices(R,G,S)
  Inputs
    R:Ring
      R must be a markovRing
    G:Digraph
      directed acyclic graph
    S:List 
      a list of independence statements that are true for the DAG G
  Outputs
    :List 
      whose elements are instances of Matrix. Minors of these matrices form the independence ideal for the independent statements of the Digraph.
  Description
    Text
      List of matrices encoding the independent statements of the Digraph G. The 2x2 minors of each matrix generate the ideal of 
      independence constraints of the Digraph G. the  This method 
      is used in markovIdeals. But it is exported to be able to see constraints not as 
      polynomials but as minors of matrices in this list. 
    Example
      G = digraph { {1, {2,3}}, {2, {4}}, {3, {4}} }
      S = localMarkov G
      R = markovRing (4:2)
      L = markovMatrices (R,G,S)   
  SeeAlso
    markovRing
    markovIdeal
///

------------------------------------
-- Documentation markovIdeal      --
------------------------------------

doc ///
  Key
    markovIdeal
    (markovIdeal,Ring,Digraph,List) 
  Headline
    Ideal of constraints associated to a list of independent statements among discrete random variables.
  Usage
    markovIdeal(R,G,S)
  Inputs
    R:Ring
      R must be a markovRing
    G:Digraph
      directed acyclic graph
    S:List
      (markovIdeal,Ring,Digraph,List) 
  Outputs       
    :Ideal
  Description
    Text
      This method computes the ideal of constraints associated to a list of independent statements among discrete random variables.
      These constraints are the 2x2 minors of the matrices computed by markovMatrices.
    Example
      G = digraph { {1, {2,3}}, {2, {4}}, {3, {4}} }
      S = localMarkov G
      R = markovRing (2,2,2,2)
      I = markovIdeal (R,G,S)   
  SeeAlso
    markovRing
    markovMatrices
///

------------------------------------
-- Documentation gaussianRing     --
------------------------------------

doc ///
  Key 
    gaussianRing
    (gaussianRing,ZZ)
    (gaussianRing,Digraph)
    (gaussianRing,MixedGraph)
    [gaussianRing, Coefficients]
    [gaussianRing, VariableName]
  Headline
    ring of gaussian correlations on n random variables
  Usage
    R = gaussianRing n or R = gaussianRing G or gaussianRing(n,Coefficients=>Ring) or gaussianRing(n,Variable=>Symbol)
  Inputs
    n:ZZ
      the number of random variables
    G:Digraph
      a directed acyclic graph, 
      or @ofClass MixedGraph@ with directed and bidirected edges
  Outputs
    R:Ring
      a ring with indeterminates $s_{(i,j)}$ for $1 \leq i \leq j \leq n$, and
      additionally $l_{(i,j)}, w_{(i,j)}$ for mixed graphs
  Description
    Text
      The routines  @TO gaussianIdeal@ and @TO trekIdeal@ require that the ring      
      be created by this function. 
    Example
      R = gaussianRing 5;
      gens R
      covarianceMatrix R
    Text
      For mixed graphs, there is a variable $l_{(i,j)}$ for
      each directed edge i->j, a variable $w_{(i,i)}$ for each node i, and a variable $w_{(i,j)}$ 
      for each bidirected edge i<->j.
    Example
      G = mixedGraph(digraph {{b,{c,d}},{c,{d}}},bigraph {{a,d}})
      R = gaussianRing G
      gens R
      covarianceMatrix(R,G)
      directedEdgesMatrix(R,G)
      bidirectedEdgesMatrix(R,G)

  SeeAlso
    gaussianIdeal
    covarianceMatrix
    directedEdgesMatrix
    bidirectedEdgesMatrix
    trekIdeal
///

------------------------------------
-- Documentation gaussianIdeal    --
------------------------------------

doc ///
   Key
     gaussianIdeal
     (gaussianIdeal,Ring,Digraph)
     (gaussianIdeal,Ring,Digraph,List)
   Headline
     correlation ideal of a Bayesian network of joint Gaussian variables
   Usage
     I = gaussianIdeal(R,G) or I = gaussianIdeal(R,G,S)
   Inputs
     R:Ring
       created with @TO gaussianRing@
     G:Digraph
       an acyclic directed graph
     S:List
       a list of independence statements for the graph G
   Outputs
     I:Ideal
        in R, of the relations in the correlations of the random variables implied by the independence statements 
	of the graph G, or the list of independence statements G
   Description
     Text
       The ideal corresponding to a conditional independence statement {A,B,C} (where A,B,C,
       are disjoint lists of integers in the range 1..n (n is the number of random variables)
       is the #C+1 x #C+1 minors of the submatrix of the generic symmetric matrix M = (s_{(i,j)}), whose
       rows are in A union C, and whose columns are in B union C.  In general, this ideal need not be prime.
       
       These ideals were first written down by Seth Sullivant, in "Algebraic geometry of Gaussian Bayesian networks". 
       The routines in this package involving Gaussian variables are all based on that paper.
     Example
       R = gaussianRing 5;
       G = digraph { {1,{2}}, {2,{3}}, {3,{4,5}},{4,{5}} } 
       (globalMarkov G)/print; 
       J = gaussianIdeal(R,G) 
     Text
       A list of independence statements (as for example returned by globalMarkov)
       can be provided instead of a graph:
     Example
       S=pairMarkov G 
       I = gaussianIdeal(R,G,S) 
       codim I
   SeeAlso
     globalMarkov
     localMarkov
     gaussianRing
     gaussianMatrices
     trekIdeal
///

---------------------------------------
-- Documentation gaussianMatrices    --
---------------------------------------

doc///
   Key
     gaussianMatrices
     (gaussianMatrices,Ring,Digraph,List)
     (gaussianMatrices,Ring,Digraph)
   Headline
     Matrices whose minors form the ideal corresponding to a conditional independence statements.
   Usage
     gaussianMatrices(R,G,S)
     gaussianMatrices(R,G)
   Inputs
     R:Ring
       must be a gaussianRing
     G:Digraph
       a directed acyclic graph
     S:List
       a conditional independence statement that holds for the graph G
   Outputs
     :Matrix
       whose minors belong to the ideal corresponding to a conditional independence statements.
   Description 
     Text
       This method displays a list of matrices whose minors generate the ideal of conditional independence
       statements. Its main purpose is to visualize these polynomials in a simpler way before they are expanded 
       as sums of monomials. 
     Example
       G = digraph { {1,{2}}, {2,{3}}, {3,{4,5}},{4,{5}} } ;
       S = localMarkov G;
       R = gaussianRing G; 
       gaussianMatrices(R,G,S)
       gaussianMatrices(R,G)
   SeeAlso
     gaussianRing
     gaussianIdeal
///

---------------------------------------
-- Documentation covarianceMatrix    --
---------------------------------------

doc/// 
   Key
     covarianceMatrix
     (covarianceMatrix,Ring)
     (covarianceMatrix,Ring,Digraph)
     (covarianceMatrix,Ring,MixedGraph)
   Headline
     the covariance matrix of a gaussian graphical model
   Usage
     S = covarianceMatrix R or S = covarianceMatrix(R,G)
   Inputs
     R:Ring
       which should be a gaussianRing
     G:Digraph
       a directed acyclic graph, 
       or @ofClass MixedGraph@ with directed and bidirected edges
   Outputs
     S:Matrix
       the n x n covariance matrix of symbols where n is the number of vertices in G
   Description 
     Text
       If this function is called without a graph G, it is assumed that R is the gauss ring of a directed acyclic graph.
     Example
       G = digraph {{a,{b,c}}, {b,{c,d}}, {c,{}}, {d,{}}}
       R = gaussianRing G
       S = covarianceMatrix R
     Text
       Note that the covariance matrix is symmetric in the symbols.
     Example
       G = mixedGraph(digraph {{b,{c,d}},{c,{d}}},bigraph {{a,d}})
       R = gaussianRing G
       S = covarianceMatrix(R,G)
   SeeAlso
     gaussianRing
     gaussianParametrization
     bidirectedEdgesMatrix
     directedEdgesMatrix
///

--------------------------------------------
-- Documentation bidirectedEdgesMatrix    --
--------------------------------------------

doc/// 
   Key
     bidirectedEdgesMatrix
     (bidirectedEdgesMatrix,Ring,MixedGraph)
   Headline
     the matrix corresponding to the bidirected edges of a mixed graph
   Usage
     W = bidirectedEdgesMatrix(R,G)
   Inputs
     R:Ring
       which should be a gaussianRing
     G:MixedGraph
       mixed graph with directed and bidirected edges
   Outputs
     S:Matrix
       the n x n symmetric matrix of symbols where we have $w_{(i,i)}$ for each vertex i, 
       $w_{(i,j)}$ if there is a bidirected edge between i and j, and 0 otherwise.
   Description 
     Text
       Note that this matrix is symmetric in the symbols.
     Example
       G = mixedGraph(digraph {{b,{c,d}},{c,{d}}},bigraph {{a,d}})
       R = gaussianRing G
       S = bidirectedEdgesMatrix(R,G)
   SeeAlso
     gaussianRing
     gaussianParametrization
     covarianceMatrix
     directedEdgesMatrix
///

------------------------------------------
-- Documentation directedEdgesMatrix    --
------------------------------------------

doc/// 
   Key
     directedEdgesMatrix
     (directedEdgesMatrix,Ring,MixedGraph)
   Headline
     the matrix corresponding to the directed edges of a mixed graph
   Usage
     L = directedEdgesMatrix(R,G)
   Inputs
     R:Ring
       which should be a gaussianRing
     G:MixedGraph
       mixed graph with directed and bidirected edges
   Outputs
     L:Matrix
       the n x n matrix of symbols where we have $l_{(i,j)}$ if there is a directed edge i-->j, and 0 otherwise.
   Description 
     Text
       Note that this matrix is NOT symmetric in the symbols.
     Example
       G = mixedGraph(digraph {{b,{c,d}},{c,{d}}},bigraph {{a,d}})
       R = gaussianRing G
       S = directedEdgesMatrix(R,G)
   SeeAlso
     gaussianRing
     gaussianParametrization
     covarianceMatrix
     bidirectedEdgesMatrix
///

----------------------------------------------
-- Documentation gaussianParametrization    --
----------------------------------------------

doc/// 
   Key
     gaussianParametrization
     (gaussianParametrization,Ring,MixedGraph)
     [gaussianParametrization, SimpleTreks]
   Headline
     the parametrization of the covariance matrix in terms of treks
   Usage
     M = gaussianParametrization(R,G)
   Inputs
     R:Ring
       which should be a gaussianRing
     G:MixedGraph
       mixed graph with directed and bidirected edges
   Outputs
     M:Matrix
       the parametrization of the covariance matrix in terms of treks
   Description 
     Text
       Given a mixed graph G with directed and bidirected edges, let L be the matrix corresponding to 
       the directed edges (see @TO directedEdgesMatrix@) and let W be the matrix corresponding to 
       the bidirected edges (see @TO bidirectedEdgesMatrix@). Then, the covariance matrix S 
       (see @TO covarianceMatrix@) of the random variables in the gaussian graphical model corresponding
       to the mixed graph G can be parametrized by the matrix equation $S = (I-L)^{-T}W(I-L)^{-1}$, where
       I is the identity matrix.
       
       The entry $S_{(i,j)}$ of the covariance matrix can also be written as the sum of all monomials corresponding
       to treks between vertices i and j. See @TO trekSeparation@ for the definition of a trek. The monomial corresponding
       to a trek is the product of all parameters associated to the directed and bidirected edges on the trek.
       
       The following example shows how to compute the ideal of the model using the parametrization.
     Example
       G = mixedGraph(digraph {{b,{c,d}},{c,{d}}},bigraph {{a,d}})
       R = gaussianRing G
       S = covarianceMatrix(R,G)
       L = directedEdgesMatrix(R,G)
       W = bidirectedEdgesMatrix(R,G)       
       M = gaussianParametrization(R,G)
       J = delete(0_R, flatten entries (L|W))
       eliminate(J, ideal(S-M))
     Text
       This next example shows how to use the option @TO SimpleTreks@ to compute a parametrization using simple treks 
       instead of all treks. The resulting covariance matrix has diagonal entries equal to 1.
     Example
       G = mixedGraph(digraph {{b,{c,d}},{c,{d}}},bigraph {{a,d}})
       R = gaussianRing G
       M = gaussianParametrization(R,G,SimpleTreks=>true)
   SeeAlso
     covarianceMatrix
     directedEdgesMatrix
     bidirectedEdgesMatrix
     trekSeparation
///

----------------------------------
-- Documentation SimpleTreks    --
----------------------------------

doc ///
  Key
    SimpleTreks
  Headline
    optional input to choose the type of parametrization in @TO gaussianParametrization@
  Description
    Text
      Put {\tt SimpleTreks => true} as an argument in the function @TO gaussianParametrization@ to compute 
      a parametrization of the covariance matrix S=(s_{(i,j)}) where s_{(i,j)} is the sum of monomials corresponding
      to simple treks between vertices i and j. Here, a simple trek is a trek (P_L,P_R) where the paths P_L and P_R 
      do not have any common vertices except perhaps at their source. See @TO trekSeparation@ for the definition of a trek.
      
      If the option {\tt SimpleTreks => false} is used, then the sum is over 
      all treks, and not just simple treks. 
  SeeAlso
    gaussianParametrization
///

-----------------------------------------
-- Documentation identifyParameters    --
-----------------------------------------

doc/// 
   Key
     identifyParameters
     (identifyParameters,Ring,MixedGraph)
   Headline
     solving the identifiability problem: expressing each parameter in terms of covariances 
   Usage
     H = identifyParameters(R,G)
   Inputs
     R:Ring
       which should be a gaussianRing
     G:MixedGraph
       mixed graph with directed and bidirected edges
   Outputs
     H:HashTable
       where H#p is the ideal of equations involving only the parameter p and the covariances s_{(i,j)}
   Description 
     Text
       If H#p contains a linear equation a*p+b where a is always nonzero, then p is identifiable.
       
       If H#p contains a linear equation a*p+b where a may be zero, then p is generically identifiable.
       
       If H#p contains a polynomial in p of degree d, then p is algebraically d-identifiable.
       
       If H#p does not contain any polynomial in p, then p is not generically identifiable.
     Example
       G = mixedGraph(digraph {{b,{c,d}},{c,{d}}},bigraph {{a,d}})
       R = gaussianRing G
       H = identifyParameters(R,G)
   SeeAlso
     gaussianRing
///

--------------------------------
-- Documentation trekIdeal    --
--------------------------------

doc/// 
   Key
     trekIdeal
     (trekIdeal,Ring,Digraph)
     (trekIdeal,Ring,MixedGraph)
     (trekIdeal,Ring,MixedGraph,List)
   Headline
     the edge elimination ideal of a directed graph, or the trek separation ideal of a mixed graph
   Usage
     I = trekIdeal(R,G) or I = trekIdeal(R,G,S)
   Inputs
     R:Ring
       which should be a gaussianRing
     G:Digraph
       a directed acyclic graph, 
       or @ofClass MixedGraph@ with directed and bidirected edges
     S:List
       trek separation statements that hold for a mixed graph G
   Outputs
     I:Ideal
       the ideal attained by eliminating edge parameters for a directed graph G,
       or the trek separation ideal implied by statements S for a mixed graph G.
       
   Description 
     Text
       For directed acyclic graphs, the covariance $s_{(i,j)}$ can be expressed as the sum of all monomials
       corresponding to treks between vertices i and j. See @TO gaussianParametrization@ for more information on treks. The trek
       ideal for a directed acyclic graph consists of polynomial relations between the covariances after 
       eliminating parameters corresponding to the directed edges.
     Example
       G = digraph {{a,{b,c}}, {b,{c,d}}, {c,{}}, {d,{}}}
       R = gaussianRing G
       trekIdeal(R,G)
     Text  
       For mixed graphs, the ideal corresponding to a trek separation statement {A,B,CA,CB} (where A,B,CA,CB
       are disjoint lists of vertices of G) is generated by the r+1 x r+1 minors of the submatrix of the covariance matrix M = (s_{(i,j)}), whose
       rows are in A, and whose columns are in B, and where r = #CA+#CB.
       
       These ideals are described in more detail by Sullivant, 
       Talaska and Draisma in "Trek Separation for Gaussian Graphical Models". 
     Example
       G = mixedGraph(digraph {{b,{c,d}},{c,{d}}},bigraph {{a,d}})
       R = gaussianRing G
       T = trekIdeal(R,G)
       ideal gens gb T
   SeeAlso
     trekSeparation
///

-------------------------------------
-- Documentation trekSeparation    --
-------------------------------------

doc/// 
   Key
     trekSeparation
     (trekSeparation,MixedGraph)
   Headline
     the trek separation statements of a mixed graph 
   Usage
     trek = trekSeparation(G)
   Inputs
     G:MixedGraph
       mixed graph with directed and bidirected edges
   Outputs
     trek:List
        of lists \{A,B,CA,CB\}, where (CA,CB) trek separates A from B
   Description 
     Text
       A trek between vertices i and j in a mixed graph G with directed and bidirected edges is a triple 
       (P_L,P_R) where P_L is a directed path of directed edges with sink i and source k, P_R is a directed path
       of directed edges with sink j and source l, and either k=l or there is a bidirected edge between k and l.
       Let A,B,CA,CB be subsets of vertices of G. 
       
       We say that (CA,CB) trek-separates A from B in G if for every trek 
       (P_L,P_R) from a vertex in A to a vertex in B, either P_L contains a vertex in CA or P_R contains a vertex in CB.
       
       The function @TO trekSeparation@ returns a list of trek separation statements \{A,B,CA,CB\}\,where 
       #CA + #CB < min(#A, #B). Each statement is maximal in the ordering where \{A1,B1,CA,CB\}\,<\,\{A2,B2,CA,CB\}\,if A1 is a 
       subset of A2 and B1 is a subset of B2. Each statement is also unique up to symmetry, since \{B,A,CB,CA\}\,is a 
       trek separation statement if and only if \{A,B,CA,CB\}.
     Example
       G = mixedGraph(digraph {{b,{c,d}},{c,{d}}},bigraph {{a,d}})
       R = gaussianRing G
       S = trekSeparation G
       trekIdeal(R,G,S)
   SeeAlso
     trekIdeal
///

--------------------------
---- TESTS GO HERE! ------
--------------------------

--------------------------
---- TEST pairMarkov  ----
--------------------------

TEST ///
G = digraph {{a,{b,c}}, {b,{c,d}}, {c,{}}, {d,{}}}
S = pairMarkov G
S = apply(S,s -> {sort s#0, sort s#1, sort s#2}) 
L = {{{c}, {d}, {a, b}}, {{a}, {d}, {b, c}}}
assert(S === L)
///

--------------------------
---- TEST localMarkov  ---
--------------------------

TEST ///
-- G = digraph { {1, {2,3}}, {2, {4}}, {3, {4}} }
-- S = localMarkov G
-- L = {{{2}, {3}, {1}}, {{1}, {4}, {2, 3}}}
-- assert(S === L)
G = digraph { {1,{2,3,4}}, {5,{2,3,4}} }
S = localMarkov G
S = apply(S,s -> {sort s#0, sort s#1, sort s#2}) 
L = {{{2}, {3, 4}, {1, 5}}, {{2, 3}, {4}, {1, 5}}, {{2, 4}, {3}, {1, 5}}, {{1}, {5}, {}}} 
assert(S === L)
///

--------------------------
--- TEST globalMarkov  ---
--------------------------

TEST ///
G = digraph { {2, {1}}, {3,{2}}, {4,{1,3}} }
S = globalMarkov G
S = apply(S,s -> {sort s#0, sort s#1, sort s#2}) 
L = {{{1}, {3}, {2, 4}}, {{2}, {4}, {3}}}
assert(S === L)
///

--------------------------
--- TEST marginMap     ---
--------------------------

TEST ///
R = markovRing (3,2)
F = marginMap(1,R) 
m = matrix {{p_(1,1)-p_(2,1)-p_(3,1), p_(1,2)-p_(2,2)-p_(3,2), p_(2,1), p_(2,2), p_(3,1), p_(3,2)}}
assert(F.matrix === m)
///

--------------------------
--- TEST hiddenMap     ---
--------------------------

TEST ///
R = markovRing (2,3,2)
F = hiddenMap(1,R) 
m = matrix {{p_(1,1,1)+p_(2,1,1), p_(1,1,2)+p_(2,1,2), p_(1,2,1)+p_(2,2,1), p_(1,2,2)+p_(2,2,2), p_(1,3,1)+p_(2,3,1), p_(1,3,2)+p_(2,3,2)}}
assert(F.matrix === m)
///

--------------------------
--- TEST markovRing    ---
--------------------------

TEST ///
d = (2,2,2)
R = markovRing (d, Coefficients=>CC, VariableName=>q)
V = {q_(1,1,1), q_(1,1,2), q_(1,2,1), q_(1,2,2), q_(2,1,1), q_(2,1,2), q_(2,2,1), q_(2,2,2)}
assert(gens R === V)
///

------------------------------
--- TEST markovMatrices    ---
------------------------------

TEST ///
G = digraph { {1, {2,3}}, {2, {4}}, {3, {4}} }
S = localMarkov G
R = markovRing (2,2,2,2)
L = markovMatrices (R,G,S) 
M = L#1
m = matrix {{p_(2,1,1,1)+p_(2,1,1,2), p_(2,1,2,1)+p_(2,1,2,2)},{p_(2,2,1,1)+p_(2,2,1,2), p_(2,2,2,1)+p_(2,2,2,2)}} 
assert(M === m)
///

----------------------------
--- TEST markovIdeals    ---
----------------------------

TEST ///
G = digraph { {1, {2,3}}, {2, {4}}, {3, {4}} }
S = localMarkov G
R = markovRing (2,2,2,2)
I = markovIdeal (R,G,S) 
J = ideal(-p_(1,1,2,1)*p_(1,2,1,1)-p_(1,1,2,2)*p_(1,2,1,1)-p_(1,1,2,1)*p_(1,2,1,2)-p_(1,1,2,2)*p_(1,2,1,2)+p_(1,1,1,1)*p_(1,2,2,1)+p_(1,1,1,2)*p_(1,2,2,1)+p_(1,1,1,1)*p_(1,2,2,2)+p_(1,1,1,2)*p_(1,2,2,2),
-p_(2,1,2,1)*p_(2,2,1,1)-p_(2,1,2,2)*p_(2,2,1,1)-p_(2,1,2,1)*p_(2,2,1,2)-p_(2,1,2,2)*p_(2,2,1,2)+p_(2,1,1,1)*p_(2,2,2,1)+p_(2,1,1,2)*p_(2,2,2,1)+p_(2,1,1,1)*p_(2,2,2,2)+p_(2,1,1,2)*p_(2,2,2,2),
-p_(1,1,1,2)*p_(2,1,1,1)+p_(1,1,1,1)*p_(2,1,1,2), -p_(1,1,2,2)*p_(2,1,2,1)+p_(1,1,2,1)*p_(2,1,2,2),
-p_(1,2,1,2)*p_(2,2,1,1)+p_(1,2,1,1)*p_(2,2,1,2),  -p_(1,2,2,2)*p_(2,2,2,1)+p_(1,2,2,1)*p_(2,2,2,2)) 	   
assert(I === J)
///

-----------------------------------------------
--- TEST Gaussian Directed Graphical Models ---
-----------------------------------------------

----------------------------
--- TEST gaussianRing    ---
----------------------------

TEST ///
R = gaussianRing 4
B = gens R
L = {s_(1,1), s_(1,2), s_(1,3), s_(1,4), s_(2,2), s_(2,3), s_(2,4), s_(3,3), s_(3,4), s_(4,4)}
assert(B === L)
///
     
TEST /// 
G = digraph {{a,{b,c}}, {b,{c,d}}, {c,{}}, {d,{}}}
assert(toString gaussianRing G === "QQ[s_(a,a), s_(a,b), s_(a,c), s_(a,d), s_(b,b), s_(b,c), s_(b,d), s_(c,c), s_(c,d), s_(d,d)]")
///

-----------------------------
--- TEST covarianceMatrix ---
-----------------------------

TEST /// 
G = digraph {{a,{b,c}}, {b,{c,d}}, {c,{}}, {d,{}}}
R = gaussianRing G
S = covarianceMatrix R
assert(0==S-matrix {{s_(a,a), s_(a,b), s_(a,c), s_(a,d)}, {s_(a,b), s_(b,b), s_(b,c), s_(b,d)}, {s_(a,c), s_(b,c), s_(c,c), s_(c,d)}, {s_(a,d), s_(b,d), s_(c,d), s_(d,d)}})
///

----------------------------
--- TEST gaussianIdeal   ---
----------------------------

TEST /// 
G = digraph {{a,{b,c}}, {b,{c,d}}, {c,{}}, {d,{}}}
R = gaussianRing G
S = globalMarkov G
I = gaussianIdeal (R, G, S)
L = gaussianMatrices(R,G)
J = minors(2, L#0)
assert(I == J)
///

------------------------------
--- TEST gaussianMatrices  ---
------------------------------

TEST ///
G = digraph { {1,{2}}, {2,{3}}, {3,{4,5}},{4,{5}} } ;
R = gaussianRing G
S = localMarkov G
L = gaussianMatrices(R,G,S)
M1 = matrix {{s_(1,4), s_(1,3)}, {s_(2,4), s_(2,3)}, {s_(3,4), s_(3,3)}}
M2 = matrix {{s_(1,5), s_(1,4), s_(1,3)},{s_(2,5), s_(2,4), s_(2,3)},{s_(4,5), s_(4,4), s_(3,4)}, {s_(3,5), s_(3,4), s_(3,3)}}
M3 = matrix {{s_(1,3), s_(1,2)},{s_(2,3), s_(2,2)}}
assert({M1,M2,M3} === L)
///

TEST ///
G = digraph { {1,{2}}, {2,{3}}, {3,{4,5}},{4,{5}} } ;
R = gaussianRing G
L = gaussianMatrices(R,G)
M1 = matrix{{s_(1,4), s_(1,5), s_(1,3)},{s_(2,4), s_(2,5), s_(2,3)},{s_(3,4), s_(3,5), s_(3,3)}}
M2 = matrix{{s_(1,3), s_(1,4), s_(1,5), s_(1,2)},{s_(2,3), s_(2,4), s_(2,5), s_(2,2)}}
assert({M1,M2} === L)
///

-----------------------
--- TEST trekIdeal  ---
-----------------------

TEST ///
G = digraph {{a,{b,c}}, {b,{c,d}}, {c,{}}, {d,{}}}
R = gaussianRing G
I = trekIdeal(R,G)
assert(I==ideal(s_(b,c)*s_(b,d)-s_(b,b)*s_(c,d),s_(a,d)*s_(b,c)-s_(a,b)*s_(c,d),s_(a,d)*s_(b,b)-s_(a,b)*s_(b,d)))
///

-----------------------------------------------
--- TEST Gaussian Mixed Graphical Models ------
-----------------------------------------------

-------------------------
--- TEST gaussianRing ---
-------------------------

TEST ///
G = mixedGraph(digraph {{b,{c,d}},{c,{d}}},bigraph {{a,d}})
R = gaussianRing G
-- commented out by dan:
--   the order of the generators of the ring seems to depend on the accidental ordering of c and d in set{c,d}
--   so this test is not a good test
-- assert(toString gaussianRing G === "QQ[l_(b,c), l_(b,d), l_(c,d), p_(a,a), p_(b,b), p_(c,c), p_(d,d), p_(a,d), s_(a,a), s_(a,b), s_(a,c), s_(a,d), s_(b,b), s_(b,c), s_(b,d), s_(c,c), s_(c,d), s_(d,d)]")
///

-----------------------------
--- TEST covarianceMatrix ---
-----------------------------

TEST ///
G = mixedGraph(digraph {{b,{c,d}},{c,{d}}},bigraph {{a,d}})
R = gaussianRing G
S = covarianceMatrix(R,G)
assert(0 == S-matrix {{s_(a,a), s_(a,b), s_(a,c), s_(a,d)}, {s_(a,b), s_(b,b), s_(b,c), s_(b,d)}, {s_(a,c), s_(b,c), s_(c,c), s_(c,d)}, {s_(a,d), s_(b,d), s_(c,d), s_(d,d)}})
///

--------------------------------
--- TEST directedEdgesMatrix ---
--------------------------------

TEST ///
G = mixedGraph(digraph {{b,{c,d}},{c,{d}}},bigraph {{a,d}})
R = gaussianRing G
L = directedEdgesMatrix(R,G)
assert(0 == L-matrix {{0, 0, 0, 0}, {0, 0, l_(b,c), l_(b,d)}, {0, 0, 0, l_(c,d)}, {0, 0, 0, 0}})
///

----------------------------------
--- TEST bidirectedEdgesMatrix ---
----------------------------------

TEST ///
G = mixedGraph(digraph {{b,{c,d}},{c,{d}}},bigraph {{a,d}})
R = gaussianRing G
W = bidirectedEdgesMatrix(R,G)
assert(0 == W-matrix {{p_(a,a), 0, 0, p_(a,d)}, {0, p_(b,b), 0, 0}, {0, 0, p_(c,c), 0}, {p_(a,d), 0, 0, p_(d,d)}})
///

------------------------------------
--- TEST gaussianParametrization ---
------------------------------------

TEST ///
G = mixedGraph(digraph {{b,{c,d}},{c,{d}}},bigraph {{a,d}})
R = gaussianRing G
M = gaussianParametrization(R,G)
assert(0 == M-matrix {{p_(a,a), 0, 0, p_(a,d)}, {0, p_(b,b), l_(b,c)*p_(b,b), l_(b,c)*l_(c,d)*p_(b,b)+l_(b,d)*p_(b,b)}, {0, l_(b,c)*p_(b,b), l_(b,c)^2*p_(b,b)+p_(c,c), l_(b,c)^2*l_(c,d)*p_(b,b)+l_(b,c)*l_(b,d)*p_(b,b)+l_(c,d)*p_(c,c)},{p_(a,d), l_(b,c)*l_(c,d)*p_(b,b)+l_(b,d)*p_(b,b),l_(b,c)^2*l_(c,d)*p_(b,b)+l_(b,c)*l_(b,d)*p_(b,b)+l_(c,d)*p_(c,c),l_(b,c)^2*l_(c,d)^2*p_(b,b)+2*l_(b,c)*l_(b,d)*l_(c,d)*p_(b,b)+l_(b,d)^2*p_(b,b)+l_(c,d)^2*p_(c,c)+p_(d,d)}})
///

TEST ///
G = mixedGraph(digraph {{b,{c,d}},{c,{d}}},bigraph {{a,d}})
R = gaussianRing G
M = gaussianParametrization(R,G,SimpleTreks=>true)
assert(0 == M-matrix {{1, 0, 0, p_(a,d)}, {0, 1, l_(b,c), l_(b,c)*l_(c,d)+l_(b,d)}, {0, l_(b,c), 1, l_(b,c)*l_(b,d)+l_(c,d)}, {p_(a,d), l_(b,c)*l_(c,d)+l_(b,d), l_(b,c)*l_(b,d)+l_(c,d), 1}})
///

------------------------------
-- TEST identifyParameters ---
------------------------------

TEST ///
G = mixedGraph(digraph {{b,{c,d}},{c,{d}}},bigraph {{a,d}})
R = gaussianRing G
H = identifyParameters(R,G)
assert(H === new HashTable from {p_(a,d) => ideal(s_(a,c),s_(a,b),p_(a,d)-s_(a,d)),p_(d,d) => ideal(s_(a,c),s_(a,b),p_(d,d)*s_(b,c)^2-p_(d,d)*s_(b,b)*s_(c,c)-s_(b,d)^2*s_(c,c)+2*s_(b,c)*s_(b,d)*s_(c,d)-s_(b,b)*s_(c,d)^2-s_(b,c)^2*s_(d,d)+s_(b,b)*s_(c,c)*s_(d,d)), l_(c,d) =>ideal(s_(a,c),s_(a,b),l_(c,d)*s_(b,c)^2-l_(c,d)*s_(b,b)*s_(c,c)-s_(b,c)*s_(b,d)+s_(b,b)*s_(c,d)), l_(b,d) =>ideal(s_(a,c),s_(a,b),l_(b,d)*s_(b,c)^2-l_(b,d)*s_(b,b)*s_(c,c)+s_(b,d)*s_(c,c)-s_(b,c)*s_(c,d)), l_(b,c) =>ideal(s_(a,c),s_(a,b),l_(b,c)*s_(b,b)-s_(b,c)), p_(a,a) =>ideal(s_(a,c),s_(a,b),p_(a,a)-s_(a,a)), p_(b,b) =>ideal(s_(a,c),s_(a,b),p_(b,b)-s_(b,b)), p_(c,c) =>ideal(s_(a,c),s_(a,b),p_(c,c)*s_(b,b)+s_(b,c)^2-s_(b,b)*s_(c,c))})
///

--------------------------
-- TEST trekSeparation  --
--------------------------

-- This TEST could potentially fail since each statement in L could be written
-- in a different order, that is, {A,B,C,D} is equivalent to {B,A,D,C}
-- and trekSeparation may return either one of these statements
-- So I have commented this test for the moment
-- but one can test trekSeparation by testing trekIdeal.
-- TEST ///
-- G = mixedGraph(digraph {{b,{c,d}},{c,{d}}},bigraph {{a,d}})
-- R = gaussianRing G
-- T = trekSeparation G
-- T = apply(T,s -> {sort s#0, sort s#1, sort s#2, sort s#3})
-- L = {{{a}, {b, c}, {}, {}}, {{b, c}, {a, b}, {}, {b}}, {{a, b}, {b, c}, {}, {b}}, {{b, c}, {a, c}, {}, {c}}, {{b, c}, {a, d}, {}, {d}}}
-- assert(T === L)
-- ///

----------------------
-- TEST trekIdeal  ---
----------------------

TEST ///
G = mixedGraph(digraph {{b,{c,d}},{c,{d}}},bigraph {{a,d}})
R = gaussianRing G
T = trekSeparation G
I = trekIdeal(R,G)
assert(I == ideal(s_(a,c),s_(a,b),s_(a,c)*s_(b,b)-s_(a,b)*s_(b,c),-s_(a,c)*s_(b,b)+s_(a,b)*s_(b,c),s_(a,c)*s_(b,c)-s_(a,b)*s_(c,c),s_(a,c)*s_(b,d)-s_(a,b)*s_(c,d)))
///
     
--------------------------------------
--------------------------------------
end
--------------------------------------
--------------------------------------


--blank documentation node:
doc/// 
   Key
     gaussianMatrix
     (gaussianMatrix,Digraph,Matrix,List) 
   Headline
   Usage
   Inputs
   Outputs
   Description 
     Text
     Example
     Text
     Example
   SeeAlso
///


uninstallPackage "GraphicalModels"
restart
--installPackage("Graphs", UserMode=>true)
installPackage ("GraphicalModels", RemakeAllDocumentation => true, UserMode=>true)
viewHelp GraphicalModels
installPackage("GraphicalModels",UserMode=>true,DebuggingMode => true)


-- Local Variables:
-- compile-command: "make -C $M2BUILDDIR/Macaulay2/packages PACKAGES=GraphicalModels pre-install"
-- End:
