-*
Copyright 2013 Luis David Garcia Puente, Sonja Petrovic,
Mike Stillman, Seth Sullivant.

You may redistribute this file under the terms of the GNU General Public
License as published by the Free Software Foundation, either version 2 of
the License, or any later version.


Copyright 2020 Carlos Amendola, Luis David Garcia Puente, Roser Homs Pons, 
Olga Kuznetsova, Harshit J Motwani.

You may redistribute this file under the terms of the GNU General Public
License as published by the Free Software Foundation, either version 2 of
the License, or any later version.
*-

-- -*- coding: utf-8-unix -*-

newPackage(
     "GraphicalModels",
     Version => "2.0",
     Date => "November, 2020",
     Authors => {
          {Name=> "Carlos Amendola", 
	   Email=> "carlos.amendola@tum.de",
	   HomePage=>"http://www.carlos-amendola.com/"},
       
	  {Name => "Luis David Garcia Puente",
	   Email => "lgarcia@shsu.edu",
	   HomePage => "http://www.shsu.edu/~ldg005"},
       
          {Name=> "Roser Homs Pons", 
	   Email=> "roser.homs@tum.de",
	   HomePage=>"https://personal-homepages.mis.mpg.de/homspons/index.html"},
       
          {Name=> "Olga Kuznetsova", 
	   Email=> "kuznetsova.olga@gmail.com",
	   HomePage=>"https://okuznetsova.com"},
       
          {Name=> "Harshit J Motwani", 
	   Email=> "harshitmotwani2015@gmail.com",
	   HomePage=>"https://sites.google.com/view/harshitjmotwani/home"},
          {Name=> "Sonja Petrovic", 
	   Email=> "sonja@psu.edu",
	   HomePage=>"http://www.personal.psu.edu/sxp61"}, 
	  {Name => "Mike Stillman",
	   Email => "mike@math.cornell.edu",
	   HomePage => "http://www.math.cornell.edu/~mike/"},
          {Name=> "Seth Sullivant", 
	   Email=> "smsulli2@ncsu.edu",
	   HomePage=>"http://www4.ncsu.edu/~smsulli2/"}
          --{Name=> "Contributing authors and collaborators: Alexander Diaz, Shaowei Lin, David Murrugarra", 
	  -- Email=> "",
	  -- HomePage=>""}      
	  },
     Headline => "discrete and Gaussian graphical models",
     Keywords => {"Algebraic Statistics", "Graph Theory"},
     PackageExports => { "Graphs","StatGraphs" },
     PackageImports => { "IntegralClosure", "Elimination" },
     Certification => {
	  "journal name" => "The Journal of Software for Algebra and Geometry",
	  "journal URI" => "https://msp.org/jsag/",
	  "article title" => "Graphical Models",
	  "acceptance date" => "2013-03-05",
	  "published article URI" => "https://msp.org/jsag/2013/5-1/p01.xhtml",
	  "published article DOI" => "10.2140/jsag.2013.5.1",
	  "published code URI" => "https://msp.org/jsag/2013/5-1/jsag-v5-n1-x01-code.zip",
	  "release at publication" => "68f41d641fadb0a1054023432eb60177f1d7cbd9",
	  "version at publication" => "1.0",
	  "volume number" => "5",
	  "volume URI" => "https://msp.org/jsag/2013/5-1/"
	  },
     DebuggingMode => false
     )
export {"bidirectedEdgesMatrix",
       "Coefficients",
       "conditionalIndependenceIdeal",
       "covarianceMatrix",
       "directedEdgesMatrix",
       "discreteVanishingIdeal",
       "gaussianMatrices",
       "gaussianParametrization",
       "gaussianVanishingIdeal",
       "gaussianRing", 
       "globalMarkov",
       "hiddenMap",
       "identifyParameters", 
       "inverseMarginMap",
       "localMarkov",
       "markovMatrices", 
       "markovRing",     
       "marginMap", 
       "pairMarkov", 
       "trekIdeal", 
       "trekSeparation",
       "SimpleTreks",
       "undirectedEdgesMatrix",
       "VariableName",
       "sVariableName",
       "kVariableName",
       "lVariableName",
       "pVariableName",
       "graphType",
       "gaussianRingData",
       "sVar",
       "kVar",
       "pVar",
       "lVar",
       "nn",
       "compU",
       "compW",
       "OldVersion" --optional argument in gaussianVanishingIdeal to use old method for gaussianRings coming from directed graphs
	} 

markovRingData = local markovRingData
markovVariables = local markovVariables
gaussianVariables = local gaussianVariables
numberOfEliminationVariables = local numberOfEliminationVariables  


--**************************--
--  INTERNAL ROUTINES       --
--**************************--

--*************************************--
--  Functions used by Markov methods   --
--*************************************--


--------------------------------------------
-- bayesBall
-- A is a set in 1..n (n = #G)
-- C is a set in 1..n (the "blocking set")
-- G is a DAG
-- Returns the subset B of 1..n which is independent of A given C.
-- The algorithm is the Bayes Ball algorithm, as implemented by Luis Garcia-Puente, 
-- after the paper of Ross D. Shachter.
--------------------------------------------

bayesBall = (A,C,G) -> (
     V := vertices G; -- it was: sort vertices G;
     visited := new MutableHashTable from apply(V, k-> k=>false);
     blocked :=  new MutableHashTable from apply(V, k-> k=>false);
     up :=  new MutableHashTable from apply(V, k-> k=>false);
     down := new MutableHashTable from apply(V, k-> k=>false);
     top :=  new MutableHashTable from apply(V, k-> k=>false);
     bottom := new MutableHashTable from apply(V, k-> k=>false);
     vqueue := new MutableList from toList A; -- toList A;
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
	  );
     set toList select(V, i -> not blocked#i and not bottom#i)     
     )     




--*************************************--
--  Functions (local) used throughout  --
--*************************************--


---------------------------------------------------------------
-- cartesian
-- cartesian({d_1,...,d_n}) returns the cartesian product 
-- of {0,...,d_1-1} x ... x {0,...,d_n-1}
---------------------------------------------------------------

cartesian := (L) -> (
     if #L == 1 then 
	return toList apply (L#0, e -> 1:e);
     L0 := L#0;
     Lrest := drop (L,1);
     C := cartesian Lrest;
     flatten apply (L0, s -> apply (C, c -> prepend (s,c))))



--------------------------------------------
-- position of an element x in a list h
--------------------------------------------

pos := (h, x) -> position(h, i->i===x)



--------------------------------------------------------------------------
-- possibleValues ((d_1,...,d_n),A) returns the cartesian product 
-- of all d_i's such that the vertex i is a member of the list A
-- it assumes that the list A is a list of integers.
--------------------------------------------------------------------------
possibleValues := (d,A) ->
     cartesian (toList apply(0..#d-1, i -> 
	       if member(i,A) 
	       then toList(1..d#i) 
	       else {0}))
     
     
     
-------------------------------------------------------
-- prob((d_1,...,d_n),(s_1,dots,s_n))
-- Note: this function assumes that R is a markovRing
-------------------------------------------------------

prob := (R,s) -> (
     d := R.markovRingData;
     p := i -> R.markovVariables#i;
     L := cartesian toList apply (#d, i -> 
	   if s#i === 0 
	   then toList(1..d#i) 
	   else {s#i});
     sum apply (L, v -> p v))



-------------------------------------------------------------------------------
-- takes a list A, and a sublist B of A, and converts 
-- the membership sequence of 0's and 1's of elements of B in A to binary
-------------------------------------------------------------------------------

setToBinary := (A,B) -> sum(toList apply(0..#A-1, i->2^i*(if (set B)#?(A#i) then 1 else 0)))



-------------------------------------------------------
-- returns all subsets of B which contain A:
-------------------------------------------------------

subsetsBetween := (A,B) -> apply(subsets ((set B) - A), i->toList (i+set A))




--***********************************************************************************--
--  Functions used within Markov relation routines to remove redundant CI statements --
--***********************************************************************************--


--------------------------------------------------------------------------------------
-- Removing redundant statements:                              
-- called from local, global, and pairwise Markov methods.     
--
-- A conditional independence statement is a list {A,B,C}
-- where A,B,C are (disjoint) subsets of labels for nodes in the graph.
-- It should be interpreted as: A independent of B given C.
-- A dependency list is a list of dependencies.
-- 
-- We have several simple routines to remove the most obvious redundant elements, 
-- but a more serious attempt to remove dependencies could be made.
--------------------------------------------------------------------------------------

equivStmts = (S,T) -> S#2 === T#2 and set{S#0,S#1} === set{T#0,T#1} 
     -- If S and T represent exactly the same dependency, return true.

setit = (d) -> {set{d#0,d#1},d#2} 
     -- More serious removal of redundancies.  

under = (d) -> (
           d01 := toList d_0;
           d0 := toList d01_0;
           d1 := toList d01_1;
           d2 := toList d_1;
           e0 := subsets d0;
           e1 := subsets d1;
           z1 := flatten apply(e0, x -> apply(e1, y -> (
      		    {set{d01_0 - set x, d01_1 - set y}, set x + set y +  d_1})));-- see caveat for removeRedundants
           z2 := flatten apply(e0, x -> apply(e1, y -> (
      		    {set{d01_0 - set x, d01_1 - set y},  d_1})));-- see caveat for removeRedundants
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

minimizeStmts = (Ds) -> (
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

--------------------------------------------------------------------------------------
-- removeRedundants: the general function
-- Ds is a list of triples of sets {A,B,C}
-- test1: returns true if D1 can be removed
-- Return a sublist of Ds which removes any that test1 declares not necessary.
-- 
--  **CAVEAT**
--  This works just fine when used internally, e.g. from localMarkov. 
--  However, if we export it and try to use it, there is a problem: we seem to be 
--  attempting to add a List to a Set in the two marked lines of the function "under".
--------------------------------------------------------------------------------------

removeRedundants = (Ds) -> (
     test1 := (D1,D2) -> (D1_2 === D2_2 and 
                          ((isSubset(D1_0, D2_0) and isSubset(D1_1, D2_1))
	               or (isSubset(D1_1, D2_0) and isSubset(D1_0, D2_1))));
     Ds = apply(Ds, d -> {set{d#0,d#1}, d#2});
     Ds = unique Ds;      -- first remove non-unique elements, if any.
     Ds = apply(Ds, d -> append(toList(d#0), d#1));
     c := toList select(0..#Ds-1, i -> (
	       a := Ds_i;
	       D0 := drop(Ds,{i,i});
	       all(D0, b -> not test1(a,b))));
     minimizeStmts(Ds_c))



--**************************--
--  METHODS 	      	   	  --
--**************************--

--****************************************************************************************--
--  Methods for creating conditional independence statements from graphs and digraphs	  --
--****************************************************************************************--

----------------------------------------------------
-- pairMarkov
-- pairMarkov Graph does the following:
-- given a graph G, returns a list of triples {A,B,C}
-- where A,B,C are disjoint sets of the form:

-- for all non-edges {i,j}:  {i,j, all other vertices} 
-- pairMarkov Digraph does the following:
-- given a digraph G, returns a list of triples {A,B,C}
-- where A,B,C are disjoint sets, and for every vertex v
-- and non-descendent w of v,
-- {v, w, nondescendents(G,v) - w}
----------------------------------------------------

pairMarkov = method()
pairMarkov Graph := List => (G) -> (
     removeRedundants flatten apply(vertices G, v -> ( -- removed sort
     	  apply(toList nonneighbors(G,v), non-> (
		    {set {v}, set {non}, set vertices G - set {v} - set {non}}
		    )
	       )
	  )
     )
)

pairMarkov Digraph := List => (G) -> (
     if isCyclic G then error("digraph must be acyclic");
     removeRedundants flatten apply(vertices G, v -> (  -- removed sort
    	       ND := nondescendents(G,v);
     	       W := ND - parents(G,v);
     	       apply(toList W, w -> {set {v}, set{w}, ND - set{w}}))))
    

----------------------------------------------------
-- localMarkov Graph
-- localMarkov Digraph
-- Given a graph G, return a list of triples {A,B,C}
-- of the form {v, nonneighbors of v, all other vertices }
-- Given a digraph G, return a list of triples {A,B,C}
-- of the form {v, nondescendents - parents, parents}
----------------------------------------------------

localMarkov = method()
localMarkov Graph := List =>  (G) -> (
     removeRedundants apply(vertices G, v -> (  -- removed sort
	   {set {v},  nonneighbors(G,v), set vertices G - set {v} - nonneighbors(G,v)}
		    )
	       )
	  )		
     	 
localMarkov Digraph := List =>  (G) -> (
     if isCyclic G then error("digraph must be acyclic");
     result := {};
     scan(vertices G, v -> (  -- removed sort
	       ND := nondescendents(G,v);
	       P := parents(G,v);
	       if #(ND - P) > 0 then
	         result = append(result,{set{v}, ND - P, P})));
     removeRedundants result)


------------------------------------------------------------------------------
-- globalMarkov Graph
-- globalMarkov Digraph
-- Given a graph G, return a list of triples {A,B,C}
-- of the form {A,B,C} if C separates A and B in the graph.
-- Given a graph G, return a complete list of triples {A,B,C}
-- so that A and B are d-separated by C (in the graph G).
-- If G is large, this should maybe be rewritten so that
-- one huge list of subsets is not made all at once
------------------------------------------------------------------------------

globalMarkov = method()
globalMarkov Graph := List => (G) ->(
     AX := subsets vertices G;
     AX = drop(AX,1); -- drop the empty set
     AX = drop(AX,-1); -- drop the entire set
     -- product should apply * to entire list. note that  * of sets is intersection.
     statements := for A in AX list (
	  B:=product apply(A, v-> nonneighbors(G,v) ); --this is the list of all B's 
	  if #B === 0 then continue; -- need both A and B to be nonempty
     	  C := (vertices G) - set A - B ;
     	  {set A,  B, set C}
	  );
    removeRedundants  statements
    ) 
 
globalMarkov Digraph := List => (G) -> (
     if isCyclic G then error("digraph must be acyclic");
     V := vertices G;  -- removed sort
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



--*************************************************************************
--  Methods for creating polynomial rings that carry information about   --
--  random variables and/or underlying graph, digraph or mixed graph     --
--*************************************************************************

------------------------------------------------------------------------------------------------
-- markovRing Sequence
-- Outputs a polynomial ring whose indeterminates are joint probabilities of discrete 
-- random variables with a given number of states. 
-- d should be a sequence of integers di >= 1
--
-- NOTE: there is a mutable hash table of all Markov rings created, so as to not re-create rings!
-- the hashtable is indexed by the sequence d, the coefficient ring kk, and the variable name p, 
-- as this information identifies the Markov ring uniquely. 
------------------------------------------------------------------------------------------------

toSymbol = (p) -> (
     if instance(p,Symbol) then p
     else
     if instance(p,String) then getSymbol p
     else
     error ("expected a string or symbol, but got: ", toString p))


markovRingList := new MutableHashTable;

markovRing = method(Dispatch=>Thing, Options=>{Coefficients=>QQ,VariableName=> "p"})
markovRing Sequence := Ring => opts -> d -> (
     if any(d, di -> not instance(di,ZZ) or di <= 0)
          then error "markovRing expected positive integers";
     kk := opts.Coefficients;
     p := toSymbol opts.VariableName;
     if not markovRingList#?(d,kk,p) then (
     	  start := (#d):1;
	  vlist := start .. d;
	  R := kk(monoid [p_start .. p_d, MonomialSize=>16]);
	  R.markovRingData = d;
	  H := new HashTable from apply(#vlist, i -> vlist#i => R_i);
	  R.markovVariables = H;
	  markovRingList#(d,kk,p) = R;
	  );
    markovRingList#(d,kk,p))

------------------------------------------------------------------------------------------------------------------------------------
-- gaussianRing ZZ
-- gaussianRing Graph 
-- gaussianRing Digraph
-- gaussianRing MixedGraph
-- Outputs a polynomial ring whose indeterminates are joint probabilities of Gaussian
-- random variables corresponding to vertices of a graph (or variables 1..n). 
-- NOTE: the mutable hash table of all gaussian rings created is indexed by:
--     (coefficient field, variable name, number of r.v.'s) --in case of ZZ input
--     (coefficient field, variable name, vertices of the directed graph) --in case of Digraph input
--     (coefficient field, variable name, whole undirected graph) --in case of Graph input
--     (coefficient field, variable name s, variable name l, variable name p, vertices of the mixed graph) -- in case of MixedGraph input.
------------------------------------------------------------------------------------------------------------------------------------

--------------------------------------------------------------------------------
-----AUXILIAR FUNCTIONS FOR GAUSSIANRING MIXED GRAPHS---------------------------
--------------------------------------------------------------------------------

-- neighbors of a vertex of a mixed graph considering the underlying graph
-- of its graph, digraph and bigraph
-- INPUT:
-- g=mixedGraph (collateVertices requires mixedGraph)
-- v=element in vertices g
-- OUTPUT:
-- V=set that contains all neighboring vertices of v in the underlying graph of g
neighborsMG := (g,v) -> (
    i := position(vertices g, u -> u === v);
    if i === null then error "v is not a vertex of g.";
    G:= collateVertices g;
    V:=set {};
    for h in keys G#graph do V=V+neighbors(underlyingGraph G#graph#h,v);
    V
 )


-- neighbors of a vertex of a mixed graph considering the underlying graph
-- of its graph, digraph and bigraph
-- INPUT:
-- g=mixedGraph (neighborsMG requires mixedGraph)
-- OUTPUT:
-- C=list of 
connectedComponentsMG := (g) -> (
    V := vertices g;
    while #V != 0 list (
        C := {first V};
        i := 0;
        while i!= #C do (
            N := toList neighborsMG(g, C_i);
            C = unique(C | N);
            V = V - set C;
            i = i + 1;
            if #V == 0 then break;
            );
        C
        )
    )
--------------------------------------------------------------------------------
--------------------------------------------------------------------------------
--------------------------------------------------------------------------------

gaussianRingList := new MutableHashTable;

gaussianRing = method(Dispatch=>Thing, Options=>{Coefficients=>QQ, sVariableName=>"s", lVariableName=>"l", 
	  pVariableName=>"p", kVariableName=>"k"})
gaussianRing ZZ :=  Ring => opts -> (n) -> (
     -- s_{1,2} is the (1,2) entry in the covariance matrix.
     -- this assumes r.v.'s are labeled by integers.
     s := toSymbol opts.sVariableName;
     kk := opts.Coefficients;
     if (not gaussianRingList#?(kk,s,n)) then ( 
	  --(kk,s,n) uniquely identifies gaussianRing in case of ZZ input.
     w := flatten toList apply(1..n, i -> toList apply(i..n, j -> (i,j)));
     v := apply (w, ij -> s_ij);
     R := kk(monoid [v, MonomialSize=>16]);
     -- create gaussianRingData HashTable
     D := new MutableHashTable;
     D#nn = n;
     R.gaussianRingData = new HashTable from D; 
     -- create gaussianVariables HashTable
     H := new HashTable from apply(#w, i -> w#i => R_i); 
     R.gaussianVariables = H;
     -- fill into internal gaussianRingList
     gaussianRingList#((kk,s,n)) = R;); 
     gaussianRingList#((kk,s,n))    
     )

gaussianRing Graph := Ring => opts -> (g) -> (    
    bb := graph g;
    vv := sort vertices g;
    s := toSymbol opts.sVariableName;
    k := toSymbol opts.kVariableName;
    kk := opts.Coefficients;
    if (not gaussianRingList#?(kk,s,k,bb)) then ( 
	 --(kk,s,k,bb) uniquely identifies gaussianRing in case of Graph input.
    sL := delete(null, flatten apply(vv, x-> apply(vv, y->if pos(vv,x)>pos(vv,y) then null else s_(x,y))));
    kL := join(apply(vv, i->k_(i,i)),delete(null, flatten apply(vv, x-> apply(toList bb#x, y->if pos(vv,x)>pos(vv,y) then null else k_(x,y)))));
    m := #kL; --eliminate the k's 
    R := kk(monoid [kL,sL,MonomialOrder => Eliminate m, MonomialSize=>16]); 
    -- create gaussianVariables hash table: (symbol s)_(i,j) => ring var with the same name, same for l, p.
    H := new MutableHashTable;
    nextvar := 0;
    for v in kL do (H#v = R_nextvar; nextvar = nextvar+1); 
    for v in sL do (H#v = R_nextvar; nextvar = nextvar+1);
    R.gaussianVariables = new HashTable from H;
    R#numberOfEliminationVariables = m;
   -- create gaussianRingData hashTable
    D := new MutableHashTable;
    D#nn=#vv;
    D#sVar=s;
    D#kVar=k;
    R.gaussianRingData=new HashTable from D;
    -- create attributes of the ring containing class and graph
    R.graphType=class g;
    R.graph= g;
    -- fill into internal gaussianRingList
    gaussianRingList#((kk,s,k,bb)) = R;); 
    gaussianRingList#((kk,s,k,bb))
    )

gaussianRing Digraph :=  Ring => opts -> (G) -> (
    return gaussianRing (mixedGraph G, opts);
     )

gaussianRing Bigraph :=  Ring => opts -> (G) -> (
    return gaussianRing (mixedGraph G, opts);
     )

gaussianRing MixedGraph := Ring => opts -> (g) -> (
     -- convert mixedGraph to hash table
     gg:= graph g;
     -- sort vertices (only according to vertex number)
     vv := sort vertices g;
     --vv := join(sort U,sort W);
     -- add all vertices to all graphs and convert them to hash tables
     G := graph collateVertices g;
     dd := graph G#Digraph;
     bb := graph G#Bigraph;
     uu := graph G#Graph;
     -- compute partition V=U\cup W
     if g#graph#Graph===graph{} then (U:={}; W:=vv) else (
     -- compute partition V=U\cup W
     (U,W)=partitionLMG g; 
     );
     -- set ring variables
     s := toSymbol opts.sVariableName;
     l := toSymbol opts.lVariableName;
     p := toSymbol opts.pVariableName;
     k := toSymbol opts.kVariableName; 
     kk := opts.Coefficients;        
     if (not gaussianRingList#?(kk,s,k,l,p,vv)) then ( 
	  --(kk,s,k,l,p,vv) uniquely identifies gaussianRing in case of MixedGraph input.
     sL := delete(null, flatten apply(vv, x-> apply(vv, y->if pos(vv,x)>pos(vv,y) then null else s_(x,y))));
     kL := join(apply(U, i->k_(i,i)),delete(null, flatten apply(U, x-> apply(toList uu#x, y->if pos(vv,x)>pos(vv,y) then null else k_(x,y)))));
     lL := delete(null, flatten apply(vv, x-> apply(toList dd#x, y->l_(x,y))));	 
     pL := join(apply(W, i->p_(i,i)),delete(null, flatten apply(W, x-> apply(toList bb#x, y->if pos(vv,x)>pos(vv,y) then null else p_(x,y)))));
     m := #kL+#lL+#pL; 
     R := kk(monoid [kL,lL,pL,sL,MonomialOrder => Eliminate m, MonomialSize=>16]); 
     -- create gaussianVariables hash table: (symbol s)_(i,j) => ring var with the same name, same for l, p.
     H := new MutableHashTable;
     nextvar := 0;
     for v in kL do (H#v = R_nextvar; nextvar = nextvar+1); 
     for v in lL do (H#v = R_nextvar; nextvar = nextvar+1);
     for v in pL do (H#v = R_nextvar; nextvar = nextvar+1);
     for v in sL do (H#v = R_nextvar; nextvar = nextvar+1);
     R.gaussianVariables = new HashTable from H;
     R#numberOfEliminationVariables = m;
     -- create gaussianRingData hashTable
     D := new MutableHashTable;
     D#nn=#vv;
     D#sVar=s;
     D#kVar=k;
     D#compU=U;
     D#compW=W;
     D#lVar=l;
     D#pVar=p;
     R.gaussianRingData=new HashTable from D;
     -- create attributes of the ring containing class and graph
     R.graphType=class g;
     R.graph= g;
     -- fill into internal gaussianRingList
     gaussianRingList#((kk,s,k,l,p,vv)) = R;); 
     gaussianRingList#((kk,s,k,l,p,vv))
     )




--************************************************************************
--  Methods for creating matrices relevant for the graphical models     --
-- (covariance matrix, matrices whose minors vanish on the model)       --
--************************************************************************

------------------------------------------------------------------
-- undirectedEdgesMatrix Ring 
------------------------------------------------------------------

undirectedEdgesMatrix = method()
undirectedEdgesMatrix Ring := Matrix =>  R -> (
     if not (R.graphType === MixedGraph or R.graphType === Graph) 
     then error "Expected ring created with gaussianRing of a Graph or MixedGraph.";
     -- retrieve graph (of the right type)
     g:=R.graph;
     -- For graphs, turn g into a hashtable and sort vertices.
     if (instance(g,Graph)) 
     then (uu:=graph g; vv:= sort vertices g)
     -- For mixedGraphs, turn undirected edges into a hashtable 
     -- (considering all the vertices of the mixedGraph)
     -- and sort vertices in component U of the partition.
     else (G:= graph collateVertices g; uu=graph G#Graph; vv= sort R.gaussianRingData#compU);
     n := #vv; --appropriate number of vertices to take into account
     k := R.gaussianRingData#kVar; -- k variables 
     H := R.gaussianVariables; -- variables in the gaussianRing
     -- Build matrix
     PM := mutableMatrix(R,n,n);
     scan(vv,i->PM_(pos(vv,i),pos(vv,i))=H#(k_(i,i)));
     scan(vv,i->scan(toList uu#i, j->PM_(pos(vv,i),pos(vv,j))=if pos(vv,i)<pos(vv,j) then H#(k_(i,j)) else H#(k_(j,i))));
     matrix PM
     ) 
     

------------------------------------------------------------------
-- directedEdgesMatrix Ring 
------------------------------------------------------------------

directedEdgesMatrix = method()
directedEdgesMatrix Ring := Matrix => R -> (
     if not (R.graphType === MixedGraph) then error "expected a ring created with gaussianRing of a MixedGraph";     
     g := R.graph;
     G := graph collateVertices g;
     dd := graph G#Digraph;
     vv := sort vertices g;
     n := R.gaussianRingData#nn;
     l := R.gaussianRingData#lVar; -- l variables
     H := R.gaussianVariables;
     LM := mutableMatrix(R,n,n);
     scan(vv,i->scan(toList dd#i, j->LM_(pos(vv,i),pos(vv,j))=H#(l_(i,j))));
     matrix LM) 


------------------------------------------------------------------
-- bidirectedEdgesMatrix Ring
------------------------------------------------------------------

bidirectedEdgesMatrix = method()
bidirectedEdgesMatrix Ring := Matrix => R -> (
     if not (R.graphType === MixedGraph) then error "expected a ring created with gaussianRing of a MixedGraph. 
     Bigraphs alone are not accepted. If you need to work with one, please reformulate it as a Graph. ";     
     g := R.graph;     
     G := graph collateVertices g;
     bb := graph G#Bigraph;
     -- take only component W of mixedGraph
     vv := sort R.gaussianRingData#compW;
     n := #vv;
     p := R.gaussianRingData#pVar; -- p variables
     H := R.gaussianVariables;
     PM := mutableMatrix(R,n,n);
     scan(vv,i->PM_(pos(vv,i),pos(vv,i))=H#(p_(i,i)));
     scan(vv,i->scan(toList bb#i, j->PM_(pos(vv,i),pos(vv,j))=if pos(vv,i)<pos(vv,j) then H#(p_(i,j)) else H#(p_(j,i))));
     matrix PM) 
 
 
 
------------------------------------------------------------------
-- markovMatrices(Ring,List,List) 
-- markovMatrices(Ring,List)
------------------------------------------------------------------

markovMatrices = method()
markovMatrices(Ring,List,List) := (R,Stmts,VarNames) -> (
     -- R should be a markovRing and Stmts a list of independence statements.
     if not R.?markovRingData then error "expected a ring created with markovRing";
     d := R.markovRingData;
     if not isSubset ( set unique flatten flatten Stmts,  set VarNames)  then error "variables names in statements do not match list of random variable names";
     flatten apply(Stmts, stmt -> (
	       Avals := possibleValues(d, apply( stmt#0, i ->  pos( VarNames,i)) );
	       Bvals := possibleValues(d, apply( stmt#1, i ->  pos( VarNames,i)) );
	       Cvals := possibleValues(d, apply( stmt#2, i ->  pos( VarNames,i)) );
     	       apply(Cvals, c -> (
                  matrix apply(Avals, 
		       a -> apply(Bvals, b -> (
				 e := toSequence(toList a + toList b + toList c);
		      		 prob(R,e))))))))
    )

markovMatrices(Ring,List) := (R,Stmts) -> (
     -- R should be a markovRin and Stmts a list of independence statements.
     if not R.?markovRingData then error "expected a ring created with markovRing";
     d := R.markovRingData;
     if not isSubset ( set unique flatten flatten Stmts,  set( 1..#d) )  then error "variables names in statements do not match list of random variable names";
     VarNames := toList (1..#d);
     flatten apply(Stmts, stmt -> (
	       Avals := possibleValues(d, apply( stmt#0, i ->  pos( VarNames,i)) );
	       Bvals := possibleValues(d, apply( stmt#1, i ->  pos( VarNames,i)) );
	       Cvals := possibleValues(d, apply( stmt#2, i ->  pos( VarNames,i)) );
     	       apply(Cvals, c -> (
                  matrix apply(Avals, 
		       a -> apply(Bvals, b -> (
				 e := toSequence(toList a + toList b + toList c);
		      		 prob(R,e))))))))
    )


------------------------------------------------------------------
-- covarianceMatrix(Ring)
------------------------------------------------------------------

covarianceMatrix = method()
covarianceMatrix(Ring) := Matrix => (R) -> (
       if not R.?gaussianRingData then error "expected a ring created with gaussianRing";    
       if R.?graphType then (
	    g := R.graph;
	    vv := sort vertices g;
	    n := R.gaussianRingData#nn;
	    s := R.gaussianRingData#sVar;
	    H := R.gaussianVariables;
	    SM := mutableMatrix(R,n,n);
	    scan(vv,i->scan(vv, j-> SM_(pos(vv,i),pos(vv,j))=if pos(vv,i)<pos(vv,j) then H#(s_(i,j)) else H#(s_(j,i)))); 
	    matrix SM
	    )   
       else (
	    n =R.gaussianRingData#nn; 
	    genericSymmetricMatrix(R,n)
	    )
  )



------------------------------------------------------------------
-- gaussianMatrices(Ring,List)
------------------------------------------------------------------

gaussianMatrices = method()
gaussianMatrices(Ring,List) := List =>  (R,Stmts) -> (
        if not R.?gaussianRingData then error "expected a ring created with gaussianRing";
	if R.?graph then (g := R.graph; vv := sort vertices g)
	else (n:=R.gaussianRingData#nn; vv=toList(1..n));
	if not isSubset ( set unique flatten flatten Stmts,  set vv)  then error "variables names in statements do not match list of random variable names";
        SM := covarianceMatrix(R);
        apply(Stmts, s -> 
	    submatrix(SM, apply(s#0,x->pos(vv,x)) | apply(s#2,x->pos(vv,x)) , 
	        apply(s#1,x->pos(vv,x)) | apply(s#2,x->pos(vv,x)) )) 
	) 
 
 
--******************************************************************--
--  Methods for creating ideals that vanish for a graphical model   --
--******************************************************************--

------------------------------------------------------------------
-- conditionalIndependenceIdeal (Ring,List)
-- conditionalIndependenceIdeal (Ring,List,List)
------------------------------------------------------------------
 
conditionalIndependenceIdeal=method()
conditionalIndependenceIdeal (Ring,List) := Ideal => (R,Stmts) ->(
     if not (R.?gaussianRingData or R.?markovRingData) then error "expected a ring created with gaussianRing or markovRing";
     if #Stmts === 0 then (ideal(0_R))
     else ( 
     	  if R.?gaussianRingData then (      
               -- Choose appropriate vertices vv for gaussianRings coming from graphs, digraphs or mixedGraphs
	       if R.?graph then (
     		    if not isSubset ( set unique flatten flatten Stmts,  set vertices(R.graph))  then error "variables names in statements do not match variable names in the Gaussian ring";
	   	    g := R.graph;
           	    vv := sort vertices g
               )    
	       -- Choose appropriate vertices vv for gaussianRings coming from an integer
	       else (
		    vv = toList (1..R.gaussianRingData#nn);
     		    if not isSubset ( set unique flatten flatten Stmts,  set vv)  then error "variables names in statements do not match variable names in the Gaussian ring";
	       );	    	    	    
	       -- compute covarianceMatrix and suitable minors given by vv
	       SM := covarianceMatrix(R);
               sum apply(Stmts, s -> minors(#s#2+1, 
	       	    submatrix(SM, apply(s#0,x->pos(vv,x)) | apply(s#2,x->pos(vv,x)) , 
		    	 apply(s#1,x->pos(vv,x)) | apply(s#2,x->pos(vv,x)) ) )) 
               )
     	  else (
               if not isSubset ( set unique flatten flatten Stmts,  set toList (1..#R.markovRingData))  then error "variables names in statements do not match variable names in the markov ring.";
	       M := markovMatrices(R,Stmts);
	       sum apply(M, m -> minors(2,m)) 
	       )
     	  )	   
)     

conditionalIndependenceIdeal (Ring,List,List) := Ideal => (R,Stmts,VarNames) ->(
     if not R.?markovRingData then error "expected a ring created with markovRing";
     if not isSubset ( set unique flatten flatten Stmts,  set VarNames)  then error "variables names in statements do not match list of random variable names";
     if #Stmts === 0 then ideal(0_R)
     else (	  	
     	  M := markovMatrices(R,Stmts,VarNames);
     	  sum apply(M, m -> minors(2,m)) 
     	  )
     )	   



------------------------------------------------------------------
-- gaussianParametrization (Ring)
------------------------------------------------------------------

gaussianParametrization = method(Options=>{SimpleTreks=>false})
gaussianParametrization Ring := Matrix => opts -> R -> (
     if not R.graphType === MixedGraph then error "Must be a gaussianRing created with a mixed graph, a digraph or a bigraph";
     g := R.graph;
     -- Not yet implemented for mixedGraphs with undirected edges
     if not undirectedEdgesMatrix R == 0 then error "Function not implemented for mixed graphs with undirected edges"; 
     S := covarianceMatrix R;    
     W := bidirectedEdgesMatrix R;     
     L := directedEdgesMatrix R;
     Li := inverse(1-matrix(L));
     M := transpose(Li)*matrix(W)*Li;
     if opts.SimpleTreks then (
       n := R.gaussianRingData#nn;
       P := matrix {apply(n,i->W_(i,i)-M_(i,i)+1)};
       Q := apply(n,i->W_(i,i)=>P_(0,i));
       scan(n,i->P=sub(P,Q));
       sub(M,apply(n,i->W_(i,i)=>P_(0,i))))
     else
       M)



------------------------------------------------------------------
-- gaussianVanishingIdeal Ring
-- Note: this method currently works on really small examples,
-- because it computes the vanishing ideal as an elimination ideal.
-- More clever ways to compute it would be of interest.
-- Currently this method is only implemented for mixedGraphs 
-- without undirected edges
------------------------------------------------------------------

gaussianVanishingIdeal=method(TypicalValue => Ideal, Options=>{OldVersion => false})
gaussianVanishingIdeal Ring := opts -> R -> (
    if not R.?graph then error "expected a ring created with gaussianRing of a Graph, Digraph, Bigraph or MixedGraph";
    if R.graphType === Graph then (    
       K:= undirectedEdgesMatrix R;
       adjK := sub(det(K)*inverse(sub(K,frac R)), R);
       Itemp:=saturate(ideal (det(K)*covarianceMatrix(R) - adjK), det(K));
       ideal selectInSubring(1, gens gb Itemp))
    --check if gaussianRing comes only from Digraph and user asked for optional method of previous versions
    else if (opts.OldVersion and R.graphType === MixedGraph and R.graph#graph#Graph===graph{} and R.graph#graph#Bigraph===bigraph{}) then (
       G := R.graph#graph#Digraph; --retrieve digraph from mixedGraph
       vv := sort vertices G;
       n := #vv;
       v := (topSort G)#map;
       v = hashTable apply(keys v, i->v#i=>i);
       v = apply(n,i->v#(i+1));
       P := toList apply(v, i -> toList parents(G,i));
       s := R.gaussianRingData#sVar; --retrieve name of variable used for sVar as symbol
       L := select(keys R.gaussianVariables, v -> first baseName v==s); --select sVar from variables in R as indexed variables
       nx :=#L; -- number of sVar
       ny := max(P/(p->#p));
       x := local x;
       y := local y;
       S := (coefficientRing R)[x_0 .. x_(nx-1),y_0 .. y_(ny-1)];
       newvars := apply(ny, i -> y_i);
       H := hashTable apply(nx,i->L#i=>x_i); --convert sVar to x_i
       sp := (i,j) -> if pos(vv,i) > pos(vv,j) then H#(s_(j,i)) else H#(s_(i,j));
       I := trim ideal(0_S);
       for i from 1 to n-1 do (
     	   J := ideal apply(i, j -> sp(v#j,v#i) - sum apply(#P#i, k ->y_k * sp(v#j,P#i#k)));
     	   I = eliminate(newvars, I + J););
       F := map(R,S,apply(nx,i->x_i=>R.gaussianVariables#(L_i))|apply(newvars,i->i=>0));
       F(I))
     else if R.graphType === MixedGraph then (
       G = R.graph;
       if (#edges(G#graph#Graph) > 0) then error "This function is currently only implemented for mixed graphs without undirected part"; 
       if (isCyclic G#graph#Digraph == true) then error "Directed part of mixed graph must be acyclic";
       S = covarianceMatrix R;    
       W := bidirectedEdgesMatrix R;     
       L = directedEdgesMatrix R;
       Li := inverse(1-matrix(L));
       M := transpose(Li)*matrix(W)*Li;
       tempideal := ideal(S-M);
       m:= (R#numberOfEliminationVariables)-1;
       elimvarlist := flatten entries (vars(R))_{0..m};
       I = trim ideal(0_R);
       I = eliminate(elimvarlist,tempideal)     
     )   
)

------------------------------------------------------------------
-- discreteVanishingIdeal (Ring,Digraph)
------------------------------------------------------------------

discreteVanishingIdeal=method()
discreteVanishingIdeal (Ring, Digraph)  := Ideal => (R, G) -> (
     if not (R.?markovRingData) then error "expected a ring created with markovRing";
     if not instance(G,Digraph) then error "expected a Digraph";
     d := R.markovRingData;
     n := #d; 
     if not (#vertices(G) == n) then error "Number of vertices of graph does not match size of ring";
     H := topSort G;
     shuffle := apply(sort vertices G, v -> H#map#v);
     dshuff := toSequence d_(shuffle - toList (n:1));
     R1 := local R1;
     R1 = markovRing dshuff;          
     p := j -> R1.markovVariables#j;
     I := trim ideal(0_R1);     
     SortedG := H#newDigraph;
     a := local a;
     S := local S;
     apply(2..n, i -> (
         familyi := append(toList parents(SortedG,i),i);
         tempd := toSequence dshuff_(familyi - toList (#familyi: 1));
	 F := inverseMarginMap(i,R1);
	 I = F(I);
         S = markovRing( tempd, VariableName => getSymbol "a");	
	 a = j1 -> S.markovVariables#j1;
	 T := R1**S;
	 newI := sub(I, T);
	 di := toSequence flatten toList append( dshuff_(toList(0..(i-1))), toList ((n-i):1));
	 indexset :=  (n:1)..di;
	 newI = newI + ideal apply(indexset, j ->  (
		  ajindex := toSequence j_(familyi - toList (#familyi: 1));
		  sub(p j,T) - (sum apply(apply(dshuff_(i-1), k -> replace(i-1, k+1, j)), 
			    l-> sub(p l,T)))*sub(a ajindex,T)) );
	 indexset = (#tempd:1)..tempd;
	 newI = newI + ideal apply(indexset, j -> 1 - sum(apply(apply(dshuff_(i-1), k-> replace(#tempd-1,k+1,j)), 
			    l -> sub(a l, T))));
         J := eliminate(flatten entries sub(vars S, T), newI);
	 I = sub(J,R1)        
	 )     
      );
      inverseshuff := toList apply(1..n, i -> pos(shuffle,i));
      q := j -> R.markovVariables#j;
      F1 := map(R,R1, toList apply((n:1)..dshuff, j ->  q (toSequence j_inverseshuff)));
      F1(I)   
)
 
------------------------------------------------------------------
-- trekSeparation MixedGraph
-- NOTE: currently, trekSeparation only works with directed and 
-- bidirected edges. 
------------------------------------------------------------------

trekSeparation = method()
trekSeparation MixedGraph := List => (g) -> (
    -- Not yet implemented for mixedGraphs with undirected edges
    if not g#graph#Graph === graph{} then error "Function not implemented for mixed graphs with undirected edges"; 
    G := graph collateVertices g;
    dd := graph G#Digraph;
    bb := graph G#Bigraph; 
    vv := sort vertices g;
    -- Construct canonical double DAG cdG associated to mixed graph G:
    cdGgraph := hashTable join(
        apply(vv, i -> (1, i) => set join(
                apply(toList parents(G#Digraph,i),j->(1,j)),
                {(2,i)},
                apply(toList bb#i,j->(2,j)))),
        apply(vv,i-> (2,i) => set apply(toList dd#i,j->(2,j))));
    aVertices := apply(vv, i->(1,i));
    bVertices := apply(vv, i->(2,i));
    allVertices := aVertices|bVertices;
    statements := {};
    cdC := new MutableHashTable from apply(allVertices,i->{i,cdGgraph#i});
    for CA in (subsets aVertices) do (
      for CB in (subsets bVertices) do (
	CAbin := setToBinary(aVertices,CA);
	CBbin := setToBinary(bVertices,CB);
	if CAbin <= CBbin then (
          C := CA|CB;
	  scan(allVertices,i->cdC#i=cdGgraph#i);
          scan(C, i->scan(allVertices, j->(
	    cdC#i=cdC#i-{j};
	    cdC#j=cdC#j-{i};)));
	  Alist := delete({},subsetsBetween(CA,aVertices));
          while #Alist > 0 do (
	    minA := first Alist;
	    pC := reachable(digraph cdC,set minA);
	    A := toList ((pC*(set aVertices)) + set CA);
	    Alist = Alist - (set subsetsBetween(minA,A));
	    B := toList ((set bVertices) - pC);
	    -- remove redundant statements:
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


------------------------------------------------------------------
-- trekIdeal (Ring,MixedGraph)
-- trekIdeal (Ring,Graph)
-- trekIdeal (Ring,Digraph)
-- NOTE: We don't work with MixedGraphs in full generality 
-- (undirected, directed, bidirected). See gaussianRing.
------------------------------------------------------------------

trekIdeal = method()
trekIdeal (Ring,MixedGraph) := Ideal => (R,g) -> (
     if not R.?gaussianRingData  then error "expected a ring created with gaussianRing";
     if R.?graphType then (
	  if not sort (vertices (R.graph))  === sort (vertices (g)) 
	  then error "vertex labels of graph do not match labels in ring";
	  if (R.graph#graph#Digraph===digraph{} and R.graph#graph#Bigraph===bigraph{}) 
	  then return trekIdeal(R,R.graph#graph#Graph))
     else if not ( 1..R.gaussianRingData#nn === sort vertices(g))  then 
         error "variables names do not match variable names in the Gaussian ring";
     Stmts:= trekSeparation g;
     vv := sort vertices g;
     SM := covarianceMatrix R ;	
     if Stmts == {} then (
         ideal(0_R))
     else 
        sum apply(Stmts,s->minors(#s#2+#s#3+1, submatrix(SM,apply(s#0,x->pos(vv,x)),apply(s#1,x->pos(vv,x)))))
     )

trekIdeal (Ring,Graph) := Ideal => (R,g) -> (
     conditionalIndependenceIdeal(R,globalMarkov(g)) -- equivalent to trek ideal for undirected graphs
          )

trekIdeal (Ring,Digraph) := Ideal => (R,g) ->(
      trekIdeal (R, mixedGraph(g))
      )          



--********************************************************************************************************************************--
--  Methods for manipulating polynomial maps frequently used in graphical models
--********************************************************************************************************************************--

------------------------------------------------------------------
-- marginMap(ZZ,Ring)
-- Return the ring map F : R --> R such that
--   F p_(u1,u2,..., +, ,un) = p_(u1,u2,..., 1, ,un)
-- and
--   F p_(u1,u2,..., j, ,un) = p_(u1,u2,..., j, ,un), for j >= 2.
-- NOTE:      -- R should be a Markov ring
-----------------------------------------------------------------

marginMap = method()
marginMap(ZZ,Ring) := RingMap => (v,R) -> (
     if (not R.?markovRingData) then error "expected a ring created with markovRing";
     v = v-1;
     d := R.markovRingData;
     p := i -> R.markovVariables#i;
     F := toList apply(((#d):1) .. d, i -> (
	       if i#v > 1 then p i
	       else (
		    i0 := drop(i,1);
		    p i - sum(apply(toList(2..d#v), j -> (
			      newi := join(take(i,v), {j}, take(i,v-#d+1));
			      p newi))))));
     map(R,R,F))



------------------------------------------------------------------
-- inverseMarginMap(ZZ,Ring) 
-- Note: R should be a Markov ring
------------------------------------------------------------------

inverseMarginMap = method()
inverseMarginMap(ZZ,Ring) := RingMap => (v,R) -> (
     if (not R.?markovRingData) then error "expected a ring created with markovRing";
     v = v-1;
     d := R.markovRingData;
     p := i -> R.markovVariables#i;
     F := toList apply(((#d):1) .. d, i -> (
	       if i#v > 1 then p i
	       else (
		    i0 := drop(i,1);
		    p i + sum(apply(toList(2..d#v), j -> (
			      newi := join(take(i,v), {j}, take(i,v-#d+1));
			      p newi))))));
     map(R,R,F))


------------------------------------------------------------------
-- hiddenMap(ZZ,Ring)
-- Creates a ring map for the model where one of the (formerly
-- observed) random variables is now a hidden variable. 
------------------------------------------------------------------

hiddenMap = method()
hiddenMap(ZZ,Ring) := RingMap => (v,A) -> (
     v = v-1;
     p := i -> A.markovVariables#i;
     if not A.?markovRingData then error "expected a ring created with markovRing";
     d := A.markovRingData;
     e := drop(d, {v,v});
      -- issue #1362 in github
     S := markovRing (e, Coefficients=>coefficientRing(A)); 
     -- S := markovRing (e);
     dv := d#v;
     F := toList apply(((#e):1) .. e, i -> (
	       sum(apply(toList(1..dv), j -> (
			      newi := join(take(i,v), {j}, take(i,v-#d+1));
			      p newi)))));
     map(A,S,F))


------------------------------------------------------------------
-- identifyParameters (Ring,MixedGraph)
------------------------------------------------------------------

identifyParameters = method()
identifyParameters Ring := HashTable => R -> (
     if not R.graphType === MixedGraph then error "must be a gaussianRing created with a mixed graph, a digraph or a bigraph";     
     g := R.graph;
     -- Not yet implemented for mixedGraphs with undirected edges
     if not g#graph#Graph === graph{} then error "Function not implemented for mixed graphs with undirected edges";  
     J := ideal unique flatten entries (covarianceMatrix(R)-gaussianParametrization(R));
     G := graph g;
     m := #edges(G#Digraph)+#edges(G#Bigraph)+#vertices(g);
     plvars := toList apply(0..m-1,i->(flatten entries vars R)#i);
     new HashTable from apply(plvars,t->{t,eliminate(delete(t,plvars),J)}))









--******************************************--
-- DOCUMENTATION     	       	    	    -- 
--******************************************--

beginDocumentation()

doc ///
  Key
    GraphicalModels
  Headline
    a package for discrete and Gaussian statistical graphical models 
  Description
  
    Text
      {\bf Graphical Models} is a package for algebraic statistics. It constructs ideals of discrete and 
      Gaussian graphical models. This package supersedes Markov.m2.
       
      This package constructs ideals of discrete Bayesian networks (directed acyclic graphs)
      as described in several places, including the paper: Luis David Garcia, Michael Stillman and Bernd Sturmfels,
      {\em The algebraic geometry of Bayesian networks}, J. Symbolic Comput., 39(3-4):331--355, 2005. 
      
      It also constructs ideals of Gaussian Bayesian networks and Gaussian graphical models 
      (graphs containing both directed and bidirected edges), as described in the papers:
      Seth Sullivant, {\em Algebraic geometry of Gaussian Bayesian networks}, Adv. in Appl. Math. 40 (2008), no. 4, 482--513; and 
      Seth Sullivant, Kelli Talaska and Jan Draisma, "Trek separation for Gaussian graphical models", 
      Annals of Statistics 38 no.3 (2010) 1665--1685. 
          
      The package also contains procedures to solve the identifiability problem for 
      Gaussian graphical models as described in the paper: 
      Luis D. Garcia-Puente, Sarah Spielvogel and Seth Sullivant, {\em Identifying causal effects with computer algebra}, 
      Proceedings of the $26^{th}$ Conference of Uncertainty in Artificial Intelligence.
          
      Furthermore, this package allows to construct the Gaussian rings of 
      loopless mixed graphs (LMG) and the corresponding matrices of indeterminates
      as introduced in Kayvan Sadeghi and Steffen Lauritzen, {\em Markov properties for mixed graphs}, 
      Bernoulli 20.2 (2014): 676-696.
      
      Here is a typical use of this package.  We create the ideal in 16 variables whose zero set 
      represents the probability distributions on four binary random variables  satisfying the
      conditional independence statements coming from the "diamond" graph $4 \to 3, 4 \to 2, 3 \to 1, 2 \to 1$.
      
    Example
       G = digraph  {{1,{}},{2,{1}},{3,{1}},{4,{2,3}}}
       R = markovRing (2,2,2,2) -- this ring corresponds to four binary random variables
       S = globalMarkov G  
       I = conditionalIndependenceIdeal (R,S);
       netList I_*  
       
    Text
      Sometimes an ideal can be simplified by changing variables. For example, conditional independence ideals are often
      transformed to binomial ideals by using @TO marginMap@.
      This is the case here.
      
    Example
       F = marginMap (1,R)        
       J = F I; 
       netList J_*
       
    Text
      This ideal has 5 primary components.  The first component is the one that has statistical significance.
      It is the defining ideal of the variety parameterized by the 
      the factorization of the probability distributions 
      according to the graph $G$. The remaining components lie on the boundary of the simplex.
      
    Example  
      netList primaryDecomposition J
      
    Text
      The ideal in the next example corresponds to a Gaussian graphical model on a graph with directed and bidirected edges.
      The method @TO trekIdeal@ computes the ideal based on the trek separation statements of the mixed graph.
      
    Example
      G = mixedGraph (digraph {{b,{c,d}},{c,{d}}},bigraph {{a,d}})
      R = gaussianRing G
      J = trekIdeal (R,G); 
      J / print;
      
    Text
      The following ideal corresponds to a set of conditional statements of 5 Gaussian random variables.
      
    Example
      R=gaussianRing 5
      S={{{1},{2},{3,4}}, {{2,3},{1},{5}}}
      I=conditionalIndependenceIdeal (R,S);
      I / print;    
      
    Text
      The following people have generously contributed their time and effort to this project:  
      
      Alexander Diaz,
      
      Shaowei Lin<@HREF"http://math.berkeley.edu/~shaowei/"@>,
      
      David Murrugarra<@HREF"http://people.math.gatech.edu/~davidmur/Home.html"@>.
      
  Caveat
     GraphicalModels requires Graphs.m2 and StatGraphs.m2. These packages allow the user to 
     create graphs whose vertices are labeled arbitrarily. 
     However, several functions in GraphicalModels sort the vertices of the graph. 
     Hence, graphs used as input to methods 
     in GraphicalModels must have sortable vertex labels, e.g., 
     all numbers or all letters. 
     
     The methods in GraphicalModels differ in the classes of acceptable graphs for input:
     
     - functions used in package GraphicalModelsMLE (@TO gaussianRing@, 
	 @TO covarianceMatrix@,  @TO bidirectedEdgesMatrix@, @TO directedEdgesMatrix@, 
	 @TO directedEdgesMatrix@, @TO undirectedEdgesMatrix@) and 
         @TO conditionalIndependenceIdeal@ accept @TO Graph@,  
         @TO Digraph@, @TO Bigraph@ and @TO MixedGraph@.  
     
     - conditional independence statement generators (@TO pairMarkov@, 
	 @TO localMarkov@ and @TO globalMarkov@) accept only @TO Graph@ or  
         @TO Digraph@;
    	
     - the remaining functions that accepts graphs, only accept @TO Graph@,  
         @TO Digraph@ or @TO MixedGraph@ without undirected edges. 	 
	 
	   	
///;

--------------------------------
-- Documentation pairMarkov ----
--------------------------------

doc ///
  Key
    pairMarkov
    (pairMarkov,Graph)
    (pairMarkov,Digraph)
  Headline
    pairwise Markov statements for a graph or a  directed graph
  Usage
    pairMarkov G
  Inputs
    G: 
      @ofClass {Graph,Digraph}@
  Outputs
    :List
      whose entries are triples $\{A,B,C\}$ representing pairwise Markov  conditional independence statements of the form
      ``$A$ is independent of $B$ given $C$'' that hold for $G$.
  Description
  
    Text
      Given an undirected graph $G$, pairwise Markov statements are statements of the form 
      \{$v$, $w$, all other vertices\}\   
      for each pair of non-adjacent vertices $v$ and $w$ of $G$.
      
      For example, for the undirected 5-cycle graph $G$, that is, the graph on $5$ vertices with edges 
      $a---b---c---d---e---a$, 
      we get the following pairwise Markov statements:
      
    Example
      G = graph({{a,b},{b,c},{c,d},{d,e},{e,a}})
      pairMarkov G
      
    Text
      Given a directed acyclic graph $G$, pairwise Markov statements are statements of the form \{$v$, $w$, nondescendents($G,v$)-$w$\}\ 
      for each vertex $v$ of $G$ and each non-descendent vertex $w$ of $v$. In other words, for every vertex $v$ of $G$ and each nondescendent $w$ of $v$, 
      this method returns the statement: $v$ is independent of $w$ given all other nondescendents. 
      
      For example, given the digraph $D$ on $7$ vertices with edges $1 \to 2, 1 \to 3, 2 \to 4, 2 \to 5, 3 \to 5, 3 \to 6, 4 \to 7, 5 \to 7$, and $6\to 7$, 
      we get the following pairwise Markov statements:
      
    Example
      D = digraph {{1,{2,3}}, {2,{4,5}}, {3,{5,6}}, {4,{7}}, {5,{7}},{6,{7}},{7,{}}}
      netList pack (3, pairMarkov D)
      
    Text
      This method displays only non-redundant statements. In general, given a set $S$  of conditional independent 
      statements and a statement $s$, then we say that $s$ is a a redundant statement if $s$ can be obtained from the 
      statements in $S$ using the semigraphoid axioms of conditional independence: symmetry, decomposition, weak 
      union,  and contraction as described in Section 1.1 of Judea Pearl, {\em Causality: models, reasoning, and inference}, 
      Cambridge University Press.  We do not use the intersection axiom since it is only valid for strictly positive 
      probability distributions.
      
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
    (localMarkov,Graph)
    (localMarkov,Digraph)
  Headline
    local Markov statements for a graph or a directed graph
  Usage
    localMarkov G
  Inputs
    G:
      @ofClass {Graph,Digraph}@ 
  Outputs
    :List
      whose entries are triples $\{A,B,C\}$ representing local Markov  conditional independence statements of the form
      ``$A$ is independent of B given C'' that hold for G.
  Description
  
    Text
      Given an undirected graph $G$, a local Markov statement is of the form
      \{$v$, non-neighbours($v$), neighbours($v$)\} .
      That is, 
      every vertex $v$ of $G$ is independent of its non-neighbours given its neighbours.
      
      For example, for the undirected  5-cycle graph $G$, that is, the graph on 5 vertices with 
      $a---b---c---d---e---a$, 
      we get the following local Markov statements:
      
    Example
      G = graph({{a,b},{b,c},{c,d},{d,e},{e,a}})
      localMarkov G
      
    Text
      Given a directed acyclic graph $G$, local Markov statements are of the form
      \{$v$, nondescendents($v$) - parents($v$), parents($v$)\} .
      In other words, 
      every vertex $v$ of $G$ is independent of its nondescendents (excluding parents) given its parents. 
      
      For example, given the digraph $D$ on $7$ vertices with edges $1 \to 2, 1 \to 3, 2 \to 4, 2 \to 5, 3 \to 5, 3 \to 6, 4 \to 7, 5 \to 7$, and $6\to 7$, 
      we get the following local Markov statements:
      
    Example
      D = digraph {{1,{2,3}}, {2,{4,5}}, {3,{5,6}}, {4,{7}}, {5,{7}},{6,{7}},{7,{}}}
      netList pack (3, localMarkov D) 
      
    Text
      This method displays only non-redundant statements. In general, given a set $S$  of conditional independent 
      statements and a statement $s$, then we say that $s$ is a a redundant statement if $s$ can be obtained from the 
      statements in $S$ using the semigraphoid axioms of conditional independence: symmetry, decomposition, weak 
      union,  and contraction as described in Section 1.1 of Judea Pearl, {\em Causality: models, reasoning, and inference}, 
      Cambridge University Press.  We do not use the intersection axiom since it is only valid for strictly positive 
      probability distributions.
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
    (globalMarkov,Graph)
  Headline
    global Markov statements for a graph or a directed graph
  Usage
    globalMarkov G
  Inputs
    G:
      @ofClass {Graph,Digraph}@     
  Outputs
    :List
      whose entries are triples {A,B,C} representing global Markov  conditional independence statements of the form
      ``A is independent of B given C'' that hold for G.
  Description
  
    Text
      Given an undirected graph $G$, a global Markov statement is of the form
      $\{A, B, C\}$, where the subset $C$ separates the subset $A$ from the subset $B$ in the graph $G$.
      
      For example, for the undirected  5-cycle graph $G$, that is, the graph on 5 vertices with 
      $a---b---c---d---e---a$, 
      we get the following global Markov statements:
      
    Example
      G = graph({{a,b},{b,c},{c,d},{d,e},{e,a}})
      globalMarkov G
      
    Text
      Given a directed acyclic graph $G$, global Markov states that      
      $A$ is independent of $B$ given $C$ for every triple of sets of vertices $A$, $B$, and $C$, 
      such that $A$ and $B$ are $d$-separated by $C$ (in the graph $G$).\break
       
      The global independent statements of a directed graph are computed using the Bayes-Ball algorithm,
      as described in the paper Ross D. Shachter, {\em Bayes-Ball: The Rational Pastime (for Determining Irrelevance and 
      Requisite Information in Belief Networks and Influence Diagrams)}  In Proceedings of the Fourteenth Conference in 
      Uncertainty in Artificial Intelligence, p. 480--487, 1998.
      
      For example, given the digraph $D$ on $7$ vertices with edges $1 \to 2, 1 \to 3, 2 \to 4, 2 \to 5, 3 \to 5, 3 \to 6, 4 \to 7, 5 \to 7$, and $6\to 7$, 
      we get the following global Markov statements:
      
    Example
      D = digraph {{1,{2,3}}, {2,{4,5}}, {3,{5,6}}, {4,{7}}, {5,{7}},{6,{7}},{7,{}}}
      netList pack (3, globalMarkov D) 
      
    Text
      This method displays only non-redundant statements. In general, given a set $S$  of conditional independent 
      statements and a statement $s$, then we say that $s$ is a a redundant statement if $s$ can be obtained from the 
      statements in $S$ using the semigraphoid axioms of conditional independence: symmetry, decomposition, weak 
      union,  and contraction as described in Section 1.1 of Judea Pearl, {\em Causality: models, reasoning, and inference}, 
      Cambridge University Press.  We do not use the intersection axiom since it is only valid for strictly positive 
      probability distributions.
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
    linear map on joint distributions for discrete random variables replacing marginals for indeterminates
  Usage
    marginMap(i,R)
  Inputs
    i:ZZ
      the index of the variable on which to perform the `marginalization trick'
    R:Ring
      a markovRing
  Outputs
    :RingMap
  Description
    Text
      The ring $R$ must be a ring of probability distributions on $n$ random variables created using @TO markovRing@. The integer $i$
      must be in the range from 1 to $n$.  
       
      Let $p_{u_1,u_2,\dots, +,\dots,u_n}$ denote the linear form $p_{u_1,u_2,\dots, 1,\dots,u_n} + \dots + p_{u_1,u_2,\dots, d_i,\dots,u_n}$, where $d_i$ is the number of
      states of random variable $X_i$.
      
      The method {\tt marginMap} returns a ring map $F : R \to R$ such that after applying $F$, the indeterminate
      $p_{u_1,u_2,\dots,1,\dots,u_n}$ refers to $ p_{u_1,u_2,\dots, +,\dots,u_n}$, where the '1' and the '$+$' are
      in the $i$th spot. 
      
      Further $F$ is the identity on all other indeterminates, that is, 
      $ F(p_{u_1,u_2,\dots, j,\dots,u_n}) = p_{u_1,u_2,\dots, j,\dots,u_n} $, for all $j\geq 2$.
      
    Example   
      F = marginMap(1,markovRing(3,2));
      compactMatrixForm =false;
      transpose F.matrix
      
    Text
      This linear transformation simplifies ideals and/or polynomials involving 
      $ p_{u_1,u_2,..., +,...,u_n} $. Sometimes, the resulting ideals are toric 
      ideals as the example below shows. For more details 
      see the paper "Algebraic Geometry of Bayesian Networks" by Garcia, Stillman, and
      Sturmfels.
      
    Example
      G = digraph  {{1,{}},{2,{1}},{3,{1}},{4,{2,3}}}
      R = markovRing (2,2,2,2)
      S = globalMarkov G
      I = conditionalIndependenceIdeal (R,S);
      I / print;	
      F = marginMap(1,R);
      transpose F.matrix
      J = F I;  
      J / print;
      
  SeeAlso
    hiddenMap 
    inverseMarginMap
///

--------------------------------
-- Documentation inverseMarginMap    --
--------------------------------

doc ///
  Key
    inverseMarginMap
    (inverseMarginMap,ZZ,Ring)
  Headline
    inverse of the marginMap
  Usage
    inverseMarginMap(i,R)
  Inputs
    i:ZZ
      the index of the variable for which to undo the "margin trick"
    R:Ring
      a markovRing
  Outputs
    :RingMap
  Description     
    Text
      This method computes the inverse of the @TO marginMap@.
      
    Example
      R = markovRing (3,2)
      F = marginMap(1,R) 
      G = inverseMarginMap(1,R)
      gens R
      F*G -- we see that the composition is the identity map:
      
  SeeAlso
    hiddenMap
    marginMap      
///
  
--------------------------------
-- Documentation hiddenMap    --
--------------------------------

doc ///
  Key
    hiddenMap
    (hiddenMap,ZZ,Ring)
  Headline
    linear map between the ring of a model with one hidden variable and the ring of the corresponding fully observed model
  Usage
    hiddenMap(i,R)
  Inputs
    i:ZZ
      the index corresponding to the hidden random variable
    R:Ring
      a markovRing
  Outputs
    :RingMap
  Description
    Text
      The ring $R$ is  a ring of probability distributions on $n$ random variables created using {\tt markovRing}.
      This method creates a ring map $F: S \to R$ from the ring $S$ of probability distributions on $n-1$ 
      random variables, leaving out the $i$th random variable from $R$. This corresponds to the situation where
      the $i$th random variable is hidden and $S$ is the ring of {\bf observed} probability distributions. 
     
    Example  
      F = hiddenMap(1,markovRing(2,3,2));
      compactMatrixForm =false;
      transpose F.matrix 
      
    Text  
      This method is frequently used when computing the vanishing ideal of a graphical model 
      with hidden variables by computing the kernel of $F$.
      For more details see the paper ``Algebraic Geometry of Bayesian Networks''
      by Garcia, Stillman, and Sturmfels.
      
    Example
      G = digraph  {{1,{}},{2,{}},{3,{}},{4,{1,2,3}}}
      R = markovRing (2,2,3,2)
      I = discreteVanishingIdeal (R,G);
      I / print;
      S = markovRing(2,2,3)
      F = hiddenMap(4,R);
      transpose F.matrix
      J = preimage (F, I);
      J / print;
      
  SeeAlso
    marginMap
///

------------------------------------
-- Documentation Coefficients     --
------------------------------------

doc ///
  Key
    Coefficients
  Headline
    optional input to choose the base field
  Description
    Text
      Put {\tt Coefficients => r} for a choice of ring(field) r as an argument in 
      the function @TO markovRing@ or @TO gaussianRing@ 
  SeeAlso
    markovRing
    gaussianRing
///


doc ///
  Key
    [markovRing, Coefficients]
    [gaussianRing, Coefficients]
  Headline
    optional input to choose the base field in markovRing or gaussianRing
  Usage
    gaussianRing(n,Coefficients=>Ring)
    gaussianRing(G,Coefficients=>Ring)  
    markovRing(d,Coefficients=>Ring)  
  Inputs
    d:Sequence
      with positive integer entries $(d_1,\dots ,d_r)$
    n:ZZ
      number of random variables
    G:Graph
      @ofClass Graph@, or @ofClass Digraph@, 
      or @ofClass MixedGraph@ with directed and bidirected edges
  Outputs
    :Ring       
  Description
    Text
      In both {\tt markovRing} and {\tt gaussianRing}, the default coefficient ring is QQ.
      Putting {\tt Coefficients => r} for a choice of ring(field) r as an argument in 
      the function @TO markovRing@ or @TO gaussianRing@ creates a ring with the
      desired coefficient ring.

    Example
      R2 = markovRing ((2,2),Coefficients=>CC); 
      coefficientRing R2

  SeeAlso
    markovRing
    gaussianRing
///





--------------------------------
-- Documentation markovRing   --
--------------------------------

doc ///
  Key
    markovRing
    (markovRing, Sequence)
  Headline
    ring of joint probability distributions on several discrete random variables
  Usage
    markovRing(d)
  Inputs
    d:Sequence
      with positive integer entries $(d_1,\dots ,d_r)$
  Outputs
    :Ring  
      a polynomial ring with $d_1*d_2*\dots   *d_r$ variables $p_{i_1,\dots ,i_r}$,
      with each $i_j$ satisfying $1\leq i_j \leq d_j$.
  Consequences
    Item
      Information about this sequence of integers is placed into the ring, and is used 
      by other functions in this package.  Also, at most one ring for each such sequence
      is created since the ring is  cached.
  Description
    Text 
      The sequence $d$ represents the number of states of each discrete random variable. 
      This example creates a ring of joint probability distributions on 4 random
      variables with 2, 3, 4, and 5 states. This ring has a total of 120 indeterminates.
      
    Example
      d=(2,3,4,5);
      R = markovRing d;
      numgens R
      R_0, R_1, R_119 --here are some of the variables in the ring
      
    Text
      If no coefficient choice is specified, the polynomial ring is created over the rationals. 
      
    Example
      coefficientRing R
      
    Text 
      The optional argument @TO Coefficients@ allows to change the base field.
 
    Example
      R2 = markovRing (d,Coefficients=>CC); 
      coefficientRing R2
      
    Text
      The indeterminates are labeled with the letter ''p'' suggesting probability distributions. However,
      it is useful to be able to create a new ring where the indeterminates are labeled different (for example, 
      they may represent marginal probabilities). This can be accomplished
      with the @TO VariableName@ option.
      
    Example
      d=(1,2);
      markovRing (d,VariableName => q);
      gens oo 
      
    Text
      The routines  @TO hiddenMap@, 
      @TO inverseMarginMap@, @TO marginMap@, @TO markovMatrices@ require the ring to be created by this function. 
      The routines @TO conditionalIndependenceIdeal@, @TO discreteVanishingIdeal@ require the ring to be created by this function or 
      the method @TO gaussianRing@.
     
  Caveat
     As opposed to @TO gaussianRing@, this method does not store information about a graph or the names of the random variables.
     In case these random variables are not numbered $1, 2, \dots, n$, then the methods @TO conditionalIndependenceIdeal@ and
     @TO markovMatrices@ require an additional input in the form of a list of the random variable names. This list must be in the same
     order as the implicit order used in the sequence $d$. The user is encouraged to read the caveat on the method 
     @TO conditionalIndependenceIdeal@ regarding probability distributions on discrete random variables that have 
     been labeled arbitrarily.
      
  SeeAlso
    conditionalIndependenceIdeal 
    discreteVanishingIdeal 
    gaussianRing 
    hiddenMap 
    inverseMarginMap 
    marginMap 
    markovMatrices
///


------------------------------------
-- Documentation VariableName     --
------------------------------------

doc ///
  Key
    VariableName
  Headline
    optional input to choose indeterminate name in markovRing
  Description
    Text
      The option {\tt VariableName => q} changes the symbol used for intedeterminates in a polynomial ring created with @TO markovRing@.
  SeeAlso
    markovRing
///

doc ///
  Key
    [markovRing, VariableName]
  Headline
    symbol used for indeterminates in a ring of discrete joint probability distributions
  Usage
    markovRing (d,VariableName => q)
  Inputs
    q:
      @ofClass Symbol@ or @ofClass String@
  Description
    Text
      The indeterminates in the polynomial ring made by {\tt markovRing} are labeled with the letter ''p'' suggesting 
      probability distributions. However, it is useful to be able to create a new ring where the indeterminates are 
      labeled different (for example, they may represent marginal probabilities). 
 
    Example
      d=(1,2);
      markovRing (d,VariableName => q);
      gens oo 

///

------------------------------------
-- Documentation markovMatrices   --
------------------------------------

doc ///
  Key
    markovMatrices
    (markovMatrices,Ring,List)
    (markovMatrices,Ring,List,List) 
  Headline
    matrices whose minors form the ideal of a list of independence statements
  Usage
    markovMatrices(R,S)
    markovMatrices(R,S,VarNames)
  Inputs
    R:Ring
      R must be a markovRing
    S:List 
      list of conditional independence statements among discrete random variables. 
    VarNames:List
      list of names of the random variables in the statements of $S$.  If this is omitted 
      it is assumed that these are integers in the range from 1 to $n$ where $n$ is the number of 
      random variables in the declaration of markovRing. 
    
  Outputs
    :List 
      list whose elements are instances of Matrix. 
  Description
    Text
      List of matrices whose 2x2 minors form the conditional independence ideal of the independence statements on the list $S$. 
      This method is used in @TO conditionalIndependenceIdeal@,  it is exported to be able to read independence constraints  
      as minors of matrices instead of their polynomial expansions. 
      
    Example
      S = {{{1},{3},{4}}}
      R = markovRing (4:2)
      compactMatrixForm =false;
      netList markovMatrices (R,S) 
      
    Text
      Here is an example where the independence statements are extracted from a graph.
      
    Example  
      G = graph{{a,b},{b,c},{c,d},{a,d}}
      S = localMarkov G
      R = markovRing (4:2)
      markovMatrices (R,S,vertices G)   
      
  Caveat
     In case the random variables are not numbered $1, 2, \dots, n$, then this method requires an additional input 
     in the form of a list of the random variable names. This list must be in the same
     order as the implicit order used in the sequence $d$. The user is encouraged to read the caveat on the method 
     @TO conditionalIndependenceIdeal@ regarding probability distributions on discrete random variables that have 
     been labeled arbitrarily.
      
  SeeAlso
    conditionalIndependenceIdeal 
    markovRing
///

------------------------------------
-- Documentation gaussianRing     --
------------------------------------

doc ///
  Key 
    gaussianRing
  Headline
    ring of Gaussian correlations on n random variables or a graphical model
  Usage
    gaussianRing n 
    gaussianRing G 
  Inputs
    n:ZZ
      the number of random variables
    G:Graph
      or @ofClass Digraph@, or @ofClass Bigraph@, or @ofClass MixedGraph@ 
  Outputs
    :Ring
      a polynomial ring with indeterminates associated to the graphical model      
  Description
    Text
      This function creates a ring whose indeterminates are the covariances of an 
      n dimensional Gaussian random vector.  Using a graph, digraph, or mixed graph $G$
      as input gives a {\tt gaussianRing} with extra indeterminates related to the parametrization
      of the graphical model associated to that graph. 
      Check the details of the {\tt gaussianRing} for each type of input:
     
      * @TO (gaussianRing,ZZ)@
      
      * @TO (gaussianRing,Graph)@
      
      * @TO (gaussianRing,Digraph)@
      
      * @TO (gaussianRing,Bigraph)@
      
      * @TO (gaussianRing,MixedGraph)@
      
      The indeterminates of the ring - $s_{(i,j)},k_{(i,j)},l_{(i,j)},p_{(i,j)}$ - can be placed into an appropriate matrix format using the
      functions @TO covarianceMatrix@, 
      @TO undirectedEdgesMatrix@, @TO directedEdgesMatrix@, and  
      @TO bidirectedEdgesMatrix@ respectively.
      
      The variable names that appear can be changed using the options sVariableName, lVariableName,
      pVariableName, and kVariableName

    Example
      G = mixedGraph(digraph {{b,{c,d}},{c,{d}}},bigraph {{a,d}})
      R = gaussianRing (G,pVariableName => psi)
      gens R      
            
    Text
      The routines  @TO conditionalIndependenceIdeal@, @TO trekIdeal@, @TO covarianceMatrix@, 
      @TO undirectedEdgesMatrix@, @TO directedEdgesMatrix@, @TO bidirectedEdgesMatrix@, 
      @TO gaussianVanishingIdeal@ and @TO gaussianParametrization@ require that the 
      ring be created by this function. 

  SeeAlso
    bidirectedEdgesMatrix
    conditionalIndependenceIdeal
    covarianceMatrix
    directedEdgesMatrix
    gaussianVanishingIdeal
    trekIdeal
    undirectedEdgesMatrix
///

doc ///
  Key 
    (gaussianRing,ZZ)
  Headline
    ring of Gaussian correlations on n random variables
  Usage
    gaussianRing n  
  Inputs
    n:ZZ
      the number of random variables
  Outputs
    :Ring
      a ring with indeterminates $s_{(i,j)}$ for $1 \leq i \leq j \leq n$      
  Description
    Text
     This function creates a polynomial ring with indeterminates $s_{(i,j)}$ for $1 \leq i \leq j \leq n$.
     The $s_{(i,j)}$ indeterminates in the {\tt gaussianRing} are the entries in the
     covariance matrix of the jointly normal random variables.  
      
    Example
      R = gaussianRing 5;
      gens R
      compactMatrixForm =false;
      covarianceMatrix R

  SeeAlso
    gaussianRing   
///

doc ///
  Key 
    (gaussianRing, Graph)
  Headline
    ring of Gaussian correlations of a graphical model coming from an undirected graph
  Usage
    gaussianRing G 
  Inputs
    G:Graph
  Outputs
    :Ring
      a polynomial ring with indeterminates $s_{(i,j)}$ and $k_{(i,j)}$      
  Description
    Text
     This function creates a polynomial ring with indeterminates $s_{(i,j)}$ for $1 \leq i \leq j \leq n$,
     where $n$ is the number of vertices in $G$, and  $k_{(i,j)}$.
     
     The $s_{(i,j)}$ indeterminates in the {\tt gaussianRing} are the entries in the
     covariance matrix of the jointly normal random variables.  
       
     The $k_{(i,j)}$ indeterminates in the {\tt gaussianRing} are the nonzero entries in the concentration
     matrix in the graphical model associated to the undirected graph.
    
    Example
      G = graph({{a,b},{b,c},{c,d},{a,d}})
      R = gaussianRing G
      gens R
      covarianceMatrix R
      undirectedEdgesMatrix R
  SeeAlso
     gaussianRing 
///

doc ///
  Key 
    (gaussianRing, Bigraph)
  Headline
    ring of Gaussian correlations of a graphical model coming from a bigraph
  Usage
    gaussianRing G 
  Inputs
    G:Bigraph
  Outputs
    :Ring
      a ring with indeterminates $s_{(i,j)}, p_{(i,j)}$       
  Description
    Text
      A {\tt gaussianRing} of a bidirected graph is built 
      as a {\tt gaussianRing} of a mixed graph with only bidirected edges, see @TO (gaussianRing,MixedGraph)@.

    Example
      G = bigraph {{a,{b,c}}, {b,{c,d}}, {c,{}}, {d,{}}};
      R = gaussianRing G;
      gens R
      covarianceMatrix R
      directedEdgesMatrix R
      bidirectedEdgesMatrix R
  
  SeeAlso
     gaussianRing   
///

doc ///
  Key 
    (gaussianRing, Digraph)
  Headline
    ring of Gaussian correlations of a graphical model coming from a digraph
  Usage
    gaussianRing G 
  Inputs
    G:Digraph
  Outputs
    :Ring
      a polynomial ring with indeterminates $s_{(i,j)},l_{(i,j)}, p_{(i,j)}$ .      
  
  Description
    Text
      This function creates a polynomial ring in the indeterminates $s_{(i,j)}$ associated to the 
      covariance matrix of the model plus two new lists of indeterminates:
      
      - The $l_{(i,j)}$ indeterminates consist of regression coefficients associated to the directed
      edges in the graph.
      
      - The $p_{(i,j)}$ indeterminates  in the {\tt gaussianRing} are the nonzero entries in the covariance matrix of the error terms
      in the graphical model associated to a mixed graph with bidirected edges. 
      
      Note that since version 2.0 of the package, 
      {\tt gaussianRing} of a directed graph is built as a {\tt gaussianRing} of a mixed graph with only directed edges, see @TO (gaussianRing,MixedGraph)@.

    Example
      G = digraph {{a,{b,c}}, {b,{c,d}}, {c,{}}, {d,{}}};
      R = gaussianRing G;
      gens R
      covarianceMatrix R
      directedEdgesMatrix R
      bidirectedEdgesMatrix R

///

doc ///
  Key 
    (gaussianRing, MixedGraph)
  Headline
    ring of Gaussian correlations of a graphical model coming from a mixed graph
  Usage 
    gaussianRing G 
  Inputs
    G:MixedGraph
  Outputs
    :Ring
      a polynomial ring with indeterminates $s_{(i,j)},k_{(i,j)},l_{(i,j)},p_{(i,j)}$ 
  Description
    Text
      This function accepts a mixed graph as input. The outputted ring contains the indeterminates $s_{(i,j)}$ associated to the 
      covariance matrix of the model plus two or three new lists of indeterminates depending on the type of edges of the graph:
      
      - The $k_{(i,j)}$ indeterminates in the {\tt gaussianRing} are the nonzero entries in the concentration
      matrix in the graphical model associated to the undirected graph.
    
      - The $l_{(i,j)}$ indeterminates consist of regression coefficients associated to the directed
      edges in the graph.
      
      - The $p_{(i,j)}$ indeterminates  in the {\tt gaussianRing} are the nonzero entries in the covariance matrix of the error terms
      in the graphical model associated to a mixed graph with bidirected edges. 
      
      Mixed graphs in this package can be of two different types depending on their edges:
      
      {\bf Directed and bidirected edges}: two new lists of indeterminates. For each  directed edge $i \to j$ 
      in the mixed graph there is an indeterminate, denoted by default $l_{(i,j)}$, corresponding to the associated direct causal effect parameter in the model. 
      For each  bidirected edge $i$<->$j$ there is an indeterminate, denoted by default $p_{(i,j)}$, corresponding to the associated noise parameter. Finally,
      for each node $i$, there is an indeterminate $p_{(i,i)}$. 
     
    Example
      G = mixedGraph(digraph {{b,{c,d}},{c,{d}}},bigraph {{a,d}})
      R = gaussianRing G
      gens R
      covarianceMatrix R
      directedEdgesMatrix R
      bidirectedEdgesMatrix R
    
    Text
      {\bf Undirected, directed and bidirected edges}: three new lists of indeterminates. Besides the two already described above, 
      undirected edges are dealt with in the same way as in {\tt gaussianRing} applied to @ofClass Graph@, 
      with the corresponding indeterminates being $k_{(i,j)}$ by default.
      
      Only loopless mixed graphs are accepted and they must have a vertex ordering compatible with @TO partitionLMG@.
      For more details about loopless mixed graphs, see the paper: 
      Kayvan Sadeghi and Steffen Lauritzen, {\em Markov properties for mixed graphs}, Bernoulli, 20 (2014), no 2, 676-696.
      
      Be aware that several functions in this package that accept mixed graphs are still not implemented for mixed graphs with undirected edges: @TO gaussianParametrization@,
      @TO gaussianVanishingIdeal@, @TO trekIdeal@, @TO trekSeparation@, @TO identifyParameters@.
      
    Example
      G = mixedGraph(digraph {{1,3},{2,4}},bigraph {{3,4}},graph {{1,2}});
      R = gaussianRing G
      gens R
      covarianceMatrix R
      undirectedEdgesMatrix R
      directedEdgesMatrix R
      bidirectedEdgesMatrix R

  SeeAlso
     gaussianRing 

  
///
---------------------------------------
-- Documentation gaussianMatrices    --
---------------------------------------

doc///
   Key
     gaussianMatrices
     (gaussianMatrices,Ring,List)
   Headline
     matrices whose minors generate the Gaussian conditional independence ideal
   Usage
     gaussianMatrices(R,S)
   Inputs
     R:Ring
       must be a gaussianRing
     S:List
       of conditional independence statements
   Outputs
     :Matrix
       whose minors generate the Gaussian conditional independence ideal
   Description 
   
     Text
       This method displays a list of matrices whose minors generate the  Gaussian 
       conditional independence ideal.  It is called as a subroutine in @TO conditionalIndependenceIdeal@
       but it is useful to list these matrices explicitly.

     Example
       R = gaussianRing 4;
       Stmts = {{{1,2},{3},{4}}, {{1},{3},{}}}
       compactMatrixForm =false;
       gaussianMatrices(R,Stmts)

   SeeAlso
     gaussianRing
     conditionalIndependenceIdeal
///

---------------------------------------
-- Documentation covarianceMatrix    --
---------------------------------------

doc/// 
   Key
     covarianceMatrix
     (covarianceMatrix,Ring)
   Headline
     covariance matrix of a Gaussian graphical model
   Usage
     covarianceMatrix R
   Inputs
     R:Ring
       which should be a gaussianRing
   Outputs
     :Matrix
       the $n \times{} n$ covariance matrix of the Gaussian graphical model.  
   Description 
   
     Text
       This method returns the $n \times{} n$ covariance matrix of the Gaussian graphical model
       where $n$ is the number of random
       variables in the model.  If the gaussianRing was created
       using a graph, $n$ will be the number of vertices of the graph.
       If this function is called without a graph $G$, it is assumed that $R$ is the {\tt gaussianRing} of a directed acyclic graph.

     Example
       compactMatrixForm =false;
       covarianceMatrix gaussianRing 4
       G = digraph {{a,{b,c}}, {b,{c,d}}, {c,{}}, {d,{}}}
       R = gaussianRing G
       S = covarianceMatrix R

     Text
       This function also works for {\tt gaussianRings} created with a {\tt graph} or {\tt mixedGraph}.

     Example
       G = graph({{a,b},{b,c},{c,d},{a,d}})
       R = gaussianRing G
       S = covarianceMatrix R      
       G = mixedGraph(digraph {{b,{c,d}},{c,{d}}},bigraph {{a,d}})
       R = gaussianRing G
       S = covarianceMatrix R

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
     (bidirectedEdgesMatrix,Ring)
   Headline
     matrix corresponding to the bidirected edges of a bigraph or a mixed graph
   Usage
     bidirectedEdgesMatrix R
   Inputs
     R:Ring
       which should be a gaussianRing created with @ofClass Bigraph@ or @ofClass MixedGraph@
   Outputs
     :Matrix
       the $n \times{} n$ covariance matrix of the noise variables in the Gaussian graphical model of a mixed graph.
   Description 
    
     Text
       This method returns the $n \times{} n$ covariance matrix of the noise variables in the Gaussian graphical model.
       The diagonal in this matrix consists of the indeterminates  $p_{(i,i)}$. Each off-diagonal entry is zero unless 
       there is a bidirected edge between i and j in which case the corresponding entry in the matrix is the indeterminate
       $p_{(i,j)}$. The documentation of @TO gaussianRing@ 
       further describes the indeterminates $p_{(i,j)}$.
       
     Example
       G = mixedGraph(digraph {{b,{c,d}},{c,{d}}},bigraph {{a,d}})
       R = gaussianRing G
       compactMatrixForm =false;
       bidirectedEdgesMatrix R
     
     Text
       For mixed graphs that also have undirected edges, 
       the size of the matrix coincides with the number of elements in @TO compW@,
       which depends on the vertex partition built in @TO partitionLMG@.
     Example
      G = mixedGraph(digraph {{1,3},{2,4}},bigraph {{3,4}},graph {{1,2}});
      R = gaussianRing G
      bidirectedEdgesMatrix R

     Text
       Bidirected graphs can also be considered:   
     Example
       G = bigraph {{a,d},{b},{c}}
       R = gaussianRing G
       bidirectedEdgesMatrix R

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
     (directedEdgesMatrix,Ring)
   Headline
     matrix corresponding to the directed edges of a digraph or a mixed graph
   Usage
     directedEdgesMatrix R
   Inputs
     R:Ring
       which should be a gaussianRing created with @ofClass Digraph@ or @ofClass MixedGraph@
   Outputs
     :Matrix
       the $n \times{} n$ matrix of direct causal effect indeterminates. 
   Description 
     Text
       This method returns the  $n \times{} n$ matrix of direct causal effect indeterminates. 
       This matrix has the parameter $l_{(i,j)}$ in the $(i,j)$ position
       if there is a directed edge $i \to j$, and 0 otherwise.
       Note that this matrix is not symmetric.
       The documentation of @TO gaussianRing@ 
       further describes the indeterminates $l_{(i,j)}$.

     Example
       G = mixedGraph(digraph {{b,{c,d}},{c,{d}}},bigraph {{a,d}})
       R = gaussianRing G
       compactMatrixForm =false;
       directedEdgesMatrix R

     Example
       D = digraph{{a,b},{c,d}}
       directedEdgesMatrix gaussianRing D

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
     (gaussianParametrization,Ring)
   Headline
     parametrization of the covariance matrix in terms of treks
   Usage
     M = gaussianParametrization(R)
   Inputs
     R:Ring
       which should be a gaussianRing of a mixed graph without undirected edges
   Outputs
     M:Matrix
       the parametrization of the covariance matrix in terms of treks
   Description 
     Text
       Given a mixed graph $G$ with directed and bidirected edges, let $L$ be the matrix corresponding to 
       the directed edges (see @TO directedEdgesMatrix@) and let $W$ be the matrix corresponding to 
       the bidirected edges (see @TO bidirectedEdgesMatrix@). Then, the covariance matrix $S$ 
       (see @TO covarianceMatrix@) of the random variables in the Gaussian graphical model corresponding
       to the mixed graph $G$ can be parametrized by the matrix equation $S = (I-L)^{-T}W(I-L)^{-1}$, where
       $I$ is the identity matrix.
       
       The entry $s_{(i,j)}$ of the covariance matrix can also be written as the sum of all monomials corresponding
       to treks between vertices $i$ and $j$. See @TO trekSeparation@ for the definition of a trek. The monomial corresponding
       to a trek is the product of all parameters associated to the directed and bidirected edges on the trek.
       
       The following example shows how to compute the ideal of the model using the parametrization,
       which could also be computed using @TO gaussianVanishingIdeal@

     Example
       G = mixedGraph(digraph {{b,{c,d}},{c,{d}}},bigraph {{a,d}})
       R = gaussianRing G
       compactMatrixForm =false;
       S = covarianceMatrix(R)
       L = directedEdgesMatrix(R)
       W = bidirectedEdgesMatrix(R)       
       M = gaussianParametrization(R)
       J = delete(0_R, flatten entries (L|W))
       eliminate(J, ideal(S-M))
       gaussianVanishingIdeal(R)
       
     Text
       This next example shows how to use the option @TO SimpleTreks@ to compute a parametrization using simple treks 
       instead of all treks. The resulting covariance matrix has diagonal entries equal to 1.  This is
       giving a parametrization of all correlation matrices of matrices that belong to the model.  This
       formulation is also known as Wright's method of path analysis.

     Example
       G = mixedGraph(digraph {{b,{c,d}},{c,{d}}},bigraph {{a,d}})
       R = gaussianRing G
       M = gaussianParametrization(R,SimpleTreks=>true)

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
    optional input for gaussianParametrization
  Description
    Text
      This is an option to tell @TO gaussianParametrization@ to use simple treks.  false
      is the default option.

  SeeAlso
    gaussianParametrization
///

doc/// 
   Key
     [gaussianParametrization, SimpleTreks]
   Headline
     optional input for gaussianParametrization
   Usage
     M = gaussianParametrization(R,SimpleTreks => true)
   Inputs
     R:Ring
       which should be a gaussianRing
   Outputs
     M:Matrix
       the parametrization of the covariance matrix in terms of treks
   Description 
     Text
       Put {\tt SimpleTreks => true} as an argument in the function @TO gaussianParametrization@ to compute 
       a parametrization of the covariance matrix $S=(s_{(i,j)})$ where $s_{(i,j)}$ is the sum of monomials corresponding
       to simple treks between vertices $i$ and $j$. Here, a simple trek is a trek $(P_L,P_R)$ where the paths $P_L$ and $P_R$ 
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
     (identifyParameters,Ring)
   Headline
     solve the identifiability problem for Gaussian graphical models 
   Usage
     H = identifyParameters(R)
   Inputs
     R:Ring
       which should be a gaussianRing created with a mixed graph without undirected edges
   Outputs
     H:HashTable
       where H#p is the ideal of equations involving only the parameter $p$ and the covariances $s_{(i,j)}$
   Description 
     Text
       Expresses each parameter in the gaussianParametrization in terms of covariances,
       if it is possible to do so, or displays that no identification formula is possible.  The identifiability
       problem for mixed graph models is described in Garcia, Spielvogel, Sullivant,  "Identifying causal effects with computer algebra",
        UAI, Proceedings of the 26th Conferences, AUAI Press, 2010.
       
       If H#p contains a linear equation $a*p+b$ where a is always nonzero, then $p$ is identifiable.
       
       If H#p contains a linear equation $a*p+b$ where a may be zero, then $p$ is generically identifiable.
       
       If H#p contains a polynomial in $p$ of degree $d$, then $p$ is algebraically $d$-identifiable.
       
       If H#p does not contain any polynomial in $p$, then $p$ is not generically identifiable.

     Example
       G = mixedGraph(digraph {{a,{b}},{b,{c}}},bigraph {{a,c}, {b,c}})
       R = gaussianRing G
       H = identifyParameters R
       
     Text
       Reading the output (first line in the HashTable), we see that parameter $l_{(a,b)}$ is identifiable by the
       formula $l_{(a,b)} = s_{(a,b)}/s_{(a,a)}$.  On the other hand, $l_{(b,c)}$ is
       not identifiable.    
     
   SeeAlso
     gaussianRing
///

--------------------------------
-- Documentation trekIdeal    --
--------------------------------

doc/// 
   Key
     trekIdeal
     (trekIdeal,Ring,MixedGraph)
     (trekIdeal,Ring,Digraph)
     (trekIdeal,Ring,Graph)
   Headline
     trek separation ideal of a mixed graph 
   Usage
     I = trekIdeal(R,G) 
   Inputs
     R:Ring
       which should be a gaussianRing
     G:Graph
      @ofClass Graph@, or @ofClass Digraph@ with no cycles, 
      or @ofClass MixedGraph@ with directed and bidirected edges
   Outputs
     I:Ideal
       the ideal of determinantal trek separation statements implied by the graph $G$.
   Description 
     Text  
       For mixed graphs, the ideal corresponding to all trek separation statements {A,B,CA,CB} (where A,B,CA,CB
       are disjoint lists of vertices of G) is generated by the r+1 x r+1 minors of the submatrix of the covariance matrix M = (s_{(i,j)}), whose
       rows are in A, and whose columns are in B, and where r = #CA+#CB.
       
       This function is not yet implemented for mixed graphs with undirected edges.
       
       These ideals are described in more detail by Sullivant, Talaska and Draisma in "Trek Separation for Gaussian Graphical Models"
       Annals of Statistics 38 no.3 (2010) 1665--1685
       and give all determinantal constraints on the covariance matrix of a Gaussian graphical model.        

     Example
       G = mixedGraph(digraph {{b,{c,d}},{c,{d}}},bigraph {{a,d}})
       R = gaussianRing G
       T = trekIdeal(R,G)
       ideal gens gb T
       
     Text
       For undirected graphs $G$, the {\tt trekIdeal(R,G)} is the same as 
       {\tt conditionalIndependenceIdeal(R,globalMarkov(G))}.  For directed graphs $G$, {\tt trekIdeal(R,G)}
        is generally larger than {\tt conditionalIndependenceIdeal(R,globalMarkov(G))}.

     Example
       G = graph{{a,b},{b,c},{c,d},{a,d}}     
       R = gaussianRing G
       T = trekIdeal(R,G);
       CI = conditionalIndependenceIdeal(R,globalMarkov(G));
       T == CI
       H = digraph{{1,{4}},{2,{4}},{3,{4,5}},{4,{5}}}
       R = gaussianRing H
       T = trekIdeal(R,H);
       CI = conditionalIndependenceIdeal(R,globalMarkov(H));
       T == CI
   Caveat
       {\tt trekSeparation} is currently only implemented with {\tt mixedGraphs} that have directed and 
       bidirected edges.  
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
     trek separation statements of a mixed graph 
   Usage
     trekSeparation(G)
   Inputs
     G:MixedGraph
       mixed graph with directed and bidirected edges
   Outputs
     :List
        of lists \{A,B,CA,CB\}, where (CA,CB) trek-separates A from B
   Description 
     Text
       A trek between vertices $i$ and $j$ in a mixed graph $G$ with directed and bidirected edges is a triple 
       $(P_L,P_R)$ where $P_L$ is a directed path of directed edges with sink $i$ and source $k$, $P_R$ is a directed path
       of directed edges with sink $j$ and source $l$, and either $k=l$ or there is a bidirected edge between $k$ and $l$.
       Let $A,B,CA,CB$ be subsets of vertices of $G$. 
       
       We say that $(CA,CB)$ trek-separates $A$ from $B$ in $G$ if for every trek 
       $(P_L,P_R)$ from a vertex in $A$ to a vertex in $B$, either $P_L$ contains a vertex in $CA$ or $P_R$ contains a vertex in $CB$.
       
       The function @TO trekSeparation@ returns a list of trek separation statements $\{A,B,CA,CB\}$\,where 
       $#CA + #CB < min(#A, #B)$. Each statement is maximal in the ordering where $\{A1,B1,CA,CB\}\,<\,\{A2,B2,CA,CB\}$\,if $A1$ is a 
       subset of $A2$ and $B1$ is a subset of $B2$. Each statement is also unique up to symmetry, since $\{B,A,CB,CA\}$\,is a 
       trek separation statement if and only if $\{A,B,CA,CB\}$.

     Example
       G = mixedGraph(digraph {{b,{c,d}},{c,{d}}},bigraph {{a,d}})
       S = trekSeparation G

   Caveat
       {\tt trekSeparation} $G$ is only implemented for mixedGraphs with directed and bidirected edges.    
   SeeAlso
     trekIdeal
///

----------------------------------------------------------------------------------
-- Documentation sVariableName, kVariableName, lVariableName, pVariableName     --
----------------------------------------------------------------------------------

doc ///
  Key
    sVariableName
  Headline
    optional input to choose the variable names for the covariance matrix
  Description
    Text
      Put {\tt sVariableName =>  Symbol} for a choice of a symbol s as an argument in the function @TO gaussianRing@
  SeeAlso
    gaussianRing
///
doc ///
  Key
    [gaussianRing, sVariableName]
  Headline
    symbol used for indeterminates in a ring of Gaussian joint probability distributions
  Usage
    gaussianRing(G,sVariableName=>t)    
  Inputs 
    t:
      a @TO Symbol@ or a @TO String@ 
  Description
    Text
      The option {\tt gaussianRing(G,sVariableName=>t)} changes the symbol used for intedeterminates in the covariance matrix 
      in a polynomial ring created with @TO gaussianRing@.
      
    Example
      R = gaussianRing 4
      gens R
      Rnew=gaussianRing(4,sVariableName => "t")
      gens Rnew
///

doc ///
  Key
    lVariableName
  Headline
    optional input to choose the variable names for the regression matrix
  Description
    Text
      Put {\tt lVariableName => Symbol} for a choice of a symbol l as an argument in 
      the function @TO gaussianRing@
  SeeAlso
    gaussianRing
///
doc ///
  Key
    [gaussianRing, lVariableName]
  Headline
    symbol used for indeterminates in a ring of Gaussian joint probability distributions
  Usage
    gaussianRing(G,lVariableName=>w)    
  Inputs 
    w:
      a @TO Symbol@ or a @TO String@ 
  Description
    Text
      The option {\tt gaussianRing(G,lVariableName=>w)} changes the symbol used for intedeterminates in the regression matrix 
      in a polynomial ring created with @TO gaussianRing@.

    Example
      G = mixedGraph(digraph {{b,{c,d}},{c,{d}}},bigraph {{a,d}})
      gens gaussianRing(G,lVariableName=>"lambda")
///

doc ///
  Key
    pVariableName
  Headline
    optional input to choose the variable names for the covariance matrix of the error terms
  Description
    Text
      Put {\tt pVariableName => Symbol} for a choice of a symbol p as an argument in 
      the function @TO gaussianRing@
  SeeAlso
    gaussianRing
///
doc ///
  Key
    [gaussianRing, pVariableName]
  Headline
    symbol used for indeterminates in a ring of Gaussian joint probability distributions
  Usage
    gaussianRing(G,pVariableName=>q)    
  Inputs 
    q:
      a @TO Symbol@ or a @TO String@ 
  Description
    Text
      The option {\tt gaussianRing(G,pVariableName=>q)} changes the symbol used for intedeterminates in the covariance matrix of the error terms 
      in a polynomial ring created with @TO gaussianRing@.
    
    Example
      G = mixedGraph(digraph {{b,{c,d}},{c,{d}}},bigraph {{a,d}})
      R = gaussianRing G
      gens R
      R = gaussianRing (G,pVariableName => psi)
      gens R      
///

doc ///
  Key
    kVariableName
  Headline
    optional input to choose the variable names for concentration matrix in gaussianRing
  Description
    Text
      The option {\tt kVariableName => Symbol} changes the symbol used for intedeterminates in a polynomial ring created with @TO gaussianRing@.
      These indeterminates, k's by default, are entries in the concentration matrix. 
  SeeAlso
    gaussianRing
///
doc ///
  Key
    [gaussianRing, kVariableName]
  Headline
    symbol used for indeterminates in a ring of Gaussian joint probability distributions
  Usage
    gaussianRing(G,kVariableName=>m)    
  Inputs 
    m:
      a @TO Symbol@ or a @TO String@ 
  Description
    Text
      The option {\tt gaussianRing(G,kVariableName=>m)} changes the symbol used for intedeterminates in the concentration 
      matrix in a polynomial ring created with @TO gaussianRing@.

    Example 
      R = gaussianRing graph({{a,b},{b,c},{c,d},{a,d}})
      compactMatrixForm =false;
      undirectedEdgesMatrix R
      gens R
      Rnew = gaussianRing( graph({{a,b},{b,c},{c,d},{a,d}}), kVariableName => kappa)
      gens Rnew
///

----------------------------------------------------------------------------------
-- Documentation of hash table gaussianRingData and its keys     --
----------------------------------------------------------------------------------
doc ///
  Key
    gaussianRingData
  Headline
    hash table with main parameters of a gaussian ring
  Description
    Text
     The contents of gaussianRingData depend on the type of gaussian ring.
     
     First, we show an example of a gaussian ring with 5 variables
    
    Example
     R = gaussianRing 5
     gaussianRingData
     
    Text
     In case of the gaussian ring of a graph, there are two options. First one, is when the graph is
     of class @TO Graph@ .
     
    Example 		 	    
     R=gaussianRing graph {{1,2},{2,3}}
     R.gaussianRingData
     
    Text
     If the graph is of any other class -i.e.,  @TO Bigraph@,  @TO Digraph@,  
     @TO MixedGraph@ - then it is internally converted to a @TO MixedGraph@ and
     the gaussianRingData has the same structure.
     
    Example 
     U = graph {{1,2},{2,3}}
     B = bigraph{{4,5}}
     D = digraph {{1,4}}

     R1 = gaussianRing B
     R2 = gaussianRing D
     R3 = gaussianRing mixedGraph(U,B,D)
     
     R1.gaussianRingData
     R2.gaussianRingData
     R3.gaussianRingData	 	 
  SeeAlso
    gaussianRing
    kVar
    pVar
    lVar
    sVar
    compU
    compW
    nn    
///

doc ///
  Key
    nn
  Headline
     key in hash table gaussianRingData: total number of variables 
  Description
    Text
     This key is present in every gaussianRingData hash table
    
    Example
     R = gaussianRing 5
     R.gaussianRingData
     
     U = graph {{1,2},{2,3}}
     B = bigraph{{4,5}}
     D = digraph {{1,4}}

     R1 = gaussianRing B
     R2 = gaussianRing D
     R3 = gaussianRing U
     R4 = gaussianRing mixedGraph(U,B,D)
     
     R1.gaussianRingData
     R2.gaussianRingData
     R3.gaussianRingData
     R4.gaussianRingData
     	 	 
  SeeAlso
    gaussianRingData
    kVar
    pVar
    lVar
    sVar
    compU
    compW   
///

doc ///
  Key
    kVar
  Headline
     key in hash table gaussianRingData: labels of k variables
  Description
    Text
     This key is present in every gaussianRingData that comes from a graph. 
     It is equal to the value of the optional input  @TO [gaussianRing, kVariableName]@.
    
    Example
     U = graph {{1,2},{2,3}}
     B = bigraph{{4,5}}
     D = digraph {{1,4}}

     R1 = gaussianRing B
     R2 = gaussianRing D
     R3 = gaussianRing U
     R4 = gaussianRing mixedGraph(U,B,D)
     
     R1.gaussianRingData
     R2.gaussianRingData
     R3.gaussianRingData
     R4.gaussianRingData
     	 	 
  SeeAlso
    kVariableName 
    gaussianRingData
    pVar
    lVar
    sVar
    compU
    compW
    nn   
///

doc ///
  Key
    sVar
  Headline
     key in hash table gaussianRingData: labels of s variables
  Description
    Text
     This key is present in every gaussianRingData that comes from a graph. 
     It is equal to the value of the optional input  @TO [gaussianRing, sVariableName]@.
    
    Example
     U = graph {{1,2},{2,3}}
     B = bigraph{{4,5}}
     D = digraph {{1,4}}

     R1 = gaussianRing B
     R2 = gaussianRing D
     R3 = gaussianRing U
     R4 = gaussianRing mixedGraph(U,B,D)
     
     R1.gaussianRingData
     R2.gaussianRingData
     R3.gaussianRingData
     R4.gaussianRingData
     	 	 
  SeeAlso
    sVariableName 
    gaussianRingData
    pVar
    lVar
    kVar
    compU
    compW
    nn   
///

doc ///
  Key
    pVar
  Headline
     key in hash table gaussianRingData: labels of p variables
  Description
    Text
     This key is present in every gaussianRingData that comes from a graph. 
     It is equal to the value of the optional input  @TO [gaussianRing, pVariableName]@.
    
    Example
     U = graph {{1,2},{2,3}}
     B = bigraph{{4,5}}
     D = digraph {{1,4}}

     R1 = gaussianRing B
     R2 = gaussianRing D
     R3 = gaussianRing U
     R4 = gaussianRing mixedGraph(U,B,D)
     
     R1.gaussianRingData
     R2.gaussianRingData
     R3.gaussianRingData
     R4.gaussianRingData
     	 	 
  SeeAlso
    pVariableName 
    gaussianRingData
    kVar
    lVar
    sVar
    compU
    compW
    nn   
///

doc ///
  Key
    lVar
  Headline
     key in hash table gaussianRingData: labels of l variables
  Description
    Text
     This key is present in every gaussianRingData that comes from a graph. 
     It is equal to the value of the optional input  @TO [gaussianRing, lVariableName]@.
    
    Example
     U = graph {{1,2},{2,3}}
     B = bigraph{{4,5}}
     D = digraph {{1,4}}

     R1 = gaussianRing B
     R2 = gaussianRing D
     R3 = gaussianRing U
     R4 = gaussianRing mixedGraph(U,B,D)
     
     R1.gaussianRingData
     R2.gaussianRingData
     R3.gaussianRingData
     R4.gaussianRingData
     	 	 
  SeeAlso
    lVariableName 
    gaussianRingData
    kVar
    pVar
    sVar
    compU
    compW
    nn   
///

doc ///
  Key
    compU
  Headline
     key in hash table gaussianRingData: component of undirected edges in vertex set of a mixed graph
  Description
    Text
     This key is present in every gaussianRingData that comes from a graph of class @TO MixedGraph@. 
     It is equal to the set of vertices that are incident to undirected edges. For more details,
     check component U in @TO partitionLMG@.
    
    Example 
     U = graph {{1,2},{2,3}}
     B = bigraph{{4,5}}
     D = digraph {{1,4}}
     R = gaussianRing mixedGraph(U,B,D)	
     R.gaussianRingData
     
    Text 
     Since the gaussian rings of graphs of classes @TO Digraph@ and @TO Bigraph@ are
     created by first changing the class to  @TO MixedGraph@, the key compU is also
     present in the gaussianRingData hashtables of these two classes of graphs and
     the corresponding value is computed according to the rules described in 
     @TO partitionLMG@.
    
    Example
     U = graph {{1,2},{2,3}}
     B = bigraph{{4,5}}
     D = digraph {{1,4}}

     R1 = gaussianRing B
     R2 = gaussianRing D
         
     R1.gaussianRingData
     R2.gaussianRingData

     	 	 
  SeeAlso
    partitionLMG
    gaussianRingData
    kVar
    pVar
    sVar
    lVar
    compW
    nn   
///

doc ///
  Key
    compW
  Headline
     key in hash table gaussianRingData: component of bidirected edges in vertex set of a mixed graph
  Description
    Text
     This key is present in every gaussianRingData that comes from a graph of class @TO MixedGraph@. 
     It is equal to the set of vertices that are incident to bidirected edges. For more details,
     check component W in @TO partitionLMG@.
    
    Example 
     U = graph {{1,2},{2,3}}
     B = bigraph{{4,5}}
     D = digraph {{1,4}}
     R = gaussianRing mixedGraph(U,B,D)	
     R.gaussianRingData
     
    Text 
     Since the gaussian rings of graphs of classes @TO Digraph@ and @TO Bigraph@ are
     created by first changing the class to  @TO MixedGraph@, the key compW is also
     present in the gaussianRingData hashtables of these two classes of graphs and
     the corresponding value is computed according to the rules described in 
     @TO partitionLMG@.
    
    Example
     U = graph {{1,2},{2,3}}
     B = bigraph{{4,5}}
     D = digraph {{1,4}}

     R1 = gaussianRing B
     R2 = gaussianRing D
         
     R1.gaussianRingData
     R2.gaussianRingData

     	 	 
  SeeAlso
    partitionLMG
    gaussianRingData
    kVar
    pVar
    sVar
    lVar
    compU
    nn   
///
--------------------------------------------
-- Documentation graphType
--------------------------------------------
doc ///
  Key
     graphType
  Headline
     class of graph used to generate a gaussian ring   	 	 
  Description
    Text 
     For a {\tt gaussianRing} R generated with @ofClass Digraph@ and @ofClass Bigraph@
     the class we retrieve when typing R.graph is MixedGraph. This is consistent with the treatment
     of such objects. Therefore, this variable is essentially used to differentiate graphs
     from @ofClass Graph@ and @ofClass MixedGraph@.  
  SeeAlso
     gaussianRing
     gaussianRingData
///
--------------------------------------------
-- Documentation conditionalIndependenceIdeal
--------------------------------------------
doc///
  Key
    conditionalIndependenceIdeal
    (conditionalIndependenceIdeal, Ring, List)
    (conditionalIndependenceIdeal, Ring, List, List)
  Headline
    ideal of a list of conditional independent statements
  Usage
    conditionalIndependenceIdeal(R,Stmts)
    conditionalIndependenceIdeal(R,Stmts,VarNames)
  Inputs
    R:Ring
      it must be a @TO gaussianRing@ or a @TO markovRing@ 
    Stmts:List
      list of conditional independence statements
    VarNames:List
       list of names of random variables in conditional independence statements in Stmts. This argument
       allows to choose a subset of random variables and is only available for markov rings. By default, 
       this is a list of integers 1 to $n$ where $n$ is the number of variables in the
       declaration of {\tt markovRing} or {\tt gaussianRing}. If R is a gaussian ring, then only the
       default input is accepted.
  Outputs
    :Ideal
      ideal of conditional independence relations
  Description
    Text
      {\tt conditionalIndependenceIdeal} computes the ideal of a list of conditional independence statements. This method works
      for both discrete and Gaussian graphical models. In the case of discrete random variables, it computes the 2x2 minors
      of the matrices produced by @TO markovMatrices@. For Gaussian graphical models, it computes the minors 
      of the matrices produced by @TO gaussianMatrices@.
      
      A single conditional independence statement is a list consisting of three disjoint
      lists of indices for random variables, e.g. $\{ \{1,2\},\{4\}, \{3\} \}$
      which represents the conditional independence statement ``$(X_1, X_2)$
      is conditionally independent of $X_4$ given $X_3$''.
      In the input to {\tt conditionalIndependenceIdeal} a list of conditional
      independence statements is used.

      A common way that we arrive at collections of conditional independence statements
      is through the Markov statements implied by a graph.
      Below are two examples of independence ideals on discrete random variables,
      using @TO globalMarkov@ statements and @TO localMarkov@ statements.

    Example
      G = graph {{1,2},{2,3},{3,4},{4,1}}
      D = digraph {{1,{}},{2,{1}},{3,{1}},{4,{2,3}}}
      R = markovRing (2,2,2,2)
      conditionalIndependenceIdeal (R, globalMarkov(G)) / print;
      conditionalIndependenceIdeal (R, localMarkov(D)) / print;
       
    Text    
       The following example is an independence ideal of a Gaussian graphical model.
       
    Example
      G = graph {{a,b},{b,c},{c,d},{d,a}}
      R=gaussianRing G
      conditionalIndependenceIdeal (R,globalMarkov(G))  / print; 
        
    Text
      For Gaussian models, 	
      {\tt conditionalIndependenceIdeal}  can compute the ideal of a list of independence statements on a graph even
      if the ring was not constructed with that specific graph.  
      However, the vertex labels in the graph should be integers. 
      
    Example
      G = graph({{1,2},{2,3},{3,4},{4,1}})  
      R=gaussianRing 4
      conditionalIndependenceIdeal (R, globalMarkov G)  / print;   
      
    Text
      This method also accepts as input arbitrary lists of independent statements that may not 
      arise from a graphical model. 
      	
    Example
      R=gaussianRing 5
      S={{{1},{2},{3,4}}, {{2,3},{1},{5}}}
      conditionalIndependenceIdeal (R,S) / print;

    Text
      For general discrete independence models (not necessarily arising from a graph), {\tt conditionalIndependenceIdeal} requires one of the 
      following two options: \break
      (1) the random variables are labelled by integers (as in the first example above) or  \break
      (2) in case the random variables have arbitrary names, an extra input parameter must be used in order to specify
      the names of the random variables. 

    Example    
      R = markovRing (2,2,2,2)
      VarNames = {c,d,e,f}
      Stmts = { {{c,d},{e},{}}, {{d,e},{c},{f}}}
      conditionalIndependenceIdeal(R,Stmts,VarNames)	/ print;  

    Text
      The following example illustrates the caveat below.
      
    Example
      D = digraph {{b,{a}},{a,{c}},{c,{}}}
      R = markovRing (2,3,2)  
      VarNames = {b,a,c}
      S = globalMarkov D
      conditionalIndependenceIdeal(R, S, VarNames) / print;
      vertices D
      conditionalIndependenceIdeal(R, S, vertices D) / print;
      
  Caveat
     We note that the list of random variable names must be in the same order as the implicit order used in the sequence $d$.
     In the previous example, we have the graph $b \to a \to c$, where $a$ has three states and $b$ and $c$ are both binary.
     Note that the ring $R$ was created with the sequence $d = (2,3,2)$, having in mind the topological order of the graph as opposed
     to the vertex labels. Note how the first instance of this method returns the correct output, however, the second instance returns
     an incorrect ideal since vertices D is not in the same order as the sequence $d$.
      
  SeeAlso
    discreteVanishingIdeal
    gaussianRing 
    gaussianVanishingIdeal
    markovRing
    trekIdeal
///

--------------------------------------------
-- Documentation undirectedEdgesMatrix------
--------------------------------------------

doc/// 
   Key
     undirectedEdgesMatrix
     (undirectedEdgesMatrix,Ring)
   Headline
     matrix corresponding to the edges of an undirected graph
   Usage
     undirectedEdgesMatrix(R)
   Inputs
     R:Ring
       which should be created with @TO gaussianRing@ created with @ofClass Graph@ or
       @ofClass MixedGraph@
   Outputs
     :Matrix
       the n x n symmetric concentration matrix of an undirected gaussian
       graphical model.  
   Description 
     Text
       This symmetric matrix has entries $k_{(i,i)}$ along the diagonal
       and entry $k_{(i,j)}$ in the $(i,j)$ position if there is an edge  between i and j, and a zero otherwise.
       The documentation of @TO gaussianRing@ 
       further describes the indeterminates $k_{(i,j)}$.

     Example
       G = graph({{a,b},{b,c},{c,d},{a,d}})
       R = gaussianRing G
       compactMatrixForm =false;
       K = undirectedEdgesMatrix(R)
       
     Text
       For mixed graphs with other types of edges, 
       the size of the matrix coincides with the number of elements in @TO compU@,
       which depends on the vertex partition built in @TO partitionLMG@.
       
     Example
       G = mixedGraph(digraph {{1,3},{2,4}},bigraph {{3,4}},graph {{1,2}});
       R = gaussianRing G;
       K = undirectedEdgesMatrix(R)
   SeeAlso
     gaussianRing
     gaussianParametrization
     covarianceMatrix
     directedEdgesMatrix
///

-----------------------------------------
-- Documentation gaussianVanishingIdeal--
-----------------------------------------

doc ///
   Key
     gaussianVanishingIdeal
     (gaussianVanishingIdeal,Ring)
   Headline
     vanishing ideal of a Gaussian graphical model 
   Usage
     gaussianVanishingIdeal(R)
   Inputs
     R:Ring
       created with @TO gaussianRing@  using a graphs of classes @TO Graph@,  @TO Digraph@ or  @TO MixedGraph@ without undirected edges as input. 
   Outputs
     :Ideal
        ideal in R
   Description
     Text
       {\tt gaussianVanishingIdeal} computes the ideal in $R$ of homogeneous polynomial relations 
       on the variance-covariance parameters of a graphical model on $G$ as explained in 
       Chapter 3.3 of ``Lectures on Algebraic Statistics'' by Drton, Sturmfels, and Sullivant.
       
     Example
       G = graph({{a,b},{b,c},{c,d},{a,d}})
       R = gaussianRing G 
       J = gaussianVanishingIdeal(R); 
       ideal mingens J / print;

     Text
       This method works for graphs, digraphs and mixed graphs without undirected edges.

     Example
       G = digraph {{a,{b,c}}, {b,{c,d}}, {c,{}}, {d,{}}}
       R = gaussianRing G
       gaussianVanishingIdeal(R) 
       H = mixedGraph(digraph {{a,{c}},{b,{c}}, {c,{d}}},bigraph {{c,d}})
       S = gaussianRing H
       gaussianVanishingIdeal(S) 
     
   Caveat
     This method currently works on really small examples because it computes 
     the vanishing ideal as an elimination ideal.  
   SeeAlso
     gaussianRing
     trekIdeal
     OldVersion
///

-----------------------------------------
-- Documentation OldVersion--
-----------------------------------------
doc ///
  Key
     OldVersion
  Headline
     optional argument in gaussianVanishingIdeal to use old method for gaussianRings coming from directed graphs    	 	 
  Description
    Text
     Alternative computation of the vanishing ideal of a Gaussian directed graphical
     model using (2) from Seth Sullivant, Kelli Talaska, and Jan Draisma, 
     {\em Trek separation for Gaussian graphical models}, The Annals of Statistics 
     38.3 (2010): 1665-1685.
       
  SeeAlso
     gaussianVanishingIdeal
///

doc ///
  Key
    [gaussianVanishingIdeal, OldVersion]
  Headline
     optional argument in gaussianVanishingIdeal to use old method for gaussianRings coming from directed graphs    	 	 
  Usage
    gaussianVanishingIdeal(R,G,OldVersion=>false)
  Inputs
    b:Boolean
       false by default
  Description
    Text
     By default, @TO gaussianVanishingIdeal@ uses the code of the current version. However, if the graph
     only has directed edges, the user can choose to use the code from the previous version which is based on
     (2) from Seth Sullivant, Kelli Talaska, and Jan Draisma, 
     {\em Trek separation for Gaussian graphical models}, The Annals of Statistics 
     38.3 (2010): 1665-1685.
     
  SeeAlso
     gaussianVanishingIdeal	
///
-----------------------------------------
-- Documentation discreteVanishingIdeal--
-----------------------------------------
doc/// 
   Key
     discreteVanishingIdeal
     (discreteVanishingIdeal,Ring,Digraph) 
   Headline
     vanishing ideal of a discrete graphical model 
   Usage
     discreteVanishingIdeal(R,G)
   Inputs
     R:Ring
       created with @TO markovRing@ 
     G:Digraph
   Outputs
     :Ideal
       an ideal in $R$ 
   Description 
     Text
       This method computes the ideal in $R$ of homogeneous polynomial 
       relations on the joint probabilities of random variables represented by the vertices of $G$. 
       
       Here is a small example that compute the vanishing ideal on the joint probabilities of two independent binary random 
       variables. In this case, this ideal equals the ideal obtained using @TO conditionalIndependenceIdeal@.
     
     Example
       G = digraph {{1,{}}, {2,{}}}
       R = markovRing (2,2)
       discreteVanishingIdeal (R,G)
       conditionalIndependenceIdeal(R, localMarkov G)
         
     Text  
       Here is an example for a graph on four vertices. The random variables a,b,c and d have 2,3,4, and 2 states, respectively. 

     Example
       G = digraph {{a,{b,c}}, {b,{c,d}}, {c,{}}, {d,{}}}
       R = markovRing (2,3,4,2)
       I = discreteVanishingIdeal (R,G);
       
     Text
       The vanishing ideal is generated by 84 quadrics, which we don't display.

     Example
       betti I 
   SeeAlso
     markovRing
     conditionalIndependenceIdeal
///



--******************************************--
-- TESTS     	       	    	      	    --
--******************************************--

--------------------------
---- TEST pairMarkov  ----
--------------------------

TEST /// 
G = graph({{a,b},{b,c},{c,d},{d,e},{e,a}})
S = pairMarkov G
Ssorted = apply(S, s-> replace(2,sort(s_2),s) )
L = {{{a}, {d}, sort {e, b, c}}, {{c}, {e}, sort {d, a, b}}, {{b}, {d},sort {e,a, c}}, {{b}, {e},sort {d, a, c}}, {{a}, {c},sort {d, e, b}}}
assert(sort Ssorted === sort L)
/// 

TEST ///
G = digraph {{a,{b,c}}, {b,{c,d}}, {c,{}}, {d,{}}}
S = pairMarkov G
S = sort apply(S,s -> {sort s#0, sort s#1, sort s#2}) 
L = sort {{{c}, {d}, {a, b}}, {{a}, {d}, {b, c}}}
assert(S === L)
///

--------------------------
---- TEST localMarkov  ---
--------------------------

TEST ///
G = graph({{a,b},{b,c},{c,d},{d,e},{e,a}})
S = localMarkov G
L = {{{a}, {c, d},sort {e, b}}, {{a, b}, {d},sort {e, c}}, {{a, e}, {c},sort {d, b}}, {{b, c}, {e}, sort{d, a}}, {{b}, {d, e}, sort{a, c}}}
Ssorted = apply(S, s-> replace(2,sort(s_2),s) )
assert(sort Ssorted === sort L)
///

TEST ///
G = digraph { {1,{2,3,4}}, {5,{2,3,4}} }
S = localMarkov G
S = sort apply(S,s -> {sort s#0, sort s#1, sort s#2}) 
L = sort {{{2}, {3, 4}, {1, 5}}, {{2, 3}, {4}, {1, 5}}, {{2, 4}, {3}, {1, 5}}, {{1}, {5}, {}}} 
assert(S === L)
///

--------------------------
--- TEST globalMarkov  ---
--------------------------

TEST ///
G = graph({{a,b},{b,c},{c,d},{d,e},{e,a}})
S = globalMarkov G
S = sort apply(S,s -> {sort s#0, sort s#1, sort s#2}) 
L= sort {{{a}, {c, d}, {b, e}}, {{a, b}, {d}, {c, e}}, {{a, e}, {c}, {b, d}}, {{b}, {d, e}, {a,c}}, {{b, c}, {e}, {a, d}}}
assert(S === L)
///

TEST ///
G = digraph { {2, {1}}, {3,{2}}, {4,{1,3}} }
S = globalMarkov G
S = sort apply(S,s -> {sort s#0, sort s#1, sort s#2}) 
L = sort {{{1}, {3}, {2, 4}}, {{2}, {4}, {3}}}
assert(S === L)
///

--------------------------
--- TEST markovRing    ---
--------------------------

TEST ///
d = (2,2,2)
R = markovRing (d, Coefficients=>CC, VariableName=>q)
V = {q_(1,1,1), q_(1,1,2), q_(1,2,1), q_(1,2,2), q_(2,1,1), q_(2,1,2), q_(2,2,1), q_(2,2,2)}
assert(sort gens R === sort V)
///

-----------------------------------------------
--- TEST gaussianRing--------------------------
-----------------------------------------------

TEST ///
R = gaussianRing 4
B = gens R
L = {s_(1,1), s_(1,2), s_(1,3), s_(1,4), s_(2,2), s_(2,3), s_(2,4), s_(3,3), s_(3,4), s_(4,4)}
assert(sort B === sort L)
///

TEST /// 
G = graph({{a,b},{b,c},{c,d},{a,d}}) 
R = gaussianRing G
correctOutput = {{k_(a,a), k_(b,b), k_(c,c), k_(d,d), k_(a,d), k_(a,b),k_(b,c), k_(c,d), s_(a,a), s_(a,b), s_(a,c), s_(a,d), s_(b,b),s_(b,c), s_(b,d), s_(c,c), s_(c,d), s_(d,d)}}
assert(ideal gens R == ideal flatten correctOutput )
/// 
     
TEST /// 
G = digraph {{a,{b,c}}, {b,{c,d}}, {c,{}}, {d,{}}}
R = gaussianRing G
assert(sort gens R === sort {l_(a,c),l_(a,b),l_(b,c),l_(b,d),p_(a,a),p_(b,b),p_(c,c),p_(d,d),s_(a,a),s_(a,b),
     s_(a,c),s_(a,d),s_(b,b),s_(b,c),s_(b,d),s_(c,c),s_(c,d),s_(d,d)})
///

TEST ///
G = mixedGraph(digraph {{b,{c,d}},{c,{d}}},bigraph {{a,d}})
R = gaussianRing G
assert(sort gens R === sort {l_(b,c), l_(b,d), l_(c,d), p_(a,a), p_(b,b), p_(c,c), p_(d,d), p_(a,d), s_(a,a), s_(a,b), s_(a,c), s_(a,d), s_(b,b), s_(b,c), s_(b,d), s_(c,c), s_(c,d), s_(d,d)})
///

-----------------------------------------------
--- TEST undirectedEdgesMatrix-----------------
-----------------------------------------------

TEST ///
G = graph({{a,b},{b,c},{c,d},{a,d}}) 
R=gaussianRing G 
M=undirectedEdgesMatrix(R)
correctOutput = {{k_(a,a), k_(a,b), 0, k_(a,d)}, {k_(a,b), k_(b,b), k_(b,c),0}, {0, k_(b,c), k_(c,c), k_(c,d)}, {k_(a,d), 0, k_(c,d),k_(d,d)}}
assert(0 == M - matrix correctOutput )
///

--------------------------------
--- TEST directedEdgesMatrix ---
--------------------------------

TEST ///
G = mixedGraph(digraph {{b,{c,d}},{c,{d}}},bigraph {{a,d}})
R = gaussianRing G
L = directedEdgesMatrix R
assert(0 == L-matrix {{0, 0, 0, 0}, {0, 0, l_(b,c), l_(b,d)}, {0, 0, 0, l_(c,d)}, {0, 0, 0, 0}})
///

----------------------------------
--- TEST bidirectedEdgesMatrix ---
----------------------------------

TEST ///
G = mixedGraph(digraph {{b,{c,d}},{c,{d}}},bigraph {{a,d}})
R = gaussianRing G
W = bidirectedEdgesMatrix R
assert(0 == W-matrix {{p_(a,a), 0, 0, p_(a,d)}, {0, p_(b,b), 0, 0}, {0, 0, p_(c,c), 0}, {p_(a,d), 0, 0, p_(d,d)}})
///

------------------------------
--- TEST markovMatrices    ---
------------------------------

TEST ///
G = digraph { {1, {2,3}}, {2, {4}}, {3, {4}} }
S = localMarkov G
R = markovRing (2,2,2,2)
L = markovMatrices (R,S) 
M = L#1
m = matrix {{p_(2,1,1,1)+p_(2,1,1,2), p_(2,1,2,1)+p_(2,1,2,2)},{p_(2,2,1,1)+p_(2,2,1,2), p_(2,2,2,1)+p_(2,2,2,2)}} 
assert(M === m)
///

TEST ///
R=markovRing (4:2)
L = markovMatrices ( R ,  {{{a},{c},{d}}},{a,b,c,d})
M = L#1
m = matrix {{ p_(1,1,1,2)+p_(1,2,1,2), p_(1,1,2,2)+p_(1,2,2,2)}, {p_(2,1,1,2)+p_(2,2,1,2), p_(2,1,2,2)+p_(2,2,2,2)}} 
assert(M === m)
///

-----------------------------------------------
--- TEST covarianceMatrix(R,G)-----------------
-----------------------------------------------

TEST ///
G = graph({{a,b},{b,c},{c,d},{a,d}}) 
R=gaussianRing G 
cov=covarianceMatrix R
correctOutput = {{s_(a,a), s_(a,b), s_(a,c), s_(a,d)}, {s_(a,b), s_(b,b),s_(b,c), s_(b,d)}, {s_(a,c), s_(b,c), s_(c,c), s_(c,d)},{s_(a,d), s_(b,d), s_(c,d), s_(d,d)}}
assert(0 == cov - matrix correctOutput )
///

TEST /// 
G = digraph {{a,{b,c}}, {b,{c,d}}, {c,{}}, {d,{}}}
R = gaussianRing G
S = covarianceMatrix R
assert(0==S-matrix {{s_(a,a), s_(a,b), s_(a,c), s_(a,d)}, {s_(a,b), s_(b,b), s_(b,c), s_(b,d)}, {s_(a,c), s_(b,c), s_(c,c), s_(c,d)}, {s_(a,d), s_(b,d), s_(c,d), s_(d,d)}})
///

TEST ///
G = mixedGraph(digraph {{b,{c,d}},{c,{d}}},bigraph {{a,d}})
R = gaussianRing G
S = covarianceMatrix R
assert(0 == S-matrix {{s_(a,a), s_(a,b), s_(a,c), s_(a,d)}, {s_(a,b), s_(b,b), s_(b,c), s_(b,d)}, {s_(a,c), s_(b,c), s_(c,c), s_(c,d)}, {s_(a,d), s_(b,d), s_(c,d), s_(d,d)}})
///

------------------------------
--- TEST gaussianMatrices  ---
------------------------------

TEST ///
G = digraph { {1,{2}}, {2,{3}}, {3,{4,5}},{4,{5}} } ;
R = gaussianRing G
S = localMarkov G
L = gaussianMatrices(R,S)
M1 = matrix {{s_(1,4), s_(1,3)}, {s_(2,4), s_(2,3)}, {s_(3,4), s_(3,3)}}
M2 = matrix {{s_(1,5), s_(1,4), s_(1,3)},{s_(2,5), s_(2,4), s_(2,3)},{s_(4,5), s_(4,4), s_(3,4)}, {s_(3,5), s_(3,4), s_(3,3)}}
M3 = matrix {{s_(1,3), s_(1,2)},{s_(2,3), s_(2,2)}}
assert({M1,M2,M3} === L)
///

TEST ///
G = digraph { {1,{2}}, {2,{3}}, {3,{4,5}},{4,{5}} } ;
R = gaussianRing G
L = gaussianMatrices(R,{{{1},{3},{4,2,5}}})
M = matrix{{s_(1,3), s_(1,4), s_(1,2), s_(1,5)},{s_(3,4), s_(4,4), s_(2,4), s_(4,5)},{ s_(2,3), s_(2,4) ,s_(2,2), s_(2,5)}, { s_(3,5), s_(4,5), s_(2,5) ,s_(5,5) }}
assert({M} === L)
///

--------------------------------------
-- TEST conditionalIndependenceIdeal
--------------------------------------

TEST///
R=gaussianRing 5
S={{{1},{2},{3,4}}, {{2,3},{1},{5}}}
I=conditionalIndependenceIdeal (R,S)
assert(numcols mingens I == 4)
assert(isSubset(ideal( -s_(1,4)*s_(2,4)*s_(3,3)+s_(1,4)*s_(2,3)*s_(3,4)+s_(1,3)*s_(2,4)*s_(3,4)-s_(1,2)*s_(3,4)^2-s_(1,3)*s_(2,3)*s_(4,4)+s_(1,2)*s_(3,3)*s_(4,4) ), I))
///

--------------------------------
-- TEST discreteVanishingIdeal
--------------------------------

TEST///
G = digraph {{a,{b,c}}, {b,{c,d}}, {c,{}}, {d,{}}}
R = markovRing (2,3,4,2)
I = discreteVanishingIdeal (R,G);
assert ( numcols mingens I == 84)
///

------------------------------------
--- TEST gaussianParametrization ---
------------------------------------

TEST ///
G = mixedGraph(digraph {{b,{c,d}},{c,{d}}},bigraph {{a,d}})
R = gaussianRing G
M = gaussianParametrization(R)
assert(0 == M-matrix {{p_(a,a), 0, 0, p_(a,d)}, {0, p_(b,b), l_(b,c)*p_(b,b), l_(b,c)*l_(c,d)*p_(b,b)+l_(b,d)*p_(b,b)}, {0, l_(b,c)*p_(b,b), l_(b,c)^2*p_(b,b)+p_(c,c), l_(b,c)^2*l_(c,d)*p_(b,b)+l_(b,c)*l_(b,d)*p_(b,b)+l_(c,d)*p_(c,c)},{p_(a,d), l_(b,c)*l_(c,d)*p_(b,b)+l_(b,d)*p_(b,b),l_(b,c)^2*l_(c,d)*p_(b,b)+l_(b,c)*l_(b,d)*p_(b,b)+l_(c,d)*p_(c,c),l_(b,c)^2*l_(c,d)^2*p_(b,b)+2*l_(b,c)*l_(b,d)*l_(c,d)*p_(b,b)+l_(b,d)^2*p_(b,b)+l_(c,d)^2*p_(c,c)+p_(d,d)}})
///

TEST ///
G = mixedGraph(digraph {{b,{c,d}},{c,{d}}},bigraph {{a,d}})
R = gaussianRing G
M = gaussianParametrization(R,SimpleTreks=>true)
assert(0 == M-matrix {{1, 0, 0, p_(a,d)}, {0, 1, l_(b,c), l_(b,c)*l_(c,d)+l_(b,d)}, {0, l_(b,c), 1, l_(b,c)*l_(b,d)+l_(c,d)}, {p_(a,d), l_(b,c)*l_(c,d)+l_(b,d), l_(b,c)*l_(b,d)+l_(c,d), 1}})
///

-----------------------------------------------
--- TEST gaussianVanishingIdeal-----------------
-----------------------------------------------

TEST ///
G = graph({{a,b},{b,c},{c,d},{a,d}}) 
R=gaussianRing G 
I = gaussianVanishingIdeal R
correctOutput = {s_(a,d)*s_(b,c)*s_(b,d)-s_(a,c)*s_(b,d)^2-s_(a,d)*s_(b,b)*s_(c,d)+s_(a,b)*s_(b,d)*s_(c,d)+s_(a,c)*s_(b,b)*s_(d,d)-s_(a,b)*s_(b,c)*s_(d,d),s_(a,c)*s_(a,d)*s_(b,c)-s_(a,c)^2*s_(b,d)-s_(a,b)*s_(a,d)*s_(c,c)+s_(a,a)*s_(b,d)*s_(c,c)+s_(a,b)*s_(a,c)*s_(c,d)-s_(a,a)*s_(b,c)*s_(c,d), s_(a,b)*s_(a,d)*s_(b,d)*s_(c,c)-s_(a,a)*s_(b,d)^2*s_(c,c)-s_(a,c)*s_(a,d)*s_(b,b)*s_(c,d)+s_(a,a)*s_(b,c)*s_(b,d)*s_(c,d)+s_(a,c)^2*s_(b,b)*s_(d,d)-s_(a,b)*s_(a,c)*s_(b,c)*s_(d,d), s_(a,b)*s_(a,c)*s_(b,d)^2*s_(c,c)-s_(a,a)*s_(b,c)*s_(b,d)^2*s_(c,c)-s_(a,c)^2*s_(b,b)*s_(b,d)*s_(c,d)+s_(a,a)*s_(b,c)^2*s_(b,d)*s_(c,d)-s_(a,b)^2*s_(b,d)*s_(c,c)*s_(c,d)+s_(a,a)*s_(b,b)*s_(b,d)*s_(c,c)*s_(c,d)+s_(a,b)*s_(a,c)*s_(b,b)*s_(c,d)^2-s_(a,a)*s_(b,b)*s_(b,c)*s_(c,d)^2+s_(a,c)^2*s_(b,b)*s_(b,c)*s_(d,d)-s_(a,b)*s_(a,c)*s_(b,c)^2*s_(d,d)-s_(a,b)*s_(a,c)*s_(b,b)*s_(c,c)*s_(d,d)+s_(a,b)^2*s_(b,c)*s_(c,c)*s_(d,d)}
assert( I == ideal correctOutput)
///

TEST ///
G = digraph {{a,{b,c}}, {b,{c,d}}, {c,{}}, {d,{}}}
R = gaussianRing G
I = gaussianVanishingIdeal(R) 
correctOutput = { -s_(a,d)*s_(b,b)+s_(a,b)*s_(b,d), s_(b,c)*s_(b,d)-s_(b,b)*s_(c,d), s_(a,d)*s_(b,c)-s_(a,b)*s_(c,d) }
assert( I == ideal correctOutput)
///

--------------------------
-- TEST trekSeparation  --
--------------------------

TEST ///
G = mixedGraph(digraph {{b,{c,d}},{c,{d}}},bigraph {{a,d}})
R = gaussianRing G
T = trekSeparation G
T = apply(T,s -> {sort s#0, sort s#1, sort s#2, sort s#3})
L = {{{a}, {b, c}, {}, {}}, {{b, c}, {a, b}, {}, {b}}, {{a, b}, {b, c}, {}, {b}}, {{b, c}, {a, c}, {}, {c}}, {{b, c}, {a, d}, {}, {d}}}
assert(sort T=== sort L)
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

TEST ///
G = mixedGraph(digraph {{b,{c,d}},{c,{d}}},bigraph {{a,d}})
R = gaussianRing G
T = trekSeparation G
I = trekIdeal(R,G)
assert(I == ideal(s_(a,c),s_(a,b),s_(a,c)*s_(b,b)-s_(a,b)*s_(b,c),-s_(a,c)*s_(b,b)+s_(a,b)*s_(b,c),s_(a,c)*s_(b,c)-s_(a,b)*s_(c,c),s_(a,c)*s_(b,d)-s_(a,b)*s_(c,d)))
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
--- TEST inverseMarginMap     ---
--------------------------

TEST ///
R = markovRing (3,2)
F = marginMap(1,R) 
m = matrix {{p_(1,1)-p_(2,1)-p_(3,1), p_(1,2)-p_(2,2)-p_(3,2), p_(2,1), p_(2,2), p_(3,1), p_(3,2)}}
G = inverseMarginMap(1,R)
assert( (F*G) .matrix == vars R)
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

------------------------------
-- TEST identifyParameters ---
------------------------------

TEST ///
G = mixedGraph(digraph {{b,{c,d}},{c,{d}}},bigraph {{a,d}})
R = gaussianRing G
H = identifyParameters(R)
assert(H === new HashTable from {p_(a,d) => ideal(s_(a,c),s_(a,b),p_(a,d)-s_(a,d)),p_(d,d) => ideal(s_(a,c),s_(a,b),p_(d,d)*s_(b,c)^2-p_(d,d)*s_(b,b)*s_(c,c)-s_(b,d)^2*s_(c,c)+2*s_(b,c)*s_(b,d)*s_(c,d)-s_(b,b)*s_(c,d)^2-s_(b,c)^2*s_(d,d)+s_(b,b)*s_(c,c)*s_(d,d)), l_(c,d) =>ideal(s_(a,c),s_(a,b),l_(c,d)*s_(b,c)^2-l_(c,d)*s_(b,b)*s_(c,c)-s_(b,c)*s_(b,d)+s_(b,b)*s_(c,d)), l_(b,d) =>ideal(s_(a,c),s_(a,b),l_(b,d)*s_(b,c)^2-l_(b,d)*s_(b,b)*s_(c,c)+s_(b,d)*s_(c,c)-s_(b,c)*s_(c,d)), l_(b,c) =>ideal(s_(a,c),s_(a,b),l_(b,c)*s_(b,b)-s_(b,c)), p_(a,a) =>ideal(s_(a,c),s_(a,b),p_(a,a)-s_(a,a)), p_(b,b) =>ideal(s_(a,c),s_(a,b),p_(b,b)-s_(b,b)), p_(c,c) =>ideal(s_(a,c),s_(a,b),p_(c,c)*s_(b,b)+s_(b,c)^2-s_(b,b)*s_(c,c))})
///










----------------------------------------------------------------------------------------------
----------------------------------------------------------------------------------------------
----------------------------------------------------------------------------------------------
----------------------------------------------------------------------------------------------
----------------------------------------------------------------------------------------------
----------------------------------------------------------------------------------------------







     
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
installPackage ("GraphicalModels", RemakeAllDocumentation => true, UserMode=>true)
installPackage("GraphicalModels", RemakeAllDocumentation => true, UserMode=>true, DebuggingMode => true, FileName => "/Users/lgp/Software/Macaulay2/Workshop-2014-Berkeley/GraphicalModels/GraphicalModels.m2")
installPackage("GraphicalModels", RemakeAllDocumentation => true, UserMode=>true, DebuggingMode => true, FileName => "/Users/lgp/Software/Macaulay2/Workshop-2014-Berkeley/GraphicalModels/GraphicalModels-Mike.m2")
installPackage("GraphicalModels", RemakeAllDocumentation => true, UserMode=>true, DebuggingMode => true, FileName => "/Users/lgp/Software/Macaulay2/Workshop-2014-Berkeley/GraphicalModels/GraphicalModels-old.m2")
----------------------
-- Parameterization -- ????????????????????????????????????????????????????????????????????????
---------------------- 
---- We need this for both directed and undirected graphs:

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


end;
restart
installPackage "GraphicalModels"
check "GraphicalModels"

restart
printWidth=75;
needsPackage "GraphicalModels";
G = digraph{{a,d},{b,d},{c,{d,e}},{d,e}}; 
R = gaussianRing G;
gens R
I = conditionalIndependenceIdeal(R,globalMarkov(G));
J = gaussianVanishingIdeal(R);
flatten degrees J
isSubset(I,J), I == J, J == trekIdeal(R,G)

d = (2,3,2); R = markovRing d;
gens R
S = {{{1},{2},{3}}, {{1},{3},{}}}; compactMatrixForm=false; markovMatrices(R,S)
I = conditionalIndependenceIdeal(R,S); flatten degrees  I 
G = graph{{1,2},{2,3},{3,4},{4,5},{1,5}}; netList pack (3,localMarkov G)

G = digraph {{1,{2}}, {2,{3}},{3,{4}},{4,{}}}; R = markovRing (2,2,2,2);
I = discreteVanishingIdeal (R,G); betti mingens I

J = conditionalIndependenceIdeal (R, localMarkov G); I == J
K = conditionalIndependenceIdeal (R, globalMarkov G); I == K

G = mixedGraph(digraph {{1,{2,3}},{2,{3}},{3,{4}}},bigraph {{1,2},{2,4}});
R = gaussianRing G; I = gaussianVanishingIdeal R;
flatten degrees I 
J = trekIdeal (R,G)

H = identifyParameters R;
H#(p_(2,4))_0

///

restart
installPackage ("GraphicalModels",FileName=>"/Users/lgp/Software/Macaulay2/Workshop-2014-Berkeley/GraphicalModels/GraphicalModels.m2", RemakeAllDocumentation => true) 

 restart
 loadPackage("GraphicalModels", FileName => "/Users/lgp/Software/Macaulay2/Workshop-2014-Berkeley/GraphicalModels/GraphicalModels.m2")
 f = () -> (
   R := gaussianRing G;
   gaussianVanishingIdeal R)
 G = graph({{a,b},{b,c},{c,d},{a,d}})
 f()

restart
 loadPackage("GraphicalModels", FileName => "/Users/lgp/Software/Macaulay2/Workshop-2014-Berkeley/GraphicalModels/GraphicalModels.m2")
 f = () -> (
   R := gaussianRing G;
   gaussianVanishingIdeal R)
 G = digraph {{1,{2}}, {2,{3}},{3,{4}},{4,{}}};
 f()
 
 restart
 loadPackage("GraphicalModels", FileName => "/Users/lgp/Software/Macaulay2/Workshop-2014-Berkeley/GraphicalModels/GraphicalModels.m2")
 f = () -> (
     R := gaussianRing G;
     gaussianVanishingIdeal R)
 G = mixedGraph(digraph {{1,{2,3}},{2,{3}},{3,{4}}},bigraph {{1,2},{2,4}});
 f()

///
