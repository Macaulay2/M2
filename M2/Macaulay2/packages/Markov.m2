newPackage("Markov",
     Author => "Luis Garcia and Mike Stillman",
     DebuggingMode => false,
     Headline => "Markov ideals, arising from Bayesian networks in statistics",
     Version => "1.0"
     )


------------------------------------------
-- markov ideals in Macaulay2
-- Authors: Luis Garcia and Mike Stillman
-- 
-- Routines:
--   makeGraph {{},{1},{1},{2,3},{2,4}}
--   displayGraph(name,G)
--
--   localMarkovStmts G -- G is a directed graph
--   globalMarkovStmts G
--   pairMarkovStmts G
--
--   removeRedundantStmts S
--  
--   markovRing (2,2,3,3)
--
--   marginMap(R,i) : R --> R
--   hiddenMap(R,i) : R --> S
--
--   markovMatrices S  -- S is a list of independence statements
--   markovIdeal S
--
-- For examples of use, see the 
------------------------------------------



export(makeGraph, displayGraph, localMarkovStmts, globalMarkovStmts, pairMarkovStmts, p,
       markovRing, marginMap, markovMatrices, markovIdeal, writeDotFile, removeRedundants, bayesBall, prob)
exportMutable(dotBinary,jpgViewer)



-------------------------
-- Graph visualization --
-------------------------

-- Give a graph as a hash table i => descendents
-- Make a graph
-- Input: a directed acyclic graph in the form of a 
--        list of lists of children.
--        the vertices must be named 1..n, some n.
--        ASSUMPTION: we assume that the descendents of vertex
--        i are all less than i.  This only represents DAGS.
-- Output: A hashtable G with keys 1..n, and G#i is the
--        the set of all children of the vertex i.
-- This routine produces a useful version of a 'graph'
-- which we use in routines throughout this package.

Graph = new Type of HashTable
     -- a directed graph is a hash table in the form:
     -- { A => set {B,C,...}, ...}, where there are edges A->B, A->C, ...
     -- and A,B,C are integers.  The nodes of the graph must be 1,2,...,N.

makeGraph = (g) -> (
     h := new MutableHashTable;
     scan(#g, i -> h#(i+1) = set g#i);
     new Graph from h)

-- dotBinary = "/sw/bin/dot"
dotBinary = "dot"
-- jpgViewer = "/usr/bin/open"
jpgViewer = "open"

writeDotFile = method()
writeDotFile(String,Graph) := (filename,G) -> (
     fil := openOut filename;
     fil << "digraph G {" << endl;
     p := pairs G;
     for i from 0 to #p-1 do (
	  e := p#i;
	  fil << "  " << toString e#0;
	  if #e#1 === 0 then
	    fil << ";" << endl
	  else (
	    fil << " -> {";
	    links := toList e#1;
	    for j from 0 to #links-1 do
		 fil << toString links#j << ";";
     	    fil << "};" << endl;
	    )
	  );
     fil << "}" << endl << close;
     )

runcmd := cmd -> (
     stderr << "--running: " << cmd << endl;
     r := run cmd;
     if r != 0 then error("--command failed, error return code ",r);
     )

displayGraph = method()

displayGraph(String,String,Graph) := (dotfilename,jpgfilename,G) -> (
     writeDotFile(dotfilename,G);
     runcmd(dotBinary | " -Tjpg "|dotfilename | " -o "|jpgfilename);
     runcmd(jpgViewer | " " | jpgfilename);
     )
displayGraph(String,Graph) := (dotfilename,G) -> (
     jpgfilename := temporaryFileName() | ".jpg";
     displayGraph(dotfilename,jpgfilename,G);
     removeFile jpgfilename;
     )
displayGraph Graph := (G) -> (
     dotfilename := temporaryFileName() | ".dot";
     displayGraph(dotfilename,G);
     removeFile dotfilename;
     )

-------------------------
-- Statements -----------
-------------------------

------------------
-- Graph basics --
------------------

descendents = method()
descendents(Graph,ZZ) := (G,v) -> (
     -- returns a set of vertices
     result := G#v;
     scan(reverse(1..v-1), i -> (
	  if member(i,result) then result = result + G#i;
     ));
     result)

nondescendents = method()
nondescendents(Graph,ZZ) := (G,v) -> set(1..#G) - descendents(G,v) - set {v}

parents = method()
parents(Graph,ZZ) := (G,v) -> set select(1..#G, i -> member(v, G#i))

children = method()
children(Graph,ZZ) := (G,v) -> G#v

--------------------------
-- Statement calculus ----
--------------------------
-- A dependency is a list {A,B,C}
--  where A,B,C are (disjoint) subsets of positive integers.
--  The meaning is: A is independent of B given C.
-- A dependency list is a list of dependencies

-- No serious attempt is made to remove redundant dependencies.
-- However, we have several very simple routines to remove
-- the most obvious redundant elements
-- If S and T represent exactly the same dependency, return true.

equivStmts = (S,T) -> S#2 === T#2 and set{S#0,S#1} === set{T#0,T#1}

removeRedundants = (Ds) -> (
     -- Ds is a list of triples of sets {A,B,C}
     -- test1: returns true if D1 can be removed
     -- Return a sublist of Ds which removes any 
     --  that test1 declares not necessary.
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
     Ds_c)

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
     -- as implemented by Luis Garcia, after
     -- the paper of Ross Schlacter
     n := #G;
     zeros := toList((n+1):false);
     visited := new MutableList from zeros;
     blocked := new MutableList from zeros;
     up := new MutableList from zeros;
     down := new MutableList from zeros;
     top := new MutableList from zeros;
     bottom := new MutableList from zeros;
     vqueue := sort toList A;
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
     set toList select(1..n, i -> not blocked#i and not bottom#i)
     )

--------------------------
-- Markov relationships --
--------------------------
pairMarkovStmts = (G) -> (
     -- given a graph G, returns a list of triples {A,B,C}
     -- where A,B,C are disjoint sets, and for every vertex v
     -- and non-descendent w of v,
     -- {v, w, nondescendents(G,v) - w}
     removeRedundants flatten apply(toList(1..#G), v -> (
	       ND := nondescendents(G,v);
	       W := ND - parents(G,v);
	       apply(toList W, w -> {set {v}, set{w}, ND - set{w}}))))
			 
localMarkovStmts = (G) -> (
     -- Given a graph G, return a list of triples {A,B,C}
     -- of the form {v, nondescendents - parents, parents}
     result := {};
     scan(1..#G, v -> (
	       ND := nondescendents(G,v);
	       P := parents(G,v);
	       if #(ND - P) > 0 then
	         result = append(result,{set{v}, ND - P, P})));
     removeRedundants result)

globalMarkovStmts = (G) -> (
     -- Given a graph G, return a complete list of triples {A,B,C}
     -- so that A and B are d-separated by C (in the graph G).
     -- If G is large, this should maybe be rewritten so that
     -- one huge list of subsets is not made all at once
     n := #G;
     vertices := toList(1..n);
     result := {};
     AX := subsets vertices;
     AX = drop(AX,1); -- drop the empty set
     AX = drop(AX,-1); -- drop the entire set
     scan(AX, A -> (
	       A = set A;
	       Acomplement := toList(set vertices - A);
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
-------------------
-- Markov rings ---
-------------------
markovRingList = new MutableHashTable;
markovRing = d -> (
     -- d should be a sequence of integers di >= 1
     if any(d, di -> not instance(di,ZZ) or di <= 0)
     then error "useMarkovRing expected positive integers";
     if not markovRingList#?d then (
     	  start := (#d):1;
     	  markovRingList#d = ZZ/32003[(symbol p)_start .. (symbol p)_d];
	  markovRingList#d.markov = d;
	  );
     markovRingList#d
     )

  --------------
  -- marginMap
  -- Return the ring map F : R --> R such that
  --   F p_(u1,u2,..., +, ,un) = p_(u1,u2,..., 1, ,un)
  -- and
  --   F p_(u1,u2,..., j, ,un) = p_(u1,u2,..., j, ,un), for j >= 2.
  --------------

marginMap = (v,R) -> (
     -- R should be a Markov ring
     v = v-1;
     d := R.markov;
     use R;
     F := toList apply(((#d):1) .. d, i -> (
	       if i#v > 1 then p_i
	       else (
		    i0 := drop(i,1);
		    p_i - sum(apply(toList(2..d#v), j -> (
			      newi := join(take(i,v), {j}, take(i,v-#d+1));
			      --print p_newi;
			      p_newi))))));
     map(R,R,F))

hideMap = (v,A) -> (
     -- creates a ring map inclusion F : S --> A.
     v = v-1;
     R := ring presentation A;
     d := R.markov;
     e := drop(d, {v,v});
     S := markovRing e;
     dv := d#v;
     use A;
     F := toList apply(((#e):1) .. e, i -> (
	       sum(apply(toList(1..dv), j -> (
			      newi := join(take(i,v), {j}, take(i,v-#d+1));
			      --print p_newi;
			      p_newi)))));
     map(A,S,F))



-------------------------------------------------------
-- Constructing the ideal of a independence relation --
-------------------------------------------------------
cartesian := (L) -> (
     if #L == 1 then 
	return toList apply (L#0, e -> 1:e);
     L0 := L#0;
     Lrest := drop (L,1);
     C := cartesian Lrest;
     flatten apply (L0, s -> apply (C, c -> prepend (s,c))))

possibleValues := (d,A) ->
     cartesian (toList apply(1..#d, i -> 
	       if member(i,A) 
	       then toList(1..d#(i-1)) 
	       else {0}))

prob = (d,s) -> (
     L := cartesian toList apply (#d, i -> 
	   if s#i === 0 
	   then toList(1..d#i) 
	   else {s#i});
     sum apply (L, v -> p_v))

markovMatrices = (R, Stmts) -> (
     -- R should be a Markov ring, and S is a list of
     -- independence statements
     d := R.markov;
     flatten apply(Stmts, stmt -> (
     	       Avals := possibleValues(d,stmt#0);
     	       Bvals := possibleValues(d,stmt#1);
     	       Cvals := possibleValues(d,stmt#2);
     	       apply(Cvals, c -> (
                  matrix apply(Avals, 
		       a -> apply(Bvals, b -> (
				 e := toSequence(toList a + toList b + toList c);
		      		 prob(d,e))))))))
     )

markovIdeal = (R,Stmts) -> (
     M := markovMatrices(R,Stmts);
     sum apply(M, m -> minors(2,m))
     )

beginDocumentation()

document {
     Key => "Markov"
     }

-- Local Variables:
-- compile-command: "make -C $M2BUILDDIR/Macaulay2/packages Markov.installed"
-- End:
