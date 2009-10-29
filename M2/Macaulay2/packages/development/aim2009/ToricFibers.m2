----------------------------------------------------------------
-- DESCRIPTION
----------------------------------------------------------------

-- input:  A = d x n matrix
--
-- GOAL#1: buildFiberGraph, which is a DAG = overlap of various fiber trees, 
-- a fiber tree = directed tree with "b" vectors of degree i located at leaf i of the tree
--
-- SOLUTION: hash table of tables:
-- table's set of keys = level, entries = subtable
-- subtable's set of keys = the "b" vectors, entries = subsubtables with keys "Edges" and "Support"
-- at each T#l#b we store: 
-- 	(1) a list of outgoing edges and 
-- 	(2) a set of supports (disjoint union)
-- start @ level 0: b=0, supports = empty; edges = empty
--
--
-- GOAL#2: getFiberTree(b), where we also input also b = d x 1 vector
-- this will be done for those b's whose graph G_b is disconnected, so that we can recover the minimal generators.
-- 1) given b extract the graph and if it is connected
-- 2) if disc. trace the paths to get the monomials (note: need to do this since we are not storing squares at any node b).
------------------------------------------------


----------------------------------------------------------------
-- FUNCTIONS for goal#2
----------------------------------------------------------------
-- two options for input: (1) b and G , or (2) b and A.
-- option (1):
isDegree = (G, b) ->(
     --return true if the vector b is a generating vector, 
     --i.e. contributes to a generator of I_A(G), 
     --i.e. its fiber is disconnected, 
     --i.e. is a (multigraded) Markov degree.
     L=0;
     --first of all, is b a key in the table?
     scan(keys G , l ->  (
	       if (G#l #? b) then L=l;
	       )
	   );
     --note if L=0 at the end of this, then b is not in the cone(A).
     if L==0 then error("--fiber is empty; this vector is not a valid degree.")
     else (
     	  if (# ( G#L#b#S) > 1 ) then
               return true
     	  else return false; 
     	  )
     )--end of isDegree
getFiberTree = (A,G, b) ->(
     --is b a key in the table?
     L=0;
     scan(keys G , l ->  (
	       if (G#l #? b) then L=l;
	       )
	   );
     if L==0 then error("--b is not a valid degree")
     else (
	  -- b is located at level L.
     E="Edges"; --export these keywords ..
     S="Support";
     T = new MutableHashTable; 
     T#L = new MutableHashTable;
     T#L#b=new MutableHashTable;
     T#L#b#E = G#L#b#E; --list of edges coming out of b.
     T#L#b#S = G#L#b#S;
     apply ( L, i->(
	       l=L-i; --going down in levels.
     	       apply( keys T#l, bCurrent->(
		         T#(l-1) = new MutableHashTable;
	                 apply ( T#l#bCurrent#E , e->(
		     	       bLower = bCurrent - A_e; --one of the children of bCurrent
			       T#(l-1)#bLower=new MutableHashTable;
			       T#(l-1)#bLower#E = G#(l-1)#bLower#E; --copy bLower into tree T
			       T#(l-1)#bLower#S = G#(l-1)#bLower#S;
			       )
		          )--end of the loop that goes through the edges of the vector bCurrent at level l.
	       	   )
		    )--end of loop that goes through the vectors at level l
	       ) 
     	  );--end of the loop that changes levels
     );--end of else
     -- return the fiber graph (tree)
     return (new HashTable from T);
     ) -- end of getFiberTree
getGenerator = (A,G,b)->( --think about returning binomial vs. exponent vector. 
	  if isDegree(G,b) then (
	       -- b is located at level L.
	       T = getFiberTree(A,G,b);
	       L = # keys T -1;
     	       paths = {};
	       apply( T#L#b#E, e ->(
	       		      pa  = new MutableList from ((numRows transpose A):0);
			      pa#e = pa#e +1; --recording A_e in the monomial
			      bCurrent = b - A_e;--moving down the path to lower level
			      eCurrent = T#(L-1)#bCurrent#E_0;--picking an arbitary (first) edge out of that new component.
			 apply( L-2, i->(
			      l=L-i-1;
			      pa#eCurrent = pa#eCurrent + 1;
			      bCurrent = bCurrent - A_eCurrent;   
			      eCurrent = T#(l-1)#bCurrent#E_0;
			      )
			 );--end inner apply
         		 pa#eCurrent = pa#eCurrent + 1;
		    	 paths = append(paths, pa);
			 )
		    );--end apply
	       )
	  else error("--this vector does not give rise to a generator.");
     --return the exponent vector u-v of the binomial x^u - x^v where Au = Av = b.
     geners={};
     apply ( #paths-1, j-> (
	       -- binomial = paths_0 - paths_j;
	       geners = append(geners, toList (paths_0) - toList (paths_(j+1))   ); 
	       )
	  );
     return geners;     
     --
     )--end of getGenerator





----------------------------------------------------------------
-- FUNCTIONS for goal#1
----------------------------------------------------------------

------------------------------------
-- build one level of the graph G --
------------------------------------
buildLevel = (k,G) ->(
     G#k = new MutableHashTable; --the table for level k
     --keys G#(k-1) are all b's on the lower leve, k-1
     apply  ( keys G#(k-1) , bLower -> ( 
     	  s = max ( G#(k-1)#bLower#E); --to avoid repetition
	  apply ( s+1, i -> (
  	      b = bLower + A_i; -- s ensures that i <= max (Edges(bLower)):
     	       if not (G#k #? b ) then (
		    --print "new b, the vector b is not at this level yet"; 
		    -- G#k#b -> Edges = {i}, Support = {i union {Support(bLower)}} 
		    G#k#b = new MutableHashTable;     
		    G#k#b#E = {i};
     	       	    G#k#b#S = { sum G#(k-1)#bLower#S + set {i} }; 
     		    --sum ensures we take the union of all disjoint pieces in the support of bLower; 
		    --this is needed since they all now form one connected component for b#Support: they share edge i.
		    )
 	      else  (
     	       	    --print "not new b";
     	       	    G#k#b#E = append (G#k#b#E , i);
		    --next, update the Support of b: 
      	       	    newSupport = sum G#(k-1)#bLower#S + set {i};--this is the new component of fiber(b) consisting of i plus the support sets of bLower
		    toDrop=new MutableList;--store indices of support sets that have nonepmty intersection with newSupport
		    dropCounter=0;
		    apply ( #(G#k#b#S), c -> (  
     	       	    	 if # (newSupport * (G#k#b#S)_c) > 0 then (
			      toDrop#dropCounter = c;
			      dropCounter=dropCounter+1;			      
			      )
			      )
			 );
		    apply ( #toDrop, dr -> (
			      dropMe = #toDrop - 1 - dr;
			      --sets with nonempty intersection with newSupport should be in the same connected component:
			      newSupport = newSupport + (G#k#b#S)_((toDrop)#dropMe);
			      G#k#b#S=drop(G#k#b#S,{toDrop#dropMe,toDrop#dropMe});--no longer disjoint components, so drop them
      	      	   	      )
			 );
      		    G#k#b#S = append(G#k#b#S,newSupport); --add one new big conn.component
		    )--end of else.
	       )--end list of commands for i loop;
	  ) --the end of the inner loop (for all i).
	  )--end list of commands for bLower loop;
     ) -- the end of the loop that goes thru all the bLower vectors.
     ) --end of buildLevel function.


-----------------------------------------
-- build the graph up to level=rank(A) --
-----------------------------------------
buildFiberGraph = A ->(
     r=rank A; --this will be use to stop the computation at level r
     d=numRows A;
     n=numRows transpose A; --numCols
     G := new MutableHashTable;
     --keys = integer level and  vector b
     --G#l#b{ "Edges" -> { list of indices of cols of A that are outgoing edges of b} 
     --       "Support" -> {indices of cols of A that are in the support of b stored as a list of disjoint sets},
     --	    	      	    where support := the list of columns that appear in each possible way of getting b.
     E="Edges";
     S="Support";
     --BuildLevel0: initialize
     l=0;
     b = vector(toList(d:0));  -- b=zero vector of size d
     Edges={}; -- Edges = empty;
     Support={};-- Support = empty;
     G#l = new MutableHashTable; 
     --G#l#b = (Edges,Support)
     G#l#b=new MutableHashTable;
     G#l#b#E=Edges;
     G#l#b#S=Support;
     k=1; --let's also build 1st level directly (speed):
     G#k = new MutableHashTable;
     apply(n, j-> (
	  newB = A_j;
     	  G#k#newB = new MutableHashTable;
     	  G#k#newB#E= {j}; 
	  G#k#newB#S={set {j}};
	  )
     );
     --1st level built.
     -- now build all others:
     apply(k=2..r, k-> buildLevel(k,G) );
     return G;
     )--end of function buildFiberGraph.




----------------------------------------------------------------
-- EXAMPLE:
-- the 3x3 independence model
----------------------------------------------------------------
A = matrix"
1,1,1,0,0,0,0,0,0;
0,0,0,1,1,1,0,0,0;
0,0,0,0,0,0,1,1,1;
1,0,0,1,0,0,1,0,0;
0,1,0,0,1,0,0,1,0;
0,0,1,0,0,1,0,0,1"

--qn : see show-and-tell-THU.m2;
--why did it complain if i called my matrix "M" ?!?!

G = buildFiberGraph(A);

bTrue=vector{1,0,1,1,1,0};
isDegree(G,bTrue)
bFalse = vector{1,0,0,1,0,0};
isDegree(G,bFalse)
--beautiful!
bStupid = vector{1,2,3,4,5,6};
isDegree(G,bStupid)

peek G

T= getFiberTree(A, G, bTrue) --perhaps attach A to G somehow? nah... the user knows A.
T = getFiberTree(A, G, bFalse) 
T= getFiberTree(A, G, bStupid) 

getGenerator(A,G,bFalse)
getGenerator(A,G,bStupid)
getGenerator(A,G,bTrue)

peek T
peek T#1



--check the size of each level:
apply(k=0..r, k-> # keys G#k) 
--perfect!!!

-- ok let's print out level 2 :
l=3; 
K = keys G#l;
#K --36 nodes @ level 2 -ok
apply(#K,j->(
	  key=K_j;
	  (key, new HashTable from G#l#key)
	  )
     )

------ test for our example A that there are no gens of degree 3, that is, there are no b's such that the Support has more then one set (the graph G_b is connected).
l=3; 
K = keys G#l;
#K --36 nodes @ level 2 -ok
apply(#K,j->(
	  key=K_j;
	  # unique (G#l#key#S)
	  )
     )
--PERFECT!!
----------------------------------------------------------------
----------------------------------------------------------------
----------------------------------------------------------------















