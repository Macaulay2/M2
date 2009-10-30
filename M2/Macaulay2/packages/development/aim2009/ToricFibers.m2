newPackage(
	"ToricFibers",
    	Version => "1.0", 
    	Date => "October 29, 2009",
    	Authors => {
         {Name => "Serkan Hosten", Email => "serkan@math.sfsu.edu", HomePage => "http://math.sfsu.edu/serkan"},
         {Name => "Sonja Petrovic", Email => "petrovic@math.uic.edu", HomePage => "http://www.math.uic.edu/~petrovic"},
         {Name => "Brandy Stigler", Email => "bstigler@smu.edu", HomePage => "http://faculty.smu.edu/bstigler"}
         },
    	Headline => "a package for computing fibers of a normal toric ideal",
    	DebuggingMode => false
    	)

--loadDepth = loadDepth - 1

export {buildFiberGraph,
        isDegree,
	isInFiberGraph,
        getFiberTree,
        getGenerators,
	Edges,
	Support,
	MaxLevel
}

----------------------------------------------------------------
-----------------------------------------
--Given a design (dxn)-matrix A of a normal toric model,
--buildFiberGraph returns the  hash table representing the graph of the overlap of fiber trees, 
--for all possible vectors b in the cone(A) such that b is a linear combination of at most d columns of A.
-----------------------------------------
buildFiberGraph = method(TypicalValue=>HashTable, Options=>{MaxLevel => infinity});
buildFiberGraph (Matrix) := ZZ => opts -> A ->(
     --     r:=rank A; --this will be use to stop the computation at level r
     if opts.MaxLevel =!= infinity  then r:=opts.MaxLevel else r=rank A;
     --     r :=min(opts.MaxLevel, rank A);
     d:=numRows A;
     n:=numRows transpose A; --numCols
     G := new MutableHashTable;
     --keys = integer level and  vector b
     --G#l#b{ "Edges" -> { list of indices of cols of A that are outgoing edges of b} 
     --       "Support" -> {indices of cols of A that are in the support of b stored as a list of disjoint sets},
     --	    	      	    where support := the list of columns that appear in each possible way of getting b.
     E:="Edges";
     S:="Support";
     --BuildLevel0: initialize
     l:=0;
     b := vector(toList(d:0));  -- b=zero vector of size d
     G#l = new MutableHashTable; 
     G#l#b=new MutableHashTable;
     G#l#b#E={};
     G#l#b#S={};
     k:=1; --let's also build 1st level directly (speed):
     G#k = new MutableHashTable;
     apply(n, j-> (
	  newB := A_j;
     	  G#k#newB = new MutableHashTable;
     	  G#k#newB#E= {j}; 
	  G#k#newB#S={set {j}};
	  )
     );
     --1st level built.
     -- now build all others:
     apply(k=2..r, k-> buildLevel(k,G) );
     return hashTable apply(keys G, l-> l=>hashTable apply(keys G#l, b->b=>hashTable apply(keys G#l#b, w-> w=> G#l#b#w)));
     )--end of function buildFiberGraph.


------------------------------------
-- The function buildLevel is internal to buildFiberGraph.
-- It builds one level of the graph G, 
-- i.e, level l of the graph consists of all b's 
-- that are a nonnegative linear combination of l columns of the design matrix A.
------------------------------------
buildLevel = (k,G) ->(
     E:="Edges";
     S:="Support";
     G#k = new MutableHashTable; --the table for level k
     --keys G#(k-1) are all b's on the lower leve, k-1
     apply  ( keys G#(k-1) , bLower -> ( 
     	  s := max ( G#(k-1)#bLower#E); --to avoid repetition
	  apply ( s+1, i -> (
  	      b := bLower + A_i; -- s ensures that i <= max (Edges(bLower)):
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
      	     newSupport := sum G#(k-1)#bLower#S + set {i};--this is the new component of fiber(b) consisting of i plus the support sets of bLower
		    toDrop:=new MutableList;--store indices of support sets that have nonepmty intersection with newSupport
		    dropCounter:=0;
		    apply ( #(G#k#b#S), c -> (  
     	       	  if # (newSupport * (G#k#b#S)_c) > 0 then (
			      toDrop#dropCounter = c;
			      dropCounter=dropCounter+1;			      
			      )
			      )
			 );
		    apply ( #toDrop, dr -> (
			      dropMe := #toDrop - 1 - dr;
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


----------------------------------------------------------------
-- Given a design matrix A and a vector b (representing a sufficient statistic of the model assoc. to A),
-- isDegree returns true if 
-- b contributes to a generator of I_A(G), 
-- i.e. the fiber A^(-1)b is disconnected, 
-- i.e. b is a (multigraded) degree of a minimal generator of the toric ideal I_A.
-- The function isDegree uses the fiber graph G associated to A;
-- so, G is an optional input to isDegree.
----------------------------------------------------------------
isDegree = method(TypicalValue=>Boolean);
isDegree (Matrix, Vector) := (A,b) -> (
    G = buildFiberGraph(A);
    return isDegree(G,b)
)

isDegree (HashTable, Vector) := (G, b) -> (
     S:="Support";
     --first of all, is b a key in the table?
     if (not isInFiberGraph(G,b)) then error "fiber is empty; this vector is not a valid degree.";
     --if condition below is true then true is returned, otherwise false
     #(G#L#b#S) > 1
)--end of isDegree

----------------------------------------------------------------
----------------------------------------------------------------
isInFiberGraph=method(TypicalValue=>Boolean);
isInFiberGraph (Matrix, Vector) := (A,b) -> (
    G = buildFiberGraph(A);
    return isInFiberGraph(G,b)
)

isInFiberGraph (HashTable, Vector) := (G, b) -> (
     (any( keys G, l-> G#l #? b )) --then 
)--end of isInFiberGraph
----------------------------------------------------------------
----------------------------------------------------------------



----------------------------------------------------------------
-- Given a design matrix A and a vector b (representing a sufficient statistic of the model assoc. to A),
-- getFiberTree computes all vectors u that are in the fiber A^(-1)b and the vectors are grouped into disjoint sets.
-- Each set contains vectors with pairwise intersecting supports.
-- getFiberTree returns a hash table representing the subgraph of the fiber graph of all vectors in the fiber.
-- Since the function extracts the tree from the fiber graph G associated to A,
-- G is an optional input to getFiberTree.
----------------------------------------------------------------

getFiberTree = method(TypicalValue=>HashTable);
getFiberTree(Matrix, Vector) := (A, b) -> (
    G := buildFiberGraph(A);
    return getFiberTree(A, G, b)   
)

getFiberTree (Matrix, HashTable, Vector) := (A, G, b) ->(
     --is b a key in the table?
     if not any (keys G , l ->  (
	       if (G#l #? b) then L=l;
	       )
	   ) then error("b is not a valid degree");
     -- b is located at level L.
     E:="Edges"; --export these keywords ..
     S:="Support";
     T := new MutableHashTable; 
     T#L = new MutableHashTable;
     T#L#b=new MutableHashTable;
     T#L#b#E = G#L#b#E; --list of edges coming out of b.
     T#L#b#S = G#L#b#S;
     apply ( L, i->(
	       l:=L-i; --going down in levels.
     	       apply( keys T#l, bCurrent->(
		         T#(l-1) = new MutableHashTable;
	                 apply ( T#l#bCurrent#E , e->(
		     	       bLower := bCurrent - A_e; --one of the children of bCurrent
			       T#(l-1)#bLower=new MutableHashTable;
			       T#(l-1)#bLower#E = G#(l-1)#bLower#E; --copy bLower into tree T
			       T#(l-1)#bLower#S = G#(l-1)#bLower#S;
			       )
		          )--end of the loop that goes through the edges of the vector bCurrent at level l.
	       	   )
		    )--end of loop that goes through the vectors at level l
	       ) 
     	  );--end of the loop that changes levels
     -- return the fiber graph (tree)
     --return (new HashTable from T);
     return hashTable apply(keys T, l-> l=>hashTable apply(keys T#l, b->b=>hashTable apply(keys T#l#b, w-> w=> T#l#b#w)));
     ) -- end of getFiberTree


----------------------------------------------------------------
-- Given a design matrix A and a vector b (representing a sufficient statistic of the model assoc. to A),
-- getGenerator returns the exponent vectors of all minimal generators of I_A of degree b, if they exist;
-- otherwise, an error statement is returned.
-- Since the function uses the fiber graph G associated to A,
-- G is an optional input to getFiberTree.
----------------------------------------------------------------

getGenerators = method(TypicalValue=>List);
getGenerators (Matrix,Vector) := (A,b)->(
    G := buildFiberGraph(A);
    return getGenerators(A,G,b)
)

getGenerators (Matrix, HashTable,Vector) := (A,G,b)->( 
     E:="Edges";
     S:="Support";
	  if isDegree(G,b) then (
	       -- b is located at level L.
	       T := getFiberTree(A,G,b);
	       L := # keys T -1;
     	       paths := {};
	       apply( T#L#b#E, e ->(
	       		      pa  := new MutableList from ((numRows transpose A):0);
			      pa#e = pa#e +1; --recording A_e in the monomial
			      bCurrent := b - A_e;--moving down the path to lower level
			      eCurrent := T#(L-1)#bCurrent#E_0;--picking an arbitary (first) edge out of that new component.
			 apply( L-2, i->(
			      l:=L-i-1;
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
	  else error("this vector does not give rise to a generator.");
     --return the exponent vector u-v of the binomial x^u - x^v where Au = Av = b.
     geners:={};
     apply ( #paths-1, j-> (
	       -- binomial = paths_0 - paths_j;
	       geners = append(geners, toList (paths_0) - toList (paths_(j+1))   ); 
	       )
	  );
     return geners;     
     --
     )--end of getGenerator

-------------------------------------------------------------------------
-------------------------------------------------------------------------
-------------------------------------------------------------------------

--loadDepth = loadDepth + 1

beginDocumentation()
needsPackage "SimpleDoc"
debug SimpleDoc

doc ///
  Key
    ToricFibers
  Headline
    a package for computing fibers of a normal toric ideal
  Description
    Text
       The goal of this package is ... (theory and defs go here)
///

doc ///
  Key
    buildFiberGraph
    (buildFiberGraph,Matrix)
  Headline
    Build the fiber graph of a matrix  
  Usage
    G = buildFiberGraph A
  Inputs
    A:Matrix
        whose columns parametrize a normal toric variety.
  Outputs
    G:HashTable
        keys are the levels of the graph
        values are hash tables with keys being vectors b in cone(A) and values being information necessary to recover the fiber $A^{-1}b$
  Description
    Text
        Given a design (dxn)-matrix A of a normal toric model,
        buildFiberGraph returns the hash table representing the graph of the overlap of fiber trees, 
        for all possible vectors b in the cone(A) such that b is a linear combination of at most d columns of A.

        Input:  A = d x n matrix        
        Output: buildFiberGraph, which is a DAG = overlap of various fiber trees, 
        a fiber tree = directed tree with "b" vectors of degree i located at leaf i of the tree
                
        Structure of G: hash table of tables:
        table's set of keys = level, entries = subtable
        subtable's set of keys = the "b" vectors, entries = subsubtables with keys "Edges" and "Support"
        at each "T#l#b" we store: 
        	(1) a list of outgoing edges and 
        	(2) a set of supports (disjoint union)
        start at level 0: b=0, supports = empty; edges = empty
    Example
        A = matrix"
        1,1,1,0,0,0,0,0,0;
        0,0,0,1,1,1,0,0,0;
        0,0,0,0,0,0,1,1,1;
        1,0,0,1,0,0,1,0,0;
        0,1,0,0,1,0,0,1,0;
        0,0,1,0,0,1,0,0,1" --the 3x3 indep model
        G = buildFiberGraph(A);
        peek G
    Text
        Add text 
  SeeAlso
    ToricFibers
    getFiberTree
///

doc ///
  Key
    isDegree
    (isDegree,Matrix,Vector)
    (isDegree,HashTable,Vector)
  Headline
    Check if b contributes to a generator of I_A(G)  
  Usage
    isDegree(A,b) or isDegree(G,b)
  Inputs
    A:Matrix
        whose columns parametrize a normal toric variety
    b:Vector
        degree vector
    G:HashTable
        which represents the fiber graph of A
  Outputs
    :Boolean
        true if b contributes to a generator of I_A(G) and false otherwise 
  Description
    Text
        Given a design (dxn)-matrix A of a normal toric model and a vector b (representing a sufficient statistic of the model assoc. to A),
        isDegree returns true if 
        b contributes to a generator of I_A(G), 
        i.e. the fiber $A^{-1}b$ is disconnected, 
        i.e. b is a (multigraded) degree of a minimal generator of the toric ideal I_A.
        The function isDegree uses the fiber graph G associated to A;
        so, G is an optional input to isDegree.
    Example
    Text
        Add text 
  SeeAlso
    ToricFibers
    buildFiberGraph
///


doc ///
  Key
    isInFiberGraph
    (isInFiberGraph,Matrix, Vector) 
    (isInFiberGraph,HashTable, Vector)
  Headline
    Check if b contributes to a generator of I_A(G)  
  Usage
    isInFiberGraph(A,b) or isInFiberGraph(G,b)
  Inputs
    A:Matrix
        whose columns parametrize a normal toric variety
    b:Vector
        degree vector
    G:HashTable
        which represents the fiber graph of A
  Outputs
    :Boolean
        true if b is a node in the fiber graph of A
  Description
    Text
        Given a design (dxn)-matrix A of a normal toric model and a vector b (representing a sufficient statistic of the model assoc. to A),
        isDegree returns true if 
        b is anywhere in the fiber graph.
    Example
    Text
        Add text 
  SeeAlso
    ToricFibers
    buildFiberGraph
    isDegree
///



doc ///
  Key
    getFiberTree
    (getFiberTree,Matrix,Vector)
    (getFiberTree,Matrix,HashTable,Vector)
  Headline
    Computes the fiber $A^{-1}b$
  Usage
    T=getFiberTree(A,b) or T=getFiberTree(A,G,b)
  Inputs
    A:Matrix
        whose columns parametrize a normal toric variety
    b:Vector
        degree vector
    G:HashTable
        which represents the fiber graph of A
  Outputs
    T:HashTable
        representing the subgraph of the fiber graph of all vectors in the fiber 
  Description
    Text
        Given a design matrix A and a vector b (representing a sufficient statistic of the model assoc. to A),
        getFiberTree computes all vectors u that are in the fiber $A^{-1}b$ and the vectors are grouped into disjoint sets.
        Each set contains vectors with pairwise intersecting supports.
        getFiberTree returns a hash table representing the subgraph of the fiber graph of all vectors in the fiber.
        Since the function extracts the tree from the fiber graph G associated to A,
        G is an optional input to getFiberTree.
        
        Structre: getFiberTree(b), where we also input also b = d x 1 vector
        this will be done for those b's whose graph G_b is disconnected, so that we can recover the minimal generators.
        1) given b extract the graph and if it is connected
        2) if disc. trace the paths to get the monomials (note: need to do this since we are not storing squares at any node b).
    Example
    Text
        Add text 
  SeeAlso
    ToricFibers
    buildFiberGraph
    isDegree
///

doc ///
  Key
    getGenerators
    (getGenerators,Matrix,Vector)
    (getGenerators,Matrix,HashTable,Vector)
  Headline
    Computes the exponent vectors of all minimal generators of I_A of degree b, if they exist  
  Usage
    L=getGenerators(A,b) or L=getGenerators(A,G,b)
  Inputs
    A:Matrix
        whose columns parametrize a normal toric variety
    b:Vector
        degree vector
    G:HashTable
        which represents the fiber graph of A
  Outputs
    L:List
        representing the exponent vectors of all minimal generators of I_A of degree b, if they exist;
        otherwise, an error statement is returned 
  Description
    Text
        Given a design matrix A and a vector b (representing a sufficient statistic of the model assoc. to A),
        getGenerator returns the exponent vectors of all minimal generators of I_A of degree b, if they exist;
        otherwise, an error statement is returned.
        Since the function uses the fiber graph G associated to A,
        G is an optional input to getFiberTree.
    Example
    Text
        Add text 
  SeeAlso
    ToricFibers
    buildFiberGraph
    isDegree
///

doc ///
  Key
    Edges
  Description
    Text
        This is a key for the hash table G#l#b, where l is a level of the hash table G, and b is a vector in the fiber graph G at level l.
        At each T#l#b we store: 
        a list of outgoing edges, SONJA ADD DESCRIPTION HERE.
    Example
    Text
        Add example 
///

doc ///
  Key
    Support
  Description
    Text
        This is a key for the hash table G#l#b, where l is a level of the hash table G, and b is a vector in the fiber graph G at level l.
        At each T#l#b we store: 
        a set of supports (disjoint union), SONJA ADD DESCRIPTION HERE.
    Example
    Text
        Add example.
///


TEST///

///

end

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






restart
--load "4ti2.m2"
installPackage ("ToricFibers", RemakeAllDocumentation => true, UserMode=>true)
installPackage("ToricFibers",UserMode=>true,DebuggingMode => true)
viewHelp ToricFibers


--check FourTiTwo

--debug FourTiTwo









