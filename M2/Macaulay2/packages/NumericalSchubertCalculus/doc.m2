document {
     Key => NumericalSchubertCalculus,
     Headline => "Numerical Algorithms for Schubert Calculus",
     "Tools for solving Schubert problems on Grassmannians using numerical homotopy continuation.",
     PARA {"The package NumericalSchubertCalculus implements the Littlewood-Richardson and Pieri homotopy algorithms."}, 
     
     HEADER3 {"General functions include:"},
     UL{
        TO randomSchubertProblemInstance, 
	--(an instance of a Schubert problem with random flags),   
	 TO changeFlags,
	 --(continues solutions from one instance to another),
	 TO checkIncidenceSolution, 
	 --(verifies that a solution satisfies the given incidence conditions).
     },
     HEADER3{"Functions implementing homotopies specific to Schubert calculus:"},
     UL{
	 TO solveSchubertProblem,
	 --(Core routine:  solves a Schubert problem using the Littlewood-Richardson homotopy)
	 TO solveSimpleSchubert
	 --(Solves a simple Schubert problem using the Pieri homotopy algorithm)
	 },
     HEADER3{"Service functions:"},
     UL{
	 TO setVerboseLevel,
	 --(how talkative the functions are)
	 TO bracket2partition,
	 --(converts a bracket into a partition)
	 TO partition2bracket
	 --(converts a partition into a bracket)
	 },
     HEADER3{"Using PHCpack:"},
     "An alternative implementation using PHCpack (download from ",
     HREF{"http://homepages.math.uic.edu/~jan/download.html","here"},
     ") includes the following functions:",
     UL{
	 TO LRrule,
	 TO LRtriple,
	 TO parseTriplet,
	 TO LRcheater,
         TO PieriRootCount,
         TO PieriHomotopies
	 },
     "Note that ",TO LRcheater," is similar to ", TO changeFlags,
     HR{},
     "For example, the Schubert problem (2,1)^3 in Gr(3,6) with respect to random flags has 2 solutions",
     EXAMPLE {
	 "k = 3; n = 6;",
	 "SchPblm = {
	 ({2,1}, random(CC^6,CC^6)),
	 ({2,1}, random(CC^6,CC^6)),
	 ({2,1}, random(CC^6,CC^6))
	 };",
     "solveSchubertProblem(SchPblm, k,n)"
     },
     HR{},
     HEADER4{"References:"},
     UL{
	 HREF{"https://arxiv.org/abs/math/0302294", "Vakil, \"A geometric Littlewood-Richardson rule\""},
	 HREF{"https://arxiv.org/abs/alg-geom/9706004", "Huber, Sottile, Sturmfels, \"Pieri's rule for flag manifolds and Schubert polynomials\",  J. Symb. Comp., 26 (1998), 767-788."},
	 HREF{"https://arxiv.org/abs/1001.4125", "Sottile, Vakil, Verschelde, \"Solving Schubert Problems with Littlewood-Richardson Homotopies\""},
	 HREF{"https://arxiv.org/abs/0710.4607", "Leykin and Sottile, \"Galois groups of Schubert problems via homotopy computation\" "},
	 HREF{"http:// ", "Leykin, Martin del Campo, Sottile, Vakil, Verschelde \"Numerical Schubert Calculus via the Littlewood-Richardson homotopy algorithm\" "}
	 },
 }

-- Documentation Littlewood-Richardson homotopies
--------------
doc ///
   Key
      randomSchubertProblemInstance
      (randomSchubertProblemInstance,List,ZZ,ZZ)
   Headline
      Returns a random instance of a given Schubert problem by computing random matrices representing flags
   Usage
      randomSchubertProblemInstance(conditions,k,n)
   Inputs
      conditions:List
        which is a list of Schubert conditions that are either all partitions or all brackets (see @TO bracket2partition@ for details)
      k:ZZ
      n:ZZ
         $k$ and $n$ define the Grassmannian $Gr(k,n)$
   Outputs
      :List
         random instance of the Schubert problem, which is a list of pairs of the form (condition,flag)
   Description
      Text
         This first verifies that the conditions are either all partitions or all brackets, and that they form a Schubert problem on $Gr(k,n)$.
	 
	 Then it creates a list of random square invertible matrices that represent flags for the Schubert problem.
      Example
         -- the problem of 4 lines is given by 4 partitions {1}^4 in Gr(2,4) 
	 randomSchubertProblemInstance({{1},{1},{1},{1}},2,4)
	 -- the same problem but using brackets instead of partitions
	 randomSchubertProblemInstance({{2,4},{2,4},{2,4},{2,4}},2,4)
   Caveat
      The output consists of random numerical matrices that are assumed invertible. The code does not check for this invertibility condition.
   SeeAlso
         solveSchubertProblem
	 partition2bracket
	 bracket2partition
///

doc ///
   Key
      [randomSchubertProblemInstance, Strategy]
   Headline
      Strategy for creating a random matrix representing a random flag
   Usage
      randomSchubertProblemInstance(...,Strategy=>S)
   Inputs
      S:String
        "unit circle" entries are random unit complex numbers [default] 
	"unitary" uses the Random Unitary Matrix from NAG4M2.
   Description
    Text
     Determines how a random matrix for a random flag is created.
///;

doc ///
   Key
      solveSchubertProblem
      (solveSchubertProblem,List,ZZ,ZZ)
   Headline
      uses Littlewood-Richardson homotopy to solve Schubert problems on Grassmannians
   Usage
      S = solveSchubertProblem(P,k,n)
   Inputs
      P:List
         Schubert problem given as a list of sequences of the 
	 form ($l,F$) where $l$ is a partition (a list of weakly 
	 decreasing integers) and $F$ is a flag ($n$ by $n$ matrix) 
      k:ZZ
      n:ZZ
      	 $k$ and $n$ denote the Grassmannian $Gr(k,n)$
      --LinearAlgebra:Boolean
      --   when True, uses Linear Algebra to glue solutions from node to node, otherwise uses parameter homotopies.
   Outputs
      S:List
         solutions of the Schubert Problem given as $n$ by $k$ matrices
   Description
      Text
      	 Represent a Schubert variety in the Grassmannian $Gr(k,n)$ 
	 by a condition $c$ either a partition or a bracket (see  @TO partition2bracket@ for details) and a flag $F$ 
	 (given as an $n{\times} n$ matrix).
	 The codimention of the Schubert variety is $|C|$.
	 A Schubert problem is a list of Schubert varieties, whose codimention
	 add up to $k(n-k)$, which is the dimension of the Grassmannian.
         -----
	 
	 The function solves the Schubert problem by the Littlewood-Richardson
	 homotopy. This algorithm uses homotopy continuation to track 
	 solutions of a simpler problem to a general problem according 
	 to the specializations of the geometric Littlewood-Richardson.

	 This algorithm is described in the paper:
 	 Leykin, Martin del Campo, Sottile, Vakil, Verschelde "Numerical Schubert Calculus via the Littlewood-Richardson homotopy algorithm".
	 
      Example
         -- Problem (2,1)^3 = 2 in Gr(3,6)
       	 -- a problem with 2 solutions
	 k = 3;
	 n = 6;
	 SchPblm = {
    	     ({2,1}, random(CC^6,CC^6)),
    	     ({2,1}, random(CC^6,CC^6)),
    	     ({2,1}, random(CC^6,CC^6))
    	     };
	 stdio << "Schubert problem {2,1}^3 in Gr(3,6) with respect to random flags"<<endl;
	 
	 solveSchubertProblem(SchPblm, k,n)
   Caveat
      The Schubert conditions are either all partitions or all brackets.
   SeeAlso
         solveSimpleSchubert
	 partition2bracket
///;
doc ///
   Key
      [solveSchubertProblem, LinearAlgebra] --verify this optional input with Anton
   Headline
      switch between Linear Algebra and Parameter Homotopy
   Usage
      solveSchubertProblem(...,LinearAlgebra=>T)
   Inputs
      T:Boolean
         true for Linear Algebra, false for Parameter Homotopy 
   Description
    Text
      Chooses the method for glueing solutions from the top of one node (tournament game) to the leaf of the previous node.
      When true (default), uses Linear Algebra, otherwise uses Parameter Homotopies.
   Caveat
      Parameter Homotopies are not necessary and usually take longer than simple Linear Algebra.  
///

doc ///
   Key
      solveSimpleSchubert
      (solveSimpleSchubert,List,ZZ,ZZ)
   Headline
      uses Pieri homotopy algorithm to solve simple Schubert problems on Grassmannians
   Usage
      S = solveSimpleSchubert(P,k,n)
   Inputs
      P:List
         Simple Schubert problem given as a list of sequences of the 
	 form (l,F) where l is a partition (a list of weakly 
	 decreasing integers) and F is a flag (n by n matrix).
         Necessarily, all partitions except possible the first two are {1}
      k:ZZ
      n:ZZ
      	 k and n denote the Grassmannian Gr(k,n)
      --LinearAlgebra:Boolean
      --   when True, uses Linear Algebra to glue solutions from node to node, otherwise uses parameter homotopies.
   Outputs
      S:List
         solutions of the simple Schubert Problem given as n by k matrices
   Description
      Text
      	 Represent a Schubert variety in the Grassmannian $Gr(k,n)$ 
	 by a partition $l$ (a weakly decreasing
	 list of nonegative integers less than $n-k$) and a flag $F$ 
	 (given as an $n{\times} n$ matrix).
	 A Schubert problem is a list of Schubert varieties 
	 $(l^1, F^1), \ldots, (l^m, F^m)$ such that 
	 $|l^1|+|l^2| + \cdots + |l^m| = k(n-k)$, where $|l^i|$ is the 
	 sum of the entries of $l_i$.
         -----
	 
	 The function solves the Schubert problem by the Pieri homotopy algorithm. 
	 This algorithm uses homotopy continuation to track 
	 solutions of a simpler problem to a general problem according 
	 to the specializations of the geometric Pieri rule.

	 This algorithm is described in the paper:
	 Huber, Sottile, and Sturmfels, "Numerical Schubert Calculus".
	 
      Example
         -- Problem (2,1)^2 1^3 = 6 in Gr(3,6)
       	 -- a problem with 2 solutions
	 k = 3;
	 n = 6;
	 SchPblm = {
    	     ({2,1}, random(CC^6,CC^6)),
    	     ({2,1}, random(CC^6,CC^6)),
    	     ({1}, random(CC^6,CC^6)),
    	     ({1}, random(CC^6,CC^6)),
    	     ({1}, random(CC^6,CC^6))
    	     };
	 stdio << "Schubert problem {2,1}^2 {1}^3 in Gr(3,6) with respect to random flags"<<endl;
	 
	 solveSimpleSchubert(SchPblm, k,n)
   Caveat
      Need to input partitions together with flags. In the future, 
      there will be an option for generating random flags and just 
      input the first two partitions.   Also, it will be able to take brackets.
   SeeAlso
         solveSchubertProblem
///

doc ///
   Key
      partition2bracket
      (partition2bracket,List,ZZ,ZZ)
   Headline
      dictionary between different notations for Schubert conditions.
   Usage
      b = partition2bracket(l,k,n)
   Inputs
      l:List
         partition representing a Schubert condition
      k:ZZ
      n:ZZ
         k and n represent the Grassmannian Gr(k,n)
   Outputs
      b:List
         the corresponding bracket
   Description
    Text
       A Schubert condition in the Grassmannian $Gr(k,n)$ is encoded either 
       by a partition $l$ or by a bracket $b$. 
       
       A partition is a weakly decreasing list of at most $k$ nonnegative 
       integers less than or equal to $n-k$.  It may be padded with zeroes to be of length $k$.
       
       A bracket is a strictly increasing list of length $k$ of 
       positive integers between $1$ and $n$.
       
       This function writes a partition as a bracket.
       They are related as follows $b_{k+1-i}=n-i-l_i$, for $i=1,...,k$.
       
    Example
       l = {2,1};
       k = 2;
       n = 4;
       partition2bracket(l,k,n)
       
       k = 3;
       n = 6;
       partition2bracket(l,k,n)
   SeeAlso
      bracket2partition
///

doc ///
   Key
      bracket2partition
      (bracket2partition,List,ZZ)
   Headline
      dictionary between different notations for Schubert conditions.
   Usage
      l = bracket2partition(b,n)
   Inputs
      b:List
         of length k, a bracket representing a Schubert condition.
      n:ZZ
         k and n represent the Grassmannian Gr(k,n)
   Outputs
      l:List
         the corresponding partition 
   Description
    Text
       A Schubert condition in the Grassmannian $Gr(k,n)$ is encoded either 
       by a partition $l$ or by a bracket $b$. 
       
       A partition is a weakly decreasing list of at most $k$ nonnegative 
       integers less than or equal to $n-k$.  It may be padded with zeroes to be of length $k$.
       
       A bracket is a strictly increasing list of length $k$ of 
       positive integers between $1$ and $n$.
       
       This function writes a bracket as a partition.
       They are related as follows  $b_{k+1-i}=n-i-l_i$, for $i=1,...,k$.
    Example
       b = {1,3};
       n = 4;
       bracket2partition(b,n)
       
       n = 6;
       bracket2partition(b,n)
       
       b = {2,4,6};
       bracket2partition(b,n)
   SeeAlso
      partition2bracket
///
document{
    Key => setVerboseLevel,
    Headline => "Set different levels of information printed on screen",
    Usage => "setVerboseLevel n",
    Inputs =>{"n" =>{TO "ZZ", " takes values 0,1,2, or greater"}},
    Consequences=>{"Prints information on the screen depending on the value of n"},
    PARA{"The function displays different levels of information visible on
       the screen:"},
    UL{"0 = no extra information displayed [default]",
       "1 = print the progress information and time the main process",
       "2 = besides the information of level 1, it also displays the checkerboard steps"},
   EXAMPLE{
       "-- The problem of 4 lines w.r.t. random flags
SchPblm = randomSchubertProblemInstance ({{1},{1},{1},{1}},2,4)",
       "setVerboseLevel 0;",
       "S = solveSchubertProblem(SchPblm,2,4)",
       "assert all(S,s->checkIncidenceSolution(s,SchPblm))",
       "setVerboseLevel 1;",
       "S = solveSchubertProblem(SchPblm,2,4)",
       "assert all(S,s->checkIncidenceSolution(s,SchPblm))"
       },
   SeeAlso => {checkIncidenceSolution}
    }
doc ///
   Key
      solutionsToAffineCoords
      (solutionsToAffineCoords, List)
   Headline
      writes solutions in global coords to affine coordinates.
   Usage
      S' = solutionsToAffineCoords S
   Inputs
      S:List
         solutions to a Schubert problem in global coordinates
   Outputs
      S':List
         solutions in coordinates of an affine patch with an identity in the bottom
   Description
    Text
       Takes a list of solutions $s\in Gr(k,n)$ in global coordinates and writes
       them as solutions in coordinates of the affine patch that has an identity
       matrix in the last $k$ rows.
       --$s = [**||id]$ the identity on bottom 
    Example
       Pblm = {
    	   ({2,1}, random(RR^6,RR^6)),
    	   ({2,1}, random(RR^6,RR^6)),
    	   ({2,1}, random(RR^6,RR^6))
    	   }
       S = solveSchubertProblem(Pblm, 3,6)
       solutionsToAffineCoords S
   Caveat
      This function may fail if the solutions are not in general position 
      (if they cannot fit the specific local coordinates)
      One way to avoid this is by applying a random linear transformation to 
      the solutions before calling this function
   SeeAlso
///

doc ///
   Key
      changeFlags 
      (changeFlags,List,Sequence)
   Headline
      Parameter homotopies to move solutions of a Schubert problem from one instance to another
   Usage
      changeFlags(Solns,conds'F'G)
   Inputs
      Solns:List
         solutions of a problem written as nxk matrices
      conds'F'G:Sequence
         a triplet (C,F,G) where C is a list of m Schubert conditions,
	 F is a list of m flags defining an instance with solution set S,
	 G is a list of m flags defining the instance whose solutions we want.
   Outputs
      :List
         solutions of the problem with respect to flags G.
   Description
    Text
       If $S$ is a set of solutions to a Schubert problem $l_1,\ldots,l_m$
       with respect to a set of flags $F_1,\ldots, F_m$, uses parameter homotopies
       to move $S$ to a solution set $S'$ for the same Schubert problem, but with
       respect to another set of flags $G_1,\ldots, G_m$.
    Example
       stdio<< "Schubert problem (2,1)^3 in Gr(3,6)"<<endl;
       k=3; n=6;
       l1={2,1}; 
       l2={2,1};
       l3={2,1};
       stdio<<"Generate flags F: standard flag, opposite flag, and one random"<<endl;
       F = {(l1, id_(CC^n)), (l2, rsort id_(CC^n)), (l3,random(CC^n,CC^n))}
       stdio<<"Generate a random set of flags G"<<endl;
       G = randomSchubertProblemInstance({l1,l2,l3},k,n)
       stdio<<"We solve with respect to F"<<endl;
       S = solveSchubertProblem(F,k,n)
       FlagsF = F/last;
       FlagsG = G/last;
       stdio<< "and we transform the solutions to get solutions with respect to G"<<endl;
       --time S' = changeFlags(S,({l1,l2,l3},FlagsF,FlagsG))
       --assert all(S', s-> checkIncidenceSolution(s,G))
       stdio<< "We can also choose a different strategy"<<endl;
       --time S' = changeFlags(S,({l1,l2,l3},FlagsF,FlagsG), "one homotopy"=>false)
       --assert all(S', s-> checkIncidenceSolution(s,G))
   Caveat
      There are two strategies: when oneHomotopy is set to true (default) it uses straight
      line homotopies to change flags, but assumes the initial flags are generic.
      
      When oneHomotopy is set to false, it makes gradual changes in the flags by changing
      one column at a time, and using only linear homotopies, but in this setting, it generates
      polynomial equations using all minors of the incidence conditions (thus is not very effective).
   SeeAlso
      randomSchubertProblemInstance
      solveSchubertProblem
      checkIncidenceSolution
///

doc ///
   Key
      [changeFlags, oneHomotopy]
   Headline
      Strategy for moving solutions to a Schubert problem from one instance to another.
   Usage
      changeFlags(...,oneHomotopy=>T)
   Inputs
      T:Boolean
        when true [default] it uses one line homotopies, 
	otherwise it makes a gradual change in the flags.
   Description
    Text
      There are two strategies: when oneHomotopy is set to true (default) it uses straight
      line homotopies to change flags, but assumes the initial flags are generic.
      
      When oneHomotopy is set to false, it makes gradual changes in the flags by changing
      one column at a time, and using only linear homotopies, but in this setting, it generates
      polynomial equations using all minors of the incidence conditions (thus is not very effective).
///

doc ///
   Key
      (changeFlags,Matrix,List,Sequence)
   Headline
      Recursive call of change flags
   Usage
      changeFlags(MX,SolsF,conds'F'G)
   Inputs
      MX:Matrix
         matrix of local coordinates
      SolsF:List
         solutions with respect to flags F
      conds'F'G:Sequence
         a triplet (C,F,G) where C is a list of m Schubert conditions,
	 F is a list of m flags defining an instance with solution set S,
	 G is a list of m flags defining an instance that we want to solve.
   Outputs
      :List
         solutions of the problem C with respect to flags G.
///
doc ///
  Key
    oneHomotopy
  Headline
    Strategy for changing flags.
  Description
    Text
      There are two strategies to change flags: when oneHomotopy is set to true (default) it uses straight
      line homotopies to change flags, but assumes the initial flags are generic.
      
      When oneHomotopy is set to false, it makes gradual changes in the flags by changing
      one column at a time, and using only linear homotopies, but in this setting, it generates
      polynomial equations using all minors of the incidence conditions (thus is not very effective).
///;
doc ///
   Key
      checkIncidenceSolution
      (checkIncidenceSolution,Matrix, List)
   Headline
      Check if a solution satisfies the incidence conditions of a Schubert problem
   Usage
      checkIncidenceSolution(s, P)
   Inputs
      P:List
         A Schubert problem as a list {($l_1,F_1$),...,($l_m,F_m$)}
      s:Matrix
         A matrix of size $k\times n$ representing a solution to the Schubert problem P
   Outputs
      :Boolean
         true if s satisfies all the incidence conditions
   Description
    Text
      For each pair $(l,F)$ in the Schubert problem $P$, where $l$ is a Schubert
      condition given as a partition, and $F$ is a flag given as an $n\times n$ matrix,
      the function verifies if the matrix $s$ satisfies the incidence conditions imposed
      by $l$ with respect to the flag $F$, for each of the pairs in $P$, by computing
      the corresponding minor conditions.
   Caveat
      This function was created for testing purposes and will be deleted later
      as it may not be numerically stable.
///

-*
doc ///
   Key
      [changeFlags, "one homotopy"]
   Headline
      Uses either one homotopy or gradual random changes of flags (one column at a time)
   Usage
      changeFlags(...,"one homotopy"=>T)
   Inputs
      T:Boolean
         true for one homotopy, false for changing one column at a time
   Description
    Text
      There are two strategies to change solutions with respect to one
      set of flags to another. When this option is set to true (default),
      uses a straight line homotopy between the two sets of flags;
      otherwise, it makes a gradual change of flags by changing one
      column at a time and uses only linear homotopies.
   Caveat
      When true, assumes that the initial flags are generic. 
      When false, it generates polynomial equations using all minors
      of the incidence conditions (and not the efficient way implemented later).
///
*-
-------------------
-- Documentation Pieri Homotopies
-*
doc ///
   Key
      createRandomFlagsForSimpleSchubert
      (createRandomFlagsForSimpleSchubert,Sequence,List,List)
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
///
doc ///
   Key
      solveSimpleSchubert
      (solveSimpleSchubert,Sequence,List,List,List)
   Headline
      Uses Pieri homotopies to solve simple Schubert problems
   Usage
      s=solveSimpleSchubert(kn,l,m,G)
   Inputs
      kn:Sequence
         two integers denoting the Grassmannian Gr(k,n)
      l:List
      m:List
         partitions of n
      G:List
         of fixed Flags G_1,...,G_d
   Outputs
      s:List
         solutions of the simple Schubert Problem defined by l and m with 
	 respect to the flags G_1,...,G_d
   Description
      Text
         Given partitions $l$ and $m$ in the Grassmannian $Gr(k,n)$, and a 
	 set of fixed flags $G_1,...,G_d$, where $d=k*(k-n) - |l| - |m|$. 
	 The function solves the system taking the first $d-1$ flags, and 
	 replacing the last one for a simpler one $G_m$. Then it uses homotopy 
	 continuation to track the solutions of this simpler system to solutions 
	 of the original system.         
    	 This function is used to solve Simple Schubert Problems, as described 
	 in the paper:          
    	 Leykin and Sottile, "Galois groups of Schubert problems via homotopy 
	 continuation", Mathematics of Computation, 78 (2009) 1749--1765.
      Example
         ---- Simple Schubert Problem
       	 k = 3
	 n = 7
       	 l = {2,1,0}
       	 m = {1,1,0}
       	 ----  Generate random flags G----
       	 d = k*(n-k)-sum(l)-sum(m);
       	 G = apply(d, i->matrix apply(n-k,i->apply(n,j->random CC)));
       	 ---------------------------------
       	 Sols =solveSimpleSchubert((k,n),l,m,G) --This problem has 77 solutions! --
	 -- to see the solutions in human form:
	 E := skewSchubertVariety((k,n),l,m);
	 apply(Sols ,s-> sub(E,matrix{s}) )
   SeeAlso
         createRandomFlagsForSimpleSchubert 
///


doc ///
   Key
      findGaloisElement
      (findGaloisElement,Sequence,List,List)
   Headline
      computes a permutation from a loop of an instance of a simple Schubert problem.
   Usage
      findGaloisElement(pblm, flag, solns)
   Inputs
      pblm:Sequence
         a sequence (l,m,k,n) that contains two partitions l,m and two integers k,n 
	 that define the simple Schubert problem l,m in the Grassmannian Gr(k,n)
      flag:List
         a list of numerical matrices that define an instance of the simple 
	 Schubert Problem
      solns:List
         solutions of the specific instance
   Outputs
      :List
         a permutation that lie in the Galois group
   Description
      Text
         Given a simple Schubert problem $(l,m)$ in $Gr(k,n)$. Fix a 
	 set of flags $F_1,...,F_d$ and let $S$ be the set of solutions of
	 the instance of the Schubert problem given by the flags $\{F_i\}$.
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
	 G = createRandomFlagsForSimpleSchubert((k,n),l,m);
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
///

doc ///
   Key
      isFullSymmetric
      (isFullSymmetric, List)
   Headline
      Check if a list of permutations generate the full symmetric group.
   Usage
      isFullSymmetric P
   Inputs
      P:List
         of permutations (each given as a list of n integers)
   Outputs
      :Boolean
   Description
      Text
	 Calls GAP to check if a set of permutations of {1,...,n}
	 (in one line notation) generate the symmetric group $S_n$.
   Caveat
      It assumes that GAP runs when you type {\tt gap} in a terminal      
///

doc ///
   Key
      isGaloisFullSymmetric
      (isGaloisFullSymmetric,Sequence,List,List,ZZ)
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
         G = createRandomFlagsForSimpleSchubert((k,n),l,m);	 
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
///
*-
end

-- previous functions
doc ///
   Key
      skewSchubertVariety
      (skewSchubertVariety,Sequence,List,List)
   Headline
      skew Schubert variety (or Richardson variety) from partitions $l$ and $m$
   Usage
      skewSchubertVariety(kn,l,m)
   Inputs
      kn:Sequence
         two integers denoting the Grassmannian Gr(k,n)
      l:List
      m:List
         partitions of n-k
   Outputs
      :Matrix
         representing local coordinates of the skew Schubert variety.
   Description
      Text
         Creates the matrix $E_{l,m}$ that parametrizes the skew 
	 Schubert variety $Y_{l,m} = Y_l \cap Y_m$, with respect to
	 the standard and the opposite flag respectively.
      Example
         -- for l = 2,1 and m = 1,1
       	 -- in Gr(3,7)
      	 skewSchubertVariety( (3,7),{2,1},{1,1} )
      
   SeeAlso
         solveSimpleSchubert
///

doc ///
    Key
       trackSimpleSchubert
       (trackSimpleSchubert,Sequence,Sequence,List,List)
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
          Given partitions $l$ and $m$ in the Grassmannian $Gr(k,n)$, and two 
	  sets of fixed flags $G_1,...,G_d$, and $F_1,...,F_d$; where 
	  $d=k*(k-n) - |l| - |m|$. The function tracks the solutions of the 
	  system defined by $G_1,...,G_d$ (if the solutions are not given, 
	      it computes them using {\tt solveSimpleSchubert}) to find 
	  solutions for the system defined by $F_1,...,F_d$. 
       Example
          ---- Simple Schubert Problem
   	  (k,n) = (3,7)
   	  l = {2,1,0}
   	  m = {1,1,0}
   	  ----  Generate random flags G and F----
   	  d = k*(n-k)-sum(l)-sum(m);
   	  G = apply(d, i->matrix apply(n-k,i->apply(n,j->random CC)));
   	  F = apply(d, i->matrix apply(n-k,i->apply(n,j->random CC)));
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
   	  G = apply(d, i->matrix apply(n-k,i->apply(n,j->random CC)));
   	  F = apply(d, i->matrix apply(n-k,i->apply(n,j->random CC)));
   	  ---------------------------------
   	  Solns = solveSimpleSchubert((k,n),l,m,G);
          trackSimpleSchubert((k,n),(l,m),G,F, StartSolutions=>Solns)
   SeeAlso
      solveSimpleSchubert
      createRandomFlagsForSimpleSchubert
///
