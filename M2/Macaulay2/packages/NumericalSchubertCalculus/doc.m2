-- Documentation Littlewood-Richardson homotopies
--------------
doc ///
   Key
      solveSchubertProblem
      (solveSchubertProblem,List,ZZ,ZZ)
      [solveSchubertProblem, LinearAlgebra] --verify this optional input with Anton
   Headline
      uses homotopies of geometric Littlewood-Richardson rule to solve Schubert problems on Grassmannians
   Usage
      s = solveSchubertProblem(P,n,m)
   Inputs
      P:List
         Schubert problem given as a list of sequences of the 
	 form (l,F) where l is a partition (a list of weakly 
	 decreasing integers) and F is a flag (n by n matrix) 
      k:ZZ
      n:ZZ
      	 k and n denote the Grassmannian G(k,n)
      LinearAlgebra:Boolean
         when True, uses Linear Algebra to glue solutions from node to node, otherwise uses parameter homotopies.
   Outputs
      s:List
         solutions of the Schubert Problem given as k by n matrices
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
	 
	 The function solves the Schubert problem by Littlewood-Richardson
	 homotopies. This algorithm uses homotopy continuation to track 
	 solutions of a simpler problem to a general problem according 
	 to the specializations of the geometric Littlewood-Richardson.

	 This algorithm is described in the paper:
	 Sottile,Vakil, and Verschelde, "Littlewood-Richardson homotopies"
	 
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
      Need to input partitions together with flags. In the future, 
      there will be an option for generating random flags and just 
      input the partitions.
   SeeAlso
         solveSimpleSchubert
///;

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
         k and n represent the Grassmannian G(k,n)
   Outputs
      b:List
         representing the bracket notation
   Description
    Text
       A Schubert condition in the Grassmannian $Gr(k,n)$ can be denoted 
       by a partition $l$ or by a bracket $b$. 
       
       A partition is a weakly decreasing list of nonnegative 
       integers less than or equal to $n-k$.
       
       A bracket is a strictly increasing list of length $k$ of 
       positive integers between $1$ and $n$.
       
       This function writes a partition as a bracket.
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
         k and n represent the Grassmannian G(k,n)
   Outputs
      l:List
         the partition notation
   Description
    Text
       A Schubert condition in the Grassmannian $Gr(k,n)$ can be denoted 
       by a partition $l$ or by a bracket $b$. 
       
       A partition is a weakly decreasing list of nonnegative 
       integers less than or equal to $n-k$.
       
       A bracket is a strictly increasing list of length $k$ of 
       positive integers between $1$ and $n$.
       
       This function writes a bracket as a partition.
    Example
       b = {1,3};
       n = 4;
       bracket2partition(b,n)
       
       n = 6;
       bracket2partition(b,n)
       
       b = {2,4,6};
       bracket2partition(b,n);
   SeeAlso
      partition2bracket
///

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

-------------------
-- Documentation Pieri Homotopies

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
