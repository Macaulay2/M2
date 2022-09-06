document {
     Key => NumericalSchubertCalculus,
     Headline => "Numerical Algorithms for Schubert Calculus",
     "This package provides implementations of both the Littlewood-Richardson and Pieri homotopy algorithms for solving (instances of) Schubert problems on Grassmannians using numerical homotopy continuation.",
     PARA{"A ", EM "Schubert problem", " on the Grassmannian ",TEX///$Gr(k,n)$///,
	 " is a list of Schubert subvarieties (assumed general) of ",TEX///$Gr(k,n)$///," 
	 whose codimensions add up to ",TEX///$k(n-k)$///,", the dimension of the Grassmannian. 
     	 A Schubert variety is represented by a pair ",TEX///$(c,F)$///,", where ",
	 TEX///$c$///," is a ", EM "Schubert condition", " (represented as a partition or a bracket) and ",
	 TEX///$F$///," is a flag, represented as an invertible ",TEX///$n\times n$///,"-matrix.
       	 The Schubert variety for the pair ",TEX///$(c,F)$///," consists of all ",
	 TEX///$k$///,"-planes that satisfy the incidence condition ",TEX///$c$///," imposed by the flag ",TEX///$F$///,"." 
       	 },
     PARA{"For a Schubert problem, we may fix one flag for each Schubert condition in the list 
	 --- i.e., describe an ", EM "instance", " of a given Schubert problem --- and look for the ", EM "solutions", 
	 " for this instance --- i.e., points in the intersection of the corresponding Schubert varieties. 
	 The methods of this package find approximations of these points."},   
     HEADER3 {"General functions:"},
     UL{
        TO randomSchubertProblemInstance, 
	--(an instance of a Schubert problem with random flags),   
	 TO changeFlags,
	 --(continues solutions from one instance to another),
	 TO checkIncidenceSolution, 
	 --(verifies that a solution satisfies the given incidence conditions).
	 TO LRnumber,
	 --(computes the number of solutions to a given Schubert problem).
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
	 TO partition2bracket,
	 --(converts a partition into a bracket)
	 TO NSC2phc
	 --(converts between list of brackets/partitions and phc notation for a Schubert problem)
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
     "For example, the Schubert problem {2,1}$^3$ in $Gr(3,6)$ with respect to random flags has 2 solutions",
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
	 HREF{"https://arxiv.org/abs/math/0302294", "Vakil, \"A geometric Littlewood-Richardson rule\",Vakil, Ravi. A geometric Littlewood-Richardson rule. Ann. of Math. (2) 164 (2006), 371-421"},
	 HREF{"https://arxiv.org/abs/alg-geom/9706004", "Huber, Sottile, Sturmfels, \"Numerical Schubert calculus\",  J. Symb. Comp., 26 (1998), 767-788."},
	 HREF{"https://arxiv.org/abs/1001.4125", "Sottile, Vakil, Verschelde, \"Solving Schubert Problems with Littlewood-Richardson Homotopies\", ISSAC 2010, 179-186, ACM, New York, 2010."},
	 HREF{"https://arxiv.org/abs/0710.4607", "Leykin and Sottile, \"Galois groups of Schubert problems via homotopy computation\",  Math. Comp. 78 (2009), 1749-1765."},
	 HREF{"https://arxiv.org/abs/1802.00984", "Leykin, Martin del Campo, Sottile, Vakil, Verschelde \"Numerical Schubert Calculus via the Littlewood-Richardson homotopy algorithm\", Math. Comp., 90 (2021), 1407-1433. "}
	 },
 }

-- Documentation Littlewood-Richardson homotopies
--------------
doc ///
   Key
      randomSchubertProblemInstance
      (randomSchubertProblemInstance,List,ZZ,ZZ)
   Headline
      returns a random instance of a given Schubert problem
   Usage
      randomSchubertProblemInstance(conditions,k,n)
   Inputs
      conditions:List
        which is a list of Schubert conditions that are either all partitions or all brackets (see @TO bracket2partition@ for details)
      k:ZZ
      n:ZZ
         $k$ and $n$ define the Grassmannian $Gr(k,n)$ of $k$-planes in $n$-space
   Outputs
      :List
         random instance of the Schubert problem, which is a list of pairs of the form (condition,flag)
   Description
      Text
         This first verifies that the conditions are either all partitions or all brackets, and that they form a Schubert problem on $Gr(k,n)$.
	 
	 Then it creates a list of random square invertible matrices that represent flags for the Schubert problem.
      Text
         For instance, consider the problem of four lines, which is given by 4 partitions {1}$^4$ in $Gr(2,4)$
      Example
	 randomSchubertProblemInstance({{1},{1},{1},{1}},2,4)
      Text
	 the same problem but using brackets instead of partitions
      Example
	 randomSchubertProblemInstance({{2,4},{2,4},{2,4},{2,4}},2,4)
   Caveat
      The output consists of random numerical matrices that are assumed invertible. The code does not check invertibility.
   SeeAlso
         solveSchubertProblem
	 partition2bracket
	 bracket2partition
///

doc ///
   Key
      [randomSchubertProblemInstance, Strategy]
   Headline
      strategy for creating a random matrix representing a flag
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
      uses Littlewood-Richardson homotopy algorithm to solve a Schubert problem
   Usage
      S = solveSchubertProblem(P,k,n)
   Inputs
      P:List
         Schubert problem given as a list of sequences of the 
	 form ($l,F$) where $l$ is a partition (a list of weakly 
	 decreasing integers) and $F$ is a flag ($n \times n$ matrix) 
      k:ZZ
      n:ZZ
      	 $k$ and $n$ define the Grassmannian $Gr(k,n)$ of $k$-planes in $n$-space
      --LinearAlgebra:Boolean
      --   when True, uses Linear Algebra to glue solutions from node to node, otherwise uses parameter homotopies.
   Outputs
      S:List
         solutions of the Schubert Problem given as $n \times k$ matrices
   Description
      Text
      	 Represent a Schubert variety in the Grassmannian $Gr(k,n)$ 
	 by a condition $c$ either a partition or a bracket (see  @TO partition2bracket@ for details) and a flag $F$ 
	 (given as an $n \times n$ matrix).
	 The codimension of the Schubert variety is $|c|$.
	 A Schubert problem is a list of Schubert varieties, whose codimension
	 add up to $k(n-k)$, which is the dimension of the Grassmannian.
         -----
	 
	 The function solveSchubertProblem solves the given instance of the Schubert problem by the Littlewood-Richardson
	 homotopy. This algorithm uses homotopy continuation to track 
	 solutions of a simpler problem to a general problem according 
	 to the specializations of the geometric Littlewood-Richardson.

	 This algorithm is described in the paper:
 	 Leykin, Martin del Campo, Sottile, Vakil, Verschelde "Numerical Schubert Calculus via the Littlewood-Richardson homotopy algorithm". 
         Math. Comp., 90 (2021), 1407-1433.  https://arxiv.org/abs/1802.00984.

      Text
        For instance, consider the Schubert problem {2,1}$^3$ in $Gr(3,6)$, which has two solutions
      Example
	 k = 3;
	 n = 6;
	 SchPblm = {
    	     ({2,1}, random(CC^6,CC^6)),
    	     ({2,1}, random(CC^6,CC^6)),
    	     ({2,1}, random(CC^6,CC^6))
    	     };
      Text
        Its solutions to an instance given by random flags
      Example
	 solveSchubertProblem(SchPblm, k,n)
   Caveat
      The Schubert conditions must be either all partitions or all brackets.
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
      uses Pieri homotopy algorithm to solve a simple Schubert problem
   Usage
      S = solveSimpleSchubert(P,k,n)
   Inputs
      P:List
         Simple Schubert problem given as a list of sequences of the 
	 form $(l,F)$ where $l$ is a partition (a list of weakly 
	 decreasing integers) and $F$ is a flag ($n \times n$ matrix).
         Necessarily, all partitions except possibly the first two are {1}
      k:ZZ
      n:ZZ
         $k$ and $n$ define the Grassmannian $Gr(k,n)$ of $k$-planes in $n$-space
      --LinearAlgebra:Boolean
      --   when True, uses Linear Algebra to glue solutions from node to node, otherwise uses parameter homotopies.
   Outputs
      S:List
         solutions of the simple Schubert Problem given as $n\times k$ matrices
   Description
      Text
      	 a Schubert variety in the Grassmannian $Gr(k,n)$ is represented by a partition $l$ (a weakly decreasing
	 list of nonegative integers less than $n-k$) and a flag $F$ (given as an $n\times n$ matrix).
	 A Schubert problem is a list of Schubert varieties 
	 $(l_1, F_1), \ldots, (l_m, F_m)$ such that 
	 $|l_1|+|l_2| + \cdots + |l_m| = k(n-k)$, where $|l_i|$ is the 
	 sum of the entries of $l_i$.
       
	 
	 The function solves a Schubert problem by the Pieri homotopy algorithm. 
	 It assumes all partitions except possibly the first two are simple (e.g. equal to {1}).
	 This algorithm uses homotopy continuation to track 
	 solutions of a simpler problem to a general problem according 
	 to the specializations of the geometric Pieri rule.

	 This algorithm is described in the paper:
	 Huber, Sottile, and Sturmfels, "Numerical Schubert Calculus",  J. Symb. Comp., 26 (1998), 767-788.
	 
      Text
         for instance, the Schubert problem {2,1}$^2$  {1}$^3$ in $Gr(3,6)$ has six solutions.
	 Consider the following instance given by random flags
      Example
 	 k = 3;
	 n = 6;
	 SchPblm = {
    	     ({2,1}, random(CC^6,CC^6)),
    	     ({2,1}, random(CC^6,CC^6)),
    	     ({1}, random(CC^6,CC^6)),
    	     ({1}, random(CC^6,CC^6)),
    	     ({1}, random(CC^6,CC^6))
    	     };
      Text
         Its solutions to this instance 
      Example
	 solveSimpleSchubert(SchPblm, k,n)
   Caveat
      Need to input partitions together with flags. 
--      In the future, 
--      there will be an option for generating random flags and just 
--      input the first two partitions.   Also, it will be able to take brackets.
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
         $k$ and $n$ represent the Grassmannian $Gr(k,n)$
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
         of length $k$, a bracket representing a Schubert condition.
      n:ZZ
         $k$ and $n$ represent the Grassmannian $Gr(k,n)$
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


doc ///
   Key
      NSC2phc
      (NSC2phc,List,ZZ,ZZ)
   Headline
      dictionary between different notations for Schubert problems.
   Usage
      M = NSC2phc(conds,k,n)
   Inputs
      conds:List
         of Schubert conditions, either partitions or brackets, that constitutes a Schubert problem on the Grassmannian $Gr(k,n)$
      k:ZZ
      n:ZZ
         $k$ and $n$ represent the Grassmannian $Gr(k,n)$
   Outputs
       :Matrix
         the corresponding Schubert problem in notation for PHCPack implementation of Littlewood-Richardson rule and homotopies.
         Its rows encode Schubert conditions with multiplicities.
         Each row is a $k+1$-tuple, {m,b}, where $m$ is a nonegative integer and $b$ a bracket (see  @TO bracket2partition@ for details).
         The bracket $b$ represents a Schubert condition and $m$ is its multiplicity in this Schubert intersection problem.

   Description
    Text
       A Schubert problem in the Grassmannian $Gr(k,n)$ is encoded by either a list 
       of partitions or brackets whose codimensions sum to $k(n-k)$. (see @TO bracket2partition@ for details on brackets and partitions)
       
       The PHCPack implementations of the geometric Littlewood-Richardson rule encode the brackets in a matrix,
       where each row has the form ${m, b}$ with $m$ the multiplicity of the bracket $b$, which is a strictly increasing
       sequence of $k$ integers between $1$ and $n$.

    Example
       k=4; 
       n = 8;
       SchubProbP = {{2,2},{2,2},{2,2},{1},{1},{1},{1}}
       NSC2phc(SchubProbP,k,n)
       
       k=4; 
       n = 8;
       SchubProbB = {{3,4,7,8},{3,4,7,8},{3,4,7,8},{4,6,7,8},{4,6,7,8},{4,6,7,8},{4,6,7,8}}
       NSC2phc(SchubProbB,4,8)

  SeeAlso
      LRrule
///


document{
    Key => {setVerboseLevel, (setVerboseLevel,ZZ)},
    Headline => "set different levels of information printed on screen",
    Usage => "setVerboseLevel n",
    Inputs =>{"n" =>{TO "ZZ", " takes values 0,1,2, or greater"}},
    Consequences=>{"Prints information on the screen depending on the value of n"},
    PARA{"The function displays different levels of information on the screen:"},
    UL{"0 = no extra information displayed [default]",
       "1 = print the progress information and time the main process",
       "2 = besides the information of level 1, it also displays the checkerboard steps"},
   PARA{"Consider the problem of four lines with respect to random flags"},
    EXAMPLE{
       "SchPblm = randomSchubertProblemInstance ({{1},{1},{1},{1}},2,4)",
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
      continue solutions of a Schubert problem to another instance
   Usage
      changeFlags(sols,CFG)
   Inputs
      sols:List
         solutions of a Schubert problem written as $n\times k$ matrices
      CFG:Sequence
         a triple {\tt (C,F,G)} where {\tt C} is a list of {\tt m} Schubert conditions,
	 {\tt F} is a list of {\tt m} flags defining an instance with solution set {\tt sols},
	 {\tt G} is a list of {\tt m} flags defining the instance whose solutions we want.
   Outputs
      :List
         solutions of the problem with respect to flags G.
   Description
    Text
       If @TT "sols"@ is a set of solutions to a Schubert problem $l_1,\ldots,l_m$
       with respect to a set of flags $F_1,\ldots, F_m$, uses parameter homotopies
       to move $S$ to a solution set $S'$ for the same Schubert problem, but with
       respect to another set of flags $G_1,\ldots, G_m$.
    Text 
       For instance, consider the Schubert problem $(2,1)^3$ in $Gr(3,6)$.
    Example
       k=3; n=6;
       l1={2,1}; 
       l2={2,1};
       l3={2,1};
    Text
       Generate flags $F$: standard flag, opposite flag, and one random.
    Example
       F = {(l1, id_(CC^n)), (l2, rsort id_(CC^n)), (l3,random(CC^n,CC^n))}
    Text 
       Generate a random set of flags $G$.
    Example
       G = randomSchubertProblemInstance({l1,l2,l3},k,n);
    Text
       We solve with respect to $F$.
    Example
       S = solveSchubertProblem(F,k,n);
       FlagsF = F/last;
       FlagsG = G/last;
    Text
       and we transform the solutions to get solutions with respect to $G$
    Example
       time S' = changeFlags(S,({l1,l2,l3},FlagsF,FlagsG))
       assert all(S', s-> checkIncidenceSolution(s,G))
    Text
       We can also choose a different strategy
    Example
       time S' = changeFlags(S,({l1,l2,l3},FlagsF,FlagsG), OneHomotopy=>false)
       assert all(S', s-> checkIncidenceSolution(s,G))
   Caveat
      There are two strategies: when @TO OneHomotopy@ is set to @TO true@ (default) it uses straight
      line homotopies to change flags, but assumes the initial flags are generic.
      
      When @TO OneHomotopy@ is set to @TO false@, it makes gradual changes in the flags by changing
      one column at a time, and using only linear homotopies, but in this setting, it generates
      polynomial equations using all minors of the incidence conditions (thus is not very effective).
   SeeAlso
      randomSchubertProblemInstance
      solveSchubertProblem
      checkIncidenceSolution
///

doc ///
   Key
      [changeFlags, OneHomotopy]
   Headline
      strategy for moving solutions to a Schubert problem 
   Usage
      changeFlags(...,OneHomotopy=>T)
   Inputs
      T:Boolean
        when true [default] it uses a single straight line homotopy, 
	otherwise it makes a gradual change in the flags, one column at a time.
   Description
    Text
      There are two strategies: when OneHomotopy is set to true (default) it uses straight
      line homotopies to change flags, but assumes the initial flags are generic.
      
      When OneHomotopy is set to false, it makes gradual changes in the flags by changing
      one column at a time, and using only linear homotopies, but in this setting, it generates
      polynomial equations using all minors of the incidence conditions (thus is not very effective).
///

doc ///
   Key
      (changeFlags,Matrix,List,Sequence)
   Headline
      recursive call of change flags
   Usage
      changeFlags(MX,SolsF,CFG)
   Inputs
      MX:Matrix
         matrix of local coordinates
      SolsF:List
         solutions with respect to flags F
      CFG:Sequence
         a triple {\tt (C,F,G)} where {\tt C} is a list of {\tt m} Schubert conditions,
	 {\tt F} is a list of {\tt m} flags defining an instance with solution set {\tt sols},
	 {\tt G} is a list of {\tt m} flags defining the instance whose solutions we want.
   Outputs
      :List
         solutions of the problem C with respect to flags G.
///
doc ///
  Key
    OneHomotopy
  Headline
    strategy for changing flags.
  Description
    Text
      There are two strategies to change flags: when OneHomotopy is set to true (default) it uses straight
      line homotopies to change flags, but assumes the initial flags are generic.
      
      When OneHomotopy is set to false, it makes gradual changes in the flags by changing
      one column at a time, and using only linear homotopies, but in this setting, it generates
      polynomial equations using all minors of the incidence conditions (thus is not very effective).
///;
doc ///
   Key
      checkIncidenceSolution
      (checkIncidenceSolution,Matrix, List)
   Headline
      check if a solution satisfies an instance of a Schubert problem
   Usage
      checkIncidenceSolution(s, P)
   Inputs
      P:List
         An instance of a Schubert problem as a list {($l_1,F_1$),...,($l_m,F_m$)}
      s:Matrix
         A matrix of size $n\times k$ representing a solution to the Schubert problem P
   Outputs
      :Boolean
         true if s satisfies all the incidence conditions
   Description
    Text
      For each pair $(l,F)$ in the Schubert problem $P$, where $l$ is a Schubert
      condition given as a partition, and $F$ is a flag given as an $n \times n$ matrix,
      the function verifies if the matrix $s$ satisfies the incidence conditions imposed
      by $l$ with respect to the flag $F$, for each of the pairs in $P$, by computing
      the corresponding minor conditions.
   Caveat
      This function was created for testing purposes and will be deleted later
      as it may not be numerically stable.
///;
doc ///
   Key 
      LRnumber
      (LRnumber,List,ZZ,ZZ)
   Headline
      returns the number of solutions to the given Schubert problem
   Usage
      LRnumber(conditions,k,n)
   Inputs
      conditions:List
        of Schubert conditions, either partitions or brackets, that constitutes a Schubert problem on the Grassmannian $Gr(k,n)$.
      k:ZZ
      n:ZZ
         $k$ and $n$ define the Grassmannian $Gr(k,n)$ of $k$-planes in $n$-space
   Outputs
      :ZZ
         The number of solutions to the given Schubert problem
   Description
      Text
         This first verifies that the conditions are either all partitions or all brackets, and that they form a Schubert problem on $Gr(k,n)$.
	 
	 Then it computes the intersection number of the product of Schubert classes in the cohomology ring of the Grassmannian
      Text
         For instance, the problem of four lines is given by 4 partitions {1}$^4$ in $Gr(2,4)$
      Example
	 LRnumber({{1},{1},{1},{1}},2,4)
      Text
	the same problem but using brackets instead of partitions
      Example
	 LRnumber({{2,4},{2,4},{2,4},{2,4}},2,4)
      Text
	 the same problem but using phc implementation of Littlewood-Richardson rule
      Example
	 LRnumber({{1},{1},{1},{1}},2,4,Strategy => "phc")
   Caveat
      This uses the package Schubert2 and the Strategy "phc" requires the string parsing capabilities of Macaulay2 version 1.17 or later
   SeeAlso
         LRrule
///

doc ///
   Key
      [LRnumber, Strategy]
   Headline
      strategy for computing the number of solutions to a Schubert problem
   Usage
      LRnumber(...,Strategy=>S)
   Inputs
      S:String
        "Schubert2", the default value, or "phc".
   Description
    Text
     Determines which method to compute the number of solutions is created.
     
        "Schubert2" uses the SchubertRings command from the package @TO Schubert2@ [default] 

	"phc" uses @TO LRrule@, the implementation of the geometric Littlewood-Richardson rule in PHCPack.
              This also requires the string parsing capabilities of Macaulay2 version 1.17 or later
   SeeAlso
     
///

doc ///
    Key
       printStatistics
       resetStatistics
    Headline
       summary statistics (used by developers)
    Usage
       printStatistics()
    Description
       Example
	 k = 3;
	 n = 6;
	 SchPblm = {
    	     ({2,1}, random(CC^6,CC^6)),
    	     ({2,1}, random(CC^6,CC^6)),
    	     ({2,1}, random(CC^6,CC^6))
	     };
    	 resetStatistics()
	 solveSchubertProblem(SchPblm, k,n)	 
    	 printStatistics()    
///
