-- documentation of the package PHCpack

doc ///
  Key 
    PHCpack 
  Headline
    a package for Polynomial Homotopy Continuation
  Description
    Text
      This package provides an interface to the software {\tt PHCpack}, a
      general-purpose polynomial system solver that uses homotopy continuation.
      The main method is a numerical blackbox solver, 
      implemented for Laurent systems.  The package also provides a fast mixed 
      volume computation, the ability to filter solutions, extract real 
      solutions, or track solution paths defined by a polynomial homotopy.
      For positive dimensional solution sets, we can
      compute a numerical irreducible decomposition.
      
      The software {\tt PHCpack} itself is available at
      @HREF"http://www.math.uic.edu/~jan/download.html"@.
      This site provides source code and its executable version {\tt phc}.
      To use the methods from this package, the user must have the executable 
      program {\tt phc} available, preferably in the execution path. 
      The functions in this package call the {\tt phc} executable,
      behind the scenes, with appropriate input options, and save 
      intermediate output to temporary files.
      For convenience, the file names are displayed, and the commands that 
      are invoked are documented under "Consequences" for each function.
      
      Below is a simple example using the most popular function, 
      the numerical blackbox solver.
      
    Example     
      R = CC[x,y,z]
      system = {y-x^2,z-x^3,x+y+z-1}
      solns = solveSystem(system)
      numSolns = #solns
      solns/print

    Text
      We see that there are three solutions to the above system. 
      Each solution is of type @TO Point@ and contains diagnostic information 
      about the quality of the solution.    
    
    Example
      oneSoln = solns_0
      peek oneSoln
    Text 
         
      The value of {\tt LastT} is the end value of the continuation parameter:
      if it equals 1, then the solver reached the end of the path properly.
  Caveat
    {\bf 1.} If you are having trouble installing the package, 
    check whether the path to your PHCpack executable was set correctly. 
    You can check this by typing the following command:
	       
	       options PHCpack 
	       
    If it is wrong, you can update it by putting the absolute path 
    into the {\tt init-PHCpack.m2} file,
    For example, if the executable {\tt phc} is located in C:/cygwin/PHC,
    then the line inside the {\tt init-PHCpack.m2} file will look like this: 
	       
	        "path" => "C:/cygwin/PHC/" .
	       
    Alternately, the path could be set when loading the package 
    using the following command:
     	       
      loadPackage ("PHCpack", 
      Configuration=>{"path"=>"C:/cygwin/PHC/","PHCexe"=>"./phc"}) 

    {\bf 2.} If the package SimpleDoc is not found when 
    installing {\tt PHCpack.m2}, see questions and answers 6, 7, and 8 
    on the Macaulay2 web site.

    {\bf 3.} The current version 1.8 of PHCpack.m2 was developed with version 
    1.9 of Macaulay2 and with version 2.4.17 of phc.
///;

-------------------
-- versionNumber --
-------------------

doc ///
  Key
    versionNumber
    (versionNumber,Nothing)
  Headline
    returns the version number and release date of phc
  Usage
    versionNumber(null)
    versionNumber(,Verbose=>true)
  Inputs
    null:Nothing
  Outputs
    :Sequence
      The sequence on return contains two strings.
      The first string on return is the version number.
      The second string on return is the release date.
  Description
    Text
      The version number and release date of the executable phc
      are important for consistency between the methods in this
      package and preparing the input batch files for the executable phc.

      A successful run of this method verifies whether the location
      of the executable phc is in the execution path.

    Example
      v = versionNumber(null)
      print v_0
      print v_1
///;

doc ///
  Key
    [versionNumber,Verbose]
  Headline
    option to print the output of phc --version to screen
  Usage
    cascade(...,Verbose=>Boolean)
  Description
    Text
      Use {\tt Verbose=>true} to see the output of {\tt phc --version}.
///;

-------------
-- CASCADE --
-------------

doc ///
  Key 
    cascade
    (cascade,List)
  Headline
    runs a cascade of homotopies to get witness sets for the variety
  Usage
    cascade L
  Inputs
    L:List 
      of polynomials
  Outputs
    :NumericalVariety
      containing the witness sets for each pure-dimensional variety 
      contained in zero set of L. 
  Consequences
    Item
      Writes the system to temporary file
    Item
      Invokes the command {\tt phc -c} (with option 0) 
    Item
      Stores output of phc in temporary file
    Item
      Parses and outputs the solutions.  
  Description
    Text
      Given a list of generators of an ideal I, this function creates an 
      embedding and then runs a cascade of homotopies. The output is a 
      @TO NumericalVariety @ that contains
      a @TO WitnessSet@ for each pure dimensional variety contained in V(I).
      
    Example
      R = CC[x,y,z];
      L = { z*(x+y), z*(x-y) };
      WitSets = cascade(L)
      W=first WitSets#2
    
    Text
      The function {\tt cascade} extends the ring of the inputted system 
      with slack variables beginning with zz.  Each witness
      set in contains the equations, points, and slices of the embedded
      system.
      
    Example
      W#Equations
      W#Points
      W#Slice
  
  Caveat
    Coefficient ring of the polynomial system must be of type @TO ComplexField@.
  
  SeeAlso
    constructEmbedding
    numericalIrreducibleDecomposition   
///;

-- options for cascade and numericalIrreducibleDecomposition

doc ///
  Key
    StartDimension
  Headline
    optional input for cascade and numericalIrreducibleDecomposition
  Description
    Text
      Optional argument for {\tt cascade} 
      and {\tt numericalIrreducibleDecomposition}.
      These methods search for positive dimensional components starting 
      at the {\tt StartDimension} 
      and then considering all the subsequent lower dimensions. 
      
      The default value for {\tt StartDimension} is the number of variables 
      in the system minus one.
      
      If the user has a good idea about the top dimension, using a smaller
      {\tt StartDimension} than the default will reduce the computational time.
  Usage
    cascade(..., StartDimension => ZZ) or numericalIrreducibleDecomposition(..., StartDimension =>ZZ)   
    
///;

doc ///
  Key
    [cascade,StartDimension]
  Headline
    Option to specify the dimension to begin searching for positive dimensional components
  Usage
    cascade(...,StartDimension=>ZZ)
///;

doc ///
  Key
    [cascade,Verbose]
  Headline
    option to specify whether additional output is wanted 
  Usage
    cascade(...,Verbose=>Boolean)
  Description
    Text
      Use {\tt Verbose=>true} for additional output which includes the 
      input and solution file names used by {\tt phc -c}.

      The output file of {\tt phc} contains timings for the stages
      in the cascade of homotopies.
///;

doc ///
  Key
    [numericalIrreducibleDecomposition,StartDimension]
  Headline
    Option to specify the dimension to begin searching for positive dimensional components
  Usage
    numericalIrreducibleDecompositon(...,StartDimension=>ZZ)
///;

doc ///
  Key
    [numericalIrreducibleDecomposition,Verbose]
  Headline
    option to specify whether additional output is wanted 
  Usage
    numericalIrreducibleDecomposition(...,Verbose=>Boolean)
  Description
    Text
      The value of the Verbose option (by default set to false)
      is passed to the methods cascade and factorWitnessSet.

      The output file of {\tt phc} contains timings for all stages
      in the numerical irreducible decomposition.
///;

-------------------------
-- CONSTRUCT EMBEDDING --
-------------------------

doc ///
  Key
    constructEmbedding
    (constructEmbedding,List,ZZ)
  Headline
    constructs an embedding of a polynomial system
  Usage
    constructEmbedding(f,k)
  Inputs
    f:List
      of polynomials
    k:ZZ 
      the expected (top) dimension of the solution set of f
  Outputs
    :List
      entries are polynomials containing the original system with k 
      random linear polynomials and k slack variables
  Consequences
    Item
      Writes the system to temporary files
    Item
      Extends the ring with slack variables 
    Item
      Uses surplus variables when the initial system is overconstrained
    Item
      Invokes the command {\tt phc -c} (with option 1). 
    Item
      Stores output of phc in temporary file
    Item
      Parses and outputs the solutions.
  Description
    Text
      To compute generic points of a k-dimensional solution set of a
      polynomial system, we add k random linear equations to the system.
      
    Example
      R = CC[x,y,z];
      f = { x^2 - y, x^3 - z };
      fe1 = constructEmbedding(f,1);
      toString fe1

    Text
      Note that the ring of the original system is extended with
      k slack variables.  Solutions of the embedded system with
      zero values for the slack variables are candidate generic points.

      If the input system is overdetermined (there are more equations
      than unknowns), then as many surplus variables are introduced as
      the difference between the number of equations and the number of
      variables.  Surplus variables start with ss.
      
    Example
      R = CC[x,y,z];
      f = { x^2-y, x^3-z, x*y-z, x*z-y^2 };
      fe1 = constructEmbedding(f,1);
      toString fe1
    Text
    
      In the example above, the system f has four equations in three unknowns,
      constructEmbedding adds one surplus variable and one slack variable.
      Only solutions with zero values for the surplus variable are relevant.
///;

-- options for constructEmbedding

doc ///
  Key
    [constructEmbedding,Verbose]
  Headline
    option to specify whether additional output is wanted 
  Usage
    constructEmbedding(...,Verbose=>Boolean)
  Description
    Text
      Use {\tt Verbose=>true} for additional output which includes the 
      input and solution file names used by {\tt phc -c}.
///;

------------
-- FACTOR --
------------

doc ///
  Key
    factorWitnessSet
    (factorWitnessSet,WitnessSet)
  Headline
    applies monodromy to factor a witness set into irreducible components
  Usage
    factorWitnessSet(w)
  Inputs
    w:WitnessSet
      properly embedded with slack variables
  Outputs
    :List
      a list of witness sets, every element of the list is irreducible
  Consequences
    Item
      writes the system to temporary files,
    Item
      invokes the command {\tt phc -f} (with option 2),
    Item
      uses monodromy to factor,
    Item
      uses default settings of path trackers ,
    Item
      stores output of phc in temporary file,
    Item
      parses and outputs the solutions.
  Description
    Text
      A witness set is irreducible if there exists a path between any two of 
      its generic points that does not pass
      through a singularity.

      We illustrate the factorization via the twisted cubic and a line.
      
    Example
      R = CC[x,y,z]; f = {(x^2-y)*(x-1), x^3 - z};
      (w,ns) = topWitnessSet(f,1);
      w
      L = factorWitnessSet(w)
    
  SeeAlso
    numericalIrreducibleDecomposition  
///;

-- options for factorWitnessSet

doc ///
  Key
    [factorWitnessSet,Verbose]
  Headline
    option to specify whether additional output is wanted 
  Usage
    factorWitnessSet(...,Verbose=>Boolean)
  Description
    Text
      Use {\tt Verbose=>true} for additional output which includes the 
      input and solution file names used by {\tt phc -f}.
///;

----------------------
-- isCoordinateZero --
----------------------

doc ///
  Key 
    isCoordinateZero
    (isCoordinateZero,AbstractPoint,ZZ,RR)
  Headline
    checks if coordinate has absolute value less than a given tolerance
  Usage
    isCoordinateZero(sol,k,tol)
  Inputs
    sol:AbstractPoint
      solution to a polynomial system
    k:ZZ
      index of coordinate
    tol:RR
      tolerance on the absolute value of the k-th coordinate
  Outputs
    :Boolean
      true if the k-th coordinate of solution has absolute value less than tol,
      false otherwise
  Description
    Text
      A solution has a zero k-th coordinate when the abs function
      evaluates to a number less than the given tolerance.
    
    Example
      R = CC[x,y];  
      f = { x^3*y^5 + y^2 + x^2*y, x*y + x^2 - 1};
      fSols = solveSystem(f, randomSeed=>3);
      fSols/print
      isCoordinateZero(fSols_0,1,1.0e-10)

    Text
      Good values for the tolerance are relative to the accuracy
      and the condition number of the solution.
      To improve the accuracy of a solution, apply
      @TO refineSolutions@ with a higher working precision.
      
  SeeAlso
    refineSolutions
    zeroFilter
    nonZeroFilter
///;

---------------------------
-- IS WITNESS SET MEMBER --
---------------------------

doc ///
  Key
    isWitnessSetMember
    (isWitnessSetMember,WitnessSet,AbstractPoint)
  Headline
    tests whether a point belongs to a solution set
  Usage
    isWitnessSetMember(W,p)
  Inputs
    W:WitnessSet
      positive dimensional, properly embedded with slack variables
    p:AbstractPoint
  Outputs
    :Boolean
      true if p is a member of the solution set of W, 
      false otherwise
  Consequences
    Item
      Invokes the command {\tt phc -f} (with option 1) 
  Description
    Text
      Uses numerical homotopy methods to test whether the point p belongs
      to the variety associated to the witness set W. 
      
      Although W should be properly embedded, p should be given in the 
      coordinates of the original system.
      
    Example
      R = CC[x11,x22,x21,x12,x23,x13];
      system = {x11*x22-x21*x12,x12*x23-x22*x13};
      V = numericalIrreducibleDecomposition (system);
      W = first V#4;
      isWitnessSetMember(W, point{{0,0,0,0,0,0}})
      
///;

-- options for isWitnessSetMember

doc ///
  Key
    [isWitnessSetMember,Verbose]
  Headline
    option to specify whether additional output is wanted 
  Usage
    isWitnessSetMember(...,Verbose=>Boolean)
  Description
    Text
       Use {\tt Verbose=>true} for additional output, which includes the 
       input and solution file names used by {\tt phc}.  

    Example
      R = CC[x11,x22,x21,x12,x23,x13];
      system = {x11*x22-x21*x12,x12*x23-x22*x13};
      V = numericalIrreducibleDecomposition (system);
      W = first V#4;
      isWitnessSetMember(W, point{{0,0,0,0,0,0}})      
///;

-----------------
-- mixedVolume --
-----------------

doc ///
  Key
    mixedVolume
    (mixedVolume, List)
  Headline
    computes mixed volume of a polynomial system
  Usage
    mv = mixedVolume(S) 
    (mv,sv) = mixedVolume(S,StableMixedVolume => true)  
    (mv,q,qsols) = mixedVolume(S,StartSystem => true)
    (mv,sv,q,qsols) = mixedVolume(S,StableMixedVolume => true,StartSystem => true)
    (mv,q,qsols) = mixedVolume(S,StartSystem => true,numThreads=4)
  Inputs
    S:List
      whose entries are the polynomials of a square system
  Outputs
    mv:ZZ
      the mixed volume of the system S
    sv:ZZ 
      the stable mixed volume of the system S
    q:List
      whose entries are polynomials in a random coefficient system,
      used as a start system for the homotopy
    qSols:List 
      whose entries are solutions of the start system q
  Consequences
    Item
      Writes the system to temporary files
    Item
      Invokes the command {\tt phc -m} (with option 4)
    Item
      Stores output of phc in temporary file
    Item
      Parses and outputs the solutions.
  Description
    Text
      The mixed volume of a polynomial system $S:=\{f_1,\dots,f_n\}$ 
      is defined as follows:
      Let $P_1,\dots,P_n$ be the Newton polytopes 
      of $f_1,\dots,f_n$, i.e., $P_i$ is the convex hull of the exponents 
      of the monomials in the support of $f_i$. 
      The mixed volume of $S$ is 
      $$ \sum_{1\leq h\leq n} 
      \sum_{1\leq i_1\dots\leq i_h\leq n} (-1)^{n-h}V_n(P_{i_1}+\dots+P_{i_h}),
      $$
      where $V_n$ denotes the $n$-dimensional Euclidean volume.
      
      Bernstein's theorem (D. N. Bernstein,{\em  The number of roots of 
      a system of equations}, Functional. Anal. Appl 9 (1975), no. 3, 183-185), 
      a generalization of the classical Bezout's theorem, shows that for 
      a zero-dimensional system, 
      the mixed volume provides an upper bound on the number of complex 
      isolated roots. 
      If the coefficients of the system are sufficiently generic, 
      the mixed volume is a sharp bound.
       
    Example
      R = CC[x,y];
      f = { x^3*y^5 + y^2 + x^2*y, x*y + x^2 - 1};
      I=ideal f;
      dim I
      degree I
      m = mixedVolume(f) -- counts the number of complex roots in the torus (without zero components)
      (mv,sv) = mixedVolume(f,StableMixedVolume=>true) 
      (mv,q,qsols) = mixedVolume(f,StartSystem=>true);
      q --let's take a look at the start system:
      qsols --and its solutions:
    Text
      {\em Note that only those solutions with nonzero components 
           are shown, even if StableMixedVolume is true.  See the end of 
           the temporary output file for the solutions with zero components.}

      The method
      {\tt mixedVolume} calls an Ada translation of ACM TOMS Algorithm 846:
      {\em MixedVol: a software package for mixed-volume computation}
      by Tangan Gao, T. Y. Li, Mengnien Wu, 
      @HREF { "http://portal.acm.org/citation.cfm?doid=1114268.1114274" ,
            "ACM TOMS" }@
      31(4):555-560, 2005.
  SeeAlso
    StableMixedVolume
    StartSystem
///;

-- general options

doc ///
  Key
    randomSeed
  Headline
    seed for the random number generators
  Description
    Text
      To avoid singularities during complex path following,
      the homotopy methods use a random constant.
      Different runs with solveSystem, trackPaths,
      or mixedVolume (with StartSystem set to true) may
      therefore lead to the solutions listed in a different order.
      Fixing the value of randomSeed leads to reproducible runs.
///;

doc ///
  Key
    computingPrecision
  Headline
    flag to switch to double double or quad double precision
  Description
    Text
      By default, all computations occur in hardware double precision.
      While this precision could be large enough to obtain accurate
      results, for larger problems, one may need to increase the
      precision to double double or to quad double precision.
      
      Setting the value of computingPrecision to 2 changes the
      precision in the path trackers to double double.

      Setting the value of computingPrecision to 4 changes the
      precision in the path trackers to quad double.

      To compensate for the cost overhead of the higher precision,
      it is useful to run the multithreaded versions of the path
      trackers, see the option numThreads.
///;

doc ///
  Key
    interactive
  Headline
    flag to run phc -p or phc -m in interactive mode
  Description
    Text
      There are too many options for the path trackers in phc -p
      to wrap them properly within the trackPaths() method.
      With interactive turned on, the user can tune all parameters
      of the path trackers, in the same way as running phc -p.

      The option interactive is also supported to run the
      polyhedral homotopies to solve random coefficient systems
      with phc -c, in the mixedVolume function with the option
      StartSystem set to true.
///;

-- options for mixedVolume

doc ///
  Key
    StableMixedVolume
  Headline
    optional input for computation of the stable mixed volume
  Description
    Text
      Put {\tt StableMixedVolume=>true} as an argument in the 
      function @TO mixedVolume@ to count solutions in affine space.
///;

doc ///
  Key
    StartSystem
  Headline
    optional input to construct and solve a random coefficient system
  Description
    Text
      Put {\tt StartSystem=>true} as an argument in the 
      function @TO mixedVolume@ to tell the method to use polyhedral 
      homotopies.  Polyhedral homotopies solve a system with the same Newton 
      polytopes as the original system and with random complex coefficients.
      This random coefficient system can serve as a start system to solve the
      original input system.
///;

doc ///
  Key
    [mixedVolume, StartSystem]
  Headline
    optional input for computation of mixed volume by solving a random coefficient system
  Usage
    mixedVolume(...,StartSystem=>Boolean)
///;

doc ///
  Key
    [mixedVolume, StableMixedVolume]
  Headline
    optional input for computation of the stable mixed volume
  Usage
    mixedVolume(...,StableMixedVolume=>Boolean)
///;

doc ///
  Key
    [mixedVolume,Verbose]
  Headline
    option to specify whether additional output is wanted 
  Usage
    mixedVolume(...,Verbose=>Boolean)
  Description
    Text
      Use {\tt Verbose=>true} for additional output which includes the 
      input and solution file names used by {\tt phc}.  

      The output file of {\tt phc} contains timings for the mixed volume
      and provides details about the mixed-cell configuration.
///;

doc ///
  Key
    [mixedVolume,numThreads]
  Headline
    option to set the number of threads when solving a start system
  Usage
    solveSystem(...,StartSystems=>true,numThreads=>ZZ)
  Description
    Text
      Use {\tt numThreads=>4} to run the path trackers with 4 threads.
///;

doc ///
  Key
    [mixedVolume,interactive]
  Headline
    option to switch to the interactive mode of phc -m
  Usage
    solveSystem(...,interactive=>true)
///;

-------------------
-- nonZeroFilter --
-------------------

doc ///
  Key 
    nonZeroFilter
    (nonZeroFilter,List,ZZ,RR)
  Headline
    returns solutions with coordinate larger than given tolerance
  Usage
    nonZeroSols = nonZeroFilter(sols,k,tol)
  Inputs
    sols:List
      solutions of a polynomial system
    k:ZZ
      index to a coordinate of a solution
    tol:RR
      tolerance on the abs value of the k-th coordinate
  Outputs
    nonZeroSols:List
      solutions with k-th coordinate larger than the given tolerance
  Description
    Text
      A solution has its k-th coordinate non-zero when the abs function
      evaluates to a number greater than the given tolerance.
    
    Example
      R = CC[x,y];  
      f = { x^3*y^5 + y^2 + x^2*y, x*y + x^2 - 1};
      fSols = solveSystem(f, randomSeed=>3);
      fSols/print
      nonZeroSols = nonZeroFilter(fSols,1,1.0e-10);
      nonZeroSols / print 
    Text
      Here is another system where we filter solutions with `large enough' first coordinate:
    
    Example
      f = {x^2+y^2,y*x+x}; 
      fSols = solveSystem(f);
      fSols/print
      zeroSols = zeroFilter(fSols,0,1.0e-10);
      zeroSols/print
    Text
      Good values for the tolerance are relative to the accuracy
      and the condition number of the solution.
      To improve the accuracy of a solution, apply
      @TO refineSolutions@ with a higher working precision.
      
      The method returns the complement of the result of @TO zeroFilter@.
  SeeAlso
    refineSolutions
    zeroFilter
///;

---------------------------------------
-- numericalIrreducibleDecomposition --
---------------------------------------

doc ///
  Key 
    numericalIrreducibleDecomposition
    (numericalIrreducibleDecomposition,List)
  Headline
    finds the irreducible components of the zero set of a system of polynomials
  Usage
    numericalIrreducibleDecomposition (system)
  Inputs
    system:List
      a system of polynomials, with no more equations than indeterminates
  Outputs
    :NumericalVariety
      containing a witness set for each irreducible component
  Consequences
    Item
      This function calls @TO cascade@ and @TO factorWitnessSet@. 
  Description
    Text
      Given a list of generators of an ideal I, this function returns 
      a @TO NumericalVariety@ with a 
      @TO WitnessSet@ for each irreducible component of V(I). 
    
    Example
      R=CC[x11,x22,x21,x12,x23,x13,x14,x24];
      system={x11*x22-x21*x12,x12*x23-x22*x13,x13*x24-x23*x14};       
      V=numericalIrreducibleDecomposition(system)
      WitSets=V#5; --witness sets are accessed by dimension
      w=first WitSets;
      w.cache.IsIrreducible  
    
    Text
    
      In the above example we found three components of dimension five, we can check the
      solution using symbolic methods.  
    
    Example  
      R=QQ[x11,x22,x21,x12,x23,x13,x14,x24];
      system={x11*x22-x21*x12,x12*x23-x22*x13,x13*x24-x23*x14};
      PD=primaryDecomposition(ideal(system))
      for i from 0 to 2 do print ("dim =" | dim PD_i | "  " | "degree=" | degree PD_i)
    
  SeeAlso
    cascade
    factorWitnessSet
    solveSystem
///;

---------------------
-- refineSolutions --
---------------------

doc ///
  Key 
    refineSolutions
    (refineSolutions,List,List,ZZ)
  Headline
    refines solutions of a system by increasing working precision
  Usage
    newSols = refineSolutions(f,sols,dp)
  Inputs
    f:List
      a system of polynomials
    sols:List
      solutions of the system f, each of type @TO AbstractPoint@ 
      (from a previous calculation)
    dp:ZZ
      the number of decimal places in working precision
  Outputs
    newSols:List
      the solutions {\tt sols} of {\tt f} refined to increased
      precision of {\tt dp} decimal places
  Consequences
    Item
      writes a system to a temporary file,
    Item
      invokes the command {\tt phc -v} (with option 3),
    Item 
      stores phc output in a temporary file,
    Item 
      parses and prints the refined solutions.
  Description
    Text
      The user can specify the number of decimal places desired 
      to refine solutions.
       
      Let us compute a square root with a working precision of 64 decimal
      places:
    
    Example
      R = CC[x,y]; S = {x^2 - 1/3, x*y - 1}; ourRoots = solveSystem(S);
      r0 = ourRoots#0#Coordinates#1
      newRoots = refineSolutions(S,ourRoots,64)
      newRoots#0 -- recall that solutions are of type Point
      r1 = newRoots#0#Coordinates#1
///;

-- options for refineSolutions

doc ///
  Key
    [refineSolutions,Verbose]
  Headline
    option to specify whether additional output is wanted 
  Usage
    refineSolutions(...,Verbose=>Boolean)
  Description
    Text
      Use {\tt Verbose=>true} for additional output which includes the 
      input and solution file names used by {\tt phc -v}.
///;

-------------------------
-- solveRationalSystem --
-------------------------

doc ///
  Key 
    solveRationalSystem
    (solveRationalSystem,List)
  Headline
    approximates solutions to a rational system of equations
  Usage
    solveRationalSystem (f)
  Inputs
    f:List
      a system of rational equations with a finite number of solutions
  Outputs
    :List
      containing the solutions of f, each of type @TO Point@
  Consequences
    Item
      converts the rational system into a Laurent system, invokes the 
      commands {\tt phc -b} and {\tt phc -z},
    Item
      adds slack variables if needed (i.e. if system is overdetermined),
    Item
      writes the system to temporary file,
    Item
      launches the blackbox solver,
    Item
      stores the output of phc in a temporary file,
    Item
      parses and outputs the solutions.
  Description
    Text
      This function returns numerical approximations of all complex 
      solutions of a rational system.
      The function converts the system to a Laurent
      polynomial system and then calls {\tt PHCpack}'s blackbox solver.   
    
    Example
      R = QQ[x,y,z];
      system = {y-x^2, z-x^3, (x+y+z-1)/x};
      sols = solveRationalSystem(system)
      
    Text
      The solutions are of type @TO Point@. Each point {\tt p} comes with cached 
      diagnostics. For example, {\tt p.cache.LastT} is the end value of the 
      continuation parameter; if it equals 1, 
      then the solver reached the end of the path properly.  
    
    Example  
      peek first sols
      
  SeeAlso
    solveSystem
    toLaurentPolynomial
///;

-- options for solveRationalSystem

doc ///
  Key
    [solveRationalSystem,Verbose]
  Headline
    option to specify whether additional output is wanted 
  Usage
    solveRationalSystem(...,Verbose=>Boolean)
  Description
    Text
      Use {\tt Verbose=>true} for additional output which includes the 
      input and solution file names used by {\tt phc}.
///;


-----------------
-- solveSystem --
-----------------

doc ///
  Key
    solveSystem
    (solveSystem, List)
  Headline
    a numerical blackbox solver
  Usage
    solveSystem(S)
    solveSystem(S,numThreads=>4)
    solveSystem(S,computingPrecision=>2)
    solveSystem(S,randomSeed=>12345)
  Inputs
    S:List
      contains a zero-dimensional system of polynomials with complex
      coefficients
      that contains at least as many equations as indeterminates 
  Outputs
    :List 
      containing the solutions of S, each of type @TO Point@. 
  Consequences
    Item
      Writes the system to temporary file
    Item
      Adds slack variables if needed (i.e. if system is overdetermined)
    Item
      Invokes the command {\tt phc -b} (launches the blackbox solver)
    Item
      Stores output of phc in temporary file
    Item
      Parses and outputs the solutions.
  Description
    Text
      Suppose we want numerical approximations of all complex isolated 
      solutions to the following system:
      
    Example
      R = CC[x,y,z]
      S = {x+y+z-1, x^2+y^2, x+y-z-3}
    Text
      We call {\tt PHCpack}'s blackbox solver:
    
    Example
      L = solveSystem(S)
    Text
      The method {\tt solveSystem} prints the {\tt PHCpack} input and output file names 
      and returns two solutions. The solutions are of type @TO Point@, defined in @TO NAGtypes@. 
      Each point {\tt p} comes with cached diagnostics.
      For example, {\tt p.cache.LastT} is the end value of the continuation parameter; 
      if it equals 1, then the solver reached the end of the path properly.
    
    Example
      oneSoln = L_0
      peek oneSoln
      
    Text
      The method handles overdetermined systems by inserting slack variables.
      
    Example
      system = {y-x^2, z-x^3, x+y+z-1, x+y+ x^3 -1}
      #system > numcols vars R --overdetermined system
      solns = solveSystem(system);
      numSolns = #solns  

  Caveat
      The method {\tt solveSystem} does not check 
      the dimension of the system; it checks for number of equations instead.
      So {\tt solveSystem} will return an error if there are less equations than unknowns 
      even if the system is zero-dimensional. In addition, if the system is square
      but not zero-dimensional, the output is meaningless.  Thus, it is suggested 
      that the user checks the dimension of the system before using
      {\tt solveSystem}.
      
  SeeAlso
    solveRationalSystem
    
///;

-- options for solveSystem

doc ///
  Key
    [solveSystem,Verbose]
  Headline
    option to specify whether additional output is wanted 
  Usage
    solveSystem(...,Verbose=>Boolean)
  Description
    Text
      Use {\tt Verbose=>true} for additional output which includes the 
      input and solution file names used by {\tt phc}.  

      The output file of {\tt phc} contains timings for the stages
      in the solver.
///;

doc ///
  Key
    [solveSystem,computingPrecision]
  Headline
    option to specify the working precision
  Usage
    solveSystem(...,computingPrecision=>ZZ)
  Description
    Text
      Use {\tt computingPrecision=>2} for double double precision.

      Use {\tt computingPrecision=>4} for quad double precision.
///;

doc ///
  Key
    [solveSystem,numThreads]
  Headline
    option to set the number of threads
  Usage
    solveSystem(...,numThreads=>ZZ)
  Description
    Text
      Use {\tt numThreads=>4} to run the path trackers with 4 threads.
///;

doc ///
  Key
    [solveSystem,randomSeed]
  Headline
    option to set the seed of the random number generators
  Usage
    solveSystem(...,randomSeed=>ZZ)
  Description
    Text
      Use {\tt randomSeed=>12345} to set the seed to 12345.
///;

-------------------------
-- toLaurentPolynomial --
-------------------------

doc ///
  Key    
    toLaurentPolynomial
    (toLaurentPolynomial,List,Symbol)
  Headline
    converts a list of rational polynomials into Laurent polynomials 
  Usage
    toLaurentPolynomial(system, w)
  Inputs
    system:List
      a list of rational polynomials from the same ring
    w:Symbol
      a symbol to be used for new indexed variables	   
  Outputs
    :List 
      the same system converted to a Laurent polynomial system
  Description
    Text 
      
      This function converts a rational system to a Laurent polynomial system.
    
    Example
      QQ[x,y,z];
      sys = {y-x^2, z-x^3, (x+y+z-1)/x};
      describe ring ideal sys 
    Text
    
      There are denominators, so the method will convert these:
    
    Example
      convertedSys = toLaurentPolynomial(sys,w);    
    Text
    
      The system convertedSys is an equivalent system living in a Laurent polynomial ring.
      For each denominator, a new variable was created starting with w.
    
    Example
      printWidth = 300;
      toString convertedSys
      ring ideal convertedSys
    Text

      Next, notice that the option "Inverses=>true" shows that this is a ring with negative exponents allowed:

    Example
      describe oo 
    Text
    
      Note that if the system is already polynomial, or in Laurent 
      polynomial form, then {\tt toLaurentPolynomial} doesn't change it.
      Of course, sometimes it is possible that the system is polynomial 
      "in disguise" as in line o10 in the following example:
    
    Example
      P = QQ[x,y,z];
      f = (x*y + z^2*y) / y 
      liftable(f,P) 
    Text
    
      But {\tt toLaurentPolynomial} detects this and simplifies the system.
      Instead of creating Laurent polynomials, 
      it updates the system using the following:
    
    Example
      lift(f,P)
///;

-------------------
-- topWitnessSet --
-------------------

doc ///
  Key
    topWitnessSet
    (topWitnessSet,List,ZZ)
  Headline
    returns a witness set and nonsolutions for the top dimensional solution set
  Usage
    (w,ns) = topWitnessSet(f,k)
  Inputs
    f:List
      of polynomials expected to have a component of dimension k
    k:ZZ 
      the expected (top) dimension of the solution set of f
  Outputs
    w:WitnessSet
      for the k-dimensional solution set of f
    ns:List
      solutions with nonzero value for the slack variable (the nonsolutions)
  Consequences
    Item
      Constructs an embedding using @TO constructEmbedding@,
      which calls {\tt phc -c}
    Item
      Solves the system using @TO solveSystem@, which calls {\tt phc -b}
    Item
      Filters the solutions
    Item
      Constructs the witness set of appropriate dimension.
  Description
    Text
      The method {\tt topWitnessSet} constructs an embedding 
      for the given polynomial system with the given dimension,
      and then computes generic points on the solution set.  

      The computation of a witness set for the twisted cubic
      is illustrated below.
      
    Example
      R = CC[x,y,z];
      f = { x^2 - y, x^3 - z };
      (w,ns) = topWitnessSet(f,1)
      dim(w)
      degree(w)
      toString equations(w)
      toString slice(w)
      toString points(w)

    Text
      A witness set for the twisted cubic consists of the embedded system,
      a random linear hyperplane to slice the space curve,
      and three generic points.  Observe that the value for the last
      coordinate of all points equals (or is close to) zero.
      This last coordinate corresponds to the added slack variable zzk.
      Solutions with a nonzero value for the slack variable are called
      nonsolutions.  In the example above, the list of nonsolutions 
      returned in ns by {\tt topWitnessSet} was empty.
     
      Often the solution of the embedded system leads to solutions
      with nonzero slack variables, as illustrated in the next example.
    
    Example
      R = CC[x,y,z]; f = { (x^2-y)*(x-2), (x^3 - z)*(y-2), (x*y - z)*(z-2) }
      (w,ns) = topWitnessSet(f,1);
      dim(w)
      degree(w)
      #ns

    Text
      The example is constructed to contain not only the twisted cubic,
      but also at least one isolated point (2,2,2).
      This is reflected in the list of nonsolutions.

      The nonsolutions may be used as start solutions in a cascade of
      homotopies to find generic points on lower dimensional components.
  
  SeeAlso
    constructEmbedding
    solveSystem    
///;

-- options for topWitnessSet

doc ///
  Key
    [topWitnessSet,Verbose]
  Headline
    option to specify whether additional output is wanted 
  Usage
    topWitnessSet(...,Verbose=>Boolean)
  Description
    Text
      Use {\tt Verbose=>true} for additional output which includes the 
      input and solution file names used by {\tt phc -c}.  

      The output file of {\tt phc} contains information about the
      application of the blackbox solver to the embedded system.
///;

----------------
-- trackPaths --
----------------

doc ///
  Key
    trackPaths
    (trackPaths, List, List, List)
  Headline
    tracks paths defined by a typical homotopy between start and end systems
  Usage
    solsT = trackPaths(T,S,solsS)
  Inputs
    T:List
      of polynomials, called the target system
    S:List
      of polynomials, called the start system
    solsS:List
      solutions of {\tt S}
  Outputs
    solsT:List
      containing the solutions of {\tt T} that are at the ends of the paths
      starting at {\tt solsS}
  Consequences
    Item
      Writes the start system and its solutions to temporary files
    Item
      Invokes the command {\tt phc -p} 
    Item
      After running {\tt phc -z}, stores output of phc in temporary file
    Item
      Parses and outputs the solutions.
  Description
    Text
      A linear homotopy between target T and start S has the form 
      (1-Lambda)*S + Lambda*T = 0.  The method {\tt trackPaths} tracks the
      solution paths defined by the homotopy for Lambda going from 0 to 1.
      In the example below, we first construct a start system using
      polyhedral homotopies using @TO mixedVolume @.
      
    Example
      R = CC[x,y]; 
      f = { x^3*y^5 + y^2 + x^2*y, x*y + x^2 - 1};
      (m,q,qsols) = mixedVolume(f,StartSystem=>true);
      fsols = trackPaths(f,q,qsols)
  SeeAlso
    gamma
    interactive
    intermediateSolutions
    loadSettingsPath
    saveSettingsPath
    numThreads
    seeProgress
    tDegree
///;

-- options for trackPaths

doc ///
  Key
    gamma
  Headline
    the constant in the gamma trick, optional input for trackPaths
  Description
    Text
      The `gamma trick' refers to the following idea:
      
      If the solutions of the start system are regular, 
      then we avoid singular solutions 
      along the paths by multiplying the start system in the homotopy 
      with a random complex constant {\tt gamma}.     
      This option allows the user to give a specific value of this 
      gamma constant.
    
    Example
      R = CC[x,y]; 
      f = { x^3*y^5 + y^2 + x^2*y, x*y + x^2 - 1};
      (m,q,qsols) = mixedVolume(f,StartSystem=>true);
      fsols = trackPaths(f,q,qsols,gamma => exp(ii*pi/3))
    Text
    
      Reference: 
      {A.J. Sommese, J. Verschelde, and C.W. Wampler. {\em Introduction 
      to numerical algebraic geometry.}
      In: Solving Polynomial Equations. Foundations, Algorithms 
      and Applications, volume 14 of 
      Algorithms and Computation in Mathematics, pages 301-337. 
      Springer-Verlag, 2005.}
///;

doc ///
  Key
    [trackPaths,gamma]
  Headline
    Option to specify the value of the constant in the gamma trick
  Usage
    trackPaths(...,gamma=>CC)
///;

doc ///
  Key
    tDegree
  Headline
    the degree of the continuation parameter
  Description
    Text
      By default, the homotopy is created with tDegree equal to 2.
      This option allows the user to give another value for tDegree;
      e.g., a linear homotopy between start system q and target system f 
      is of the form (1-t)*q + t*f.
      
      In general, if tDegree is k, then the homotopy is 
      of the form (1-t)^k*q + t^k*f. 
      
      A reason for changing the tDegree would be the following: 
      higher degree homotopies ensure that the system doesn't change as fast 
      in the beginning and at the end of the homotopy; 
      that is, they force smaller step sizes in the beginning and end. 
      The default value, 2, is usually sufficient. 
    
    Example
      R = CC[x,y]; 
      f = { x^3*y^5 + y^2 + x^2*y, x*y + x^2 - 1};
      (m,q,qsols) = mixedVolume(f,StartSystem=>true);
      fsols = trackPaths(f,q,qsols,tDegree => 1)      
///;

doc ///
  Key
    [trackPaths,tDegree]
  Headline
    Option to specify the degree of the continuation parameter
  Usage
    trackPaths(...,tDegree=>ZZ)
///;

doc ///
  Key
    numThreads
  Headline
    the number of threads in the path tracker
  Description
    Text
      Tracking many solution paths is a pleasingly parallel computation.
      A multithreaded path tracker has a number of threads working on
      a queue of path tracking jobs.  Every path tracking job can be
      computed without communication overhead.  For sufficiently large
      problems, the speedup can as large as the number of threads.
///;

doc ///
  Key
    [trackPaths,numThreads]
  Headline
    Option to define the number of threads in the path tracker.
  Usage
    trackPaths(...,numThreads=>ZZ)
///;

doc ///
  Key
    seeProgress
  Headline
    flag to monitor the progress of the multithreaded path tracker
  Description
    Text
      For a long path tracking job, one could check the progress of the
      computation by checking the end of the output file.
      A multithreaded path tracker no longer writes the solutions to the
      output file as soon as they are computed.  For path tracking jobs
      that take a very long time, the lack of information on the progress
      can be annoying.  With seeProgess turned on, every thread will write
      a message to screen for each path tracking job.
///;

doc ///
  Key
    [trackPaths,seeProgress]
  Headline
    Option to follow the progress of the multithreaded path tracker.
  Usage
    trackPaths(...,seeProgress=>Boolean)
///;

doc ///
  Key
    [trackPaths,interactive]
  Headline
    Option to run phc -p in interactive mode.
  Usage
    trackPaths(...,interactive=>Boolean)
///;

doc ///
  Key
    [trackPaths,Verbose]
  Headline
    option to specify whether additional output is wanted 
  Usage
    trackPaths(...,Verbose=>Boolean)
  Description
    Text
      Use {\tt Verbose=>true} for additional output which includes the 
      input and solution file names used by {\tt phc}.  

      The output file of {\tt phc} contains timings for the path tracker
      and additional diagnostics for each path.
///;

doc ///
  Key
    intermediateSolutions
  Headline
    option of trackPaths to get all intermediate solutions on a path
  Description
    Text
      By default, when this option is false, on return are only the
      end points of each solution path.

      With this option set to true, on return are all intermediate
      solutions along a path.  For large systems and/or complicated
      solution paths, the list on return can be rather large.
///;

doc ///
  Key
    [trackPaths,intermediateSolutions]
  Headline
    option to get all intermediate solutions on a path
  Usage
    trackPaths(...,intermediateSolutions=>Boolean)
///;

doc ///
  Key
    saveSettingsPath
  Headline
    option of trackPaths to save the settings for a reproducible rerun
  Description
    Text
      By default, this option is set to the empty string.

      If the user provides a string that is not the empty string,
      then the settings of the path tracker are saved for a rerun.
      Calling trackPaths, giving the same string to loadSettingsPath,
      enables a reproducible run.
///;

doc ///
  Key
    [trackPaths,saveSettingsPath]
  Headline
    option to save the settings of the path trackers for a reproducible rerun
  Usage
    trackPaths(...,saveSettingsPath=>String)
///;

doc ///
  Key
    loadSettingsPath
  Headline
    option of trackPaths to load the settings for a reproducible rerun
  Description
    Text
      By default, this option is set to the empty string.

      To apply the option, the user should give as string the argument
      used for the option saveSettingsPath.
      With this option, one gets a reproducible run.
///;

doc ///
  Key
    [trackPaths,loadSettingsPath]
  Headline
    option to load the settings of the path trackers for a reproducible rerun
  Usage
    trackPaths(...,loadSettingsPath=>String)
///;

----------------
-- zeroFilter --
----------------

doc ///
  Key 
    zeroFilter
    (zeroFilter,List,ZZ,RR)
  Headline
    returns solutions with k-th coordinate less than the given tolerance
  Usage
    zeroSols = zeroFilter(sols,k,tol)
  Inputs
    sols:List
      containing the solutions of a polynomial system
    k:ZZ
      index of a coordinate of a solution
    tol:RR
      tolerance on the absolute value of the k-th coordinate
  Outputs
    zeroSols:List
      solutions with k-th coordinate less than the given tolerance
  Description
    Text
      
      A solution has its k-th coordinate zero when the abs function
      evaluates to a number less than or equal to the given tolerance.
    
    Example
      R = CC[x,y];  
      f = { x^3*y^5 + y^2 + x^2*y, x*y + x^2 - 1};
      fSols = solveSystem(f, randomSeed=>3);
      fSols/print
    Text
    
      There is one solution with zero second coordinate:
    
    Example
      zeroSols = zeroFilter(fSols,1,1.0e-10);
      zeroSols / print 
    Text
    
      Here is another system where we filter solutions with `small' 
      first coordinate:
    
    Example
      f = {x^2+y^2,y*x+x}; 
      fSols = solveSystem(f);
      fSols/print
      zeroSols = zeroFilter(fSols,0,1.0e-10);
      zeroSols/print
    Text
    
      Good values for the tolerance are relative to the accuracy
      and the condition number of the solution.
      To improve the accuracy of a solution, apply
      @TO refineSolutions@ with a higher working precision.
  SeeAlso
    refineSolutions
    nonZeroFilter      
///;

---------------------
-- intersectSlice  --
---------------------

doc ///
  Key 
    intersectSlice
    (intersectSlice,WitnessSet,List)
  Headline
    intersects a witness set by a slice
  Usage
    fSols = intersectSlice(w, slice)
  Inputs
    w:WitnessSet
      a witness set for a solution set
    slice:List
      a list of linear equations
  Outputs
    fSols:List
      solutions that satisfy w.Equations and the equations in the slice
  Description
    Text
      
      A typical application is to find solutions for slices with
      real coefficients.
    
    Example
      R=CC[a,b,c,d];
      M=matrix for i to 2 list for j to 3 list random(1,R)+random(0,R);
      I=minors(3,M);
      f=flatten entries gens I;
      (w,ns) = topWitnessSet(f,2);
      slcmat = matrix applyTable (entries w.Slice, x->1_CC*realPart x);
      Rtwo = ring w.Equations;
      X = transpose matrix {gens Rtwo | {1_CC}};
      slcRR = flatten entries (promote(slcmat,Rtwo) * X);
      fsols = intersectSlice(w,slcRR)
    
  SeeAlso
    topWitnessSet
///;

------------------
-- realSlice1D  --
------------------

doc ///
  Key 
    realSlice1D
    (realSlice1D, WitnessSet)
  Headline
    computes a real slice for a one dimensional witness set
  Usage
    slc = realSlice1D(w)
  Inputs
    w:WitnessSet
      a witness set for a solution set
  Outputs
    slc:List
      list of linear equations with the largest number of real solutions
  Description
    Text
      
      A real slice is a set of linear equations with the largest number
      of real solutions of the equations for a given witness set.
    
    Example
      R = CC[x,y,z];
     -- twisted = {z^2-y, y*z-x, y^2-x*z};
     -- (w, ns) = topWitnessSet(twisted, 1);
     -- slc = realSlice1D(w);
     -- solsRR = intersectSlice(w,slc)
     -- for i to #solsRR-1 do print solsRR_i
    
  SeeAlso
    intersectSlice
///;

------------------
-- realSlice2D  --
------------------

doc ///
  Key 
    realSlice2D
    (realSlice2D, WitnessSet)
  Headline
    computes a real slice for a two dimensional witness set
  Usage
    slc = realSlice2D(w)
  Inputs
    w:WitnessSet
      a witness set for a solution set
  Outputs
    slc:List
      list of linear equations with the largest number of real solutions
  Description
    Text
      
      A real slice is a set of linear equations with the largest number
      of real solutions of the equations for a given witness set.
    
    Example
      R = CC[x,y,z];
     -- paraboloid = {z - x^2 - y^2};
     -- (w, ns) = topWitnessSet(paraboloid, 2);
     -- slc = realSlice2D(w, searchNpoints=>5);
     -- solsRR = intersectSlice(w,slc)
     -- for i to #solsRR-1 do print solsRR_i
    
  SeeAlso
    intersectSlice
///;

doc ///
  Key
    searchNpoints
  Headline
    option of realSlice1D
  Description
    Text
      Before the line search, a discretization of the range of the slices
      is computed.  The value of searchNpoints sets the number of equidistant
      points in this range of slices.
///;

doc ///
  Key
    [realSlice1D,searchNpoints]
  Headline
    option of realSlice1D
  Usage
    realSlice1D(...,searchNpoints=>Number)
///;

doc ///
  Key
    [realSlice2D,searchNpoints]
  Headline
    option of realSlice2D
  Usage
    realSlice2D(...,searchNpoints=>Number)
///;

doc ///
  Key
    searchDelta
  Headline
    option of realSlice1D
  Description
    Text
      In the line search we need to set the width of the search interval.
      After a discretization, the golden section search method is applied
      to the interval [p - searchDelta, p + searchDelta], where p is the
      point where the minimum value after the discretization was found.
///;

doc ///
  Key
    [realSlice1D,searchDelta]
  Headline
    option of realSlice1D
  Usage
    realSlice1D(...,searchDelta=>Number)
///;

doc ///
  Key
    [realSlice2D,searchDelta]
  Headline
    option of realSlice2D
  Usage
    realSlice2D(...,searchDelta=>Number)
///;

doc ///
  Key
    searchTolerance
  Headline
    option of realSlice1D
  Description
    Text
      The golden section search method stops when the width of the current
      interval which contains the minimum is smaller than searchTolerance.
      For unimodal functions, searchTolerance will be the bound on the
      accuracy of the location of the minimum.
///;

doc ///
  Key
    [realSlice1D,searchTolerance]
  Headline
    option of realSlice1D
  Usage
    realSlice1D(...,searchTolerance=>Number)
///;

doc ///
  Key
    [realSlice2D,searchTolerance]
  Headline
    option of realSlice2D
  Usage
    realSlice2D(...,searchTolerance=>Number)
///;
