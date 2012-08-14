-- documentation of the package PHCpack

doc ///
  Key 
    PHCpack 
  Headline
    a package for Polynomial Homotopy Continuation
  Description
    Text
      Interfaces the functionality of the software {\tt PHCpack} 
      to solve polynomial systems and perform calculations in
      {\em numerical algebraic geometry}.  The software is available at
      @HREF"http://www.math.uic.edu/~jan/download.html"@.
      The site provides source code and its executable versions {\tt phc}.
      The user must have the executable program {\tt phc} available,
      preferably in the executation path.
      
      Below is a simple example using the most popular function, 
      the numerical blackbox solver.
    Example     
      R = QQ[x,y,z]
      system = {y-x^2,z-x^3,x+y+z-1}
      solns = solveSystem(system)
      numSolns = #solns
      solns/print
    Text
      We see that there are three solutions to the above system. 
      In addition, we can obtain diagnostic information 
      about the quality of each solution.    
      Each solution is of type Point: 
    Example
      oneSoln = solns_0
      peek oneSoln
    Text      
      The value of LastT is the end value of the continuation parameter:
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
///;

-----------------------------------
-- solveSystem
-----------------------------------

doc ///
  Key
    solveSystem
    (solveSystem, List)
  Headline
    a numerical blackbox solver  (invokes "phc -b")
  Usage
    L = solveSystem(S)
  Inputs
    S:List
      containing a zero-dimensional system of polynomials with complex coefficients
      that contains at least as many equations as indeterminates 
  Outputs
    L:List 
      containing the solutions of S, each of type Point. 
  Description
    Text
      Suppose we want numerical approximations of all complex isolated 
      solutions to the following system:
      
    Example
      R = CC[x,y,z]
      S = {x+y+z-1, x^2+y^2, x+y-z-3}
    Text
      We call PHCpack's blackbox solver:
    
    Example
      L = solveSystem(S)
    Text
      The method solveSystem prints the the PHCpack input and output file names 
      and returns two solutions. The solutions are of type Point, defined in @TO NAGtypes@. 
      Each Point comes with diagnostics.
      For example, {\tt LastT} is the end value of the continuation parameter; 
      if it equals 1, then the solver reached the end of the path properly.
    
    Example
      oneSoln = L_0
      peek oneSoln
    
    Text
      If the coefficient ring of the polynomial ring of the system is not a ComplexField, 
      then the method will return an error.  One way around this is to use sub first:
    
    Example
      R = QQ[x,y,z]
      S = {x+y+z-1, x^2+y^2, x+y-z-3} 
      system = sub(ideal S, CC[gens R])
      sols = solveSystem( flatten entries gens system )
      
    Text
      The method handles overdetermined systems by inserting slack variables.
      
    Example
      system = {y-x^2, z-x^3, x+y+z-1, x+y+ x^3 -1}
      #system > numcols vars R --overdetermined system
      solns = solveSystem(system);
      numSolns = #solns  

  Caveat
      The method solveSystem does not check 
      the dimension of the system; it checks for number of equations instead.
      So solveSystem will return an error if there are less equations than unknowns 
      even if the system is zero-dimensional. In addition, if the system is square
      but not zero-dimensional, the output is meaningless.  Thus, it is suggested 
      that the user checks the dimension of the system before using
      solveSystem.
      

    
///;

-----------------------------------
-- refineSolutions
-----------------------------------

doc ///
  Key 
    refineSolutions
    (refineSolutions,List,List,ZZ)
  Headline
    refines solutions of a system by increasing working precision (invokes "phc -v")
  Usage
    newSols = refineSolutions(f,sols,dp)
  Inputs
    f:List
      polynomials in the given system
    sols:List
      containing solutions of the system (from a previous calculation)
    dp:ZZ
      the number of decimal places in working precision
  Outputs
    newSols:List
      containing the solutions {\tt sols} of {\tt f} refined to increased
      precision of {\tt dp} decimal places
  Description
    Text
      The user can specify the number of decimal places desired 
      to refine solutions.
      A simple example of when to use this function: 
      suppowe we would like  to decide if some of the solutions are really
      close to zero. 
      Let us compute a square root with a working precision of 64 decimal
      places:
    Example
      R = QQ[x,y]; S = {x^2 - 1/3, x*y - 1}; roots = solveSystem(S);
      r0 = roots#0#Coordinates#1
      newRoots = refineSolutions(S,roots,64)
      newRoots#0 -- recall that solutions are of type Point
      r1 = newRoots#0#Coordinates#1
///;

-----------------------------------
-- mixedVolume
-----------------------------------

doc ///
  Key
    mixedVolume
    (mixedVolume, List)
    [mixedVolume, StartSystem]
    [mixedVolume, StableMixedVolume]
  Headline
    mixed volume of a polynomial system (invokes "phc -m")
  Usage
    mv = mixedVolume(S) 
    (mv,sv) = mixedVolume(S,StableMixedVolume => true)  
    (mv,q,qsols) = mixedVolume(S,StartSystem => true)     
    (mv,sv,q,qsols) = mixedVolume(S,StableMixedVolume => true, StartSystem => true)
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
  Description
    Text
      The mixed volume of a polynomial system $S:=\{f_1,\dots,f_n\}$ 
      is defined as follows:
      Let $P_1,\dots,P_n$ be the Newton polytopes of $f_1,\dots,f_n$, i.e., $P_i$ is the convex hull of the exponents of the monomials in the support of $f_i$. 
      The mixed volume of $S$ is 
      $$ \sum_{1\leq h\leq n} \sum_{1\leq i_1\dots\leq i_h\leq n} (-1)^{n-h}V_n(P_{i_1}+\dots+P_{i_h}),
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
      I=ideal f
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

-- options for mixedVolume

doc ///
  Key
    StableMixedVolume
  Headline
    optional input for computation of the stable mixed volume
  Description
    Text
      Put {\tt StableMixedVolume=>true} as an argument in the function @TO mixedVolume@ to count solutions in affine space.
///;

doc ///
  Key
    StartSystem
  Headline
    optional input for computation of mixed volume by solving a random coefficient system
  Description
    Text
      PUt {\tt StartSystem=>true} as an argument in the function @TO mixedVolume@ to tell the method to use polyhedral homotopies. 
      Polyhedral homotopies solve a system with the same Newton polytopes as the original system and with random complex coefficients.
      This random coefficient system can serve as a start system to solve the original input system.
///;

-----------------------------------
-- trackPaths
-----------------------------------

doc ///
  Key
    trackPaths
    (trackPaths, List, List, List)
    [trackPaths, tDegree]
    [trackPaths, gamma]
  Headline
    tracking paths defined by a typical homotopy between start and end systems (invokes "phc -p")
  Usage
    solsT = trackPaths(T,S,solsS)
    solsT = trackPaths(f,q,qsols,tDegree => ZZ)
    solsT = trackPaths(f,q,qsols,gamma => CC)
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
  Description
    Text
      A linear homotopy between target T and start S has the form 
      (1-Lambda)*S + Lambda*T = 0.  The method {\tt trackPaths} tracks the
      solution paths defined by the homotopy for Lambda going from 0 to 1.
      In the example below, we first construct a start system using
      polyhedral homotopies using @TO mixedVolume @
    Example
      R = CC[x,y]; 
      f = { x^3*y^5 + y^2 + x^2*y, x*y + x^2 - 1};
      (m,q,qsols) = mixedVolume(f,startSystem=>true);
      fsols = trackPaths(f,q,qsols)
  SeeAlso
    tDegree
    gamma
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
      (m,q,qsols) = mixedVolume(f,startSystem=>true);
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
      higher degree homotopies ensure that the system doesn't change as fast in the beginning and the end of the homotopy; 
      that is, they force smaller step sizes in the beginning and end. 
      The default value, 2, is usually sufficient. 
    Example
      R = CC[x,y]; 
      f = { x^3*y^5 + y^2 + x^2*y, x*y + x^2 - 1};
      (m,q,qsols) = mixedVolume(f,startSystem=>true);
      fsols = trackPaths(f,q,qsols,tDegree => 1)      
///;

-----------------------------------
----CONSTRUCT EMBEDDING------------
-----------------------------------

doc ///
  Key
    constructEmbedding
    (constructEmbedding,List,ZZ)
  Headline
    calls phc -c to construct an embedding of a polynomial system
  Usage
    g = constructEmbedding(f,k)
  Inputs
    f:List
      of polynomials expected to have a component of dimension k
    k:ZZ 
      the expected (top) dimension of the solution set of f
  Outputs
    g:List
      of polynomials containing the original system with k random
      linear polynomials and k slack variables
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
      The ring in which the original system lives is extended with
      as many slack variables as the dimension provided as the second
      argument k to constructEmbedding.  The slack variables start with zz.
      Solutions of the embedded system with
      zero values for the slack variables are candidate generic points.

      If the system on input is overdetermined (there are more equations
      than unknowns), then as many surplus variables are introduced as
      the difference between the number of equations and the number of
      variables.  Surplus variables start with ss.
    Example
      R = CC[x,y,z];
      f = { x^2-y, x^3-z, x*y-z, x*z-y^2 };
      fe1 = constructEmbedding(f,1);
      toString fe1
    Text
      In the example above we gave four equations in three unknowns
      and phcEmbed added one surplus variable ss1 and one slack variable zz1.
      Only solutions with zero values for the surplus variable ss1 matter.
///;

-----------------------------------
-- topWitnessSet
-----------------------------------

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
  Description
    Text
      The method topWitnessSet constructs and embedding for the given
      polynomial system with the given dimension and then calls solveSystem
      for the computation of generic points on the solution set.

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
      A witness set for the twisted cubic consists of the original system,
      a random linear hyperplane to slice the space curve,
      and three generic points.  Observe that the value for the last
      coordinate of all points equals (or is close to) zero.
      This last coordinate corresponds to the added slack variable.
      Solutions with nonzero value for the slack variable are called
      nonsolutions.  In the example above, the list of nonsolutions 
      returned in ns by topWitnessSet was empty.
     
      Often the solution of the embedded system leads to solutions
      with nonzero slack variables as illustrated in the next example.
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
///;

-----------------------------------
-----------  FACTOR  --------------
-----------------------------------

doc ///
  Key
    factorWitnessSet
    (factorWitnessSet,WitnessSet)
  Headline
    applies monodromy to factor a witness set into irreducible components
  Usage
    L = factorWitnessSet(w)
  Inputs
    w:WitnessSet
      properly embedded with slack variables
  Outputs
    L:List
      a lists of witness sets, every element of the list is irreducible
  Description
    Text
      A witness set is irreducible if there exists a path not passing
      through a singularity between any two of its generic points.

      We illustrate the factorization via the twisted cubic and a line.
    Example
      R = CC[x,y,z]; f = {(x^2-y)*(x-1), x^3 - z};
      (w,ns) = topWitnessSet(f,1);
      degree(w)
      L = factorWitnessSet(w)
///;

-----------------------------------
-- cascade
-----------------------------------

doc ///
  Key 
    cascade
    (cascade,List)
  Headline
    runs a cascade of homotopies to get witness sets for each component of the variety (invokes "phc -c")
  Usage
    C = cascade L
  Inputs
    L:List
  Outputs
    V:NumericalVariety
      containing the witness points for each pure-dimensional variety 
      contained in V(I). 
  Description
    Text
      Given an ideal, I, this function creates an embedding and runs the
      cascades of homotopies. The output is a list of witness points for 
      each pure dimensional variety contained in V(I). 
      For each pure dimensional subvariety of dimension d, a file is created
      with the ending {\tt target_sw"d"}. This file contains the complete 
      witness set: the witness points and the d general hyperplanes. 
      Each entry in the list 
      is  a hashtable, which lists the dimension, filename of
      the witness set, and witness points for the stated dimension.
    Example
      R=CC[x11,x22,x21,x12,x23,x13,x14,x24]
      system={x11*x22-x21*x12,x12*x23-x22*x13,x13*x24-x23*x14}
      dim ideal system
      degree ideal system
      cascade system
  Caveat
    This function should be returning witness sets using the type WitnessSet 
    from @TO NAGtypes@. This will be completed in version 1.1.
///;

-----------------------------------
-- toLaurentPolynomial
-----------------------------------

doc ///
  Key    
    toLaurentPolynomial
    (toLaurentPolynomial,List,Symbol)
  Headline
    converts a list of rational polynomials into Laurent polynomials 
  Usage
    toLaurentPolynomial(system, x)
  Inputs
    system:List
      a list of rational polynomials from the same ring
    x:Symbol
      a symbol to be used for new indexed variables	   
  Outputs
    s:List 
      the same system converted to a Laurent polynomial system
  Description
    Text 
      Here is an example when this function might be used:
    Example
      QQ[x,y,z];
      sys = {y-x^2, z-x^3, (x+y+z-1)/x};
      describe ring ideal sys -- "there are denominators, so the method will convert these"
      convertedSys = toLaurentPolynomial(sys,w);    
    Text
      The system convertedSys is an equivalent system living in a Laurent polynomial ring.
      For each denominator, a new variable was created.
    Example
      printWidth = 300;
      toString convertedSys
      ring ideal convertedSys
      describe oo -- the option "Inverses=>true shows that this
                  -- that this is a ring with negative exponents allowed                
    Text
      Note that if the system is already polynomial, or in Laurent 
      polynomial form, the toLaurentPolynomial doesn't change it.
      Of course, sometimes it is possible that the system is polynomial 
      "in disguise" as in line o10 in the following example:
    Example
      P = QQ[x,y,z];
      f = (x*y + z^2*y) / y 
      liftable(f,P) 
    Text
      But toLaurentPolynomial detects this and simplifies the system.
      Instead of creating Laurent polynomials, 
      it updates the system using the following:
    Example
      lift(f,P)
///;


-----------------------------------
-- zeroFilter
-----------------------------------

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
      R = QQ[x,y];  
      f = { x^3*y^5 + y^2 + x^2*y, x*y + x^2 - 1};
      fSols = solveSystem(f);
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

-----------------------------------
-- nonZeroFilter
-----------------------------------

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
      R = QQ[x,y];  
      f = { x^3*y^5 + y^2 + x^2*y, x*y + x^2 - 1};
      fSols = solveSystem(f);
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

