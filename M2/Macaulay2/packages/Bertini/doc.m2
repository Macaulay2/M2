doc ///
  Key
    Bertini
  Headline
    software for numerical algebraic geometry
  Description
    Text      
      Interfaces the functionality of the software {\tt Bertini}
      to solve polynomial systems and perform calculations in
      {\em numerical algebraic geometry}. The software is available at
      @HREF"http://www.nd.edu/~sommese/bertini/"@.
      The site currently provides only executable versions named {\tt bertini} or {\tt bertini.exe} (for Cygwin).
      The user must have the executable program {\tt bertini} available,
      preferably in the executation path.

      Below is a simple example using the most popular function,
      a basic zero-dimensional solve with no special options.
    Example
      R = CC[x,y]
      F = {x^2-1,y^2-1}
      solns = bertiniZeroDimSolve(F)
///;

doc ///
 Key
   bertiniZeroDimSolve
   (bertiniZeroDimSolve,List)
 Headline
   solve zero-dimensional system of equations
 Usage
   S = bertiniZeroDimSolve F
 Inputs
   F:List
     whose entries are polynomials (system need not be square)
 Outputs
   S:List
     of solutions of type Point    
 Description
   Text
     Finds solutions to the zero-dimensional system F via numerical polynomial homotopy continuation.
     This function builds a Bertini input file from the system F and calls Bertini on
     this input file. Solutions are pulled from machine readable file {\tt finitesolutions}
     and returned as a list.
   Example
     R = CC[x,y]
     F = {x^2-1,y^2-1}
     S = bertiniZeroDimSolve F
///;

doc ///
  Key
    Bertini optional configuration settings
    MPTYPE
    PRECISION
    ODEPREDICTOR
    TRACKTOLBEFOREEG
    TRACKTOLDURINGEG
    FINALTOL
    MAXNORM
    MINSTEPSIZEBEFOREEG
    MINSTEPSIZEDURINGEG
    IMAGTHRESHOLD
    COEFFBOUND
    DEGREEBOUND
    CONDNUMTHRESHOLD
    RANDOMSEED
    SINGVALZEROTOL
    ENDGAMENUM
    USEREGENERATION
    SECURITYLEVEL
    SCREENOUT
    OUTPUTLEVEL
    STEPSFORINCREASE
    MAXNEWTONITS
    MAXSTEPSIZE
    MAXNUMBERSTEPS
    MAXCYCLENUM
    REGENSTARTLEVEL
  Headline
    Options for bertini.m2 functions.
  Description
    Text
      Every function of the package takes ALL optional arguments listed here.
      The default value for EACH option is -1, which tells Bertini to use its internal default.
      Refer to Appendix E of SIAM Bertini book for full details and list of options. 

      MPTYPE: Type of precision (0=double, 1=fixed higher, 2=adaptive).
      PRECISION: Precision, in bits, when used MPTYPE=1.
      ODEPREDICTOR: Choice of predictor method (9 choices).
      TRACKTOLBEFOREEG: Before endgame zone, Newton error must be less than this for success. 
      TRACKTOLDURINGEG: Same as previous, but during endgame.
      FINALTOL: Path is deemed successful if final two endpoint approximations agree to FINALTOL.
      MAXNORM: If SECURITYLEVEL=0, path is truncated if two consecutive endpoint approximations exceed this value. 
      MINSTEPSIZEBEFOREEG: Path is truncated if stepsize drops below this level before endgame.
      MINSTEPSIZEDURINGEG: Same as previous, but during endgame.
      IMAGTHRESHOLD: Endpoint deemed real if infinity norm is smaller than this. 
      COEFFBOUND: Useful only if MPTYPE=2, bound on sum of coefficients of each polynomial. 
      DEGREEBOUND: Useful only if MPTYPE=2, bound on degree of each polynomial.
      CONDNUMTHRESHOLD: Endpoint is deemed singular if multiple paths lead to it or condition number exceeds this. 
      RANDOMSEED: Useful to repeat runs with the same random numbers.
      SINGVALZEROTOL: Singular value is considered 0 if less than this value, when using fixed precision.
      ENDGAMENUM: Choice of endgame (1=power series, 2=Cauchy, 3=trackback Cauchy).
      USEREGENERATION: 1 to use regeneration for a zero-dimensional run.
      SECURITYLEVEL: 1 to avoid truncation of possibly-infinite paths.
      SCREENOUT: Level of output to the screen.
      OUTPUTLEVEL: Level of output to files.
      STEPSFORINCREASE: Number of consecutive Newton corrector successes before increase of stepsize.
      MAXNEWTONITS: Newton corrector step deemed failed if no convergence prior to this number of iterations. 
      MAXSTEPSIZE: Largest stepsize allowed. 
      MAXNUMBERSTEPS: Max number of steps for entire path.  Path failure if number of steps exceeds this.
      MAXCYCLENUM: Max cycle number considered during endgame.
      REGENSTARTLEVEL: Level at which regeneration begins. 

      There are two recommended ways of using the optional arguments.
    
      (1) Specify individual parameters in a function call:
    Example
      CC[x,y]; F = {x^2-1,y^2-1};
      bertiniZeroDimSolve(F,RANDOMSEED=>0,TRACKTOLBEFOREEG=>1e-6,FINALTOL=>1e-100)
    Text
      (2) Store your frequently used favorites in an OptionTable
      and pass it as the last argument in each function call:
    Example
      opts = new OptionTable from {RANDOMSEED=>0,TRACKTOLBEFOREEG=>1e-6,FINALTOL=>1e-100}
      bertiniZeroDimSolve(F,opts)
      G = {x^2+y^2-1};
      bertiniPosDimSolve(G,opts)
///;

doc ///
 Key
   bertiniPosDimSolve
   (bertiniPosDimSolve,List)
 Headline
   solve positive-dimensional system of equations
 Usage
   V = bertiniPosDimSolve F
 Inputs
   F:List
     whose entries are polynomials 
 Outputs
   V:NumericalVariety 
     a numerical description of irreducible components of the variety defined by F
 Consequences
   Item
     Writes the system to temporary files
   Item
     Invokes {\tt Bertini}'s solver with {\tt TRACKTYPE: 1}. Bertini uses a
      cascade homotopy to find witness supersets in each dimension, removes extra
      points using a membership test or local dimension test, deflates singular
      witness points, then factors using a combination of monodromy and a linear trace
      test 
   Item
     Stores output of {\tt Bertini} in temporary file
   Item
     Parses and outputs the solutions       
 Description
   Text
     The method {\tt bertiniPosDimSolve} calls  {\tt Bertini} to find
     a numerical irreducible decomposition of the zero-set of F.  The decomposition is
     returned as the @TO NumericalVariety@ NV.  Witness sets of NV contain approximations
     to solutions of the system F=0. 
   Example
     R = QQ[x,y,z]
     F = {(y^2+x^2+z^2-1)*x,(y^2+x^2+z^2-1)*y}
     S = bertiniPosDimSolve F
     S#1_0#Points -- 1_0 chooses the first witness set in dimension 1
   Text
     Each @TO WitnessSet@ is accessed by dimension and then list position.
   Example
     S#1 --first specify dimension
     peek oo_0 --then list position   
   Text
     In the example, we find two components, one component has dimension 1 and degree 1 and the other has
     dimension 2 and degree 2.  We get the same results using symbolic methods.
   Example
     PD=primaryDecomposition( ideal F)
     dim PD_0
     degree PD_0
     dim PD_1
     degree PD_1
        
///;


-- bertiniSample(
  -- {f1},dimen=>1,compnum=>1,numpts=>12,WitnessData=>wdf)


doc ///
 Key
   bertiniSample
   (bertiniSample,WitnessSet,ZZ)
 Headline
   sample points from an irreducible component of a variety
 Usage
   V = bertiniSample (W, n)
 Inputs
   W:WitnessSet
     irreducible
   n:ZZ
     number of desired sample points
 Outputs
   L:List
     sample points
 Consequences
  Item
    Writes the witness set to a temporary file
  Item
    Invokes {\tt Bertini}'s solver with option {\tt TRACKTYPE: 2}. 
    To sample, {\tt Bertini} moves the hyperplannes defined in the @TO WitnessSet@ W within
    the space until the desired points are sampled.
  Item
    Stores output of {\tt Bertini} in temporary file
  Item
    Parses and outputs the solutions    
 Description
   Text
     Samples points from an irreducible component of a variety using Bertini.  The irreducible
     component needs to be in its numerical form as a @TO WitnessSet@.  The method
     @TO bertiniPosDimSolve@ can be used to generate a witness set for the component.
   Example
     R = CC[x,y,z]     
     F = { (y^2+x^2+z^2-1)*x, (y^2+x^2+z^2-1)*y }
     NV = bertiniPosDimSolve(F)
     W = NV#1_0 --z-axis
     bertiniSample(W,4)
///;

doc ///
 Key
   bertiniTrackHomotopy
   (bertiniTrackHomotopy,List,RingElement,List)
 Headline
   track a user-defined homotopy
 Usage
   S0=bertiniTrackHomotopy(H, t, S1)
 Inputs
   H:List
     of polynomials that define the homotopy
   t:RingElement
     path variable
   S1:List
     of solutions to the start system  
 Outputs
   S0:List
     of solutions to the target system
 Consequences
  Item
    Writes the homotopy and start solutions to temporary files
  Item
    Invokes {\tt Bertini}'s solver with configuration keyword {\tt USERHOMOTOPY}. 
  Item
    Stores output of {\tt Bertini} in temporary file
  Item
    Parses and outputs the solutions       
 Description
   Text
     This method calls {\tt Bertini} to track a user-defined homotopy.  The
     user needs to specify the homotopy H, the path variable t, and a list
     of start solutions S1. In the following example, we solve $x^2-2$ by moving
     from $x^2-1$ with a linear homotopy. {\tt Bertini} tracks homotopies starting at
     $t=1$ and ending at $t=0$. Final solutions are of type Point.
   Example
     R = CC[x,t]; -- include the path variable in the ring     
     H = { (x^2-1)*t + (x^2-2)*(1-t)};
     sol1 = point {{1,0}};
     sol2 = point {{-1,0}};
     S1= { sol1, sol2  };	  
     S0 = bertiniTrackHomotopy (H, t, S1) 
     peek S0_0
///;

doc ///
 Key
   bertiniComponentMemberTest
   (bertiniComponentMemberTest, NumericalVariety, List)
 Headline
   tests whether points lie on a given variety
 Usage
   L = bertiniComponentMemberTest (NV, pts)
 Inputs
   NV:NumericalVariety
   pts:List
     points to test
 Outputs
   L:List
     entries are lists of witness sets containing the test point
 Description
   Text
     Tests whether pts lie on a given variety using Bertini.
   Example
     R = CC[x,y,z]
     F = {(y^2+x^2+z^2-1)*x,(y^2+x^2+z^2-1)*y}
     NV = bertiniPosDimSolve(F)
     pts = {{0,0,0}} --z-axis
     bertiniComponentMemberTest(NV,pts)
 Caveat
   In the current implementation, at most one witness set is listed for each test point although the point may lie on more than one component.
///;

doc ///
 Key
   bertiniRefineSols
   (bertiniRefineSols,List,List,ZZ)
 Headline
   sharpen solutions to a prescribed number of digits
 Usage
   S = bertiniRefineSols(F,l,d)
 Inputs
   F:List
     whose entries are polynomials (system need not be square)
   l:List
     whose entries are points to be sharpened
   d:ZZ
     number of digits
   
 Outputs
   S:List
     of solutions of type Point
 Description
   Text
     We take the list l of solutions of F and sharpen them to d digits.
   Example
     R = CC[x,y]
     F = {x^2-2,y^2-2}
     sols = bertiniZeroDimSolve (F)
     S = bertiniRefineSols (F,sols,100)
     coords = coordinates S_0
     coords_0
///;
doc ///
 Key
   bertiniParameterHomotopy
   (bertiniParameterHomotopy,List,List,List)
 Headline
   Runs parameter homotopy in Bertini
 Usage
   S = bertiniParameterHomotopy(F,P,T)
 Inputs
   F:List
     whose entries are polynomials (system need not be square)
   P:List
     parameter names
   T:List
     whose entries are lists of target parameter values
   
 Outputs
   S:List
     whose entries are lists of solutions for each target system
 Description
   Text
     Runs both stages of Bertini's parameter homotopy method.
   Example
     R=CC[u1,u2,u3,x,y]
     f1=u1*(y-1)+u2*(y-2)+u3*(y-3)
     f2=(x-11)*(x-12)*(x-13)
     finalParameters0={{1,0,0}}
     finalParameters1={{0,1+2*ii,0}}
     bPH=bertiniParameterHomotopy( {f1,f2}, {u1,u2,u3},{finalParameters0 ,finalParameters1 })
     bPH_0--the solutions to the system with finalParameters0
///;



doc ///
  Key
    [bertiniZeroDimSolve, COEFFBOUND]
  Headline
    optional input 
///;

doc ///
  Key
    [bertiniZeroDimSolve, CONDNUMTHRESHOLD]
  Headline
    optional input 
///;

doc ///
  Key
    [bertiniZeroDimSolve, DEGREEBOUND]
  Headline
    optional input 
///;

doc ///
  Key
    [bertiniZeroDimSolve, ENDGAMENUM]
  Headline
    optional input 
///;

doc ///
  Key
    [bertiniZeroDimSolve, FINALTOL]
  Headline
    optional input 
///;

doc ///
  Key
    [bertiniZeroDimSolve, IMAGTHRESHOLD]
  Headline
    optional input 
///;

doc ///
  Key
    [bertiniZeroDimSolve, MAXCYCLENUM]
  Headline
    optional input 
///;

doc ///
  Key
    [bertiniZeroDimSolve, MAXNEWTONITS]
  Headline
    optional input 
///;

doc ///
  Key
    [bertiniZeroDimSolve, MAXNORM]
  Headline
    optional input 
///;

doc ///
  Key
    [bertiniZeroDimSolve, MAXNUMBERSTEPS]
  Headline
    optional input 
///;

doc ///
  Key
    [bertiniZeroDimSolve, MAXSTEPSIZE]
  Headline
    optional input 
///;

doc ///
  Key
    [bertiniZeroDimSolve, MINSTEPSIZEBEFOREEG]
  Headline
    optional input 
///;

doc ///
  Key
    [bertiniZeroDimSolve, MINSTEPSIZEDURINGEG]
  Headline
    optional input 
///;

doc ///
  Key
    [bertiniZeroDimSolve, MPTYPE]
  Headline
    optional input 
///;

doc ///
  Key
    [bertiniZeroDimSolve, ODEPREDICTOR]
  Headline
    optional input 
///;

doc ///
  Key
    [bertiniZeroDimSolve, OUTPUTLEVEL]
  Headline
    optional input 
///;

doc ///
  Key
    [bertiniZeroDimSolve, PRECISION]
  Headline
    optional input 
///;

doc ///
  Key
    [bertiniZeroDimSolve, RANDOMSEED]
  Headline
    optional input 
///;

doc ///
  Key
    [bertiniZeroDimSolve, REGENSTARTLEVEL]
  Headline
    optional input 
///;

doc ///
  Key
    [bertiniZeroDimSolve, SCREENOUT]
  Headline
    optional input 
///;

doc ///
  Key
    [bertiniZeroDimSolve, SECURITYLEVEL]
  Headline
    optional input 
///;

doc ///
  Key
    [bertiniZeroDimSolve, SINGVALZEROTOL]
  Headline
    optional input 
///;

doc ///
  Key
    [bertiniZeroDimSolve, STEPSFORINCREASE]
  Headline
    optional input 
///;


doc ///
  Key
    [bertiniZeroDimSolve, STEPSFORINCREASE]
  Headline
    optional input 
///;

doc ///
  Key
    [bertiniZeroDimSolve, TRACKTOLBEFOREEG]
  Headline
    optional input 
///;

doc ///
  Key
    [bertiniZeroDimSolve, TRACKTOLDURINGEG]
  Headline
    optional input 
///;

doc ///
  Key
    [bertiniZeroDimSolve, USEREGENERATION]
  Headline
    optional input 
///;


doc ///
  Key
    [bertiniPosDimSolve, COEFFBOUND]
  Headline
    optional input 
///;

doc ///
  Key
    [bertiniPosDimSolve, CONDNUMTHRESHOLD]
  Headline
    optional input 
///;

doc ///
  Key
    [bertiniPosDimSolve, DEGREEBOUND]
  Headline
    optional input 
///;

doc ///
  Key
    [bertiniPosDimSolve, ENDGAMENUM]
  Headline
    optional input 
///;

doc ///
  Key
    [bertiniPosDimSolve, FINALTOL]
  Headline
    optional input 
///;

doc ///
  Key
    [bertiniPosDimSolve, IMAGTHRESHOLD]
  Headline
    optional input 
///;

doc ///
  Key
    [bertiniPosDimSolve, MAXCYCLENUM]
  Headline
    optional input 
///;

doc ///
  Key
    [bertiniPosDimSolve, MAXNEWTONITS]
  Headline
    optional input 
///;

doc ///
  Key
    [bertiniPosDimSolve, MAXNORM]
  Headline
    optional input 
///;

doc ///
  Key
    [bertiniPosDimSolve, MAXNUMBERSTEPS]
  Headline
    optional input 
///;

doc ///
  Key
    [bertiniPosDimSolve, MAXSTEPSIZE]
  Headline
    optional input 
///;

doc ///
  Key
    [bertiniPosDimSolve, MINSTEPSIZEBEFOREEG]
  Headline
    optional input 
///;

doc ///
  Key
    [bertiniPosDimSolve, MINSTEPSIZEDURINGEG]
  Headline
    optional input 
///;

doc ///
  Key
    [bertiniPosDimSolve, MPTYPE]
  Headline
    optional input 
///;

doc ///
  Key
    [bertiniPosDimSolve, ODEPREDICTOR]
  Headline
    optional input 
///;

doc ///
  Key
    [bertiniPosDimSolve, OUTPUTLEVEL]
  Headline
    optional input 
///;

doc ///
  Key
    [bertiniPosDimSolve, PRECISION]
  Headline
    optional input 
///;

doc ///
  Key
    [bertiniPosDimSolve, RANDOMSEED]
  Headline
    optional input 
///;

doc ///
  Key
    [bertiniPosDimSolve, REGENSTARTLEVEL]
  Headline
    optional input 
///;

doc ///
  Key
    [bertiniPosDimSolve, SCREENOUT]
  Headline
    optional input 
///;

doc ///
  Key
    [bertiniPosDimSolve, SECURITYLEVEL]
  Headline
    optional input 
///;

doc ///
  Key
    [bertiniPosDimSolve, SINGVALZEROTOL]
  Headline
    optional input 
///;

doc ///
  Key
    [bertiniPosDimSolve, STEPSFORINCREASE]
  Headline
    optional input 
///;


doc ///
  Key
    [bertiniPosDimSolve, STEPSFORINCREASE]
  Headline
    optional input 
///;

doc ///
  Key
    [bertiniPosDimSolve, TRACKTOLBEFOREEG]
  Headline
    optional input 
///;

doc ///
  Key
    [bertiniPosDimSolve, TRACKTOLDURINGEG]
  Headline
    optional input 
///;

doc ///
  Key
    [bertiniPosDimSolve, USEREGENERATION]
  Headline
    optional input 
///;

doc ///
  Key
    [bertiniComponentMemberTest, COEFFBOUND]
  Headline
    optional input 
///;

doc ///
  Key
    [bertiniComponentMemberTest, CONDNUMTHRESHOLD]
  Headline
    optional input 
///;

doc ///
  Key
    [bertiniComponentMemberTest, DEGREEBOUND]
  Headline
    optional input 
///;

doc ///
  Key
    [bertiniComponentMemberTest, ENDGAMENUM]
  Headline
    optional input 
///;

doc ///
  Key
    [bertiniComponentMemberTest, FINALTOL]
  Headline
    optional input 
///;

doc ///
  Key
    [bertiniComponentMemberTest, IMAGTHRESHOLD]
  Headline
    optional input 
///;

doc ///
  Key
    [bertiniComponentMemberTest, MAXCYCLENUM]
  Headline
    optional input 
///;

doc ///
  Key
    [bertiniComponentMemberTest, MAXNEWTONITS]
  Headline
    optional input 
///;

doc ///
  Key
    [bertiniComponentMemberTest, MAXNORM]
  Headline
    optional input 
///;

doc ///
  Key
    [bertiniComponentMemberTest, MAXNUMBERSTEPS]
  Headline
    optional input 
///;

doc ///
  Key
    [bertiniComponentMemberTest, MAXSTEPSIZE]
  Headline
    optional input 
///;

doc ///
  Key
    [bertiniComponentMemberTest, MINSTEPSIZEBEFOREEG]
  Headline
    optional input 
///;

doc ///
  Key
    [bertiniComponentMemberTest, MINSTEPSIZEDURINGEG]
  Headline
    optional input 
///;

doc ///
  Key
    [bertiniComponentMemberTest, MPTYPE]
  Headline
    optional input 
///;

doc ///
  Key
    [bertiniComponentMemberTest, ODEPREDICTOR]
  Headline
    optional input 
///;

doc ///
  Key
    [bertiniComponentMemberTest, OUTPUTLEVEL]
  Headline
    optional input 
///;

doc ///
  Key
    [bertiniComponentMemberTest, PRECISION]
  Headline
    optional input 
///;

doc ///
  Key
    [bertiniComponentMemberTest, RANDOMSEED]
  Headline
    optional input 
///;

doc ///
  Key
    [bertiniComponentMemberTest, REGENSTARTLEVEL]
  Headline
    optional input 
///;

doc ///
  Key
    [bertiniComponentMemberTest, SCREENOUT]
  Headline
    optional input 
///;

doc ///
  Key
    [bertiniComponentMemberTest, SECURITYLEVEL]
  Headline
    optional input 
///;

doc ///
  Key
    [bertiniComponentMemberTest, SINGVALZEROTOL]
  Headline
    optional input 
///;

doc ///
  Key
    [bertiniComponentMemberTest, STEPSFORINCREASE]
  Headline
    optional input 
///;


doc ///
  Key
    [bertiniComponentMemberTest, STEPSFORINCREASE]
  Headline
    optional input 
///;

doc ///
  Key
    [bertiniComponentMemberTest, TRACKTOLBEFOREEG]
  Headline
    optional input 
///;

doc ///
  Key
    [bertiniComponentMemberTest, TRACKTOLDURINGEG]
  Headline
    optional input 
///;

doc ///
  Key
    [bertiniComponentMemberTest, USEREGENERATION]
  Headline
    optional input 
///;

doc ///
  Key
    [bertiniParameterHomotopy, COEFFBOUND]
  Headline
    optional input 
///;

doc ///
  Key
    [bertiniParameterHomotopy, CONDNUMTHRESHOLD]
  Headline
    optional input 
///;

doc ///
  Key
    [bertiniParameterHomotopy, DEGREEBOUND]
  Headline
    optional input 
///;

doc ///
  Key
    [bertiniParameterHomotopy, ENDGAMENUM]
  Headline
    optional input 
///;

doc ///
  Key
    [bertiniParameterHomotopy, FINALTOL]
  Headline
    optional input 
///;

doc ///
  Key
    [bertiniParameterHomotopy, IMAGTHRESHOLD]
  Headline
    optional input 
///;

doc ///
  Key
    [bertiniParameterHomotopy, MAXCYCLENUM]
  Headline
    optional input 
///;

doc ///
  Key
    [bertiniParameterHomotopy, MAXNEWTONITS]
  Headline
    optional input 
///;

doc ///
  Key
    [bertiniParameterHomotopy, MAXNORM]
  Headline
    optional input 
///;

doc ///
  Key
    [bertiniParameterHomotopy, MAXNUMBERSTEPS]
  Headline
    optional input 
///;

doc ///
  Key
    [bertiniParameterHomotopy, MAXSTEPSIZE]
  Headline
    optional input 
///;

doc ///
  Key
    [bertiniParameterHomotopy, MINSTEPSIZEBEFOREEG]
  Headline
    optional input 
///;

doc ///
  Key
    [bertiniParameterHomotopy, MINSTEPSIZEDURINGEG]
  Headline
    optional input 
///;

doc ///
  Key
    [bertiniParameterHomotopy, MPTYPE]
  Headline
    optional input 
///;

doc ///
  Key
    [bertiniParameterHomotopy, ODEPREDICTOR]
  Headline
    optional input 
///;

doc ///
  Key
    [bertiniParameterHomotopy, OUTPUTLEVEL]
  Headline
    optional input 
///;

doc ///
  Key
    [bertiniParameterHomotopy, PRECISION]
  Headline
    optional input 
///;

doc ///
  Key
    [bertiniParameterHomotopy, RANDOMSEED]
  Headline
    optional input 
///;

doc ///
  Key
    [bertiniParameterHomotopy, REGENSTARTLEVEL]
  Headline
    optional input 
///;

doc ///
  Key
    [bertiniParameterHomotopy, SCREENOUT]
  Headline
    optional input 
///;

doc ///
  Key
    [bertiniParameterHomotopy, SECURITYLEVEL]
  Headline
    optional input 
///;

doc ///
  Key
    [bertiniParameterHomotopy, SINGVALZEROTOL]
  Headline
    optional input 
///;

doc ///
  Key
    [bertiniParameterHomotopy, STEPSFORINCREASE]
  Headline
    optional input 
///;


doc ///
  Key
    [bertiniParameterHomotopy, STEPSFORINCREASE]
  Headline
    optional input 
///;

doc ///
  Key
    [bertiniParameterHomotopy, TRACKTOLBEFOREEG]
  Headline
    optional input 
///;

doc ///
  Key
    [bertiniParameterHomotopy, TRACKTOLDURINGEG]
  Headline
    optional input 
///;

doc ///
  Key
    [bertiniParameterHomotopy, USEREGENERATION]
  Headline
    optional input 
///;

doc ///
  Key
    [bertiniRefineSols, COEFFBOUND]
  Headline
    optional input 
///;

doc ///
  Key
    [bertiniRefineSols, CONDNUMTHRESHOLD]
  Headline
    optional input 
///;

doc ///
  Key
    [bertiniRefineSols, DEGREEBOUND]
  Headline
    optional input 
///;

doc ///
  Key
    [bertiniRefineSols, ENDGAMENUM]
  Headline
    optional input 
///;

doc ///
  Key
    [bertiniRefineSols, FINALTOL]
  Headline
    optional input 
///;

doc ///
  Key
    [bertiniRefineSols, IMAGTHRESHOLD]
  Headline
    optional input 
///;

doc ///
  Key
    [bertiniRefineSols, MAXCYCLENUM]
  Headline
    optional input 
///;

doc ///
  Key
    [bertiniRefineSols, MAXNEWTONITS]
  Headline
    optional input 
///;

doc ///
  Key
    [bertiniRefineSols, MAXNORM]
  Headline
    optional input 
///;

doc ///
  Key
    [bertiniRefineSols, MAXNUMBERSTEPS]
  Headline
    optional input 
///;

doc ///
  Key
    [bertiniRefineSols, MAXSTEPSIZE]
  Headline
    optional input 
///;

doc ///
  Key
    [bertiniRefineSols, MINSTEPSIZEBEFOREEG]
  Headline
    optional input 
///;

doc ///
  Key
    [bertiniRefineSols, MINSTEPSIZEDURINGEG]
  Headline
    optional input 
///;

doc ///
  Key
    [bertiniRefineSols, MPTYPE]
  Headline
    optional input 
///;

doc ///
  Key
    [bertiniRefineSols, ODEPREDICTOR]
  Headline
    optional input 
///;

doc ///
  Key
    [bertiniRefineSols, OUTPUTLEVEL]
  Headline
    optional input 
///;

doc ///
  Key
    [bertiniRefineSols, PRECISION]
  Headline
    optional input 
///;

doc ///
  Key
    [bertiniRefineSols, RANDOMSEED]
  Headline
    optional input 
///;

doc ///
  Key
    [bertiniRefineSols, REGENSTARTLEVEL]
  Headline
    optional input 
///;

doc ///
  Key
    [bertiniRefineSols, SCREENOUT]
  Headline
    optional input 
///;

doc ///
  Key
    [bertiniRefineSols, SECURITYLEVEL]
  Headline
    optional input 
///;

doc ///
  Key
    [bertiniRefineSols, SINGVALZEROTOL]
  Headline
    optional input 
///;

doc ///
  Key
    [bertiniRefineSols, STEPSFORINCREASE]
  Headline
    optional input 
///;


doc ///
  Key
    [bertiniRefineSols, STEPSFORINCREASE]
  Headline
    optional input 
///;

doc ///
  Key
    [bertiniRefineSols, TRACKTOLBEFOREEG]
  Headline
    optional input 
///;

doc ///
  Key
    [bertiniRefineSols, TRACKTOLDURINGEG]
  Headline
    optional input 
///;

doc ///
  Key
    [bertiniRefineSols, USEREGENERATION]
  Headline
    optional input 
///;

doc ///
  Key
    [bertiniSample, COEFFBOUND]
  Headline
    optional input 
///;

doc ///
  Key
    [bertiniSample, CONDNUMTHRESHOLD]
  Headline
    optional input 
///;

doc ///
  Key
    [bertiniSample, DEGREEBOUND]
  Headline
    optional input 
///;

doc ///
  Key
    [bertiniSample, ENDGAMENUM]
  Headline
    optional input 
///;

doc ///
  Key
    [bertiniSample, FINALTOL]
  Headline
    optional input 
///;

doc ///
  Key
    [bertiniSample, IMAGTHRESHOLD]
  Headline
    optional input 
///;

doc ///
  Key
    [bertiniSample, MAXCYCLENUM]
  Headline
    optional input 
///;

doc ///
  Key
    [bertiniSample, MAXNEWTONITS]
  Headline
    optional input 
///;

doc ///
  Key
    [bertiniSample, MAXNORM]
  Headline
    optional input 
///;

doc ///
  Key
    [bertiniSample, MAXNUMBERSTEPS]
  Headline
    optional input 
///;

doc ///
  Key
    [bertiniSample, MAXSTEPSIZE]
  Headline
    optional input 
///;

doc ///
  Key
    [bertiniSample, MINSTEPSIZEBEFOREEG]
  Headline
    optional input 
///;

doc ///
  Key
    [bertiniSample, MINSTEPSIZEDURINGEG]
  Headline
    optional input 
///;

doc ///
  Key
    [bertiniSample, MPTYPE]
  Headline
    optional input 
///;

doc ///
  Key
    [bertiniSample, ODEPREDICTOR]
  Headline
    optional input 
///;

doc ///
  Key
    [bertiniSample, OUTPUTLEVEL]
  Headline
    optional input 
///;

doc ///
  Key
    [bertiniSample, PRECISION]
  Headline
    optional input 
///;

doc ///
  Key
    [bertiniSample, RANDOMSEED]
  Headline
    optional input 
///;

doc ///
  Key
    [bertiniSample, REGENSTARTLEVEL]
  Headline
    optional input 
///;

doc ///
  Key
    [bertiniSample, SCREENOUT]
  Headline
    optional input 
///;

doc ///
  Key
    [bertiniSample, SECURITYLEVEL]
  Headline
    optional input 
///;

doc ///
  Key
    [bertiniSample, SINGVALZEROTOL]
  Headline
    optional input 
///;

doc ///
  Key
    [bertiniSample, STEPSFORINCREASE]
  Headline
    optional input 
///;


doc ///
  Key
    [bertiniSample, STEPSFORINCREASE]
  Headline
    optional input 
///;

doc ///
  Key
    [bertiniSample, TRACKTOLBEFOREEG]
  Headline
    optional input 
///;

doc ///
  Key
    [bertiniSample, TRACKTOLDURINGEG]
  Headline
    optional input 
///;

doc ///
  Key
    [bertiniSample, USEREGENERATION]
  Headline
    optional input 
///;

doc ///
  Key
    [bertiniTrackHomotopy, COEFFBOUND]
  Headline
    optional input 
///;

doc ///
  Key
    [bertiniTrackHomotopy, CONDNUMTHRESHOLD]
  Headline
    optional input 
///;

doc ///
  Key
    [bertiniTrackHomotopy, DEGREEBOUND]
  Headline
    optional input 
///;

doc ///
  Key
    [bertiniTrackHomotopy, ENDGAMENUM]
  Headline
    optional input 
///;

doc ///
  Key
    [bertiniTrackHomotopy, FINALTOL]
  Headline
    optional input 
///;

doc ///
  Key
    [bertiniTrackHomotopy, IMAGTHRESHOLD]
  Headline
    optional input 
///;

doc ///
  Key
    [bertiniTrackHomotopy, MAXCYCLENUM]
  Headline
    optional input 
///;

doc ///
  Key
    [bertiniTrackHomotopy, MAXNEWTONITS]
  Headline
    optional input 
///;

doc ///
  Key
    [bertiniTrackHomotopy, MAXNORM]
  Headline
    optional input 
///;

doc ///
  Key
    [bertiniTrackHomotopy, MAXNUMBERSTEPS]
  Headline
    optional input 
///;

doc ///
  Key
    [bertiniTrackHomotopy, MAXSTEPSIZE]
  Headline
    optional input 
///;

doc ///
  Key
    [bertiniTrackHomotopy, MINSTEPSIZEBEFOREEG]
  Headline
    optional input 
///;

doc ///
  Key
    [bertiniTrackHomotopy, MINSTEPSIZEDURINGEG]
  Headline
    optional input 
///;

doc ///
  Key
    [bertiniTrackHomotopy, MPTYPE]
  Headline
    optional input 
///;

doc ///
  Key
    [bertiniTrackHomotopy, ODEPREDICTOR]
  Headline
    optional input 
///;

doc ///
  Key
    [bertiniTrackHomotopy, OUTPUTLEVEL]
  Headline
    optional input 
///;

doc ///
  Key
    [bertiniTrackHomotopy, PRECISION]
  Headline
    optional input 
///;

doc ///
  Key
    [bertiniTrackHomotopy, RANDOMSEED]
  Headline
    optional input 
///;

doc ///
  Key
    [bertiniTrackHomotopy, REGENSTARTLEVEL]
  Headline
    optional input 
///;

doc ///
  Key
    [bertiniTrackHomotopy, SCREENOUT]
  Headline
    optional input 
///;

doc ///
  Key
    [bertiniTrackHomotopy, SECURITYLEVEL]
  Headline
    optional input 
///;

doc ///
  Key
    [bertiniTrackHomotopy, SINGVALZEROTOL]
  Headline
    optional input 
///;

doc ///
  Key
    [bertiniTrackHomotopy, STEPSFORINCREASE]
  Headline
    optional input 
///;


doc ///
  Key
    [bertiniTrackHomotopy, STEPSFORINCREASE]
  Headline
    optional input 
///;

doc ///
  Key
    [bertiniTrackHomotopy, TRACKTOLBEFOREEG]
  Headline
    optional input 
///;

doc ///
  Key
    [bertiniTrackHomotopy, TRACKTOLDURINGEG]
  Headline
    optional input 
///;

doc ///
  Key
    [bertiniTrackHomotopy, USEREGENERATION]
  Headline
    optional input 
///;

end
doc ///
 Key
   bertiniTrackHomotopy
   (bertiniTrackHomotopy,List,RingElement,List)
 Headline
   runs user-defined homotopy in Bertini
 Usage
   S0 = bertiniTrackHomotopy(H,t,sols)
 Inputs
   H:List
     whose entries are polynomials depending on t (must be square)
   t:RingElement
     path variable
   S1:List
     start solutions (solutions of H when t=1)
 Outputs
   S0:List
     target solutions (solutions of H when t=0)
 Description
   Text
     Runs Bertini's user-defined homotopy.
   Example
///;

doc ///
 Key
   bertiniSegmentHomotopy
   (bertiniSegmentHomotopy,List, List, List)
 Headline
   constructs and tracks a straight-line homotopy
 Usage
   S0 = bertiniSegmentHomotopy()
 Inputs
   start:List
     start system, list of polynomial equations
   tar:List
     target system, list of polynomial equations
   S1:List
     start solutions (solutions of start system when t=1)
 Outputs
   S0:List
     target solutions (solutions of target system when t=0)
 Description
   Text
     Constructs and tracks a straight line homotopy from the start system to the target system.
   Example
     R=CC[x,y]
     start={x^3-1, y^3-1}
     tar={x^3+y^3-4,x^3+2*y-1}
     --input start system, target system, list of start points
     bertiniSegmentHomotopy(
     start,tar,{{1,1},{-.5-0.86603*ii,1},{1,-0.5+0.86603*ii}})
///;

