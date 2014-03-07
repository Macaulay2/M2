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

      The user may place the executable program {\tt bertini} in the executation path. 
      Alternatively, the path to the executable needs to be specified, for instance,
    Example
      needsPackage("Bertini", Configuration=>{"BERTINIexecutable"=>"/folder/subfolder/bertini"})  
    Text
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
     R = CC[x,y];
     F = {x^2-1,y^2-1};
     S = bertiniZeroDimSolve F
   Text
     Each solution is of type @TO Point@.  Additional information about the solution can be accessed by using @TO peek@. 
   Example
     peek S_0  
///;

doc ///
  Key
    "Bertini options"
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
    [bertiniZeroDimSolve, MPTYPE]
    [bertiniZeroDimSolve, PRECISION]
    [bertiniZeroDimSolve, ODEPREDICTOR]
    [bertiniZeroDimSolve, TRACKTOLBEFOREEG]
    [bertiniZeroDimSolve, TRACKTOLDURINGEG]
    [bertiniZeroDimSolve, FINALTOL]
    [bertiniZeroDimSolve, MAXNORM]
    [bertiniZeroDimSolve, MINSTEPSIZEBEFOREEG]
    [bertiniZeroDimSolve, MINSTEPSIZEDURINGEG]
    [bertiniZeroDimSolve, IMAGTHRESHOLD]
    [bertiniZeroDimSolve, COEFFBOUND]
    [bertiniZeroDimSolve, DEGREEBOUND]
    [bertiniZeroDimSolve, CONDNUMTHRESHOLD]
    [bertiniZeroDimSolve, RANDOMSEED]
    [bertiniZeroDimSolve, SINGVALZEROTOL]
    [bertiniZeroDimSolve, ENDGAMENUM]
    [bertiniZeroDimSolve, USEREGENERATION]
    [bertiniZeroDimSolve, SECURITYLEVEL]
    [bertiniZeroDimSolve, SCREENOUT]
    [bertiniZeroDimSolve, OUTPUTLEVEL]
    [bertiniZeroDimSolve, STEPSFORINCREASE]
    [bertiniZeroDimSolve, MAXNEWTONITS]
    [bertiniZeroDimSolve, MAXSTEPSIZE]
    [bertiniZeroDimSolve, MAXNUMBERSTEPS]
    [bertiniZeroDimSolve, MAXCYCLENUM]
    [bertiniZeroDimSolve, REGENSTARTLEVEL]
    [bertiniComponentMemberTest, MPTYPE]
    [bertiniComponentMemberTest, PRECISION]
    [bertiniComponentMemberTest, ODEPREDICTOR]
    [bertiniComponentMemberTest, TRACKTOLBEFOREEG]
    [bertiniComponentMemberTest, TRACKTOLDURINGEG]
    [bertiniComponentMemberTest, FINALTOL]
    [bertiniComponentMemberTest, MAXNORM]
    [bertiniComponentMemberTest, MINSTEPSIZEBEFOREEG]
    [bertiniComponentMemberTest, MINSTEPSIZEDURINGEG]
    [bertiniComponentMemberTest, IMAGTHRESHOLD]
    [bertiniComponentMemberTest, COEFFBOUND]
    [bertiniComponentMemberTest, DEGREEBOUND]
    [bertiniComponentMemberTest, CONDNUMTHRESHOLD]
    [bertiniComponentMemberTest, RANDOMSEED]
    [bertiniComponentMemberTest, SINGVALZEROTOL]
    [bertiniComponentMemberTest, ENDGAMENUM]
    [bertiniComponentMemberTest, USEREGENERATION]
    [bertiniComponentMemberTest, SECURITYLEVEL]
    [bertiniComponentMemberTest, SCREENOUT]
    [bertiniComponentMemberTest, OUTPUTLEVEL]
    [bertiniComponentMemberTest, STEPSFORINCREASE]
    [bertiniComponentMemberTest, MAXNEWTONITS]
    [bertiniComponentMemberTest, MAXSTEPSIZE]
    [bertiniComponentMemberTest, MAXNUMBERSTEPS]
    [bertiniComponentMemberTest, MAXCYCLENUM]
    [bertiniComponentMemberTest, REGENSTARTLEVEL]
    [bertiniParameterHomotopy, MPTYPE]
    [bertiniParameterHomotopy, PRECISION]
    [bertiniParameterHomotopy, ODEPREDICTOR]
    [bertiniParameterHomotopy, TRACKTOLBEFOREEG]
    [bertiniParameterHomotopy, TRACKTOLDURINGEG]
    [bertiniParameterHomotopy, FINALTOL]
    [bertiniParameterHomotopy, MAXNORM]
    [bertiniParameterHomotopy, MINSTEPSIZEBEFOREEG]
    [bertiniParameterHomotopy, MINSTEPSIZEDURINGEG]
    [bertiniParameterHomotopy, IMAGTHRESHOLD]
    [bertiniParameterHomotopy, COEFFBOUND]
    [bertiniParameterHomotopy, DEGREEBOUND]
    [bertiniParameterHomotopy, CONDNUMTHRESHOLD]
    [bertiniParameterHomotopy, RANDOMSEED]
    [bertiniParameterHomotopy, SINGVALZEROTOL]
    [bertiniParameterHomotopy, ENDGAMENUM]
    [bertiniParameterHomotopy, USEREGENERATION]
    [bertiniParameterHomotopy, SECURITYLEVEL]
    [bertiniParameterHomotopy, SCREENOUT]
    [bertiniParameterHomotopy, OUTPUTLEVEL]
    [bertiniParameterHomotopy, STEPSFORINCREASE]
    [bertiniParameterHomotopy, MAXNEWTONITS]
    [bertiniParameterHomotopy, MAXSTEPSIZE]
    [bertiniParameterHomotopy, MAXNUMBERSTEPS]
    [bertiniParameterHomotopy, MAXCYCLENUM]
    [bertiniParameterHomotopy, REGENSTARTLEVEL]
    [bertiniPosDimSolve, MPTYPE]
    [bertiniPosDimSolve, PRECISION]
    [bertiniPosDimSolve, ODEPREDICTOR]
    [bertiniPosDimSolve, TRACKTOLBEFOREEG]
    [bertiniPosDimSolve, TRACKTOLDURINGEG]
    [bertiniPosDimSolve, FINALTOL]
    [bertiniPosDimSolve, MAXNORM]
    [bertiniPosDimSolve, MINSTEPSIZEBEFOREEG]
    [bertiniPosDimSolve, MINSTEPSIZEDURINGEG]
    [bertiniPosDimSolve, IMAGTHRESHOLD]
    [bertiniPosDimSolve, COEFFBOUND]
    [bertiniPosDimSolve, DEGREEBOUND]
    [bertiniPosDimSolve, CONDNUMTHRESHOLD]
    [bertiniPosDimSolve, RANDOMSEED]
    [bertiniPosDimSolve, SINGVALZEROTOL]
    [bertiniPosDimSolve, ENDGAMENUM]
    [bertiniPosDimSolve, USEREGENERATION]
    [bertiniPosDimSolve, SECURITYLEVEL]
    [bertiniPosDimSolve, SCREENOUT]
    [bertiniPosDimSolve, OUTPUTLEVEL]
    [bertiniPosDimSolve, STEPSFORINCREASE]
    [bertiniPosDimSolve, MAXNEWTONITS]
    [bertiniPosDimSolve, MAXSTEPSIZE]
    [bertiniPosDimSolve, MAXNUMBERSTEPS]
    [bertiniPosDimSolve, MAXCYCLENUM]
    [bertiniPosDimSolve, REGENSTARTLEVEL]
    [bertiniRefineSols, MPTYPE]
    [bertiniRefineSols, PRECISION]
    [bertiniRefineSols, ODEPREDICTOR]
    [bertiniRefineSols, TRACKTOLBEFOREEG]
    [bertiniRefineSols, TRACKTOLDURINGEG]
    [bertiniRefineSols, FINALTOL]
    [bertiniRefineSols, MAXNORM]
    [bertiniRefineSols, MINSTEPSIZEBEFOREEG]
    [bertiniRefineSols, MINSTEPSIZEDURINGEG]
    [bertiniRefineSols, IMAGTHRESHOLD]
    [bertiniRefineSols, COEFFBOUND]
    [bertiniRefineSols, DEGREEBOUND]
    [bertiniRefineSols, CONDNUMTHRESHOLD]
    [bertiniRefineSols, RANDOMSEED]
    [bertiniRefineSols, SINGVALZEROTOL]
    [bertiniRefineSols, ENDGAMENUM]
    [bertiniRefineSols, USEREGENERATION]
    [bertiniRefineSols, SECURITYLEVEL]
    [bertiniRefineSols, SCREENOUT]
    [bertiniRefineSols, OUTPUTLEVEL]
    [bertiniRefineSols, STEPSFORINCREASE]
    [bertiniRefineSols, MAXNEWTONITS]
    [bertiniRefineSols, MAXSTEPSIZE]
    [bertiniRefineSols, MAXNUMBERSTEPS]
    [bertiniRefineSols, MAXCYCLENUM]
    [bertiniRefineSols, REGENSTARTLEVEL]
    [bertiniSample, MPTYPE]
    [bertiniSample, PRECISION]
    [bertiniSample, ODEPREDICTOR]
    [bertiniSample, TRACKTOLBEFOREEG]
    [bertiniSample, TRACKTOLDURINGEG]
    [bertiniSample, FINALTOL]
    [bertiniSample, MAXNORM]
    [bertiniSample, MINSTEPSIZEBEFOREEG]
    [bertiniSample, MINSTEPSIZEDURINGEG]
    [bertiniSample, IMAGTHRESHOLD]
    [bertiniSample, COEFFBOUND]
    [bertiniSample, DEGREEBOUND]
    [bertiniSample, CONDNUMTHRESHOLD]
    [bertiniSample, RANDOMSEED]
    [bertiniSample, SINGVALZEROTOL]
    [bertiniSample, ENDGAMENUM]
    [bertiniSample, USEREGENERATION]
    [bertiniSample, SECURITYLEVEL]
    [bertiniSample, SCREENOUT]
    [bertiniSample, OUTPUTLEVEL]
    [bertiniSample, STEPSFORINCREASE]
    [bertiniSample, MAXNEWTONITS]
    [bertiniSample, MAXSTEPSIZE]
    [bertiniSample, MAXNUMBERSTEPS]
    [bertiniSample, MAXCYCLENUM]
    [bertiniSample, REGENSTARTLEVEL]
    [bertiniTrackHomotopy, MPTYPE]
    [bertiniTrackHomotopy, PRECISION]
    [bertiniTrackHomotopy, ODEPREDICTOR]
    [bertiniTrackHomotopy, TRACKTOLBEFOREEG]
    [bertiniTrackHomotopy, TRACKTOLDURINGEG]
    [bertiniTrackHomotopy, FINALTOL]
    [bertiniTrackHomotopy, MAXNORM]
    [bertiniTrackHomotopy, MINSTEPSIZEBEFOREEG]
    [bertiniTrackHomotopy, MINSTEPSIZEDURINGEG]
    [bertiniTrackHomotopy, IMAGTHRESHOLD]
    [bertiniTrackHomotopy, COEFFBOUND]
    [bertiniTrackHomotopy, DEGREEBOUND]
    [bertiniTrackHomotopy, CONDNUMTHRESHOLD]
    [bertiniTrackHomotopy, RANDOMSEED]
    [bertiniTrackHomotopy, SINGVALZEROTOL]
    [bertiniTrackHomotopy, ENDGAMENUM]
    [bertiniTrackHomotopy, USEREGENERATION]
    [bertiniTrackHomotopy, SECURITYLEVEL]
    [bertiniTrackHomotopy, SCREENOUT]
    [bertiniTrackHomotopy, OUTPUTLEVEL]
    [bertiniTrackHomotopy, STEPSFORINCREASE]
    [bertiniTrackHomotopy, MAXNEWTONITS]
    [bertiniTrackHomotopy, MAXSTEPSIZE]
    [bertiniTrackHomotopy, MAXNUMBERSTEPS]
    [bertiniTrackHomotopy, MAXCYCLENUM]
    [bertiniTrackHomotopy, REGENSTARTLEVEL]
  Headline
    options for methods of Bertini package
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
     sol1 = point {{1}};
     sol2 = point {{-1}};
     S1= { sol1, sol2  };--solutions to H when t=1	  
     S0 = bertiniTrackHomotopy (H, t, S1) --solutions to H when t=0
     peek S0_0
   Example     
     R=CC[x,y,t]; -- include the path variable in the ring     
     f1=(x^2-y^2);
     f2=(2*x^2-3*x*y+5*y^2);
     H = { f1*t + f2*(1-t)}; --H is a list of polynomials in x,y,t
     sol1=    point{{1,1}}--{{x,y}} coordinates
     sol2=    point{{ -1,1}}
     S1={sol1,sol2}--solutions to H when t=1
     S0=bertiniTrackHomotopy(  H,t,S1,ISPROJECTIVE=>1) --solutions to H when t=0       

///;

doc ///
 Key
   bertiniComponentMemberTest
   (bertiniComponentMemberTest, NumericalVariety, List)
 Headline
   test whether points lie on a given variety
 Usage
   L = bertiniComponentMemberTest (NV, pts)
 Inputs
   NV:NumericalVariety
   pts:List
     points to test
 Outputs
   L:List
     entries are lists of witness sets containing the test point
 Consequences
  Item
    Writes the witness set information of NV and the test points to temporary files
  Item
    Invokes {\tt Bertini}'s solver with option {\tt TRACKTYPE: 3} 
  Item
    Stores output of {\tt Bertini} in temporary file
  Item
    Parses and outputs the solutions    
 Description
   Text
     This method checks whether the test points pts lie on NV using {\tt Bertini}.  
   Example
     R = CC[x,y,z];
     F = {(y^2+x^2+z^2-1)*x,(y^2+x^2+z^2-1)*y};
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
     This method takes the list l of solutions of F and sharpens them to d digits using the sharpening module of {\tt Bertini}.
   Example
     R = CC[x,y];
     F = {x^2-2,y^2-2};
     sols = bertiniZeroDimSolve (F)
     S = bertiniRefineSols (F,sols,100)
     coords = coordinates S_0
     coords_0
///;

doc ///
 Key
   importPoints   
   (importPoints,String,ZZ)
 Headline
   importPoints reads solutions from a Bertini solution file to store as points in M2
 Usage
   A=importPoints(s,n) 
 Inputs
   s: String
     A string giving the  locaton of a Bertini solution file.
   n: ZZ
     Number of coordinates for each solution.
 Description
   Text
     The input is a string giving the location of the solution file,
     and an integer giving the number of coordinates for a solution.
     The output is a list of points.
     The user can specify which solutions and which coordinates they want to read from the file.
   Example
     locationOfSolutionFile="/Users/.../YourFolder/solution_file";
     A=importPoints(locationOfSolutionFile,4)
     --The output would be a list of points that have 4 coordinates.          
   Example 
     locationOfSolutionFile="/Users/.../YourFolder/solution_file";
     B=importPoints(locationOfSolutionFile,4,specifyPoints=>{0,2})
     --The output would be the first and third solutions of the file. 
   Example 
     locationOfSolutionFile="/Users/.../YourFolder/solution_file";
     C=importPoints(locationOfSolutionFile,4,specifyCoordinates=>{0,1})
     --The output would be the first and second coordinate of each solution of the file.  
 Caveat
   For importPoints to be successful, the Bertini solution file must have a particular format.

   The first line is an integer, the number of solutions in the.
   The next lines consist of a blank line followed by a line for each coordinate;
   these lines consist of: RR|"e"|ZZ" "RR|"e"|ZZ for scientific notation of the real and imaginary parts of the coordinate.
///;

doc ///
 Key
   phPostProcess
   (phPostProcess,String,String,List,ZZ)
 Headline
   Does post processing parameter homotopy.
 Usage
   S=phPostProcess(sIn,sOut,L,n) 
 Inputs
   sIn: String
     A string giving the dicterory of the input files.
   sOut: String
     A string giving the directory where Bertini will output files
   L: List
     A list of parameters. 
   n: ZZ
     Number of coordinates in a solution.
 Description
   Text
     The purpose of this function is to allow a person
     to share their Bertini computations with a second user,
     who can then analyze the data easily with the Bertini.M2 interface.   
     
     Instead of doing a parameter run by calling Bertini, 
     the printNotes option prints a file titled "notes"  located in the input file's directory.
     If the "notes" file does not exist it returns an error.   
   Example
     inputFileLocation="/Users/.../YourFolderA";
     outputFileLocation="/Users/.../YourFolderB";
     L={.8234+ii*8,9}--A list of two parameter values.
     n=3--A solution has n coordinates.
     phPostProcess(inputFileLocation,outputFileLocation,L,n)     --The output will be a list of points that have 3 coordinates, that are solutions to a parameterized system of equations evaluated at L, found by doing a parameter homotopy. 
   Example
     inputFileLocation="/Users/.../YourFolderA";
     phPostProcess(inputFileLocation,"",{},0,printNotes=>1)
 Caveat
   Even if Bertini is called but does not run,  
   an error may not be reported if previous solution files were already in the outputDirectory.
///;


doc ///
 Key
   ISPROJECTIVE
   [bertiniTrackHomotopy, ISPROJECTIVE]
   [bertiniParameterHomotopy, ISPROJECTIVE]
   [bertiniComponentMemberTest, ISPROJECTIVE]
   [bertiniPosDimSolve, ISPROJECTIVE]
   [bertiniRefineSols, ISPROJECTIVE]
   [bertiniSample, ISPROJECTIVE]
   [bertiniZeroDimSolve, ISPROJECTIVE]
 Headline
   optional argument to specify whether to use homogeneous coordinates
 Description
   Text
     When set to 1, this option indicates that the input system is homogenized and
     the output should be given in projective space.
   Example
     R = CC[x,y,z];
     f = {(x^2+y^2-z^2)*(z-x),(x^2+y^2-z^2)*(z+y)};
     NV = bertiniPosDimSolve(f,ISPROJECTIVE=>1)
   Example 
     R=CC[x,y,z,u1,u2];--u1,u2 are parameters
     f1=x^2+y^2-z^2;
     f2=u1*x+u2*y;
     finalParameters={{0,1}};
     bPH=bertiniParameterHomotopy( {f1,f2}, {u1,u2},{finalParameters },ISPROJECTIVE=>1)            
///;

doc ///
 Key
   bertiniParameterHomotopy
   (bertiniParameterHomotopy,List,List,List)
 Headline
   run parameter homotopy in Bertini
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
 Consequences
  Item
    Writes systems to temporary files
  Item
    Invokes {\tt Bertini}'s solver with configuration keyword {\tt PARAMETERHOMOTOPY}.
    First with {\tt PARAMETERHOMOTOPY} set to 1, then with {\tt PARAMETERHOMOTOPY} set to
    2 for each set of parameter values.
  Item
    Stores output of {\tt Bertini} in temporary files
  Item
    Parses and outputs the solutions    
 Description
   Text
     This method numerically solves several polynomial systems from
     a parameterized family at once.  The list F is a system of polynomials
     in ring variables and the parameters listed in P.  The list T is the
     set of parameter values for which solutions to F are desired.  Both stages of
     {\tt Bertini}'s parameter homotopy method are called with {\tt bertiniParameterHomotopy}. 
     First, {\tt Bertini} assigns a random complex number to each parameter
     and solves the resulting system, then, after this initial phase, {\tt Bertini} computes solutions
     for every given choice of parameters using a number of paths equal to the exact root count in the
     first stage.
   Example
     R=CC[u1,u2,u3,x,y];
     f1=u1*(y-1)+u2*(y-2)+u3*(y-3); --parameters are u1, u2, and u3
     f2=(x-11)*(x-12)*(x-13)-u1;
     paramValues0={{1,0,0}};
     paramValues1={{0,1+2*ii,0}};
     bPH=bertiniParameterHomotopy( {f1,f2}, {u1,u2,u3},{paramValues0 ,paramValues1 })
     bPH_0--the solutions to the system with parameters set equal to paramValues0
   Example
     R=CC[x,y,z,u1,u2]
     f1=x^2+y^2-z^2
     f2=u1*x+u2*y
     finalParameters0={{0,1}}
     finalParameters1={{1,0}}
     bPH=bertiniParameterHomotopy( {f1,f2}, {u1,u2},{finalParameters0 ,finalParameters1 },ISPROJECTIVE=>1)            
     bPH_0--The two solutions for finalParameters0
///;

doc///
 Key
   allowStrings
   [bertiniTrackHomotopy, allowStrings]
   [bertiniParameterHomotopy, allowStrings]
--   [bertiniComponentMemberTest, ISPROJECTIVE]
   [bertiniPosDimSolve, allowStrings]
--   [bertiniRefineSols, ISPROJECTIVE]
--   [bertiniSample, ISPROJECTIVE]
   [bertiniZeroDimSolve, allowStrings]
 Headline
   optional argument to specify whether one can input a system of polynomials as strings.   
 Description
   Text
     When set to a list of variables one can input a polynomial system as a List of strings or polynomials 
   Example
     R = CC[x,y,z];
     f = {"(x^2+y^2-z^2)*(z-x)",toString (hold (x^2+y^2-z^2)*(z+y)),z-1};
     sols = bertiniZeroDimSolve(f,allowStrings=>{x,y,z})
   Example 
     R=CC[x,y,z];--u1,u2 are parameters
     f1=x^2+y^2-z^2;
     f2="u1*x+u2*y";
     f3=z-1;
     finalParameters={{0,1}};
     bPH=bertiniParameterHomotopy( {f1,f2,f3}, {u1,u2},{finalParameters },allowStrings=>{x,y,z})            
   Example 
     R=CC[x,t1];
     f1="x^2+cos(1-t1)-2*exp(1-t1)";
     H={f1};
     sol1 = point {{1}};
     sol2 = point {{-1}};
     S1={sol1,sol2}--solutions to H when t=1                 
     S0 = bertiniTrackHomotopy (H, t1, S1,allowStrings=>{x}) --solutions to H when t=0|
     peek S0
///;

doc///
 Key
   subFunctions
   [bertiniTrackHomotopy, subFunctions]
   [bertiniParameterHomotopy, subFunctions]
--   [bertiniComponentMemberTest, ISPROJECTIVE]
   [bertiniPosDimSolve, subFunctions]
--   [bertiniRefineSols, ISPROJECTIVE]
--   [bertiniSample, ISPROJECTIVE]
   [bertiniZeroDimSolve, subFunctions]
 Headline
   optional argument to specify subfunctions that will be written to the Bertini input file.   
 Description
   Text
     The option is a list of pairs that define a subfunction. 
   Example
     R = CC[x,y,z][s1,s2];
     sF={ {s1,(x^2+y^2-z^2)},{s2,z-x}}--s1=x^2+y^2-z^2; s2=z-x;
     f = {s1*s2, s1*(z+y),z-1};
     sols = bertiniZeroDimSolve(f,allowStrings=>{x,y,z},subFunctions=>sF)
///;

end


-- to be added in another version
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

