doc ///
  Key
    Bertini
  Headline
    software for numerical algebraic geometry
  Description
    Text
      Interfaces the functionality of the software {\tt Bertini}
      to solve polynomial systems and perform calculations in
      {\em numerical algebraic geometry}.  The software is available at
      @HREF"http://www.nd.edu/~sommese/bertini/"@.
      The site currently provides only executable versions named {\tt bertini} or {\tt bertini.exe} (for Cygwin).
      The user must have the executable program {\tt phc} available,
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
     this input file.  Solutions are pulled from machine readable file finitesolutions
     and returned as a list.
   Example
     R = CC[x,y]
     F = {x^2-1,y^2-1}
     S = bertiniZeroDimSolve F
///;


doc ///
  Key
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
     whose entries are polynomials (system need not be square) 
 Outputs
   V:NumericalVariety 
     a numerical description of irreducible components of the variety defined by F
 Description
   Text
     Finds solutions to the positive-dimensional system F via numerical polynomial homotopy continuation.
     This function builds a Bertini input file from the system F and calls Bertini on 
     this input file.  
   Example
     R = CC[x,y,z]
     F = {(y^2+x^2+z^2-1)*x,
	  (y^2+x^2+z^2-1)*y}
     S = bertiniPosDimSolve F
///;


-- bertiniSample(
  --   {f1},dimen=>1,compnum=>1,numpts=>12,WitnessData=>wdf)


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
 Description
   Text
     Samples points from an irreducible component of a variety using Bertini.   
   Example
     R = CC[x,y,z]
     F = {(y^2+x^2+z^2-1)*x,
	  (y^2+x^2+z^2-1)*y}
     NV = bertiniPosDimSolve(F)
     W = NV#1_0 --z-axis
     bertiniSample(W,12)
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
     F = {(y^2+x^2+z^2-1)*x,
	  (y^2+x^2+z^2-1)*y}
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
     bPH=bertiniParameterHomotopy(
     {f1,f2},--list of polynomials that are a square system
     {u1,u2,u3},--your parameters
     {finalParameters0  ,finalParameters1 })
     bPH_0--the solutions to the system with finalParameters0
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
