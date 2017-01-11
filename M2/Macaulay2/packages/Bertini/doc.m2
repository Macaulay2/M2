-----------------------
-------MAIN------------
-----------------------

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
      @HREF"http://bertini.nd.edu/"@. {\tt Bertini} is under ongoing development by
      D. Bates, J. Hauenstein, A. Sommese, and C. Wampler.

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

------------------------------------------------------
------FUNCTIONS BERTINI VERSION 1------------
------------------------------------------------------
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
 Caveat
   Variables must begin with a letter (lowercase or capital) and
   can only contain letters, numbers, underscores, and square brackets.
      
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
 Caveat
   Variables must begin with a letter (lowercase or capital) and
   can only contain letters, numbers, underscores, and square brackets.            
///;



doc ///
 Key
   bertiniSample
   (bertiniSample, ZZ, WitnessSet)
 Headline
   sample points from an irreducible component of a variety
 Usage
   V = bertiniSample (n, W)
 Inputs
   n:ZZ
     number of desired sample points
   W:WitnessSet
     irreducible
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
     bertiniSample(4, W)
///;

doc ///
 Key
   bertiniTrackHomotopy
   (bertiniTrackHomotopy, RingElement, List, List)
 Headline
   track a user-defined homotopy
 Usage
   S0=bertiniTrackHomotopy(t, H, S1)
 Inputs
   t:RingElement
     path variable
   H:List
     of polynomials that define the homotopy
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
     S0 = bertiniTrackHomotopy (t, H, S1) --solutions to H when t=0
     peek S0_0
   Example     
     R=CC[x,y,t]; -- include the path variable in the ring     
     f1=(x^2-y^2);
     f2=(2*x^2-3*x*y+5*y^2);
     H = { f1*t + f2*(1-t)}; --H is a list of polynomials in x,y,t
     sol1=    point{{1,1}}--{{x,y}} coordinates
     sol2=    point{{ -1,1}}
     S1={sol1,sol2}--solutions to H when t=1
     S0=bertiniTrackHomotopy(t, H, S1, IsProjective=>1) --solutions to H when t=0 
 Caveat
   Variables must begin with a letter (lowercase or capital) and
   can only contain letters, numbers, underscores, and square brackets.           
///;

doc ///
 Key
   bertiniComponentMemberTest
   (bertiniComponentMemberTest, List, NumericalVariety)
 Headline
   test whether points lie on a given variety
 Usage
   L = bertiniComponentMemberTest (pts, NV)
 Inputs
   pts:List
     points to test
   NV:NumericalVariety
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
     bertiniComponentMemberTest(pts, NV)
 Caveat
   In the current implementation, at most one witness set is listed for each test point although the point may lie on more than one component.
///;

doc ///
 Key
   bertiniRefineSols
   (bertiniRefineSols, ZZ, List, List)
 Headline
   sharpen solutions to a prescribed number of digits
 Usage
   S = bertiniRefineSols(d, F, l)
 Inputs
   d:ZZ
     number of digits
   F:List
     whose entries are polynomials (system need not be square)
   l:List
     whose entries are points to be sharpened   
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
     S = bertiniRefineSols (100, F, sols)
     coords = coordinates S_0
     coords_0
 Caveat
   @TO bertiniRefineSols@ will only refine non-singular solutions and does not
   currently work for homogeneous systems.  
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
     paramValues0={1,0,0};
     paramValues1={0,1+2*ii,0};
     bPH=bertiniParameterHomotopy( {f1,f2}, {u1,u2,u3},{paramValues0 ,paramValues1 })
     bPH_0--the solutions to the system with parameters set equal to paramValues0
   Example
     R=CC[x,y,z,u1,u2]
     f1=x^2+y^2-z^2
     f2=u1*x+u2*y
     finalParameters0={0,1}
     finalParameters1={1,0}
     bPH=bertiniParameterHomotopy( {f1,f2}, {u1,u2},{finalParameters0 ,finalParameters1 },HomVariableGroup=>{x,y,z})            
     bPH_0--The two solutions for finalParameters0
   Example
     finParamValues={{1},{2}}
     bPH1=bertiniParameterHomotopy( {"x^2-u1"}, {u1},finParamValues,AffVariableGroup=>{x})            
     bPH2=bertiniParameterHomotopy( {"x^2-u1"}, {u1},finParamValues,AffVariableGroup=>{x},OutputSyle=>"OutSolutions")            
     class bPH1_0_0
     class bPH2_0_0
   Example
     dir1 := temporaryFileName(); -- build a directory to store temporary data 
     makeDirectory dir1;  
     bPH5=bertiniParameterHomotopy( {"x^2-u1"}, {u1},{{1},{2}},AffVariableGroup=>{x},OutputSyle=>"OutNone",TopDirectory=>dir1)            
     B0=importSolutionsFile(dir1,NameSolutionsFile=>"ph_jade_0")     
     B1=importSolutionsFile(dir1,NameSolutionsFile=>"ph_jade_1")     
 Caveat
   Variables must begin with a letter (lowercase or capital) and
   can only contain letters, numbers, underscores, and square brackets.     
///;

------------------------------------------------------
------FUNCTIONS BERTINI VERSION 2------------
------------------------------------------------------

doc ///
 Key
   runBertini
   (runBertini,String)
 Headline
   Calls Bertini.
 Usage
   runBertini(s) 
 Inputs
   s:String
     The directory where Bertini will store files.
 Description
   Text
     To run bertini we need to say where we want to output the files, which is given by s. 
     Additional options include speciifying the location of the input file (the default is that the input file is located where we output the files)
     B'Exe is how we call Bertini. The default option is how Bertini is usually called in M2 in the init file. 
     InputFileName has its default as "input". 
   Example
     makeB'InputFile(storeBM2Files,
     	 AffVariableGroup=>{x,y},
	 B'Polynomials=>{"x^2-1","y^2-4"})
     runBertini(storeBM2Files)
     readFile(storeBM2Files)
   Example
     makeB'InputFile(storeBM2Files,
     	 AffVariableGroup=>{x,y},
	 B'Polynomials=>{"x^2-1","y^2-4"})
     runBertini(storeBM2Files,StorageFolder=>"StoreMyFiles")
     readFile(storeBM2Files|"/StoreMyFiles")           
///;


doc ///
 Key
   b'PHSequence
   (b'PHSequence,String,List)
 Headline
   b'PHSequence performs a sequence of parameter homotopies.
 Usage
   b'PHSequence(s,l) 
 Inputs
   s:String
     The directory where the files are stored.
   l:List
     A list of list of parameter values.
 Description
   Text
     The string s is a directory where the Bertini files are stored. 
     A Bertini input file should be stored in s.
     The Bertini input file should be named "input", or the NameB'InputFile should be set to a string giving the name of the input file. 
     A Bertini start (solutions) file should be stored in s. 
     The Bertini start (solutions) file should be named "start", or the NameStartFile should be set to a string giving the name of the start file. 
     A Bertini start parameter file should be stored in s. 
     The Bertini start parameter file should be named "start_parameters", or the NameParameterFile should be set to a string giving the name of the start parameter file. 
     b'PHSequence loops through the elements of l doing a sequence of parameter homotopies.
     Each element of l gives the parameter values  for a parameter homotopy.
     These parameter values are written to a "final_parameters" file in s or the StorageFolder. 
     Bertini is called (the option B'Exe allows one to change the way in which Bertini is called).
     The resulting nonsingular_solutions and final parameters become start solutions and start parameters for the next parameter homotopy in the sequence.  
     
   Example
     makeB'InputFile(storeBM2Files, ParameterGroup=>{t1,t2},AffVariableGroup=>{{x,y}},	 
	 B'Configs=>{{"PARAMETERHOMOTOPY",1}},
	 B'Polynomials=>{"x^2-t1","y-t2"})
     runBertini(storeBM2Files,PreparePH2=>true)
     b'PHSequence(storeBM2Files,{{1,1},{1,2}},SaveData=>true)
     importSolutionsFile(storeBM2Files,NameSolutionsFile=>"nonsingular_solutions1")
   Example
     makeB'InputFile(storeBM2Files, ParameterGroup=>{t1,t2},AffVariableGroup=>{{x,y}},	 
	 B'Configs=>{{"PARAMETERHOMOTOPY",1}},
	 B'Polynomials=>{"x^2-t1","y-t2"})
     runBertini(storeBM2Files,PreparePH2=>true)
     b'PHSequence(storeBM2Files,{{1,1},{1,2}},SaveData=>true,StorageFolder=>"StoreMyFiles")
     importSolutionsFile(storeBM2Files,StorageFolder=>"StoreMyFiles",NameSolutionsFile=>"nonsingular_solutions1")
///;



doc ///
 Key
   b'PHMonodromyCollect
   (b'PHMonodromyCollect,String)
 Headline
   Uses monodromy to find more solutions to a parameterized system of polynomial equations.
 Usage
   b'PHSequence(s) 
 Inputs
   s:String
     The directory where the files are stored.
 Description
   Text
     Given a directory that has a Bertini input file that has ParameterHomotopy set to 2, a start file, for start_parameters,
     this function uses parameter homotopies to perform a monodromy homotopy.           
   Example
     f="x^3+x*y^2+y+y^3+x-2";     h="a1*x+a2*y-1";
     --f defines a cubic surface. The intersection with the hyerplane h is 3 points. 
     --We consider f=h=0 as a parameterized system with parameters a1,a2 and unknowns x,y.
     --The parameters (a1,a2)=(1,0) has a solution (x,y)=(1,0).
     --we write a start file:
     writeStartFile(storeBM2Files,{{1,0}},NameStartFile=>"startSave");
     --write a start_parameter file. Note that you need to name the parameter file as "start_parameters" because the default is "final_parameters"
     writeParameterFile(storeBM2Files,{1,0},NameParameterFile=>"start_parameters");
     --Now we write our Bertini input file with PARAMETERHOMOTOPY set to 2. 
     makeB'InputFile(storeBM2Files, 
    	 B'Configs=>{{PARAMETERHOMOTOPY,2},{MPTYPE,2}},AffVariableGroup=>{x,y},ParameterGroup=>{a1,a2}, B'Polynomials=>{f,h}    )
     b'PHMonodromyCollect(storeBM2Files,
	 NameStartFile=>"startSave",
   	 NameSolutionsFile=>"simple_raw_solutions",
      	 NumberOfLoops=>10,NumSolBound=>3,
	 MonodromyStartParameters=>{1,0}	)
   Example
     f="x^3+x*y^2+y+y^3+x-2";     h="a1*x+a2*y-1";
     makeB'InputFile(storeBM2Files, 
    	 B'Configs=>{{PARAMETERHOMOTOPY,2},{MPTYPE,2}},AffVariableGroup=>{x,y},ParameterGroup=>{a1,a2}, B'Polynomials=>{f,h}    )
     b'PHMonodromyCollect(storeBM2Files,
	 StorageFolder=>"StoreFiles",
	 MonodromyStartPoints=>{{1,0}},
      	 NumberOfLoops=>10,NumSolBound=>3,
	 MonodromyStartParameters=>{1,0}	)
      
///;

doc ///
 Key
   b'PHGaloisGroup
   (b'PHGaloisGroup,String)
 Headline
   Uses homotopy continuation to compute monodromy/Galois groups.
 Usage
   b'PHGaloisGroup(s)
 Inputs
   s:String
     The directory where the files are stored.
 Description
   Text
     Given a directory that has a Bertini input file that has ParameterHomotopy set to 2, a start file, for start_parameters,
     this function uses parameter homotopies to perform a monodromy homotopy.           
   Example 
     R=CC[x,T]
     f=x^6+2*x^4+3*x^2+T
     makeB'InputFile(storeBM2Files,AffVariableGroup=>{x,T},B'Polynomials=>{f,diff(x,f)})
     runBertini(storeBM2Files)
     TCoordinates=importSolutionsFile(storeBM2Files)/last
     TBranchPoints=radicalList(TCoordinates)
     makeB'InputFile(storeBM2Files,B'Configs=>{{ParameterHomotopy,1}},AffVariableGroup=>{x},ParameterGroup=>{T},B'Polynomials=>{f})
     runBertini(storeBM2Files,PreparePH2=>true)
     b'PHGaloisGroup(storeBM2Files,BranchPoints=>TBranchPoints)
     b'PHGaloisGroup(storeBM2Files,BranchPoints=>TBranchPoints,LoopRadius=>.5)
///;



doc ///
 Key
   b'TraceTestImage
   (b'TraceTestImage,String)
 Headline
   A trace test for the image of a polynomial map.
 Usage
   b'TraceTestImage(s) 
 Inputs
   s:String
     The directory where we store the files.
 Description
   Text
     Assume that the directory contains a Bertini input file with a B'Configs including {ParameterHomotopy,2}. Only the first parameter is moved.
   Example
     R=CC[x0,x1,x2]
     F={x0^3-x1^3+x2^3+1}
     sliceH=makeB'Slice({2},{{x0,x1,x2,1}},NameB'Slice=>"H")
     makeB'InputFile(storeBM2Files,
    	 AffVariableGroup=>{x0,x1,x2},
    	 ParameterGroup=>{T},
    	 B'Configs=>{{ParameterHomotopy,1}},
    	 B'Functions=>{sliceH},
    	 B'Polynomials=>{"H0+T","H1"}|F)
     runBertini(storeBM2Files,PreparePH2=>true)
     b'TraceTestImage(storeBM2Files)
   Example    
     R=CC[x,y,z]**CC[a,b]
     xyzSub={{x,a},{y,a^2+b},{z,a^2+b^2}}
     sliceH=makeB'Slice({2},{{x,y,z,1}},NameB'Slice=>"H")
     makeB'InputFile(storeBM2Files,
    	 AffVariableGroup=>{a,b},
    	 ParameterGroup=>{T},
    	 B'Configs=>{{ParameterHomotopy,1}},
    	 B'Functions=>xyzSub|{sliceH},
    	 B'Polynomials=>{"H1","H0+T"})
     runBertini(storeBM2Files,PreparePH2=>true)
     s=importSolutionsFile(storeBM2Files)
     b'TraceTestImage(storeBM2Files,MapPoints=>({a,a^2+b,a^2+b^2},{a,b}))

   Example
     R=CC[x0,x1,x2]
     F={x0^3-x1^3+x2^3+1}
     sliceH=makeB'Slice(2,{x0,x1,x2,1},NameB'Slice=>"H")
     makeB'InputFile(storeBM2Files,
    	 AffVariableGroup=>{x0,x1,x2},
    	 ParameterGroup=>{T},
    	 B'Configs=>{{ParameterHomotopy,1}},
    	 B'Functions=>{sliceH},
    	 B'Polynomials=>{"H0+T","H1"}|F)
     runBertini(storeBM2Files,PreparePH2=>true)
     b'TraceTestImage(storeBM2Files,StopBeforeTest=>true)--Returns the trace for each parameter homotopy using -gammma, 0, and gamma respectively. 

///;


------------------------------------------------------------------
------FUNCTIONS WRITING FILES WITHOUT CALLING BERTINI ------------
------------------------------------------------------------------

doc ///
 Key
   makeB'InputFile
   (makeB'InputFile,String)
 Headline
   write a Bertini input file in a directory
 Usage
   makeB'InputFile(s) 
 Inputs
   s:String
     a directory where the input file will be written
 Description
   Text
     This function writes a Bertini input file. 
     The user can specify CONFIGS for the file using the B'Configs option.
     The user should specify variable groups with the AffVariableGroup (affine variable group) option or HomVariableGroup (homogenous variable group) option. 
     The user should specify the polynomial system they want to solve with the  B'Polynomials option or B'Functions option.
     If B'Polynomials is not used then the user should use the  NamePolynomials option. 
   Example
     R=QQ[x1,x2,y]
     theDir = temporaryFileName()
     makeDirectory theDir
     makeB'InputFile(theDir,
	 B'Configs=>{{"MPTYPE",2}},
     	 AffVariableGroup=>{{x1,x2},{y}},
	 B'Polynomials=>{y*(x1+x2+1)^2+1,x1-x2+1,y-2})
   Example
     R=QQ[x1,x2,y,X]
     makeB'InputFile(theDir,
	 B'Configs=>{{"MPTYPE",2}},
     	 AffVariableGroup=>{{x1,x2},{y}},
	 NamePolynomials=>{f1,f2,f3},
	 B'Functions=>{
	     {X,x1+x2+1},
	     {f1,y*X^2+1},
	     {f2,x1-x2+1},
	     {f3,y-2}})     
   Example
     R=QQ[x1,x2,y,X]
     makeB'InputFile(theDir,
	 B'Configs=>{{"MPTYPE",2}},
     	 AffVariableGroup=>{{x1,x2},{y}},
	 B'Polynomials=>{y*X^2+1,x1-x2+1,y-2},
	 B'Functions=>{
	     {X,x1+x2+1}})          
 Caveat
   Variables must begin with a letter (lowercase or capital) and
   can only contain letters, numbers, underscores, and square brackets.
   "jade" should not be used in any expression. 
   "I" can only be used to represent the complex number sqrt(-1).
      
///;


doc ///
 Key
   writeStartFile
   (writeStartFile,String,List)
 Headline
   Writes the list of list of coordinates to a file that Bertini can read. 
 Usage
   writeStartFile(s,v) 
 Inputs
   s:String
     The directory where the Bertini file will be written.
   v:List
     A list of list numbers that will be written to the file.   
 Description
   Text
     This function can be used to write "start" files and any other solution file using the option NameStartFile=>"AnyNameYouWant". 
   Example
     coordinatesOfTwoPnts={{1,0},{3,4}}
     writeStartFile(storeBM2Files,coordinatesOfTwoPnts)

///;



doc ///
 Key
   importSolutionsFile
   (importSolutionsFile,String)
 Headline
   Imports coordinates from a Bertini solution file.
 Usage
   importSolutionsFile(s) 
 Inputs
   s:String
     The directory where the file is stored.
 Description
   Text
     After Bertini does a run many files are created. 
     This function imports the coordinates of solutions from the simple "raw_solutions" file. 
     By using the option NameSolutionsFile=>"real_finite_solutions" we would import solutions from real finite solutions. 
     Other common file names are "nonsingular_solutions", "finite_solutions", "infinite_solutions", and "singular_solutions".     
   Text
     If the NameSolutionsFile option 
     is set to 0 then "nonsingular_solutions" is imported,
     is set to 1 then "real_finite_solutions" is imported,
     is set to 2 then "infinite_solutions" is imported,
     is set to 3 then "finite_solutions" is imported,
     is set to 4 then "start" is imported,
     is set to 5 then "raw_solutions" is imported.
   Example
     R=QQ[x,y]
     makeB'InputFile(storeBM2Files,
     	 AffVariableGroup=>{{x,y}},
	 B'Polynomials=>{x^2-1,y^3-1})
     runBertini(storeBM2Files)
     importSolutionsFile(storeBM2Files)
     importSolutionsFile(storeBM2Files,NameSolutionsFile=>"real_finite_solutions")
     importSolutionsFile(storeBM2Files,NameSolutionsFile=>0)     
      
///;


doc ///
 Key
   importParameterFile
   (importParameterFile,String)
 Headline
   Imports parameters from a Bertini parameter file.
 Usage
   importParameterFile(s) 
 Inputs
   s:String
     The directory where the file is stored.
 Description
   Text
     After Bertini does a parameter homotopy many files are created. 
     This function imports the parameters from  the "final_parameters" file as the default.  
   Example
     writeParameterFile(storeBM2Files,{1,2},NameParameterFile=>"final_parameters")
     importParameterFile(storeBM2Files)
      
///;

doc ///
 Key
   importMainDataFile
   (importMainDataFile,String)
 Headline
   This function imports points from the main data file form a Bertini run.
 Usage
   importMainDataFile(theDir) 
 Inputs
   theDir:String
     The directory where the main_data file is located.
 Description
   Text
     This function does not import a list of coordinates. Instead it imports points from a main_data file. These points contain coordinates, condition numbers, and etc.      
     The information the points contain depend on if regeneration was used and if a TrackType 0 or 1 was used.
     When TrackType 1 is used, UNCLASSIFIED points will have component number -1.    
   Example
     makeB'InputFile(storeBM2Files,
       AffVariableGroup=>{x,y,z},
       B'Configs=>{{TrackType,1}},
       B'Polynomials=>{"(x^2+y^2+z^2-1)*y"})
     runBertini(storeBM2Files)
     thePoints=importMainDataFile(storeBM2Files)
     witnessPointsDim1= importMainDataFile(storeBM2Files,SpecifyDim=>1)--We can choose which dimension we import points from. There are no witness points in dimension 1.
     sortMainDataComponents(thePoints)      
///;




doc ///
 Key
   importIncidenceMatrix
   (importIncidenceMatrix,String)
 Headline
   Imports an incidence matrix file after running makeMembershipFile.
 Usage
   importIncidenceMatrix(s) 
 Inputs
   s:String
     The directory where the file is stored.
 Description
   Text
     After running makeMembershipFile Bertini produces an incidence_matrix file. 
     The incidence_matrix says which points belong to which components. 
     Our incidence matrix is flattened to a list.    
     The number of elemenets in theIM is equal to the number of points in the solutions file. 
     Each element of theIM is a list of sequences of 2 elements (codim,component Number). 
     Note that we follow the Bertini convention and switch from (dimension,component number) indexing to (codimension,component number) indexing. 
   Text
     If the NameIncidenceMatrixFile option is set when we want to import files with a different name.
   Example
    makeB'InputFile(storeBM2Files,
    	B'Configs=>{{TrackType,1}},    AffVariableGroup=>{x,y,z},    B'Polynomials=>{"z*((x+y+z)^3-1)","z*(y^2-3+z)"}    )
    runBertini(storeBM2Files)
    makeSampleSolutionsFile(storeBM2Files,2,SpecifyComponent=>{1,0})
    makeMembershipFile(storeBM2Files,NameSolutionsFile=>"sample_solutions_file")
    theIM=importIncidenceMatrix(storeBM2Files)
      
///;


doc ///
 Key
   readFile
   (readFile,String,String,Number)
   (readFile,String,Number)
   (readFile,String)
 Headline
   Read the first lines of a file.
 Usage
   readFile(filesGoHere,fileName,numCharacters) 
   readFile(filesGoHere,numCharacters) 
 Inputs
   filesGoHere:String
     The directory where the files are stored.
   fileName:String
     The file whose lines we will read.
   numCharacters:Number
     The number of lines we read. 
 Description
   Text
     Read the first characters of a file. When fileName is omitted "bertini_session.log" is read.
   Example
     makeB'InputFile(storeBM2Files,AffVariableGroup=>{x,y},B'Polynomials=>{"x^2+2","y^2+3"})
     runBertini(storeBM2Files)
     readFile(storeBM2Files,"nonsingular_solutions",1)
     readFile(storeBM2Files,10000)
     readFile(storeBM2Files)
         
///;



doc ///
 Key
   makeMembershipFile
   (makeMembershipFile,String)
 Headline
   Creates a Bertini incidence_matrix file using Tracktype 3..
 Usage
   makeMembershipFile(s) 
 Inputs
   s:String
     The directory where the input file, witness_data, and member_points is stored. 
 Description
   Text
     After doing a positive dimensional solve with Bertini a witness data file is produced.
     This function requires an input file, member_points file and witness_data file. 
     It appends "TrackType: 3" to the configurations in the input file and calls Bertini to produce and incidence_matrix file with respect to the member_points file.
   Text
     The option NameWitnessPointFile has "member_points" set as its default. 
   Text
     The option TestSolutions can be set to a list of coordinates of points which will be written to a file. 
   Example
    makeB'InputFile(storeBM2Files,
    	B'Configs=>{{TrackType,1}},    AffVariableGroup=>{x,y,z},    B'Polynomials=>{"z*((x+y+z)^3-1)","z*(y^2-3+z)"}    )
    runBertini(storeBM2Files)
    makeSampleSolutionsFile(storeBM2Files,2,SpecifyComponent=>{1,0})
    makeMembershipFile(storeBM2Files,NameSolutionsFile=>"sample_solutions_file")
    makeMembershipFile(storeBM2Files,TestSolutions=>{{1,2,0},{3,2,1}})
    importIncidenceMatrix(storeBM2Files)
      
///;



doc ///
 Key
   makeWitnessSetFiles
   (makeWitnessSetFiles,String,Number)
 Headline
   This function creates a witness point file and a slice file. 
 Usage
   makeWitnessSetFiles(theDir,d) 
 Inputs
   theDir:String
     The directory where Bertini will store files and the witness_data file and input file are located.
   d:Number 
     The dimension of the variety that we intersect with the slice defined by the linear system of equations.
 Description
   Text
     This function does a track type 4 Bertini run creating a linear system file. The slice information for a positive dimensional run can be recovered from such a file.     
   Example
     makeB'InputFile(storeBM2Files,
       AffVariableGroup=>{x,y,z},
       B'Configs=>{{TrackType,1}},
       B'Polynomials=>{"(x^2+y^2+z^2-1)*y"})
     runBertini(storeBM2Files)
     makeWitnessSetFiles(storeBM2Files,2)--creats a witness point file for all dimension 2 components and a linear slice file for dimension 2 components. 
     L=importSliceFile(storeBM2Files) 
--
     makeWitnessSetFiles(storeBM2Files,2,
       NameSolutionsFile=>"custom_name_witness_points",--creates a witness point file with a custom name. 
       SpecifyComponent=>0)  --Component indexing begins at 0. The function creates a witness point file for only a particular component. 
     L1=importSliceFile(storeBM2Files) 
     S0=importSolutionsFile(storeBM2Files,NameSolutionsFile=>"custom_name_witness_points")
--
     makeWitnessSetFiles(storeBM2Files,2,
       NameSolutionsFile=>"custom_name_witness_points")
     S=importSolutionsFile(storeBM2Files,NameSolutionsFile=>"custom_name_witness_points")
      
///;

doc ///
 Key
   makeSampleSolutionsFile
   (makeSampleSolutionsFile,String,Number)
 Headline
   This function samples points from a component by performing a Bertini TrackType 2.
 Usage
   makeSampleSolutionsFile(theDir,s) 
 Inputs
   theDir:String
     The directory where Bertini will store files and the witness_data file and input file are located.
   s:Number 
     The number of sample points we want.
 Description
   Text
     This function does a track type 2 Bertini run creating "sample_solutions_file" that contains a list of witness points in the standard Bertini format.     
   Example
     makeB'InputFile(storeBM2Files,
       AffVariableGroup=>{x,y,z},
       B'Configs=>{{TrackType,1}},
       B'Polynomials=>{"(x^2+y^2+z^2-1)*y"})
     runBertini(storeBM2Files)
     makeSampleSolutionsFile(storeBM2Files,4,SpecifyComponent=>{2,0})--creates a witness point file with 4 sample points for the 0th component in dimension 2. 
     theSols=importSolutionsFile(storeBM2Files,NameSolutionsFile=>"sample_solutions_file") 
      
///;


doc ///
 Key
   writeParameterFile
   (writeParameterFile,String,List)
 Headline
   Writes the list of parameter values to a file that Bertini can read. 
 Usage
   writeParameterFile(s,v) 
 Inputs
   s:String
     The directory where the Bertini file will be written.
   v:List
     A list of numbers that will be written to the file.   
 Description
   Text
     To do a paremeter homotopy one must have a start_parameters file and a final_parameters file. 
   Example
     R=QQ[x,y,t]
     makeB'InputFile(storeBM2Files,
     	 B'Configs=>{{"PARAMETERHOMOTOPY",1}},
	 ParameterGroup=>{t},    AffVariableGroup=>{{x,y}},
	 B'Polynomials=>{x^2-1,y^2-t})
     runBertini(storeBM2Files)
     copyFile(storeBM2Files|"/nonsingular_solutions",storeBM2Files|"/start")
     makeB'InputFile(storeBM2Files,
     	 B'Configs=>{{"PARAMETERHOMOTOPY",2}},
	 ParameterGroup=>{t},    AffVariableGroup=>{{x,y}},
	 B'Polynomials=>{x^2-1,y^2-t})
     writeParameterFile(storeBM2Files,{1})
     runBertini(storeBM2Files)

///;





------------------------------------------------------------------
------SUPPORTING FUNCTIONS ------------
------------------------------------------------------------------

doc ///
 Key
   sortMainDataComponents
   (sortMainDataComponents,List)
 Headline
   Sorts imported main data into components.  
 Usage
   sortMainDataComponents(l) 
 Inputs
   l:List
     A list of points imported from a main_data file.
 Description
   Text
     After importing a main_data file we have a list of points. This function organizes the components by irreducible component. 
   Example
     F={"x*(x+2*y+3*z^2)","(y^3-x+z)*(z)*(x+2*y+3*z^2)"}
     makeB'InputFile(storeBM2Files,B'Configs=>{{TrackType,1}},AffVariableGroup=>{x,y,z},B'Polynomials=>F)
     runBertini(storeBM2Files)
     listPoints=importMainDataFile(storeBM2Files)
     #listPoints
     theComponents=sortMainDataComponents(listPoints)
     for i in theComponents_0 list (i#ComponentNumber,i#Dimension)     
     for i in theComponents_1 list (i#ComponentNumber,i#Dimension)          
     for i in theComponents_2 list (i#ComponentNumber,i#Dimension)          
      
///;


doc ///
 Key
   moveB'File
   (moveB'File,String,String,String)
 Headline
   Move or copy files. 
 Usage
   moveB'File(s,f,n) 
 Inputs
   s:String
     A string giving a directory.
   f:String
     A name of a file.
   s:String
     A new name for the file.
 Description
   Text
     This function takes the file f in the directory s and renames it to n. 
   Example
     writeParameterFile(storeBM2Files,{2,3,5,7})
     fileExists(storeBM2Files|"/final_parameters")
     moveB'File(storeBM2Files,"final_parameters","start_parameters")
     fileExists(storeBM2Files|"/final_parameters")
     fileExists(storeBM2Files|"/start_parameters")
     moveB'File(storeBM2Files,"start_parameters","backup",CopyB'File=>true)
     fileExists(storeBM2Files|"/start_parameters")
     fileExists(storeBM2Files|"/backup")
   Example
     Dir1 = temporaryFileName()
     makeDirectory Dir1
     writeParameterFile(storeBM2Files,{2,3,5,7})
     moveB'File(storeBM2Files,"final_parameters","start_parameters",MoveToDirectory=>Dir1)
     fileExists(Dir1|"/start_parameters")
   Example
     makeDirectory (storeBM2Files|"/Dir2")
     writeParameterFile(storeBM2Files,{2,3,5,7})
     moveB'File(storeBM2Files,"final_parameters","start_parameters",SubFolder=>"Dir2")
     fileExists(storeBM2Files|"/Dir2/start_parameters")
      
///;


doc ///
 Key
   makeB'Section
   (makeB'Section,List)
 Headline
   makeB'Section creates a hash table that represents a hyperplane. 
 Usage
   makeB'Section(l) 
 Inputs
   l:List
     A list of variables.
 Description
   Text
     makeB'Section allows for easy creation of equations that define hyperplanes.
     The default creates a hash table with two keys: B'NumberCoefficients and B'SectionString.
     The first key is a list of numbers in CC that are coefficients, and the second key is a string representing the linear polynomial.
     The option RandomCoefficientGenerator can be set to a function to generate random numbers for the coefficients. 
   Text   
     To get affine linear equations include 1 in the input list. 
   Text
     To have an affine linear equation that contains a particular point we set the ContainsPoint option to a list of coordinates or a point.
     To get an homogeneous equation that contains a projective point we have to set the ContainsPoint option as well as the B'Homogenization option.
   Example
     s=makeB'Section({x,y,z})
     class s
     randomRealCoefficientGenerator=()->random(RR)
     sReal=makeB'Section({x,y,z},RandomCoefficientGenerator=>randomRealCoefficientGenerator)
     sReal#B'NumberCoefficients
     randomRationalCoefficientGenerator=()->random(QQ)
     sRational=makeB'Section({x,y,z},RandomCoefficientGenerator=>randomRationalCoefficientGenerator)
     sRational#B'NumberCoefficients
   Example
     affineSection=makeB'Section({x,y,z,1})
   Example
     X={x,y,z}
     P={1,2,3}
     affineContainingPoint=makeB'Section({x,y,z},ContainsPoint=>P)
     r= affineContainingPoint#B'SectionString
     print r
   Example
     rHomogeSection= makeB'Section({x,y,z},ContainsPoint=>P,B'Homogenization=>"x+y+z")
     peek rHomogeSection
     print rHomogeSection#B'SectionString
   Example
     f="y^3-x*y+1"
     s1=makeB'Section({x,y,1})
     makeB'InputFile(storeBM2Files,
       AffVariableGroup=>{x,y},
       B'Polynomials=>{f,s1})
     runBertini(storeBM2Files)
     #importSolutionsFile(storeBM2Files)==3
      
///;


doc ///
 Key
   makeB'Slice
   NameB'Slice
   (makeB'Slice,Thing,List)
 Headline
   makeB'Slice creates a hash table that represents a linear slice. 
 Usage
   makeB'Slice(sliceType,variableGroups) 
 Inputs
   sliceType:List
     A list of integers or integer.
   variableGroups:List
     A list of list of variables or list of variables.
 Description
   Text
     makeB'Slice allows for easy creation of equations that define linear spaces, i.e. slices.
     The default creates a hash table with two keys: B'NumberCoefficients and B'SectionString.
     When we have a multiprojective variety we can different types of slices.
     To make a slice we need to specify the type of slice we want followed by variable groups.
   Example
     sliceType={1,1}
     variableGroups={{x0,x1},{y0,y1,y2}}
     xySlice=makeB'Slice(sliceType,variableGroups)
     peek xySlice
     --Our slice consists of two sections. 
     --The ith section is in the variables variableGroups_(sliceType_i)
     for i in  xySlice#B'SectionString do print i
   Example
     --Using the NameB'Slice option we can put a slice in the B'Functions option.
     aSlice=makeB'Slice(3,{x,y,z,1},NameB'Slice=>"f");
     aSlice#NameB'Slice
     makeB'InputFile(storeBM2Files,AffVariableGroup=>{x,y,z},B'Functions=>{aSlice},NamePolynomials=>{"f0","f1","f2"})
   Example
     --We can use slices to determine multidegrees.
     f1="x0*y0+x1*y0+x2*y2"
     f2="x0*y0^2+x1*y1*y2+x2*y0*y2"
     variableGroups={{x0,x1,x2},{y0,y1,y2}}
     xxSlice=makeB'Slice({2,0},variableGroups)
     xySlice=makeB'Slice({1,1},variableGroups)
     yySlice=makeB'Slice({0,2},variableGroups)
     makeB'InputFile(storeBM2Files,
    	 HomVariableGroup=>variableGroups,
    	 B'Polynomials=>{f1,f2}|xxSlice#ListB'Sections)
     runBertini(storeBM2Files)
     xxDegree=#importSolutionsFile(storeBM2Files)
     makeB'InputFile(storeBM2Files,
    	 HomVariableGroup=>variableGroups,
    	 B'Polynomials=>{f1,f2}|xySlice#ListB'Sections)
     runBertini(storeBM2Files)
     xyDegree=#importSolutionsFile(storeBM2Files)
     makeB'InputFile(storeBM2Files,
    	 HomVariableGroup=>variableGroups,
    	 B'Polynomials=>{f1,f2}|yySlice#ListB'Sections)
     runBertini(storeBM2Files)
     yyDegree=#importSolutionsFile(storeBM2Files)
      
///;


doc ///
 Key
   replaceFirstLine
   (replaceFirstLine,String,String,Thing)
 Headline
   Replaces the first line of a file with a string.
 Usage
   replaceFirstLine(filesGoHere,fileName,aString) 
 Inputs
   filesGoHere:String
     The directory where the files are stored.
   fileName:String
     The file whose first line we will replace with aString.
   aString:Thing
     The string which we write in the first line. 
 Description
   Text
     replaceFirstLine replaces the first line of a file with a string. This function is used in the trace test functions.
   Example
     writeStartFile(storeBM2Files,{{2,3},{4,5}})
     replaceFirstLine(storeBM2Files,"start","1")
         
///;


doc ///
 Key
   radicalList
   (radicalList,List)
   (radicalList,List,Number)
 Headline
   A support function that removes multiplicities of numbers in a list up to a tolerance.
 Usage
   radicalList(List,Number)
   radicalList(List) 
 Inputs
   L:List
     A list of complex or real numbers.
   N:Number
     A small real number. 
 Description
   Text
     This outputs a sublist of complex or real numbers that all have distinct norms up to the tolerance N (default is 1e-10).
   Example
     radicalList({2.000,1.999})
     radicalList({2.000,1.999},1e-10)         
     radicalList({2.000,1.999},1e-2)         
///;


doc ///
 Key
   NumberToB'String
   (NumberToB'String,Thing)
 Headline
   Translates a number to a string that Bertini can read. 
 Usage
   NumberToB'String(n) 
 Inputs
   n:Thing
     n is a number.
 Description
   Text
     This function takes a number as an input then outputs a string to represent this number to Bertini.
     The numbers are converted to floating point to precision determined by the option M2Precision.       
   Example
     NumberToB'String(2+5*ii)
     NumberToB'String(1/3,M2Precision=>16)
     NumberToB'String(1/3,M2Precision=>128)

///;


doc ///
 Key
   valueBM2
   (valueBM2,String)
 Headline
   This function makes a number in CC from a string.
 Usage
   valueBM2(s) 
 Inputs
   s:String
     A string that gives a coordinate.
 Description
   Text
     This function take a string representing a coordinate in a Bertini solutions file or parameter file and makes a number in CC. 
     We can adjust the precision using the M2Precision option.
     Fractions should not be in the string s. 
   Example
     valueBM2("1.22e-2 4e-5")
     valueBM2("1.22 4e-5")
     valueBM2("1.22 4")     
     valueBM2("1.22e+2 4 ")           
     n1=valueBM2("1.11",M2Precision=>52)
     n2=valueBM2("1.11",M2Precision=>300)
     toExternalString n1
     toExternalString n2
///;


doc ///
 Key
   subPoint
   (subPoint,Thing,List,Thing)
 Headline
   This function evaluates a polynomial or matrix at a point.
 Usage
   subPoint(f,v,p) 
 Inputs
   f:Thing
     A polynomial or a matrix.
   v:List
     List of variables that we will be evaluated at the point.
   p:Thing
     A point or a list of coordinates or a matrix. 
 Caveat
   When SubIntoCC is set to true then unset variables will be set to zero or unexpected values.  
 Description
   Text
     Evaluate f at a point. 
   Example
     R=CC[x,y,z]
     f=z*x+y
     subPoint(f,{x,y},{.1,.2})
     subPoint(f,{x,y,z},{.1,.2,.3},SpecifyVariables=>{y})
   Example 
     R=CC_200[x,y,z]
     f=z*x+y
     subPoint(f,{x,y,z},{.1,.2,.3},SubIntoCC=>true)
     subPoint(f,{x,y,z},{.1234567890123456789012345678901234567890p200,
	     0,1},SubIntoCC=>true,M2Precision=>200)

///;












-------------------
-----OPTIONS-------
-------------------
doc ///
  Key
   [bertiniTrackHomotopy, Verbose]
   [bertiniComponentMemberTest, Verbose]
   [bertiniPosDimSolve, Verbose]
   [bertiniRefineSols, Verbose]
   [bertiniSample, Verbose]
   [bertiniZeroDimSolve, Verbose]
   [bertiniParameterHomotopy, Verbose]
   [makeB'InputFile, Verbose]
   [makeMembershipFile, Verbose]
   [b'PHGaloisGroup,Verbose]
   [b'PHMonodromyCollect,Verbose]
   [importIncidenceMatrix,Verbose]
   [importMainDataFile,Verbose]
   [importSliceFile,Verbose]
   [importSolutionsFile,Verbose]
   [runBertini,Verbose]
   [makeWitnessSetFiles,Verbose]
   [b'PHSequence,Verbose]
   [b'TraceTestImage,Verbose]
   [makeSampleSolutionsFile,Verbose]
  Headline
    Option to silence additional output 
  Usage
    bertiniTrackHomotopyVerbose(...,Verbose=>Boolean)
    bertiniPosDimSolve(...,Verbose=>Boolean)
    bertiniRefineSols(...,Verbose=>Boolean)
    bertiniSample(...,Verbose=>Boolean)
    bertiniZeroDimSolve(...,Verbose=>Number)
    bertiniParameterHomotopy(...,Verbose=>Number)
    makeB'InputFile(...,Verbose=>Number)
    makeMembershipFile(...,Verbose=>Number)
    b'PHGaloisGroup(...,Verbose=>Number)
    b'PHMonodromyCollect(...,Verbose=>Number)
    importIncidenceMatrix(...,Verbose=>Number)
    importMainDataFile(...,Verbose=>Number)
    importSliceFile(...,Verbose=>Number)
    importSolutionsFile(...,Verbose=>Number)
    runBertini(...,Verbose=>Number)
  Description
    Text
       Use {\tt Verbose=>false} or {\tt Verbose=>0}  to silence additional output.
///;

doc ///
  Key
   OutputSyle
   [bertiniParameterHomotopy, OutputSyle]
   [bertiniZeroDimSolve,OutputSyle]
  Headline
    Used to change the output style.
  Usage
    bertiniParameterHomotopy(...,OutputSyle=>String)
    bertiniZeroDimSolve(...,OutputSyle=>String)
  Description
    Text
       Use OutputSyle to change the style of output. 
///;


doc ///
  Key
   TopDirectory
   [bertiniParameterHomotopy, TopDirectory]
   [bertiniZeroDimSolve,TopDirectory]
  Headline
    Option to change directory for file storage.
  Usage
    bertiniParameterHomotopy(...,TopDirectory=>String)
    bertiniZeroDimSolve(...,TopDirectory=>String)
  Description
    Text
       Use TopDirectory to specify the directory where computations will occur. 
///;

doc ///
  Key
   M2Precision
   [bertiniParameterHomotopy,M2Precision]
   [bertiniZeroDimSolve,M2Precision]   
  Headline
    Option to change the precision Macaulay2 uses to import the files.
  Usage
    bertiniParameterHomotopy(...,M2Precision=>ZZ)
    bertiniZeroDimSolve(...,TopDirectory=>ZZ)
  Description
    Text
       When importing solutions, they are first converted to an external string where precision is set. The default is 53. 
///;



doc ///
 Key
   IsProjective
   [bertiniTrackHomotopy, IsProjective]
--   [bertiniParameterHomotopy, IsProjective]
   [bertiniComponentMemberTest, IsProjective]
   [bertiniPosDimSolve, IsProjective]
   [bertiniRefineSols, IsProjective]
   [bertiniSample, IsProjective]
--   [bertiniZeroDimSolve, IsProjective]
 Headline
   optional argument to specify whether to use homogeneous coordinates
 Description
   Text
     When set to 1, this option indicates that the input system is homogenized and
     the output should be given in projective space.
   Example
     R = CC[x,y,z];
     f = {(x^2+y^2-z^2)*(z-x),(x^2+y^2-z^2)*(z+y)};
     bertiniPosDimSolve(f,IsProjective=>1);
                
///;

doc ///
 Key
   AffVariableGroup
   HomVariableGroup   
   [bertiniParameterHomotopy, AffVariableGroup]
   [bertiniParameterHomotopy, HomVariableGroup]
   [makeB'InputFile, AffVariableGroup]
   [makeB'InputFile, HomVariableGroup]
   [bertiniZeroDimSolve,HomVariableGroup]
   [bertiniZeroDimSolve,AffVariableGroup]
 Headline
   See help for bertiniParameterHomotopy and/or makeB'InputFile.
 Description
   Text
     
                
///;



doc ///
 Key
   CopyB'File
   [moveB'File, CopyB'File]
 Headline
   optional argument to specify whether make a copy of the file.
 Description
   Text
     When set to true, a file is copy of the file is made rather than just moved. The default in moveB'File is set to false.
///;






doc///
 Key
   ParameterGroup
   [makeB'InputFile, ParameterGroup]
 Headline
   An option which designates the parameters for a Parameter Homotopy.    
 Description
   Text
     This option should be set to a list of parameters. 
   Example
     R=QQ[x,y,u]
     makeB'InputFile(storeBM2Files,
	 B'Configs=>{{"PARAMETERHOMOTOPY",1}},
	 AffVariableGroup=>{{x,y}},
	 ParameterGroup=>{u},
	 B'Polynomials=>{y-(x^2-1),y-u})
     
///;

doc///
 Key
   B'Configs
   [makeB'InputFile, B'Configs]
   [makeB'TraceInput,B'Configs]
   [bertiniParameterHomotopy,B'Configs]
   [bertiniZeroDimSolve,B'Configs]
 Headline
   An option to designate the CONFIG part of a Bertini Input file.
 Description
   Text
     This option should be set to a list of lists of 2 elements. The first element is the name of the Bertini option, e.g. "MPType" and and the second element is what the Bertini option will be set to e.g. "2".
   Example
     R=QQ[x0,x1,y0,y1,z]
     makeB'InputFile(storeBM2Files,
	 B'Configs=>{{"MPTYPE",2}},
     	 HomVariableGroup=>{{x0,x1},{y0,y1}},
	 AffVariableGroup=>{{z}},
	 B'Polynomials=>{z*x1^2+x0^2,y0*z+y1,y0-2*z^2*y1})
     
///;


doc///
 Key
   B'Constants
   [makeB'InputFile, B'Constants]
   [bertiniParameterHomotopy,B'Constants]
   [bertiniZeroDimSolve,B'Constants]
 Headline
   An option to designate the constants for a Bertini Input file.
 Description
   Text
     This option should be set to a list of lists of 2 elements or options. 
     The first element of the list of two elements is the name of the Constant, and
     the second element is the value that the consant will be set. 
   Example
     R=QQ[z,a,b,c]
     makeB'InputFile(storeBM2Files,
	 B'Configs=>{{"MPTYPE",2}},
	 AffVariableGroup=>{{z}},
	 B'Constants=>{{a,2},{b,3+2*ii},c=>3/2},
	 B'Polynomials=>{a*z^2+b*z+c})
     
///;



doc///
 Key
   RandomReal
   [makeB'InputFile, RandomReal]
   RandomComplex
   [makeB'InputFile, RandomComplex]
   [bertiniParameterHomotopy,RandomComplex]
   [bertiniParameterHomotopy,RandomReal]
   [bertiniZeroDimSolve,RandomComplex]
   [bertiniZeroDimSolve,RandomReal]
 Headline
   An option which designates symbols/strings/variables that will be set to be a random real number or random complex number.
 Description
   Text
     This option should be set to a list of symbols, strings, or variables. 
     Elemenets of this list will be set to random real/complex numbers when Bertini is called.
   Example
     R=QQ[x,y,c1,c2]
     makeB'InputFile(storeBM2Files,
	 AffVariableGroup=>{{x,y}},
	 RandomReal=>{c1,c2},--c1=.1212, c2=.4132 may be written to the input file.
	 B'Polynomials=>{x-c1,y-c2})
   Example
     R=QQ[x,y,c1,c2]
     makeB'InputFile(storeBM2Files,
	 AffVariableGroup=>{{x,y}},
	 RandomComplex=>{c1,c2},--c1=.1212+ii*.1344, c2=.4132-ii*.2144 are written to the input file.
	 B'Polynomials=>{x-c1,y-c2})
   Text
     AFTER Bertini is run, the random values are stored in a file named "random_values".
     
///;


doc///
 Key
   B'Polynomials
   [makeB'InputFile, B'Polynomials]
 Headline
   An option which designates the polynomials that we want to solve.    
 Description
   Text
     The user should specify the polynomial system they want to solve with the  B'Polynomials option or B'Functions option.
     If B'Polynomials is not used then the user should use the  NamePolynomials option. 
   Example
     R=QQ[x,y]
     makeB'InputFile(storeBM2Files,
	 B'Configs=>{{"MPTYPE",2}},
	 AffVariableGroup=>{{x,y}},
	 B'Polynomials=>{x+y-1,x^2-2})
   Text
     B'Polynomials can be in combination with B'Functions. B'Functions allows the user to define subfunctions.  
   Example
     R=QQ[x,y,A]
     makeB'InputFile(storeBM2Files,
	 AffVariableGroup=>{{x,y}},
	 B'Functions=>{{A,x^2-1}},
	 B'Polynomials=>{A+y,x+y-2})
     
///;


doc///
 Key
   B'Functions
   [makeB'InputFile, B'Functions]
   [bertiniZeroDimSolve,B'Functions]
   [bertiniParameterHomotopy,B'Functions]
 Headline
   An option which designates sub-functions or a polynomial system as a straight line program.  
 Description
   Text
     The user should specify the polynomial system they want to solve with the  B'Polynomials option or the B'Functions option.
     The user should use the  NamePolynomials option in conjunction with B'Functions whenever B'Polynomials is not used. 
   Example
     R=QQ[x,y]
     makeB'InputFile(storeBM2Files,
	 AffVariableGroup=>{{x,y}},
	 NamePolynomials=>{f1,f2},
	 B'Functions=>{{f1,x+y-1},f2=>x^2-2})--f1=x+y+1,f2=x^2-2 is written to the input file
   Text
     B'Polynomials can be in combination with B'Functions. B'Functions allows the user to define subfunctions.  
   Example
     R=QQ[x,y,A]
     makeB'InputFile(storeBM2Files,
	 AffVariableGroup=>{{x,y}},
	 B'Functions=>{{A,x^2-1}},--A=x^2-1 is written to the input file
	 B'Polynomials=>{A+y,x+y-2})
     
///;


doc///
 Key
   NamePolynomials
   [makeB'InputFile, NamePolynomials]
 Headline
   An option which designates the names of the polynomials we want to solve.  
 Description
   Text
     The user should specify the polynomial system they want to solve with the  B'Polynomials option or the B'Functions option.
     The user should use the  NamePolynomials option in conjunction with B'Functions whenever B'Polynomials is not used. 
   Example
     R=QQ[x,y]
     makeB'InputFile(storeBM2Files,
	 AffVariableGroup=>{{x,y}},
	 NamePolynomials=>{f1,f2},
	 B'Functions=>{{f1,x+y-1},{f2,x^2-2}})--f1=x+y+1,f2=x^2-2 is written to the input file
     
///;


doc///
 Key
   NameB'InputFile
   [makeB'InputFile, NameB'InputFile]
 Headline
   This option names the input file. 
 Description
   Text
     Set this option to a string to name the input file. 
   Example
     R=QQ[x,y]
     makeB'InputFile(storeBM2Files,
	 AffVariableGroup=>{{x,y}},
	 B'Polynomials=>{x+y-1,x^2-2},
	 NameB'InputFile=>"testInput")--the input file will be named "testInput" rather than the default "input".
     
///;


doc///
 Key
   NameParameterFile
   [writeParameterFile, NameParameterFile]
 Headline
   This option names the parameter file. 
 Description
   Text
     Set this option to a string to name the parameter file.
   Example
     writeParameterFile(storeBM2Files,{.1,.2,.5},NameParameterFile=>"testParameters")  --this function writes a parameter file named testParameters
     
///;


doc///
 Key
   SolutionFileStyle
   [b'PHSequence, SolutionFileStyle]
 Headline
   This is to adjust to the different ways Bertini stores solutions.
 Description
   Text      
     The option can be set to the "simple" style (coordinates only), "raw_solutions" style (path number and coordinates), and "main_data" style (detailed information).          

///;
 

doc///
 Key
   NameSolutionsFile
   [importSolutionsFile, NameSolutionsFile]
   NameFunctionFile
   NameWitnessSliceFile
   NameWitnessSolutionsFile
   NameMainDataFile
   NameSampleSolutionsFile
   [b'PHGaloisGroup,NameB'InputFile]
   [b'PHMonodromyCollect,NameB'InputFile]
   [b'PHSequence,NameB'InputFile]
   [calculateB'Trace,NameB'InputFile]
   [makeB'TraceInput,NameB'InputFile]
   [makeSampleSolutionsFile,NameB'InputFile]
   [makeWitnessSetFiles,NameB'InputFile]
   [runBertini,NameB'InputFile]
   [runBertini,B'Exe]
   [b'PHGaloisGroup,NameParameterFile]
   [b'PHMonodromyCollect,NameParameterFile]
   [b'PHSequence,NameParameterFile]
   [importParameterFile,NameParameterFile]
   [b'PHGaloisGroup,NameSolutionsFile]
   [b'PHMonodromyCollect,NameSolutionsFile]
   [b'PHSequence,NameSolutionsFile]
   [makeMembershipFile,NameSolutionsFile]
   [makeSampleSolutionsFile,NameSolutionsFile]
   [makeWitnessSetFiles,NameSolutionsFile]
   [b'PHGaloisGroup,NameStartFile]
   [calculateB'Trace,NameStartFile]
   [makeWitnessSetFiles,NameWitnessSliceFile]
   [importMainDataFile,NameMainDataFile]
   NameGaloisGroupGeneratorFile
   [b'TraceTestImage,NameB'InputFile]
   [b'PHGaloisGroup,NameGaloisGroupGeneratorFile]
   [makeMembershipFile,NameB'InputFile]
   [b'PHMonodromyCollect,NameStartFile]
   [b'PHSequence,NameStartFile]
   NameIncidenceMatrixFile
   NameStartFile
   [writeStartFile,NameStartFile]
   [importIncidenceMatrix, NameIncidenceMatrixFile]
   [bertiniZeroDimSolve,NameSolutionsFile]
   [bertiniZeroDimSolve,NameMainDataFile]
 Headline
   options determine the name of a file to be imported or written. 
 Description
   Text
     Set this option to a string to name the file to be imported or written.
   Example
     makeB'InputFile(storeBM2Files,AffVariableGroup=>{x},B'Polynomials=>{"x^2-2"});
     runBertini(storeBM2Files);
     importSolutionsFile(storeBM2Files,NameSolutionsFile=>"nonsingular_solutions");     
     importSolutionsFile(storeBM2Files,NameSolutionsFile=>"real_finite_solutions");     
   Example
     writeStartFile(storeBM2Files,{{2},{ -2}},NameStartFile=>"start");
     writeParameterFile(storeBM2Files,{4},NameParameterFile=>"start_parameters");
     writeParameterFile(storeBM2Files,{3},NameParameterFile=>"final_parameters");
     makeB'InputFile(storeBM2Files,B'Polynomials=>{"x^2-t"},ParameterGroup=>{t},B'Configs=>{{ParameterHomotopy,2}},AffVariableGroup=>{x},NameB'InputFile=>"inputWin");
     runBertini(storeBM2Files,NameB'InputFile=>"inputWin");
     importSolutionsFile(storeBM2Files,NameSolutionsFile=>"nonsingular_solutions")
     importSolutionsFile(storeBM2Files,NameSolutionsFile=>"real_finite_solutions")          
///;





doc ///
  Key
    "Bertini options"
    MPType
    PRECISION
    ODEPredictor
    TrackTolBeforeEG
    TrackTolDuringEG
    FinalTol
    MaxNorm  
    MinStepSizeBeforeEG
    MinStepSizeDuringEG
    ImagThreshold
    CoeffBound
    DegreeBound
    CondNumThreshold
    RandomSeed
    SingValZeroTol
    EndGameNum
    UseRegeneration
    SecurityLevel
    ScreenOut
    OutputLevel
    StepsForIncrease
    MaxNewtonIts
    MaxStepSize
    MaxNumberSteps
    MaxCycleNum
    RegenStartLevel
    [bertiniComponentMemberTest, MPType]
    [bertiniComponentMemberTest, PRECISION]
    [bertiniComponentMemberTest, ODEPredictor]
    [bertiniComponentMemberTest, TrackTolBeforeEG]
    [bertiniComponentMemberTest, TrackTolDuringEG]
    [bertiniComponentMemberTest, FinalTol]
    [bertiniComponentMemberTest, MaxNorm]
    [bertiniComponentMemberTest, MinStepSizeBeforeEG]
    [bertiniComponentMemberTest, MinStepSizeDuringEG]
    [bertiniComponentMemberTest, ImagThreshold]
    [bertiniComponentMemberTest, CoeffBound]
    [bertiniComponentMemberTest, DegreeBound]
    [bertiniComponentMemberTest, CondNumThreshold]
    [bertiniComponentMemberTest, RandomSeed]
    [bertiniComponentMemberTest, SingValZeroTol]
    [bertiniComponentMemberTest, EndGameNum]
    [bertiniComponentMemberTest, UseRegeneration]
    [bertiniComponentMemberTest, SecurityLevel]
    [bertiniComponentMemberTest, ScreenOut]
    [bertiniComponentMemberTest, OutputLevel]
    [bertiniComponentMemberTest, StepsForIncrease]
    [bertiniComponentMemberTest, MaxNewtonIts]
    [bertiniComponentMemberTest, MaxStepSize]
    [bertiniComponentMemberTest, MaxNumberSteps]
    [bertiniComponentMemberTest, MaxCycleNum]
    [bertiniComponentMemberTest, RegenStartLevel]
    [bertiniPosDimSolve, MPType]
    [bertiniPosDimSolve, PRECISION]
    [bertiniPosDimSolve, ODEPredictor]
    [bertiniPosDimSolve, TrackTolBeforeEG]
    [bertiniPosDimSolve, TrackTolDuringEG]
    [bertiniPosDimSolve, FinalTol]
    [bertiniPosDimSolve, MaxNorm]
    [bertiniPosDimSolve, MinStepSizeBeforeEG]
    [bertiniPosDimSolve, MinStepSizeDuringEG]
    [bertiniPosDimSolve, ImagThreshold]
    [bertiniPosDimSolve, CoeffBound]
    [bertiniPosDimSolve, DegreeBound]
    [bertiniPosDimSolve, CondNumThreshold]
    [bertiniPosDimSolve, RandomSeed]
    [bertiniPosDimSolve, SingValZeroTol]
    [bertiniPosDimSolve, EndGameNum]
    [bertiniPosDimSolve, UseRegeneration]
    [bertiniPosDimSolve, SecurityLevel]
    [bertiniPosDimSolve, ScreenOut]
    [bertiniPosDimSolve, OutputLevel]
    [bertiniPosDimSolve, StepsForIncrease]
    [bertiniPosDimSolve, MaxNewtonIts]
    [bertiniPosDimSolve, MaxStepSize]
    [bertiniPosDimSolve, MaxNumberSteps]
    [bertiniPosDimSolve, MaxCycleNum]
    [bertiniPosDimSolve, RegenStartLevel]
    [bertiniRefineSols, MPType]
    [bertiniRefineSols, PRECISION]
    [bertiniRefineSols, ODEPredictor]
    [bertiniRefineSols, TrackTolBeforeEG]
    [bertiniRefineSols, TrackTolDuringEG]
    [bertiniRefineSols, FinalTol]
    [bertiniRefineSols, MaxNorm]
    [bertiniRefineSols, MinStepSizeBeforeEG]
    [bertiniRefineSols, MinStepSizeDuringEG]
    [bertiniRefineSols, ImagThreshold]
    [bertiniRefineSols, CoeffBound]
    [bertiniRefineSols, DegreeBound]
    [bertiniRefineSols, CondNumThreshold]
    [bertiniRefineSols, RandomSeed]
    [bertiniRefineSols, SingValZeroTol]
    [bertiniRefineSols, EndGameNum]
    [bertiniRefineSols, UseRegeneration]
    [bertiniRefineSols, SecurityLevel]
    [bertiniRefineSols, ScreenOut]
    [bertiniRefineSols, OutputLevel]
    [bertiniRefineSols, StepsForIncrease]
    [bertiniRefineSols, MaxNewtonIts]
    [bertiniRefineSols, MaxStepSize]
    [bertiniRefineSols, MaxNumberSteps]
    [bertiniRefineSols, MaxCycleNum]
    [bertiniRefineSols, RegenStartLevel]
    [bertiniSample, MPType]
    [bertiniSample, PRECISION]
    [bertiniSample, ODEPredictor]
    [bertiniSample, TrackTolBeforeEG]
    [bertiniSample, TrackTolDuringEG]
    [bertiniSample, FinalTol]
    [bertiniSample, MaxNorm]
    [bertiniSample, MinStepSizeBeforeEG]
    [bertiniSample, MinStepSizeDuringEG]
    [bertiniSample, ImagThreshold]
    [bertiniSample, CoeffBound]
    [bertiniSample, DegreeBound]
    [bertiniSample, CondNumThreshold]
    [bertiniSample, RandomSeed]
    [bertiniSample, SingValZeroTol]
    [bertiniSample, EndGameNum]
    [bertiniSample, UseRegeneration]
    [bertiniSample, SecurityLevel]
    [bertiniSample, ScreenOut]
    [bertiniSample, OutputLevel]
    [bertiniSample, StepsForIncrease]
    [bertiniSample, MaxNewtonIts]
    [bertiniSample, MaxStepSize]
    [bertiniSample, MaxNumberSteps]
    [bertiniSample, MaxCycleNum]
    [bertiniSample, RegenStartLevel]
    [bertiniTrackHomotopy, MPType]
    [bertiniTrackHomotopy, PRECISION]
    [bertiniTrackHomotopy, ODEPredictor]
    [bertiniTrackHomotopy, TrackTolBeforeEG]
    [bertiniTrackHomotopy, TrackTolDuringEG]
    [bertiniTrackHomotopy, FinalTol]
    [bertiniTrackHomotopy, MaxNorm]
    [bertiniTrackHomotopy, MinStepSizeBeforeEG]
    [bertiniTrackHomotopy, MinStepSizeDuringEG]
    [bertiniTrackHomotopy, ImagThreshold]
    [bertiniTrackHomotopy, CoeffBound]
    [bertiniTrackHomotopy, DegreeBound]
    [bertiniTrackHomotopy, CondNumThreshold]
    [bertiniTrackHomotopy, RandomSeed]
    [bertiniTrackHomotopy, SingValZeroTol]
    [bertiniTrackHomotopy, EndGameNum]
    [bertiniTrackHomotopy, UseRegeneration]
    [bertiniTrackHomotopy, SecurityLevel]
    [bertiniTrackHomotopy, ScreenOut]
    [bertiniTrackHomotopy, OutputLevel]
    [bertiniTrackHomotopy, StepsForIncrease]
    [bertiniTrackHomotopy, MaxNewtonIts]
    [bertiniTrackHomotopy, MaxStepSize]
    [bertiniTrackHomotopy, MaxNumberSteps]
    [bertiniTrackHomotopy, MaxCycleNum]
    [bertiniTrackHomotopy, RegenStartLevel]
    [bertiniZeroDimSolve, MPType]
    [bertiniZeroDimSolve, PRECISION]
    [bertiniZeroDimSolve, ODEPredictor]
    [bertiniZeroDimSolve, TrackTolBeforeEG]
    [bertiniZeroDimSolve, TrackTolDuringEG]
    [bertiniZeroDimSolve, FinalTol]
    [bertiniZeroDimSolve, MaxNorm]
    [bertiniZeroDimSolve, MinStepSizeBeforeEG]
    [bertiniZeroDimSolve, MinStepSizeDuringEG]
    [bertiniZeroDimSolve, ImagThreshold]
    [bertiniZeroDimSolve, CoeffBound]
    [bertiniZeroDimSolve, DegreeBound]
    [bertiniZeroDimSolve, CondNumThreshold]
    [bertiniZeroDimSolve, RandomSeed]
    [bertiniZeroDimSolve, SingValZeroTol]
    [bertiniZeroDimSolve, EndGameNum]
    [bertiniZeroDimSolve, UseRegeneration]
    [bertiniZeroDimSolve, SecurityLevel]
    [bertiniZeroDimSolve, ScreenOut]
    [bertiniZeroDimSolve, OutputLevel]
    [bertiniZeroDimSolve, StepsForIncrease]
    [bertiniZeroDimSolve, MaxNewtonIts]
    [bertiniZeroDimSolve, MaxStepSize]
    [bertiniZeroDimSolve, MaxNumberSteps]
    [bertiniZeroDimSolve, MaxCycleNum]
    [bertiniZeroDimSolve, RegenStartLevel]
  Headline
    options for methods of Bertini package
  Description
    Text
      Many functions of the package takes ALL optional arguments listed here.
      The default value for EACH option is -1, which tells Bertini to use its internal default.
      Refer to Appendix E of SIAM Bertini book for full details and list of options. 

      MPType: Type of precision (0=double, 1=fixed higher, 2=adaptive).

      PRECISION: Precision, in bits, when used MPType=1.

      ODEPredictor: Choice of predictor method (9 choices).

      TrackTolBeforeEG: Before endgame zone, Newton error must be less than this for success. 

      TrackTolDuringEG: Same as previous, but during endgame.

      FinalTol: Path is deemed successful if final two endpoint approximations agree to FinalTol.

      MaxNorm: If SecurityLevel=0, path is truncated if two consecutive endpoint approximations exceed this value. 

      MinStepSizeBeforeEG: Path is truncated if stepsize drops below this level before endgame.

      MinStepSizeDuringEG: Same as previous, but during endgame.

      ImagThreshold: Endpoint deemed real if infinity norm is smaller than this. 

      CoeffBound: Useful only if MPType=2, bound on sum of coefficients of each polynomial. 

      DegreeBound: Useful only if MPType=2, bound on degree of each polynomial.

      CondNumThreshold: Endpoint is deemed singular if multiple paths lead to it or condition number exceeds this. 

      RandomSeed: Useful to repeat runs with the same random numbers.

      SingValZeroTol: Singular value is considered 0 if less than this value, when using fixed precision.

      EndGameNum: Choice of endgame (1=power series, 2=Cauchy, 3=trackback Cauchy).

      UseRegeneration: 1 to use regeneration for a zero-dimensional run.

      SecurityLevel: 1 to avoid truncation of possibly-infinite paths.

      ScreenOut: Level of output to the screen.

      OutputLevel: Level of output to files.

      StepsForIncrease: Number of consecutive Newton corrector successes before increase of stepsize.

      MaxNewtonIts: Newton corrector step deemed failed if no convergence prior to this number of iterations. 

      MaxStepSize: Largest stepsize allowed. 

      MaxNumberSteps: Max number of steps for entire path.  Path failure if number of steps exceeds this.

      MaxCycleNum: Max cycle number considered during endgame.

      RegenStartLevel: Level at which regeneration begins. 

      There are two recommended ways of using the optional arguments based on zero-dim solving and pos-dim solving.
    
      (1) Specify individual parameters in a function call:
    Example
      CC[x,y]; F = {x^2-1,y^2-1};
      bertiniZeroDimSolve(F,B'Configs=>{RandomSeed=>0,TrackTolBeforeEG=>1e-6,FinalTol=>1e-100})
    Text
      (2) Store your frequently used favorites in an OptionTable
      and pass it as the last argument in each function call:
    Example
      opts = new OptionTable from {RandomSeed=>0,TrackTolBeforeEG=>1e-6,FinalTol=>1e-100}
      G = {x^2+y^2-1};
      bertiniPosDimSolve(G,opts)
///;





-------------------
-------Types-------
-------------------
doc ///
 Key
   B'Section
 Headline
   This is a mutable hash table that gives information about a hyperplane used to slice a numerical variety.
 Description
   Text
     B'Section is a type of mutable hash table. It can be created using makeB'Section.
///;

doc ///
 Key
   B'Slice
 Headline
   This is a mutable hash table that gives information about a linear space used to slice a numerical variety.
 Description
   Text
     B'Slice is a type of mutable hash table. It can be created using makeB'Slice.
///;


-------------------
---EXPERIMENTAL------
-------------------
doc ///
 Key
   StartFileDirectory
   InputFileDirectory
   StartParameterFileDirectory
   NumSolBound
   OrderPaths
   NumberOfLoops
   BranchPoints
   SpecifyLoops
   SaveData
   ListB'Sections
   TextScripts
   PreparePH2
   SpecifyDim
   MonodromyStartPoints
   PathNumber
   TestSolutions
   SpecifyComponent
   ContainsMultiProjectivePoint
   UseStartPointsFirst
   MonodromyStartParameters
   B'Exe
   calculateB'Trace
   makeB'TraceInput
   importSliceFile
   DeflationsNeeded
   Dimension
   MaxPrecisionUtilized
   FinalTValue
   (calculateB'Trace,String)
   (importSliceFile,String)
   SolutionType
--   storeBM2Files
   [b'PHMonodromyCollect,NumSolBound]
   (makeB'TraceInput,String,Number,Number)
   [makeB'InputFile,PathVariable]
   [makeB'InputFile,VariableList]
   [makeMembershipFile,TestSolutions]
   [makeMembershipFile,M2Precision]
   [valueBM2,M2Precision]
   [NumberToB'String,M2Precision]
   [makeSampleSolutionsFile,SpecifyComponent]
   [b'PHMonodromyCollect,B'Exe]
   [b'PHMonodromyCollect,MonodromyStartParameters]
   [b'PHMonodromyCollect,MonodromyStartPoints]
   [b'PHMonodromyCollect,NumberOfLoops]
   [b'PHMonodromyCollect,SaveData]
   [b'PHMonodromyCollect,SpecifyLoops]
   B'Homogenization
   ContainsPoint
   RandomCoefficientGenerator
   NameB'Section
   B'NumberCoefficients
   B'SectionString
   [makeB'Slice,B'Homogenization]
   [makeB'Slice,B'NumberCoefficients]
   [makeB'Slice,ContainsMultiProjectivePoint]
   [makeB'Slice,ContainsPoint]
   [makeB'Slice,NameB'Slice]
   [makeB'Slice,RandomCoefficientGenerator]
   [makeB'Section,B'NumberCoefficients]
   [makeB'Section,NameB'Section]
   [runBertini,PreparePH2]
   [runBertini,TextScripts]
   [importSolutionsFile,OrderPaths]
   [importSolutionsFile,M2Precision]
   [importParameterFile,M2Precision]
   [writeParameterFile,M2Precision]
   [importMainDataFile,SpecifyDim]
   [importMainDataFile,M2Precision]
   [b'PHSequence,B'Exe]
   [b'PHSequence,SaveData]
   [makeWitnessSetFiles,SpecifyComponent]
   [writeStartFile,M2Precision]
   [makeB'Section,B'Homogenization]
   [makeB'Section,ContainsPoint]
   [makeB'Section,RandomCoefficientGenerator]
   PathVariable
   PathsWithSameEndpoint
   FunctionResidual
   ParameterValues
   CycleNumber
   VariableList
   AccuracyEst
   PrecisionIncreased
   AccuracyEstInternal
   ComponentNumber 
   [b'PHGaloisGroup,LoopRadius]
   [b'TraceTestImage,MapPoints]
   [moveB'File,MoveToDirectory]
   [b'TraceTestImage,OnlyCalculateTrace]
   [b'TraceTestImage,RandomGamma]
   [b'PHGaloisGroup,ReturnGaloisGroupGeneratorFile]
   [subPoint,SpecifyVariables]
   [b'TraceTestImage,StartParameters]
   [b'TraceTestImage,StartPoints]
   [b'TraceTestImage,StopBeforeTest]
   [b'PHGaloisGroup,StorageFolder]    
   [b'PHMonodromyCollect,StorageFolder]	   
   [b'PHSequence,StorageFolder]
   [importIncidenceMatrix,StorageFolder]
   [importParameterFile,StorageFolder]
   [importSolutionsFile,StorageFolder]
   [makeB'InputFile,StorageFolder]
   [makeMembershipFile,StorageFolder]
   [makeSampleSolutionsFile,StorageFolder]
   [makeWitnessSetFiles,StorageFolder]
   [runBertini,StorageFolder]
   [writeParameterFile,StorageFolder]
   [writeStartFile,StorageFolder]
   [b'TraceTestImage,SubFolder]
   [moveB'File,SubFolder]
   [b'TraceTestImage,SubIntoCC]
   [subPoint,SubIntoCC]
   [subPoint,M2Precision]
   [b'TraceTestImage,B'Exe]
   [b'TraceTestImage,M2Precision]
   [b'PHGaloisGroup,B'Exe]
   [b'PHGaloisGroup,BranchPoints]
   [b'PHGaloisGroup,MonodromyStartParameters]
   [b'PHGaloisGroup,MonodromyStartPoints]
   [b'PHGaloisGroup,NumberOfLoops]
   [b'PHGaloisGroup,SaveData]
   [b'PHGaloisGroup,M2Precision]
   SpecifyVariables
   MapPoints
   ReturnGaloisGroupGeneratorFile
   OnlyCalculateTrace
   StopBeforeTest
   LoopRadius
   MoveToDirectory
   RandomGamma
   StorageFolder
   SubFolder
   StartPoints
   StartParameters
   SubIntoCC	  

 Headline
   This option or key is for a function that is in active development. 
 Description
   Text
     The Bertini.m2 package is in active development for version 2 to provide additional functionality.
     This option is for a function in version 2. 
     For more information contact Jose Israel Rodriguez at JoIsRo[AT]UChicago.edu.
///;




end--ENDFILE


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

doc ///
 Key
   importPoints   
   (importPoints,String,ZZ)
 Headline
   importPoints reads solutions from a Bertini solution file to store as points in M2
 Usage
   S=importPoints(l,n) 
 Inputs
   l: String
     A string giving the  locaton of a Bertini solution file.
   n: ZZ
     Number of coordinates for each solution.
 Outputs
   S: List
     of solutions of type Point
 Description
   Text
     This method imports points from a solution file that Bertini created while solving 
     a zero-dimensional system.
     The string l is a path which gives the location of the solution file and n is
     an integer stating the desired number of coordinates for each solution. 
     When solving a zero-dimensional
     system, Bertini creates several solution files; @TO importPoints@ works 
     with the following Bertini solution files: "finite_solutions", "nonsingluar-solutions",
     "real_finite_solutions", "singluar_solutions". 
--     The output is a list of points.
     The user can specify which solutions to read from the file using the @TO SpecifyPoints@ option
     or which coordinates to select using the @TO SpecifyCoordinates@ option.
     
--   Example
     --locationOfSolutionFile="/Users/.../YourFolder/solution_file";
--     A=importPoints(locationOfSolutionFile,4)
     --The output would be a list of points that have 4 coordinates.          
--   Example 
--     locationOfSolutionFile="/Users/.../YourFolder/solution_file";
--     B=importPoints(locationOfSolutionFile,4,SpecifyPoints=>{0,2})
     --The output would be the first and third solutions of the file. 
--   Example 
--     locationOfSolutionFile="/Users/.../YourFolder/solution_file";
--     C=importPoints(locationOfSolutionFile,4,SpecifyCoordinates=>{0,1})
     --The output would be the first and second coordinate of each solution of the file.  
 --Caveat
   --The method importPoints will not
   --For importPoints to be successful, the Bertini solution file must have a particular format.

   --The first line is an integer, the number of solutions in the.
   --The next lines consist of a blank line followed by a line for each coordinate;
   --these lines consist of: RR|"e"|ZZ" "RR|"e"|ZZ for scientific notation of the real and imaginary parts of the coordinate.
///;

doc ///
 Key
   phPostProcess
   (phPostProcess,String,List,ZZ)
 Headline
   Does post processing parameter homotopy.
 Usage
   S=phPostProcess(sIn,L,n) 
 Inputs
   sIn: String
     A string giving the directory of the input files.
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
     the PrintNotes option prints a file titled "notes"  located in the input file's directory.
     If the "notes" file does not exist it returns an error.   
     
     The output will be a list of points that have 3 coordinates, that are solutions to a parameterized system of equations evaluated at L, found by doing a parameter homotopy. 
--   Example
--     inputFileLocation="/Users/.../YourFolderA";
--     L={.8234+ii*8,9}--A list of two parameter values.
--     n=3--A solution has n coordinates.
--     phPostProcess(inputFileLocation,L,n)     
--   Example
--     inputFileLocation="/Users/.../YourFolderA";
--     phPostProcess(inputFileLocation,"",{},0,PrintNotes=>1)
 Caveat
   Even if Bertini is called but does not run,  
   an error may not be reported if previous solution files were already in the outputDirectory.
///;


doc ///
 Key
   phMonodromy
   (phMonodromy,String,ZZ,ZZ)
 Headline
   Does a sequence of parameter homotopies.
 Usage
   S=phMonodromy(sIn,p,n) 
 Inputs
   sIn: String
     A string giving the directory of start files: input, start, start_parameters
   p: ZZ
     Number of parameters.
   n: ZZ
     Number of coordinates of a point.
         
///;
--ref{} need to add about the option ParameterValues

doc ///
 Key
   SpecifyPoints
   [importPoints, SpecifyPoints]
 Headline
   optional argument to specify which solutions to import
 Usage
    importPoints(...,SpecifyPoints=>List)
              
///;

doc///
 Key
   AffVariableGroup
   [makeB'InputFile, AffVariableGroup]
 Headline
   An option which designates the Affine Variable Groups.    
 Description
   Text
     We can group variables together when using zero-dimensional runs in Bertini. 
   Example
     R=QQ[x1,x2,y]
     makeB'InputFile(storeBM2Files,
	 B'Configs=>{{"MPTYPE",2}},
     	 AffVariableGroup=>{{x1,x2},{y}},
	 B'Polynomials=>{y*(x1+x2+1)^2+1,x1-x2+1,y-2})
     
///;

 
doc ///
 Key
   NameIncidenceMatrixFile
   [importIncidenceMatrix, NameIncidenceMatrixFile]
 Headline
   An optional argument to import an incidence matrix that has a different name than "incidence_matrix".
 Description
   Text
     When this option is set to "another_incidence_matrix", a file named "another_incidence_matrix" is imported. The default is "incidence_matrix".
///;

doc ///
 Key
   NameStartFile
   [writeStartFile, NameStartFile]
 Headline
   An optional argument to write a start file that has a different name than "start".
 Description
   Text
     When this option is set to "another_start_file", a file named "another_start_file" is written. The default is "start".
///;


