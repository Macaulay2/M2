





doc///
 Key
   BertiniInputConfiguration
   SecurityLevel
   ScreenOut
   OutputLevel
   StepsForIncrease
   MaxNewtonIts
   MaxStepSize
   MaxNumberSteps
   MaxCycleNum
   RegenStartLevel
   AccuracyEst
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
 Headline
   a key to designate a configuration in a Bertini input file
 Description
   Text
     These are some of the configurations that can be set in Bertini.
     Bertini ignores configurations that are not needed for a computation.
///;





------------------------------------------------------
------FUNCTIONS BERTINI VERSION 2------------
------------------------------------------------------




------------------------------------------------------------------
------SUPPORTING FUNCTIONS ------------
------------------------------------------------------------------


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
     writeStartFile(storeBM2Files,{{2,3},{4,5}});
     replaceFirstLine(storeBM2Files,"start","1")

///;


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
	 B'Polynomials=>{"x^2-1","y^2-4"});
     runBertini(storeBM2Files)
     readFile(storeBM2Files)
   Example
     makeB'InputFile(storeBM2Files,
     	 AffVariableGroup=>{x,y},
	 B'Polynomials=>{"x^2-1","y^2-4"});
     runBertini(storeBM2Files,StorageFolder=>"StoreMyFiles")
     readFile(storeBM2Files|"/StoreMyFiles")
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
     makeB'InputFile(storeBM2Files,AffVariableGroup=>{x,y},B'Polynomials=>{"x^2+2","y^2+3"});
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
    	BertiniInputConfiguration=>{{TrackType,1}},    AffVariableGroup=>{x,y,z},    B'Polynomials=>{"z*((x+y+z)^3-1)","z*(y^2-3+z)"}    );
    runBertini(storeBM2Files)
    makeSampleSolutionsFile(storeBM2Files,2,SpecifyComponent=>{1,0})
    makeMembershipFile(storeBM2Files,NameSolutionsFile=>"sample_solutions_file")
    makeMembershipFile(storeBM2Files,TestSolutions=>{{1,2,0},{3,2,1}})
    importIncidenceMatrix(storeBM2Files)

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
       BertiniInputConfiguration=>{{TrackType,1}},
       B'Polynomials=>{"(x^2+y^2+z^2-1)*y"});
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
     To do a parameter homotopy one must have a start_parameters file and a final_parameters file.
   Example
     R=QQ[x,y,t]
     makeB'InputFile(storeBM2Files,
     	 BertiniInputConfiguration=>{{"PARAMETERHOMOTOPY",1}},
	 ParameterGroup=>{t},    AffVariableGroup=>{{x,y}},
	 B'Polynomials=>{x^2-1,y^2-t});
     runBertini(storeBM2Files)
     copyFile(storeBM2Files|"/nonsingular_solutions",storeBM2Files|"/start")
     makeB'InputFile(storeBM2Files,
     	 BertiniInputConfiguration=>{{"PARAMETERHOMOTOPY",2}},
	 ParameterGroup=>{t},    AffVariableGroup=>{{x,y}},
	 B'Polynomials=>{x^2-1,y^2-t});
     writeParameterFile(storeBM2Files,{1});
     runBertini(storeBM2Files)

///;


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
     makeB'InputFile(storeBM2Files,BertiniInputConfiguration=>{{TrackType,1}},AffVariableGroup=>{x,y,z},B'Polynomials=>F);
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
    OutputStyle
    [bertiniParameterHomotopy, OutputStyle]
    [bertiniZeroDimSolve,OutputStyle]
  Headline
    Used to change the output style.
  Usage
    bertiniParameterHomotopy(...,OutputStyle=>String)
    bertiniZeroDimSolve(...,OutputStyle=>String)
  Description
    Text
      Use OutputStyle to change the style of output.
///;


-------------------
-----OPTIONS-------
-------------------


undocumented {
    "Bertini options",
    NameMainDataFile,
    NameIncidenceMatrixFile,
    NameB'Slice,
    NameFunctionFile,
    NameSampleSolutionsFile,
   NameIncidenceMatrixFile,
   NameStartFile,
   NameWitnessSliceFile,
   NameWitnessSolutionsFile,
   OrderPaths,
   PathNumber,
   PreparePH2,
    RandomComplex,
   RandomReal,
   SaveData,
   SpecifyComponent,
   SpecifyDim,
   StartParameterFileDirectory,
   TestSolutions,
   TextScripts,
   PathVariable,
   PathsWithSameEndpoint,
   FunctionResidual,
   ParameterValues,
   VariableList,
   CycleNumber,
    UseRegeneration,
   PrecisionIncreased,
   AccuracyEstInternal,
   ComponentNumber,
   B'Exe,
      DeflationsNeeded,
   Dimension,
   MaxPrecisionUtilized,
   FinalTValue,
   SolutionType,
   B'Homogenization,
   ContainsPoint,
   RandomCoefficientGenerator,
   NameB'Section,
   B'NumberCoefficients,
   SpecifyVariables,
   RandomGamma,
   StorageFolder,
   SubFolder,
   StartPoints,
   StartParameters,
   SubIntoCC,
   storeBM2Files,
   MultiplicityTol,
   SetParameterGroup,
   ReturnPoints,
   ConditionNumTol,
   ContainsMultiProjectivePoint,
   HomVariableGroup,
   importSliceFile,
   InputFileDirectory,
   ListB'Sections,
   makeB'TraceInput,
  MoveToDirectory
}
end


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
	 BertiniInputConfiguration=>{{"PARAMETERHOMOTOPY",1}},
	 AffVariableGroup=>{{x,y}},
	 ParameterGroup=>{u},
	 B'Polynomials=>{y-(x^2-1),y-u});

///;

doc///
 Key
   [makeB'InputFile, BertiniInputConfiguration]
   [bertiniParameterHomotopy,BertiniInputConfiguration]
   [bertiniZeroDimSolve,BertiniInputConfiguration]
 Headline
   An option to designate the CONFIG part of a Bertini Input file.
 Description
   Text
     This option should be set to a list of lists of 2 elements. The first element is the name of the Bertini option, e.g. "MPType" and the second element is what the Bertini option will be set to e.g. "2".
   Example
     R=QQ[x0,x1,y0,y1,z]
     makeB'InputFile(storeBM2Files,
	 BertiniInputConfiguration=>{"MPTYPE"=>2},
     	 HomVariableGroup=>{{x0,x1},{y0,y1}},
	 AffVariableGroup=>{{z}},
	 B'Polynomials=>{z*x1^2+x0^2,y0*z+y1,y0-2*z^2*y1});

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
	 BertiniInputConfiguration=>{{"MPTYPE",2}},
	 AffVariableGroup=>{{x,y}},
	 B'Polynomials=>{x+y-1,x^2-2});
   Text
     B'Polynomials can be in combination with B'Functions. B'Functions allows the user to define subfunctions.
   Example
     R=QQ[x,y,A]
     makeB'InputFile(storeBM2Files,
	 AffVariableGroup=>{{x,y}},
	 B'Functions=>{{A,x^2-1}},
	 B'Polynomials=>{A+y,x+y-2});

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
	 B'Functions=>{{f1,x+y-1},f2=>x^2-2});  --f1=x+y+1,f2=x^2-2 is written to the input file
   Text
     B'Polynomials can be in combination with B'Functions. B'Functions allows the user to define subfunctions.
   Example
     R=QQ[x,y,A]
     makeB'InputFile(storeBM2Files,
	 AffVariableGroup=>{{x,y}},
	 B'Functions=>{{A,x^2-1}},--A=x^2-1 is written to the input file
	 B'Polynomials=>{A+y,x+y-2});

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
	 B'Functions=>{{f1,x+y-1},{f2,x^2-2}}); --f1=x+y+1,f2=x^2-2 is written to the input file

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
	 NameB'InputFile=>"testInput"); --the input file will be named "testInput" rather than the default "input".

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
     writeParameterFile(storeBM2Files,{.1,.2,.5},NameParameterFile=>"testParameters");  --this function writes a parameter file named testParameters

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
     makeB'InputFile(storeBM2Files,B'Polynomials=>{"x^2-t"},ParameterGroup=>{t},BertiniInputConfiguration=>{{ParameterHomotopy,2}},AffVariableGroup=>{x},NameB'InputFile=>"inputWin");
     runBertini(storeBM2Files,NameB'InputFile=>"inputWin");
     importSolutionsFile(storeBM2Files,NameSolutionsFile=>"nonsingular_solutions")
     importSolutionsFile(storeBM2Files,NameSolutionsFile=>"real_finite_solutions")
///;




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
    [bertiniUserHomotopy,CoeffBound]
    [bertiniUserHomotopy,CondNumThreshold]
    [bertiniUserHomotopy,DegreeBound]
    [bertiniUserHomotopy,EndGameNum]
    [bertiniUserHomotopy,FinalTol]
    [bertiniUserHomotopy,ImagThreshold]
    [bertiniUserHomotopy,MaxCycleNum]
    [bertiniUserHomotopy,MaxNewtonIts]
    [bertiniUserHomotopy,MaxNorm]
    [bertiniUserHomotopy,MaxNumberSteps]
    [bertiniUserHomotopy,MaxStepSize]
    [bertiniUserHomotopy,MinStepSizeBeforeEG]
    [bertiniUserHomotopy,MinStepSizeDuringEG]
    [bertiniUserHomotopy,MPType]
    [bertiniUserHomotopy,ODEPredictor]
    [bertiniUserHomotopy,OutputLevel]
    [bertiniUserHomotopy,PRECISION]
    [bertiniUserHomotopy,RandomSeed]
    [bertiniUserHomotopy,RegenStartLevel]
    [bertiniUserHomotopy,ScreenOut]
    [bertiniUserHomotopy,SecurityLevel]
    [bertiniUserHomotopy,SingValZeroTol]
    [bertiniUserHomotopy,StepsForIncrease]
    [bertiniUserHomotopy,TrackTolBeforeEG]
    [bertiniUserHomotopy,TrackTolDuringEG]
    [bertiniUserHomotopy,UseRegeneration]
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





-------------------
-------Types-------
-------------------



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
   (calculateB'Trace,String)
   (importSliceFile,String)
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

   [bertiniImageMonodromyCollect,AffVariableGroup]
   [bertiniUserHomotopy,AffVariableGroup]
   [bertiniUserHomotopy,BertiniInputConfiguration]
   [bertiniImageMonodromyCollect,B'Constants]
   [bertiniUserHomotopy,B'Constants]
   [bertiniImageMonodromyCollect,B'Functions]
   [bertiniUserHomotopy,B'Functions]
   [bertiniImageMonodromyCollect,ContinueLoop]
   [bertiniImageMonodromyCollect,EquivalentCoordinates]
   [bertiniImageMonodromyCollect,GeneralCoordinate]
   [bertiniImageMonodromyCollect,ImageCoordinates]
   [bertiniImageMonodromyCollect,M2Precision]
   [bertiniUserHomotopy,M2Precision]
   [bertiniImageMonodromyCollect,NameB'InputFile]
   [bertiniImageMonodromyCollect,NameParameterFile]
   [bertiniImageMonodromyCollect,NameSolutionsFile]
   [bertiniImageMonodromyCollect,OnlyMoveParameters]
   [bertiniUserHomotopy,OutputStyle]
   [bertiniImageMonodromyCollect,PrintMidStatus]
   [bertiniUserHomotopy,RandomReal]
   [bertiniImageMonodromyCollect,ReturnPoints]
   [makeB'InputFile,SetParameterGroup]
   [bertiniUserHomotopy,TopDirectory]
   [bertiniImageMonodromyCollect,B'Exe]
   [bertiniImageMonodromyCollect,MonodromyStartParameters]
   [bertiniImageMonodromyCollect,MonodromyStartPoints]
   [bertiniImageMonodromyCollect,NameStartFile]
   [bertiniImageMonodromyCollect,NumberOfLoops]
   [bertiniImageMonodromyCollect,NumSolBound]
   [bertiniImageMonodromyCollect,SaveData]
   [bertiniImageMonodromyCollect,SpecifyLoops]
   [bertiniImageMonodromyCollect,StorageFolder]
   [bertiniImageMonodromyCollect,Verbose]
   [bertiniUserHomotopy,HomVariableGroup]
   [bertiniUserHomotopy,RandomComplex]


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
     A string giving the location of a Bertini solution file.
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
     with the following Bertini solution files: "finite_solutions", "nonsingular-solutions",
     "real_finite_solutions", "singular_solutions".
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
	 BertiniInputConfiguration=>{{"MPTYPE",2}},
     	 AffVariableGroup=>{{x1,x2},{y}},
	 B'Polynomials=>{y*(x1+x2+1)^2+1,x1-x2+1,y-2});

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



    [bertiniComponentMemberTest, MPType],
    [bertiniComponentMemberTest, PRECISION],
    [bertiniComponentMemberTest, ODEPredictor],
    [bertiniComponentMemberTest, TrackTolBeforeEG],
    [bertiniComponentMemberTest, TrackTolDuringEG],
    [bertiniComponentMemberTest, FinalTol],
    [bertiniComponentMemberTest, MaxNorm],
    [bertiniComponentMemberTest, MinStepSizeBeforeEG],
    [bertiniComponentMemberTest, MinStepSizeDuringEG],
    [bertiniComponentMemberTest, ImagThreshold],
    [bertiniComponentMemberTest, CoeffBound],
    [bertiniComponentMemberTest, DegreeBound],
    [bertiniComponentMemberTest, CondNumThreshold],
    [bertiniComponentMemberTest, RandomSeed],
    [bertiniComponentMemberTest, SingValZeroTol],
    [bertiniComponentMemberTest, EndGameNum],
    [bertiniComponentMemberTest, UseRegeneration],
    [bertiniComponentMemberTest, SecurityLevel],
    [bertiniComponentMemberTest, ScreenOut],
    [bertiniComponentMemberTest, OutputLevel],
    [bertiniComponentMemberTest, StepsForIncrease],
    [bertiniComponentMemberTest, MaxNewtonIts],
    [bertiniComponentMemberTest, MaxStepSize],
    [bertiniComponentMemberTest, MaxNumberSteps],
    [bertiniComponentMemberTest, MaxCycleNum],
    [bertiniComponentMemberTest, RegenStartLevel],
    [bertiniPosDimSolve, MPType],
    [bertiniPosDimSolve, PRECISION],
    [bertiniPosDimSolve, ODEPredictor],
    [bertiniPosDimSolve, TrackTolBeforeEG],
    [bertiniPosDimSolve, TrackTolDuringEG],
    [bertiniPosDimSolve, FinalTol],
    [bertiniPosDimSolve, MaxNorm],
    [bertiniPosDimSolve, MinStepSizeBeforeEG],
    [bertiniPosDimSolve, MinStepSizeDuringEG],
    [bertiniPosDimSolve, ImagThreshold],
    [bertiniPosDimSolve, CoeffBound],
    [bertiniPosDimSolve, DegreeBound],
    [bertiniPosDimSolve, CondNumThreshold],
    [bertiniPosDimSolve, RandomSeed],
    [bertiniPosDimSolve, SingValZeroTol],
    [bertiniPosDimSolve, EndGameNum],
    [bertiniPosDimSolve, UseRegeneration],
    [bertiniPosDimSolve, SecurityLevel],
    [bertiniPosDimSolve, ScreenOut],
    [bertiniPosDimSolve, OutputLevel],
    [bertiniPosDimSolve, StepsForIncrease],
    [bertiniPosDimSolve, MaxNewtonIts],
    [bertiniPosDimSolve, MaxStepSize],
    [bertiniPosDimSolve, MaxNumberSteps],
    [bertiniPosDimSolve, MaxCycleNum],
    [bertiniPosDimSolve, RegenStartLevel],
    [bertiniRefineSols, MPType],
    [bertiniRefineSols, PRECISION],
    [bertiniRefineSols, ODEPredictor],
    [bertiniRefineSols, TrackTolBeforeEG],
    [bertiniRefineSols, TrackTolDuringEG],
    [bertiniRefineSols, FinalTol],
    [bertiniRefineSols, MaxNorm],
    [bertiniRefineSols, MinStepSizeBeforeEG],
    [bertiniRefineSols, MinStepSizeDuringEG],
    [bertiniRefineSols, ImagThreshold],
    [bertiniRefineSols, CoeffBound],
    [bertiniRefineSols, DegreeBound],
    [bertiniRefineSols, CondNumThreshold],
    [bertiniRefineSols, RandomSeed],
    [bertiniRefineSols, SingValZeroTol],
    [bertiniRefineSols, EndGameNum],
    [bertiniRefineSols, UseRegeneration],
    [bertiniRefineSols, SecurityLevel],
    [bertiniRefineSols, ScreenOut],
    [bertiniRefineSols, OutputLevel],
    [bertiniRefineSols, StepsForIncrease],
    [bertiniRefineSols, MaxNewtonIts],
    [bertiniRefineSols, MaxStepSize],
    [bertiniRefineSols, MaxNumberSteps],
    [bertiniRefineSols, MaxCycleNum],
    [bertiniRefineSols, RegenStartLevel],
    [bertiniSample, MPType],
    [bertiniSample, PRECISION],
    [bertiniSample, ODEPredictor],
    [bertiniSample, TrackTolBeforeEG],
    [bertiniSample, TrackTolDuringEG],
    [bertiniSample, FinalTol],
    [bertiniSample, MaxNorm],
    [bertiniSample, MinStepSizeBeforeEG],
    [bertiniSample, MinStepSizeDuringEG],
    [bertiniSample, ImagThreshold],
    [bertiniSample, CoeffBound],
    [bertiniSample, DegreeBound],
    [bertiniSample, CondNumThreshold],
    [bertiniSample, RandomSeed],
    [bertiniSample, SingValZeroTol],
    [bertiniSample, EndGameNum],
    [bertiniSample, UseRegeneration],
    [bertiniSample, SecurityLevel],
    [bertiniSample, ScreenOut],
    [bertiniSample, OutputLevel],
    [bertiniSample, StepsForIncrease],
    [bertiniSample, MaxNewtonIts],
    [bertiniSample, MaxStepSize],
    [bertiniSample, MaxNumberSteps],
    [bertiniSample, MaxCycleNum],
    [bertiniSample, RegenStartLevel],
    [bertiniTrackHomotopy, MPType],
    [bertiniTrackHomotopy, PRECISION],
    [bertiniTrackHomotopy, ODEPredictor],
    [bertiniTrackHomotopy, TrackTolBeforeEG],
    [bertiniTrackHomotopy, TrackTolDuringEG],
    [bertiniTrackHomotopy, FinalTol],
    [bertiniTrackHomotopy, MaxNorm],
    [bertiniTrackHomotopy, MinStepSizeBeforeEG],
    [bertiniTrackHomotopy, MinStepSizeDuringEG],
    [bertiniTrackHomotopy, ImagThreshold],
    [bertiniTrackHomotopy, CoeffBound],
    [bertiniTrackHomotopy, DegreeBound],
    [bertiniTrackHomotopy, CondNumThreshold],
    [bertiniTrackHomotopy, RandomSeed],
    [bertiniTrackHomotopy, SingValZeroTol],
    [bertiniTrackHomotopy, EndGameNum],
    [bertiniTrackHomotopy, UseRegeneration],
    [bertiniTrackHomotopy, SecurityLevel],
    [bertiniTrackHomotopy, ScreenOut],
    [bertiniTrackHomotopy, OutputLevel],
    [bertiniTrackHomotopy, StepsForIncrease],
    [bertiniTrackHomotopy, MaxNewtonIts],
    [bertiniTrackHomotopy, MaxStepSize],
    [bertiniTrackHomotopy, MaxNumberSteps],
    [bertiniTrackHomotopy, MaxCycleNum],
    [bertiniTrackHomotopy, RegenStartLevel],
    [bertiniUserHomotopy,CoeffBound],
    [bertiniUserHomotopy,CondNumThreshold],
    [bertiniUserHomotopy,DegreeBound],
    [bertiniUserHomotopy,EndGameNum],
    [bertiniUserHomotopy,FinalTol],
    [bertiniUserHomotopy,ImagThreshold],
    [bertiniUserHomotopy,MaxCycleNum],
    [bertiniUserHomotopy,MaxNewtonIts],
    [bertiniUserHomotopy,MaxNorm],
    [bertiniUserHomotopy,MaxNumberSteps],
    [bertiniUserHomotopy,MaxStepSize],
    [bertiniUserHomotopy,MinStepSizeBeforeEG],
    [bertiniUserHomotopy,MinStepSizeDuringEG],
    [bertiniUserHomotopy,MPType],
    [bertiniUserHomotopy,ODEPredictor],
    [bertiniUserHomotopy,OutputLevel],
    [bertiniUserHomotopy,PRECISION],
    [bertiniUserHomotopy,RandomSeed],
    [bertiniUserHomotopy,RegenStartLevel],
    [bertiniUserHomotopy,ScreenOut],
    [bertiniUserHomotopy,SecurityLevel],
    [bertiniUserHomotopy,SingValZeroTol],
    [bertiniUserHomotopy,StepsForIncrease],
    [bertiniUserHomotopy,TrackTolBeforeEG],
    [bertiniUserHomotopy,TrackTolDuringEG],
    [bertiniUserHomotopy,UseRegeneration],
    [bertiniZeroDimSolve, MPType],
    [bertiniZeroDimSolve, PRECISION],
    [bertiniZeroDimSolve, ODEPredictor],
    [bertiniZeroDimSolve, TrackTolBeforeEG],
    [bertiniZeroDimSolve, TrackTolDuringEG],
    [bertiniZeroDimSolve, FinalTol],
    [bertiniZeroDimSolve, MaxNorm],
    [bertiniZeroDimSolve, MinStepSizeBeforeEG],
    [bertiniZeroDimSolve, MinStepSizeDuringEG],
    [bertiniZeroDimSolve, ImagThreshold],
    [bertiniZeroDimSolve, CoeffBound],
    [bertiniZeroDimSolve, DegreeBound],
    [bertiniZeroDimSolve, CondNumThreshold],
    [bertiniZeroDimSolve, RandomSeed],
    [bertiniZeroDimSolve, SingValZeroTol],
    [bertiniZeroDimSolve, EndGameNum],
    [bertiniZeroDimSolve, UseRegeneration],
    [bertiniZeroDimSolve, SecurityLevel],
    [bertiniZeroDimSolve, ScreenOut],
    [bertiniZeroDimSolve, OutputLevel],
    [bertiniZeroDimSolve, StepsForIncrease],
    [bertiniZeroDimSolve, MaxNewtonIts],
    [bertiniZeroDimSolve, MaxStepSize],
    [bertiniZeroDimSolve, MaxNumberSteps],
    [bertiniZeroDimSolve, MaxCycleNum],
    [bertiniZeroDimSolve, RegenStartLevel]



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
       BertiniInputConfiguration=>{{TrackType,1}},
       B'Polynomials=>{"(x^2+y^2+z^2-1)*y"});
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
