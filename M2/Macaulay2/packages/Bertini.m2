needsPackage "NAGtypes"
newPackage(
  "Bertini",
  Version => "2.1.2.2", 
  Date => "June 6, 2016",
  Authors => {
    {Name => "Elizabeth Gross",
     Email=> "elizabeth.gross@sjsu.edu",
     HomePage => "http://math.sjsu.edu/~egross"},
    {Name => "Jose Israel Rodriguez",
     Email => "JoIsRo@UChicago.edu",
     HomePage =>"http://home.uchicago.edu/~joisro"},
    {Name => "Dan Bates",
     Email => "bates@math.colostate.edu",
     HomePage => "http://www.math.colostate.edu/~bates"}, 
    {Name => "Anton Leykin",
     Email => "leykin@math.gatech.edu",
     HomePage => "http://www.math.gatech.edu/~leykin"}
  }, 
  Headline => "Interface to Bertini",
  Configuration => { "BERTINIexecutable"=>"bertini" },
 -- DebuggingMode => true,
  DebuggingMode => true,
  AuxiliaryFiles => true,
  CacheExampleOutput => true
) 

export {
  "OutputSyle",
  "TopDirectory",
  "StorageFolder", 
  "ReturnGaloisGroupGeneratorFile",
  "StopBeforeTest",
  "MapPoints",
  "RandomGamma",
  "SubFolder",
  "StartParameters",
  "StartPoints",
  "OnlyCalculateTrace",
  "b'TraceTestImage", 
  "subPoint",
  "OrderPaths",
  "storeBM2Files",
  "bertiniZeroDimSolve",
  "bertiniParameterHomotopy",
  "bertiniPosDimSolve",
  "bertiniSample",
  "bertiniTrackHomotopy",
  "bertiniComponentMemberTest",
  "bertiniRefineSols",    
  "MultiplicityTol",
  "ConditionNumTol",
  "MPType",  
  "PRECISION",
  "IsProjective",  
  "ODEPredictor",  
  "TrackTolBeforeEG",  
  "TrackTolDuringEG",
  "FinalTol",  
  "MaxNorm",  
  "MinStepSizeBeforeEG",  
  "MinStepSizeDuringEG",  
  "ImagThreshold",
  "CoeffBound",  
  "DegreeBound",  
  "CondNumThreshold",  
  "RandomSeed",  
  "SingValZeroTol",
  "EndGameNum", 
  "UseRegeneration",  
  "SecurityLevel",  
  "ScreenOut",  
  "OutputLevel",
  "StepsForIncrease",  
  "MaxNewtonIts",  
  "MaxStepSize",  
  "MaxNumberSteps",  
  "MaxCycleNum",
  "RegenStartLevel",
  "ParameterValues",
  "NameB'InputFile",--This option allows us to change the name of the input file.
  "NameParameterFile",
  "NameSolutionsFile",
  "NameIncidenceMatrixFile",
  "NameStartFile",
  "NameFunctionFile",
--
  "makeB'InputFile",
  "B'Configs", --This option is a list of pairs of strings. These will be written in the CONFIG part of the Bertini input file. 
  "HomVariableGroup", --A list of lists of homogeneous variable groups. 
  "AffVariableGroup", --A list of lists of affine variable groups. 	
  "ParameterGroup",
  "VariableList",
  "PathVariable",
  "RandomComplex",
  "RandomReal",  --a list of unknowns whose values will be fixed by Bertini
  "B'Constants",--A list of pairs
  "B'Polynomials", --a list of polynomials whose zero set we want to solve; when used then the NamePolynomials option is disabled and the polynomials are automatically named "jade"  	    	  
  "NamePolynomials", --A list of names of the polynomials which we want to find the common zero set of.
  "B'Functions", --A list of list of pairs.  
--
  "runBertini",
  "InputFileDirectory",
  "StartFileDirectory",
  "StartParameterFileDirectory",  
  "B'Exe",
  "NumberToB'String",
  "M2Precision",--needs doc
  "writeParameterFile",
  "writeStartFile",  
  "importParameterFile",   --need doc
--  "b'TraceTest", Depracated.
  "calculateB'Trace",
  "UseStartPointsFirst",
  "b'PHSequence"   ,
  "b'PHMonodromyCollect",
  "importSolutionsFile",
  "importIncidenceMatrix",
  "SaveData",
  "MonodromyStartPoints",
  "MonodromyStartParameters",
  "NumberOfLoops",
  "NumSolBound",
  "SpecifyLoops",
  "b'PHGaloisGroup",
  "LoopRadius",
  "NameGaloisGroupGeneratorFile",
  "BranchPoints",
  "SolutionFileStyle",
  "B'Section",
  "B'Slice",
  "radicalList",
--  "B'MultiProjectivePoint",
  "makeB'Section",
  "makeB'Slice",
  "ContainsPoint", 
  "B'NumberCoefficients", 
  "B'Homogenization", 
  "RandomCoefficientGenerator", 
  "B'SectionString",
  "NameB'Section",
  "ContainsMultiProjectivePoint",--Eventually we will want to have multiprojective points.
  "NameB'Slice",
  "ListB'Sections",
  "makeB'TraceInput",
  "replaceFirstLine",
  "PreparePH2",
  "readFile",
  "valueBM2",
  "NameMainDataFile",
--  "linesPerSolutions",
  "PathNumber",
  "FinalTValue",
  "MaxPrecisionUtilized",
  "PrecisionIncreased", 
  "AccuracyEstInternal", 
  "AccuracyEst", 
  "PathsWithSameEndpoint",
  "importMainDataFile",
  "CycleNumber",
  "FunctionResidual",
  "Dimension",
  "SolutionType",
  "DeflationsNeeded",
--  "B'WitnessSet",
  "SpecifyDim",
  "NameWitnessSliceFile",
  "importSliceFile",
  "TextScripts",
  "NameWitnessSolutionsFile",
  "SpecifyComponent",
  "makeWitnessSetFiles",
  "makeSampleSolutionsFile",
  "NameSampleSolutionsFile",
  "TestSolutions",
  "makeMembershipFile",
  "ComponentNumber",
  "sortMainDataComponents",
  "moveB'File",
  "CopyB'File",
  "MoveToDirectory",
  "SpecifyVariables",
  "SubIntoCC"
    }
  
  protect SolutionNumber
  protect StartSystem
  protect NewtonResidual
  protect MaximumPrecision
  protect runType
  protect compnum
  protect dimen
  protect numpts
  protect digits
  protect RawData
  protect WitnessData
  protect WitnessDataFileName
  protect ComponentNumber
  protect NVariety
  protect PathVariable
--  protect Parameters -- used in NAGtypes
  protect ParameterValues
  protect CycleNumber
  protect FunctionResidual
  protect StartSolutions
  protect FailedPath
  protect AllowStrings
       
needsPackage "NAGtypes"

--##########################################################################--
-- GLOBAL VARIABLES 
--##########################################################################--

DBG = 0 -- debug level (10=keep temp files)
BERTINIexe=(options Bertini).Configuration#"BERTINIexecutable"

needsPackage "SimpleDoc"
     storeBM2Files = temporaryFileName()
     makeDirectory storeBM2Files
-- Bertini interface for M2
-- used by ../NumericalAlgebraicGeometry.m2

-- The following seven exported methods are front ends for various Bertini 
-- functions.
-- Each calls bertiniSolve() with the appropriate input data and 
-- toggle (corresp. to the type of run).
-- bertiniSolve then does all the work of building the input file, 
-- calling bertini, and calling the appropriate output parser. 
knownConfigs={
	MPType=>-1,PRECISION=>-1,ODEPredictor=>-1,
	TrackTolBeforeEG=>-1,TrackTolDuringEG=>-1,FinalTol=>-1,MaxNorm=>-1,
	MinStepSizeBeforeEG=>-1,MinStepSizeDuringEG=>-1,ImagThreshold=>-1,
	CoeffBound=>-1,DegreeBound=>-1,CondNumThreshold=>-1,RandomSeed=>-1,
	SingValZeroTol=>-1,EndGameNum=>-1,UseRegeneration=>-1,SecurityLevel=>-1,
	ScreenOut=>-1,OutputLevel=>-1,StepsForIncrease=>-1,MaxNewtonIts=>-1,
	MaxStepSize=>-1,MaxNumberSteps=>-1,MaxCycleNum=>-1,RegenStartLevel=>-1
	}

bertiniZeroDimSolve = method(TypicalValue => List, Options=>knownConfigs|{
    	OutputSyle=>"OutPoints",--{"OutPoints","OutSolutions","OutNone"}--The output can be lists of Points (A muteable hash table), or lists of Solutions (list of complex numbers that are coordinates), or can be None (All information is stored on as a text file in the directory where the computation was ran).
    	TopDirectory=>storeBM2Files,
	B'Configs=>{},
	AffVariableGroup=>{},
	HomVariableGroup=>{},
      	RandomComplex=>{}, --A list or a list of list of symbols that denote random complex numbers.
      	RandomReal=>{}, --A list or a list of list of symbols that denote random real numbers.
      	B'Constants=>{},--A list of pairs. Each pair consists of a symbol that will be set to a string and a number. 
      	B'Functions=>{},--A list of pairs consisting of a name and a polynomial.  	
    	NameSolutionsFile=>"raw_solutions",
    	NameMainDataFile=>"main_data",
	M2Precision=>53,
    	Verbose=>1
	} )
bertiniZeroDimSolve(List) := o -> (myPol) ->(        
    --myPol are your polynomial system that you want to solve.
--%%--Bertini is text based. So directories have to be specified to store these text files which are read by Bertini. 
--%%%%--When loading Bertini.m2 a temporary directory is made where files are stored by default: storeBM2Files. 
--%%%%--To change the default directory, set the TopDirectory option to the directory you would like.
  myTopDir:=o.TopDirectory;
--%%-- We set AffVariableGroup and HomVariableGroup. If the user does not specify these groups then AffVariableGroup is taken to be the generators of the ring the first element of myPol. 
  myAVG:= o.AffVariableGroup;
  myHVG:= o.HomVariableGroup;
  if myAVG==={} and myHVG==={} 
  then (
    if not member (class first myPol,{String,B'Section,B'Slice,Product})
    then (myAVG=gens ring first myPol)
  else error"AffVariableGroup or HomVariableGroup need to be set. "    );
--%%-- Verbose set greater than 1 will print the variable groups.
  if o.Verbose>1 then print myAVG;
  if o.Verbose>1 then  print myHVG;
--%%--We need to set the CONFIGS of the Bertini input file. 
--%%%%--These CONFIGS come in two flavors: 
--%%%%--If the same configuration is set twice then Bertini will use the one set last.
--%%%%--The first is in B'Configs wher we just list the configurations. 
  myConfigs:=(o.B'Configs);
--%%%%--The second is as individual options from 'knownConfigs' (search in Beritni.m2 to see the knownConfigs).
    if o.MPType=!=-1 then myConfigs=append(myConfigs,{"MPType",o.MPType});
    if o.PRECISION=!=-1 then myConfigs=append(myConfigs,{"PRECISION",o.PRECISION});
    if o.ODEPredictor=!=-1 then myConfigs=append(myConfigs,{"ODEPredictor",o.ODEPredictor});
    if o.TrackTolBeforeEG=!=-1 then myConfigs=append(myConfigs,{"TrackTolBeforeEG",o.TrackTolBeforeEG});
    if o.TrackTolDuringEG=!=-1 then myConfigs=append(myConfigs,{"TrackTolDuringEG",o.TrackTolDuringEG});
    if o.FinalTol=!=-1 then myConfigs=append(myConfigs,{"FinalTol",o.FinalTol});
    if o.MinStepSizeBeforeEG=!=-1 then myConfigs=append(myConfigs,{"MaxNorm",o.MaxNorm});
    if o.MPType=!=-1 then myConfigs=append(myConfigs,{"MinStepSizeBeforeEG",o.MinStepSizeBeforeEG});
    if o.ImagThreshold=!=-1 then myConfigs=append(myConfigs,{"ImagThreshold",o.ImagThreshold});
    if o.CoeffBound=!=-1 then myConfigs=append(myConfigs,{"CoeffBound",o.CoeffBound});
    if o.DegreeBound=!=-1 then myConfigs=append(myConfigs,{"DegreeBound",o.DegreeBound});
    if o.CondNumThreshold=!=-1 then myConfigs=append(myConfigs,{"CondNumThreshold",o.CondNumThreshold});
    if o.RandomSeed=!=-1 then myConfigs=append(myConfigs,{"RandomSeed",o.RandomSeed});
    if o.SingValZeroTol=!=-1 then myConfigs=append(myConfigs,{"SingValZeroTol",o.SingValZeroTol});
    if o.EndGameNum=!=-1 then myConfigs=append(myConfigs,{"EndGameNum",o.EndGameNum});
    if o.UseRegeneration=!=-1 then myConfigs=append(myConfigs,{"UseRegeneration",o.UseRegeneration});
    if o.SecurityLevel=!=-1 then myConfigs=append(myConfigs,{"SecurityLevel",o.SecurityLevel});
    if o.ScreenOut=!=-1 then myConfigs=append(myConfigs,{"ScreenOut",o.ScreenOut});
    if o.OutputLevel=!=-1 then myConfigs=append(myConfigs,{"OutputLevel",o.OutputLevel});
    if o.StepsForIncrease=!=-1 then myConfigs=append(myConfigs,{"StepsForIncrease",o.StepsForIncrease});
    if o.MaxNewtonIts=!=-1 then myConfigs=append(myConfigs,{"MaxNewtonIts",o.MaxNewtonIts});
    if o.MaxStepSize=!=-1 then myConfigs=append(myConfigs,{"MaxStepSize",o.MaxStepSize});
    if o.MaxNumberSteps=!=-1 then myConfigs=append(myConfigs,{"MaxNumberSteps",o.MaxNumberSteps});
    if o.MaxCycleNum=!=-1 then myConfigs=append(myConfigs,{"MaxCycleNum",o.MaxCycleNum});
    if o.RegenStartLevel=!=-1 then myConfigs=append(myConfigs,{"RegenStartLevel",o.RegenStartLevel});    
--    print myConfigs;
--%%-- We use the makeB'InputFile method to write a Bertini file. 
  makeB'InputFile(myTopDir,
    B'Polynomials=>myPol,
    AffVariableGroup=>myAVG,
    HomVariableGroup=>myHVG,
--%%--These are extra options the user can specify. For more information refer to their documentation.
    B'Configs=>o.B'Configs,
    RandomComplex=>o.RandomComplex,--A list or a list of list of symbols that denote random complex numbers.
    RandomReal=>o.RandomReal, --A list or a list of list of symbols that denote random real numbers.
    B'Constants=>o.B'Constants,--A list of pairs. Each pair consists of a symbol that will be set to a string and a number. 
    B'Functions=>o.B'Functions--A list of pairs consisting of a name and a polynomial.  	
    );
--%%--Check for some errors.
--%%%%--
  if o.NameSolutionsFile=!="raw_solutions" and o.OutputSyle=!="OutSolutions" 
  then error"If NameSolutionsFile is set then OutputSyle should be set to OutSolutions. ";
--%%--We call Bertini and solve the zero dimensional system. 
    successRun:=runBertini(myTopDir,Verbose=>o.Verbose);
--    print successRun;
--%%--After completing the Bertini runs we import the results into Macaulay2; this is the list called theSols below.
--%%%%--Depending on the OutputStyle option we import nothing, main_data files to give Points, or raw_solutions files. 
    if o.OutputSyle==="OutPoints" 
    then theSols:=importMainDataFile(myTopDir,NameMainDataFile=>o.NameMainDataFile,M2Precision=>o.M2Precision);
    if o.OutputSyle==="OutSolutions" 
    then theSols=importSolutionsFile(myTopDir,NameSolutionsFile=>o.NameSolutionsFile,OrderPaths=>true,M2Precision=>o.M2Precision);
--
    if o.OutputSyle=!="OutNone"
    then return theSols)   
         
--For zero dim solve OutStyle and NameSolutionsFile need to both be changed.
--Do an error for this. 

 
bertiniPosDimSolve = method(TypicalValue => NumericalVariety, Options=>{
	Verbose=>true,MPType=>-1,PRECISION=>-1,
	IsProjective=>-1,ODEPredictor=>-1,TrackTolBeforeEG=>-1,
	TrackTolDuringEG=>-1,FinalTol=>-1,MaxNorm=>-1,MinStepSizeBeforeEG=>-1,
	MinStepSizeDuringEG=>-1,ImagThreshold=>-1,CoeffBound=>-1,DegreeBound=>-1,
	CondNumThreshold=>-1,RandomSeed=>-1,SingValZeroTol=>-1,EndGameNum=>-1,
	UseRegeneration=>-1,SecurityLevel=>-1,ScreenOut=>-1,OutputLevel=>-1,
	StepsForIncrease=>-1,MaxNewtonIts=>-1,MaxStepSize=>-1,MaxNumberSteps=>-1,
	MaxCycleNum=>-1,RegenStartLevel=>-1})
bertiniPosDimSolve List := o -> F -> (  
--F is the list of polynomials
         L := {runType=>2};
         o2 := new OptionTable from L;
         o3 := o ++ o2;
         bertiniSolve(F,o3)
         ) 

bertiniSample = method(TypicalValue => List, Options=>{Verbose=>true, MPType=>-1,
	PRECISION=>-1, IsProjective=>-1,ODEPredictor=>-1,TrackTolBeforeEG=>-1,
	TrackTolDuringEG=>-1,FinalTol=>-1,MaxNorm=>-1,MinStepSizeBeforeEG=>-1,
	MinStepSizeDuringEG=>-1,ImagThreshold=>-1,CoeffBound=>-1,DegreeBound=>-1,
	CondNumThreshold=>-1,RandomSeed=>-1,SingValZeroTol=>-1,EndGameNum=>-1,
	UseRegeneration=>-1,SecurityLevel=>-1,ScreenOut=>-1,OutputLevel=>-1,
	StepsForIncrease=>-1,MaxNewtonIts=>-1,MaxStepSize=>-1,MaxNumberSteps=>-1,
	MaxCycleNum=>-1,RegenStartLevel=>-1})
bertiniSample (ZZ, WitnessSet) := o -> (n, W) -> (  
--W is a witness set
-- n is the number of points to sample
         L := {runType=>3,dimen=>dim W, compnum=>W.ComponentNumber,numpts=>n, 
	     WitnessData=>W.WitnessDataFileName};
	 o2 := new OptionTable from L;
         o3 := o ++ o2 ;
         bertiniSolve(W.Equations,o3)
         ) 


bertiniComponentMemberTest = method(TypicalValue => List, Options=>{Verbose=>true, 
	MPType=>-1, 
	PRECISION=>-1,IsProjective=>-1,ODEPredictor=>-1,TrackTolBeforeEG=>-1,
	TrackTolDuringEG=>-1,FinalTol=>-1,MaxNorm=>-1,MinStepSizeBeforeEG=>-1,
	MinStepSizeDuringEG=>-1,ImagThreshold=>-1,CoeffBound=>-1,DegreeBound=>-1,
	CondNumThreshold=>-1,RandomSeed=>-1,SingValZeroTol=>-1,EndGameNum=>-1,
	UseRegeneration=>-1,SecurityLevel=>-1,ScreenOut=>-1,OutputLevel=>-1,
	StepsForIncrease=>-1,MaxNewtonIts=>-1,MaxStepSize=>-1,MaxNumberSteps=>-1,
	MaxCycleNum=>-1,RegenStartLevel=>-1})
bertiniComponentMemberTest (List, NumericalVariety) := o -> (pts, NV) -> (  
--pts, list of pts to test 
--NV, numerical variety
	 L := {runType=>4, StartSolutions=>pts, WitnessData=>NV.WitnessDataFileName, 
	     NVariety=>NV};
         o2 := new OptionTable from L;
         o3 := o ++ o2;
         bertiniSolve(NV.Equations, o3)
         ) 

bertiniRefineSols = method(TypicalValue => List, Options=>{Verbose=>true, 
	MPType=>-1, 
	PRECISION=>-1,IsProjective=>-1,ODEPredictor=>-1,TrackTolBeforeEG=>-1,
	TrackTolDuringEG=>-1,FinalTol=>1e-4,MaxNorm=>-1,MinStepSizeBeforeEG=>-1,
	MinStepSizeDuringEG=>-1,ImagThreshold=>-1,CoeffBound=>-1,
	DegreeBound=>-1,CondNumThreshold=>-1,RandomSeed=>-1,SingValZeroTol=>-1,
	EndGameNum=>-1,UseRegeneration=>-1,SecurityLevel=>-1,ScreenOut=>-1,
	OutputLevel=>-1,StepsForIncrease=>-1,MaxNewtonIts=>-1,MaxStepSize=>-1,
	MaxNumberSteps=>-1,MaxCycleNum=>-1,RegenStartLevel=>-1})
bertiniRefineSols (ZZ, List, List) := o -> (d, F,p) -> ( 
--d, number of digits
--F is the list of polynomials.
--p, list of points to sharpen
         L := {runType=>5, StartSolutions=>p, digits=>d};
         o2 := new OptionTable from L;
         o3 := o ++ o2;
         bertiniSolve(F, o3)
         ) 


bertiniTrackHomotopy = method(TypicalValue => List, Options=>{
	  Verbose=>true,MPType=>-1,PRECISION=>-1, 
	  IsProjective=>-1,ODEPredictor=>-1,TrackTolBeforeEG=>-1,
	  TrackTolDuringEG=>-1,FinalTol=>-1,MaxNorm=>-1,MinStepSizeBeforeEG=>-1,
	  MinStepSizeDuringEG=>-1,ImagThreshold=>-1,CoeffBound=>-1,
	  DegreeBound=>-1,CondNumThreshold=>-1,RandomSeed=>-1,
	  SingValZeroTol=>-1,EndGameNum=>-1,UseRegeneration=>-1,
	  SecurityLevel=>-1,ScreenOut=>-1,OutputLevel=>-1,StepsForIncrease=>-1,
	  MaxNewtonIts=>-1,MaxStepSize=>-1,MaxNumberSteps=>-1,MaxCycleNum=>-1,
	  RegenStartLevel=>-1} )
bertiniTrackHomotopy (RingElement, List, List) := o -> (t, H, S1) -> (
--t, path variable
--H, homotopy
--S1, solutions to start system     
         L := {runType=>6, StartSolutions=>S1, PathVariable=>t};
         o2 := new OptionTable from L;
         o3 := o ++ o2;
         bertiniSolve(H,o3)
         )

bertiniParameterHomotopy = method(TypicalValue => List, Options=>{
    	OutputSyle=>"OutPoints",--{"OutPoints","OutSolutions","OutNone"}--The output can be lists of Points (A muteable hash table), or lists of Solutions (list of complex numbers that are coordinates), or can be None (All information is stored on as a text file in the directory where the computation was ran).
    	TopDirectory=>storeBM2Files,
    	B'Functions=>{},
	B'Configs=>{},
	AffVariableGroup=>{},
	HomVariableGroup=>{},
      	RandomComplex=>{}, --A list or a list of list of symbols that denote random complex numbers.
      	RandomReal=>{}, --A list or a list of list of symbols that denote random real numbers.
      	B'Constants=>{},--A list of pairs. Each pair consists of a symbol that will be set to a string and a number. 
      	B'Functions=>{},--A list of pairs consisting of a name and a polynomial.  	
    	M2Precision=>53,
	Verbose=>1
	} )
bertiniParameterHomotopy (List, List, List) := o -> (myPol, myParams, myParValues) ->(
    --myPol are your polynomial system that you want to solve.
    --myParams are your parameters.
    --myParValues are the values the parametres will take.
--%%--Bertini is text based. So directories have to be specified to store these text files which are read by Bertini. 
--%%%%--When loading Bertini.m2 a temporary directory is made where files are stored by default: storeBM2Files. 
--%%%%--To change the default directory, set the TopDirectory option to the directory you would like.
  myTopDir:=o.TopDirectory;
--%%-- We set AffVariableGroup and HomVariableGroup. If the user does not specify these groups then AffVariableGroup is taken to be the generators of the ring the first element of myPol with myParams deleted. 
  myAVG:= o.AffVariableGroup;
  myHVG:= o.HomVariableGroup;
  if myAVG==={} and myHVG==={} 
  then (
    if not member (class first myPol,{String,B'Section,B'Slice,Product})
    then (myAVG=gens ring first myPol;
      for i in myParams do myAVG=delete(i,myAVG))
  else error"AffVariableGroup or HomVariableGroup need to be set. "    );
--  print myAVG;
--  print myHVG;
--%%-- We use the makeB'InputFile method to write a Bertini file. 
  makeB'InputFile(myTopDir,
    ParameterGroup=>myParams,
    B'Polynomials=>myPol,
    AffVariableGroup=>myAVG,
    HomVariableGroup=>myHVG,
--%%--These are extra options the user can specify. For more information refer to their documentation.
    B'Configs=>({{ParameterHomotopy,1}}|o.B'Configs),
    B'Functions=>o.B'Functions,
    RandomComplex=>o.RandomComplex,--A list or a list of list of symbols that denote random complex numbers.
    RandomReal=>o.RandomReal, --A list or a list of list of symbols that denote random real numbers.
    B'Constants=>o.B'Constants,--A list of pairs. Each pair consists of a symbol that will be set to a string and a number. 
    B'Functions=>o.B'Functions--A list of pairs consisting of a name and a polynomial.  	
    );
--%%--We call Bertini and solve the parameter homotopy for random parameters.
--%%%%--The PreparePH2=>true, will automatically adjust the Bertini input file to set ParameterHomotopy=2.
--&&&&--Refer to the Bertini manual for more details on parameter homotopies.
    runBertini(myTopDir,PreparePH2=>true,Verbose=>o.Verbose);
--%%--For each set of parameter values, i.e. each element of myParValues we will do a Bertini run. 
--%%%%--The output of run # will be stored as a text file named "ph_jade_#".
--%%%%--Depending on the OutputSyle option, the style of this text file can be main_data or a list of coordinates.
    runNumber:=0;
    for i in myParValues do(
      writeParameterFile(myTopDir,i);
      runBertini(myTopDir,Verbose=>o.Verbose);
      if o.OutputSyle==="OutPoints" then moveB'File(myTopDir,"main_data","ph_jade_"|runNumber);
      if o.OutputSyle==="OutNone" then moveB'File(myTopDir,"raw_solutions","ph_jade_"|runNumber);
      if o.OutputSyle==="OutSolutions" then moveB'File(myTopDir,"raw_solutions","ph_jade_"|runNumber);
      runNumber=runNumber+1
      );
--%%--After completing the Bertini runs we import the results into Macaulay2; this is the list called allSols below.
--%%%%--Depending on the OutputStyle option we import nothing, main_data files to give Points, or raw_solutions files. 
    allSols:={};
    if o.OutputSyle==="OutPoints" 
    then for i from 0 to #myParValues-1 do allSols=allSols|{importMainDataFile(myTopDir,M2Precision=>o.M2Precision,NameMainDataFile=>"ph_jade_"|i)};
    if o.OutputSyle==="OutSolutions" 
    then for i from 0 to #myParValues-1 do allSols=allSols|{importSolutionsFile(myTopDir,NameSolutionsFile=>"ph_jade_"|i,OrderPaths=>true,M2Precision=>o.M2Precision)};
--
    if o.OutputSyle=!="OutNone"
    then return allSols)   
         

---------------------------------------------------
-- bertiniSolve: This is the main control function:
---------------------------------------------------

bertiniSolve = method(TypicalValue => List, Options=>{
	AllowStrings=>-1,
	Verbose=>true,MultiplicityTol=>1e-6,ConditionNumTol=>1e10,
	IsProjective=>-1,Parameters=>null,ParameterValues=>null,StartSystem=>{},
	StartSolutions=>{},NVariety=>null, RawData=>null,WitnessData=>null,
	MPType=>-1,PRECISION=>-1,IsProjective=>-1,ODEPredictor=>-1,
	TrackTolBeforeEG=>-1,TrackTolDuringEG=>-1,FinalTol=>-1,MaxNorm=>-1,
	MinStepSizeBeforeEG=>-1,MinStepSizeDuringEG=>-1,ImagThreshold=>-1,
	CoeffBound=>-1,DegreeBound=>-1,CondNumThreshold=>-1,RandomSeed=>-1,
	SingValZeroTol=>-1,EndGameNum=>-1,UseRegeneration=>-1,SecurityLevel=>-1,
	ScreenOut=>-1,OutputLevel=>-1,StepsForIncrease=>-1,MaxNewtonIts=>-1,
	MaxStepSize=>-1,MaxNumberSteps=>-1,MaxCycleNum=>-1,RegenStartLevel=>-1,
	dimen=>-1,compnum=>-1,numpts=>-1,Points=>{},digits=>-1,runType=>0,
	PathVariable=>null})
bertiniSolve List := o -> F -> (  -- F is the list of polynomials
	  dir := makeBertiniInput(F,o);   -- creates the input file
          if o.Verbose==true then stdio << "The version of Bertini 
	    you have installed on your computer 
	    was used for this run. \nBertini is under ongoing development by 
	    D. Bates, J. Hauenstein, A. Sommese, and C. Wampler.\n\n";
          
	  --if o.WriteOnly=!=-1 then break "Write Only";
	  
	  if o.runType == 0 then ( -- ZeroDim 
    	    run("cd "|dir|"; "|BERTINIexe|" >bertini_session.log");  
	    -- runs Bertini, storing screen output to bertini_session.log
            );
	
          if o.runType == 1 then ( -- segment homotopy
    	    run("cd "|dir|"; "|BERTINIexe|" >bertini_session.log");  
	    -- runs Bertini, storing screen output to bertini_session.log
            );
          
	  if o.runType == 2 then ( -- PosDim 
    	    run("cd "|dir|"; "|BERTINIexe|" >bertini_session.log");  
	    -- runs Bertini, storing screen output to bertini_session.log
            );
	
           if o.runType == 3 then ( -- Sample 
    	    run("cd "|dir|"; "|BERTINIexe|" < sample_script >bertini_session.log");  
	    -- runs Bertini, storing screen output to bertini_session.log
            );
	
          if o.runType == 4 then ( -- Membership 
	    run("cd "|dir|"; "|BERTINIexe|" >bertini_session.log");  
	    -- runs Bertini, storing screen output to bertini_session.log
            );
	
          if o.runType == 5 then ( -- Refine/Sharpen 
    	    run("cd "|dir|"; "|BERTINIexe|" < sharpen_script >bertini_session.log");  
	    -- runs Bertini, storing screen output to bertini_session.log--OUREDIT
            );
	
          if o.runType == 6 then ( -- track homotopy
    	    run("cd "|dir|"; "|BERTINIexe|" >bertini_session.log");  
	    -- runs Bertini, storing screen output to bertini_session.log
            );
	
          if o.runType == 7 then ( -- parameter homotopy, stage 1
    	    run("cd "|dir|"; "|BERTINIexe|" >bertini_session.log");  
	    -- runs Bertini, storing screen output to bertini_session.log
	    o2 := o;
	    o2 = delete(runType => 7, o2);
	    o2 = o2 ++ {runType => 8};
	    dir2:=makeBertiniInput(F,o2);
	    copyFile(dir2|"/input",dir|"/input");
	    stageTwoParameterRun(dir,F,o2) ) 
	
         else readSolutionsBertini(dir,F,o) -- o contains runType, 
	 --so we can switch inside readSolutionsBertini
         )


-------------------
-- makeBertiniInput
-------------------

makeBertiniInput = method(TypicalValue=>Nothing,Options=>{
	AllowStrings=>-1,
	Verbose=>true,MultiplicityTol=>1e-6,ConditionNumTol=>1e10, 
	Parameters=>null,ParameterValues=>null,StartSystem=>{}, 
	StartSolutions=>{},RawData=>null,WitnessData=>null,NVariety=>null,
	MPType=>-1,PRECISION=>-1,IsProjective=>-1,ODEPredictor=>-1,
	TrackTolBeforeEG=>-1,TrackTolDuringEG=>-1,FinalTol=>-1,MaxNorm=>-1,
	MinStepSizeBeforeEG=>-1,MinStepSizeDuringEG=>-1,ImagThreshold=>-1,
	CoeffBound=>-1,DegreeBound=>-1,CondNumThreshold=>-1,RandomSeed=>-1,
	SingValZeroTol=>-1,EndGameNum=>-1,UseRegeneration=>-1,SecurityLevel=>-1,
	ScreenOut=>-1,OutputLevel=>-1,StepsForIncrease=>-1,MaxNewtonIts=>-1,
	MaxStepSize=>-1,MaxNumberSteps=>-1,MaxCycleNum=>-1,RegenStartLevel=>-1,
	dimen=>-1,compnum=>-1,numpts=>-1,Points=>{},digits=>-1,runType=>0,PathVariable=>null})  
makeBertiniInput List := o -> T -> ( -- T=polynomials 
    startS1:=apply(o.StartSolutions,
	p->(if class(p)===Point then coordinates(p) else p));
    t:=o.PathVariable;
    gamma:=random(CC);
    params:=o.Parameters;
    --v := gens ring T#0; -- variables
    
    if o.AllowStrings===-1 then   v := gens ring T#0 -- variables
      else v = o.AllowStrings;
      
    if o.runType==6  then (v=delete(t,v));  --special for runtype6
    
    if (o.runType==7 or o.runType==8) then (for i in params do v=delete(i,v));  
    
    dir := temporaryFileName(); -- build a directory to store temporary data 
    makeDirectory dir;  
    f := openOut (dir|"/input"); -- typical (but not only possible) name for 
    --Bertini's input file 

    -- The following block is the config section of the input file 
    
    f << "CONFIG\n\n";-- starting the config section of the input file 

    -- for each user-provided option, we write the appropriate config to the file:
    
    if o.MPType==0 or o.MPType==1 or o.MPType==2 then (
	f << "MPType: " << o.MPType << ";\n") 
        else (if o.MPType=!=-1 then error "MPType has an invalid option;");
    
    if o.PRECISION =!= -1 then
        f << "PRECISION: " << o.PRECISION << ";\n";
  
    if o.ODEPredictor =!= -1 then
        f << "ODEPredictor: " << o.ODEPredictor << ";\n";
  
    if o.TrackTolBeforeEG =!= -1 then
        f << "TrackTolBeforeEG: " << o.TrackTolBeforeEG << ";\n";
    
    if o.TrackTolDuringEG =!= -1 then
        f << "TrackTolDuringEG: " << o.TrackTolDuringEG << ";\n";
    
    if o.FinalTol =!= -1 then
        f << "FinalTol: " << o.FinalTol << ";\n";
  
    if o.MaxNorm =!= -1 then
        f << "MaxNorm: " << o.MaxNorm << ";\n";
  
    if o.MinStepSizeBeforeEG =!= -1 then
        f << "MinStepSizeBeforeEG: " << o.MinStepSizeBeforeEG << ";\n";
  
    if o.MinStepSizeDuringEG =!= -1 then
        f << "MinStepSizeDuringEG: " << o.MinStepSizeDuringEG << ";\n";
  
    if o.ImagThreshold =!= -1 then
        f << "ImagThreshold: " << o.ImagThreshold << ";\n";
  
    if o.CoeffBound =!= -1 then
        f << "CoeffBound: " << o.CoeffBound << ";\n";
  
    if o.DegreeBound =!= -1 then
        f << "DegreeBound: " << o.DegreeBound << ";\n";
  
    if o.CondNumThreshold =!= -1 then
        f << "CondNumThreshold: " << o.CondNumThreshold << ";\n";
  
    if o.RandomSeed =!= -1 then
        f << "RandomSeed: " << o.RandomSeed << ";\n";
  
    if o.SingValZeroTol =!= -1 then
        f << "SingValZeroTol: " << o.SingValZeroTol << ";\n";
  
    if o.EndGameNum =!= -1 then
        f << "EndGameNum: " << o.EndGameNum << ";\n";
  
    if o.UseRegeneration == 1 then
        f << "UseRegeneration: " << o.UseRegeneration << ";\n"
        else (  if o.UseRegeneration =!= -1 then error "UseRegeneration 
	    has an invalid option");
  
    if o.SecurityLevel =!= -1 then
        f << "SecurityLevel: " << o.SecurityLevel << ";\n";
  
    if o.ScreenOut =!= -1 then
        f << "ScreenOut: " << o.ScreenOut << ";\n";
  
    if o.OutputLevel =!= -1 then
        f << "OutputLevel: " << o.OutputLevel << ";\n";
  
    if o.StepsForIncrease =!= -1 then
        f << "StepsForIncrease: " << o.StepsForIncrease << ";\n";
  
    if o.MaxNewtonIts =!= -1 then
        f << "MaxNewtonIts: " << o.MaxNewtonIts << ";\n";
  
    if o.MaxStepSize =!= -1 then
        f << "MaxStepSize: " << o.MaxStepSize << ";\n";
  
    if o.MaxNumberSteps =!= -1 then
        f << "MaxNumberSteps: " << o.MaxNumberSteps << ";\n";
  
    if o.MaxCycleNum =!= -1 then 
        f << "MaxCycleNum: " << o.MaxCycleNum << ";\n";
  
    if o.RegenStartLevel =!= -1 then
        f << "RegenStartLevel: " << o.RegenStartLevel << ";\n";

    -- now we handle the various runType options:
    
    if o.runType == 1 then --segment run 
        f << "USERHOMOTOPY: 1;\n";
  
    if o.runType == 2 then --pos dim run
        f << "TRACKTYPE: 1;\n";
  
    if o.runType == 3 then --sample component 
        f << "TRACKTYPE: 2;\n";
  
    if o.runType == 4 then --membership test 
        f << "TRACKTYPE: 3;\n";
  
    if o.runType == 5 then ( --refine solutions
        if o.IsProjective==-1 then f << "SHARPENONLY: 1;\n UserHomotopy: 1; \n" 
	    else f << "SHARPENONLY: 1;\n UserHomotopy: 2; \n");
  
    if o.runType == 6 then ( --trackHomotopy
        if o.IsProjective==-1 then f << "USERHOMOTOPY: 1;\n" 
	    else f << "USERHOMOTOPY: 2;\n");
  
    if o.runType == 7 then --parameterHomotopy, stage 1
        f << "PARAMETERHOMOTOPY: 1;\n";
        --  else ( if o.runType == 7 and o.WriteOnly=!=-1 
	---then  f << "PARAMETERHOMOTOPY: 2;\n");
  
    if o.runType == 8 then --parameterHomotopy, stage 2
        f << "PARAMETERHOMOTOPY: 2;\n";    
    
    f << endl << "END;\n\n";  -- end of config section

    -- The following block is the input section of the input file
  
    f << "INPUT" << endl << endl;
  
    if o.IsProjective==1 then (
	f << "hom_variable_group ") 
        else (
	    if member(o.runType,{1,5,6}) then  -- if user-defined, 
	        --declaration type of vars is "variable"
		f << "variable " 
		else f << "variable_group ");-- if not user-defined, 
	        --dec type of vars if "variable_group"
    
    scan(#v, i->  -- now we list the variables in a single list  
       if i<#v-1 then f << toString v#i << ", "
	   else f << toString v#i << ";" << endl
       );
    f << "function "; -- "function" section
    scan(#T, i-> -- here are the function names
       if i<#T-1
          then f << "f" << i << ", "
          else f << "f" << i << ";" << endl << endl
       );
  
    if (o.runType==6) then (f << "pathvariable "<<" daejT; " <<endl; 
	--we chose daejT because we needed a name no one would choose 
	--so we chose our initials and T
	f << "parameter "<<toString(t)|" ;" <<endl;
        f << toString(t)|"= daejT ;"<<endl);
    
    if (member(o.runType, {7,8})) then (
	f << "parameter ";
        scan(#params, i->  -- now we list the variables in a single list 
       	    if i<#params-1 
       	    then f << toString params#i << ", "
       	    else f << toString params#i << ";" << endl
	    ));   

    bertiniNumbers := p->if class p === CC then (
	toString realPart p | "+" | toString imaginaryPart p | "*I"
	)
        else (
	    L := toExternalString p; 
            L = replace("p"|toString precision p, "", L);
            L = replace("\\bii\\b", "I", L); 
            L = replace("([0-9])e([0-9])", "\\1E\\2", L);
            L
            );
     
     -- if o.SubFunctions=!=-1 then (
	--  for i in o.SubFunctions do (
	  --    f << toString (i_0) << " = " << toString(i_1)<< " ;\n")       
	  -- );
      
      --The next lines of code write the polynomials to the input file called f:
      
      -- non-param runs: just write out the polynomials
      if (o.runType!=1 and o.runType!=5) then (  
	   scan(#T, i -> f << "f" << i << " = " << (if class T#i===String then 
		   T#i else bertiniNumbers T#i) << ";" << endl)
	   ) 
      -- param runs: write out polys and some other stuff
      else (if (o.runType==1) then (
	      if #o.StartSystem != #T then error "expected equal number of 
	          equations in start and target systems";              
	      f << "pathvariable t;\n" << "parameter s;\n"<< "s = t;\n\n";  
              scan(#T, i -> f << "f" << i << " = (" << bertiniNumbers T#i << ")
		  *(1-s)+s*("<< bertiniNumbers gamma << ")*(" 
		  << bertiniNumbers o.StartSystem#i << ");" << endl 
	      );
              )
             else (  -- refine sols runs: write out polys and some other stuff
                 f << "pathvariable t;\n" 
                 << "parameter s;\n"
                 << "s = t;\n\n";  
                 scan(#T, i -> f << "f" << i << " = " 
		     << (if class T#i===String then T#i else bertiniNumbers T#i) 
		     << ";" << endl)
	         );
             );
      
      f << endl << "END;" << endl << endl;
      close f;

      --Now we build auxilary files for various sorts of runs:
  
      if member(o.runType,{1,6}) then ( -- writing out start file in the case of a param run
	  f = openOut (dir|"/start"); -- the only name for Bertini's start solutions file 
          f << #startS1 << endl << endl;
          scan(startS1, s->(
		 scan(s, c-> f << realPart c << " " << imaginaryPart c << ";" << endl );
		 f << endl;
		 ));
          close f;
          );

       if (o.runType==4) then ( -- writing out file with points in the case of 
	   --a membership run
          f = openOut (dir|"/member_points"); -- the only name for Bertini's 
	  --membership points file 
          f << #startS1 << endl << endl;
          scan(startS1, s->(
		 scan(s, c-> f << realPart c << " " << imaginaryPart c << ";" << endl );
		 f << endl;
		 ));
          close f;
          );


       if (o.runType==5) then ( -- writing out file with points in the case of 
	   --a refine run
	   f = openOut (dir|"/sharpen_script"); -- writing out file with 
	   --query responses in case of a refine/sharpen run
           f << "5" << endl << o.digits << endl << "1" << endl;
           close f;
       
           --create raw_data in tmp directory
           
	   f =openOut(dir|"/raw_data");
  	   f << toString(#v)<<endl;
	   f << toString(0)<<endl;
	   for i from 0 to #startS1-1 do(
	       f << toString(i)<<endl;
	       f << toString(52)<<endl;
	       --f << "1 0" <<endl; --working in affine space, don't need this line
	       scan(startS1_i, 
		   c->f<<realPart(c) <<" "<<imaginaryPart(c)<<endl);
		   f << "1" <<endl;
		   f << "1" <<endl;
		   f << "1" <<endl;
		   f << "1" <<endl;
		   f << "1" <<endl;
		   f << "1" <<endl;
		   f << "1" <<endl;
		   f << "1" <<endl;);
	 
	   f << "-1"<<endl;
	   f << endl;
	   f << "2 0"<<endl; -- precision type, not using equation by equation
	   f << endl;
	   f<< "0 "|toString(#v)<<endl; -- no patch, number of variables
	   f << endl;
     	   f << "-1"<<endl;
	   f << "1 1"<<endl; -- gamma
	   f << endl;
	   f<< "0 0"<<endl;
	   f << endl;
	   f<<"0 0"<<endl;
	   close f;
	 
           --create midpath_data in tmp directory
       
           f =openOut(dir|"/midpath_data");
           f << "This file needs to be created by bertiniRefineSols for Bertini" << endl;
           close f;
           );
  
      if (o.runType==3) then (  --copies witness_data file to tmp directory
	  copyFile(o.WitnessData, dir|"/witness_data")
          );
  
      if (o.runType==4) then (  --copies witness_data file to tmp directory
	  copyFile(o.WitnessData, dir|"/witness_data")
          );
     
      if (o.runType==3) then ( -- writing out file with query responses in case 
	  --of a sample run
	  f = openOut(dir|"/sample_script");
	  f << o.dimen << endl << o.compnum << endl << o.numpts << endl << "0" << 
	      endl << "sample_points" << endl;  
       -- sampled points will be written file named sample_points
          close f;
          );

      if (o.runType=!=8) then (
	  if o.Verbose==true then stdio 
	       << "Temporary directory for input and output files:" << dir << endl << endl);
      
      dir
      )



-----------------------
-- readSolutionsBertini
-----------------------

readSolutionsBertini = method(TypicalValue=>NumericalVariety, Options=>{
	Verbose=>true,MultiplicityTol=>1e-6, AllowStrings=>-1,
	ConditionNumTol=>1e10,IsProjective=>-1,Parameters=>null,
	ParameterValues=>null, StartSystem=>{},NVariety=>null, 
	StartSolutions=>{},RawData=>null,WitnessData=>null,MPType=>-1,
	PRECISION=>-1,ODEPredictor=>-1,TrackTolBeforeEG=>-1,TrackTolDuringEG=>-1,
	FinalTol=>-1,MaxNorm=>-1,MinStepSizeBeforeEG=>-1,MinStepSizeDuringEG=>-1,
	ImagThreshold=>-1,CoeffBound=>-1,DegreeBound=>-1,CondNumThreshold=>-1,
	RandomSeed=>-1,SingValZeroTol=>-1,EndGameNum=>-1,UseRegeneration=>-1,
	SecurityLevel=>-1,ScreenOut=>-1,OutputLevel=>-1,StepsForIncrease=>-1,
	MaxNewtonIts=>-1,MaxStepSize=>-1,MaxNumberSteps=>-1,MaxCycleNum=>-1,
	RegenStartLevel=>-1,dimen=>-1,compnum=>-1,numpts=>-1,Points=>{},
	digits=>-1,runType=>0,PathVariable=>null})

readSolutionsBertini (String,List) := o -> (dir,F) -> (  
  -- dir=directory holding the output files, options are same as bertiniSolve
  local pt;
  local coord;
  local coords;
  local funcResid;
  local condNum;
  local newtonResid;
  local lastT;
  local cycleNum;
  local success;
  local solNum;
  local numVars;
  local a;
  local numCodims;
  local ptsInCodim;
  local ptType; 
  local ptMult;
  local compNum;
  local numDeflations;
  local nv;
  local ws;
  local codimen;
  local listOfCodims;
  local randDims;
  local numRands;
  local numToSkip;
  local linCoeffDims;
  local numLinCoeffs;
  local rw;
  local mat;
  local coefParts;
  local M;
  local colsToSkip;
  local N;
  local dehomCoords;
  local vars;
  local R;

  s := {};
  
  if (member(o.runType,{0,8})) then ( 
    sessionLog:= lines get (dir|"/bertini_session.log"); -- get contents of session log 
    --and check for rank error
    
    scan(sessionLog, i->if i=="The system has no zero dimensional solutions based on its rank!" then 
	error  "The system has no zero dimensional solutions based on its rank!");
    
    failedPaths := lines get (dir|"/failed_paths"); -- get contents of failed paths file and check if non-empty
    
    if  failedPaths=!={""} then (
	if o.Verbose==true then 
	stdio << "Warning: Some paths failed, the set of solutions may be incomplete" <<endl<<endl) ;
        
  --raw_data, for zeroDim 

--raw_data output file structure:
--  #var's (incl. homog. var.!!) 
--  0  
--  blocks as follows:
--    path_num
--    max prec used
--    coords (proj!!)
--    fxn resid
--    cond_num
--    Newton resid
--    last Tval
--    useless here (accuracy estimate -- diff bw last two extrapolations to t=0)
--    useless here (Tval of first prec increase)
--    cycle number
--    success?  (1 for yes)
--    NOTE:  # paths ending at same point is NOT reported in this file 
-- needs to be computed...only available in human-readable main_data!!! 
--  -1 (at end of blocks)
--  junk at end is the matrix of patch coefficients 
    -- MPType on first line, then number or rows & columns on second,
    -- then the coeffs

    l := lines get (dir|"/raw_data"); -- grabs all lines of the file
    numVars = value(first l);
    l = drop(l,2);
    solNum = value(first l);
    l = drop(l,1);
    
    --Now we go through all blocks of solutions 
    -- each block contains the coordinates of the solution 
    -- and a bunch of other stuff.

    wList := {}; --list of witness sets
    pts:={}; 
    while solNum > -1 do ( -- -1 in solNum position (top of solution block) 
	--is key to end of solutions.
        maxPrec := value(first l);
        l = drop(l,1);
	coords = {};
            
	for j from 1 to numVars do ( -- grab each coordinate
	    -- use regexp to get the two numbers from the string
	    coord = select("[0-9.e+-]+", cleanupOutput(first l));  
	    coords = join(coords, {toCC(53, value(coord#0),value(coord#1))});  
	    -- NOTE: we convert to a 53 bit floating point complex type 
	    -- beware that we might be losing data here!!!
            l = drop(l,1);
	    );
	 
        -- now we dehomogenize, assuming the first variable is the hom coord:   
        
	dehomCoords = {};
	if o.IsProjective==-1 then
            for j from 1 to numVars-1 do (
	      dehomCoords = join(dehomCoords, {coords#j / coords#0});
              )
	    else for j from 0 to numVars-1 do (
	      dehomCoords = join(dehomCoords, {coords#j });
              );


 	pt = new Point;
        pt.MaximumPrecision=maxPrec;
	pt.FunctionResidual = value(cleanupOutput(first l)); l=drop(l,1);
        pt.ConditionNumber = value(cleanupOutput(first l)); l=drop(l,1);
        pt.NewtonResidual = value(cleanupOutput(first l)); l=drop(l,1);
        pt.LastT = value(cleanupOutput(first l)); l=drop(l,3);
        pt.CycleNumber = value(first l); l=drop(l,1);
        if(value(first l)=!=1) then pt.SolutionStatus=FailedPath else pt.SolutionStatus=null;       
	l=drop(l,1);
        pt.SolutionNumber = value(first l);
     	solNum=pt.SolutionNumber;
        l = drop(l,1); 

           
        pt.Coordinates = dehomCoords; --we want to output these
	pts=join(pts,{pt});
            
	);
	
    pts=solutionsWithMultiplicity(pts,Tolerance=>o.MultiplicityTol); 
	
    if o.UseRegeneration=!=1 then checkMultiplicity(pts);
       
    if o.UseRegeneration==1 then return pts 
       else ( 
	   checkConditionNumber(pts, o.ConditionNumTol);
	   for i in pts do (
	       if (i.SolutionStatus=!=Singular 
	           and i.SolutionStatus=!=FailedPath 
		   and i.SolutionStatus=!=RefinementFailure) 
	       then i.SolutionStatus=Regular);
	   return pts
	   )
    )

  else if (o.runType == 1 or o.runType==6 or o.runType==5) then ( 
    -- get contents of session log and check errors
    sessionLog = lines get (dir|"/bertini_session.log"); 
    scan(sessionLog, i->if i=="ERROR: The matrix has more columns than rows in QLP_L_mp!!" then 
	  error  "The matrix has more columns than rows in QLP_L_mp!"
	  );
              
    l = lines get (dir|"/raw_data"); -- grabs all lines of the file
    numVars = value(first l);
    l = drop(l,2);
    solNum = value(first l);
    l = drop(l,1);
    
    --Now we go through all blocks of solutions
    -- (each block contains the coordinates of the solution and other stuff)

    pts={}; 

    prec'value := (P,s) -> ( -- P:ZZ and s:String
	where'is'e := regex("e",s);
	if where'is'e===null then value(s|"p" | toString P)
	   else (
	       pos := first first where'is'e;
	       value (substring((0,pos),s) | "p" | toString P | substring((pos,#s-pos),s))
	       )
	 );
    
    while solNum > -1 do ( 
	-- -1 in solNum position (top of solution block) is key to end of solutions.
	maxPrec = value(first l);
	l = drop(l,1);
	bitPrec := ceiling((log 10/log 2)*o.digits);
	coords = {};
	for j from 1 to numVars do ( -- grab each coordinate
	    -- use regexp to get the two numbers from the string
	    coord = select("[0-9.e+-]+", cleanupOutput(first l));  
	    if (o.runType==1 or o.runType==6) then (
		coords = join(coords, {toCC(53, value(coord#0),value(coord#1))}))
	        -- NOTE: we convert to a 53 bit floating point complex type 
		-- beware that we might be losing data here!!!
	        else (coords = join(coords, 
			{toCC(bitPrec, prec'value(bitPrec,coord#0), prec'value(bitPrec,coord#1))}    
		        ));
            l = drop(l,1);
	    );
 	    
	pt = new Point;
        pt.MaximumPrecision=maxPrec;
	pt.FunctionResidual = value(cleanupOutput(first l)); l=drop(l,1);
        pt.ConditionNumber = value(cleanupOutput(first l)); l=drop(l,1);
        pt.NewtonResidual = value(cleanupOutput(first l)); l=drop(l,1);
        pt.LastT = value(cleanupOutput(first l)); l=drop(l,3);
        pt.CycleNumber = value(first l); l=drop(l,1);
	
	if(value(first l)=!=1) and o.runType==5 then 
	    pt.SolutionStatus=RefinementFailure else pt.SolutionStatus=null;
	
	if(value(first l)=!=1) and o.runType=!=5 then 
	         pt.SolutionStatus=FailedPath; 		 
      
	l=drop(l,1);
        pt.SolutionNumber = value(first l);
     	solNum=pt.SolutionNumber;
        l = drop(l,1); 
        pt.Coordinates = coords; --we want to output these
	pts=join(pts,{pt})
        );
    
    pts=solutionsWithMultiplicity(pts,Tolerance=>o.MultiplicityTol); 
	
    if o.UseRegeneration=!=1 then checkMultiplicity(pts);
       
    if o.UseRegeneration==1 then return pts 
       else ( 
	   checkConditionNumber(pts, o.ConditionNumTol);
	   for i in pts do if (i.SolutionStatus=!=Singular 
	       and i.SolutionStatus=!=FailedPath 
	       and i.SolutionStatus=!=RefinementFailure) 
	       then i.SolutionStatus=Regular;
	   return pts)
         
    )

  --if PosDim, we read in the output from witness_data
  
  else if (o.runType == 2) then ( 
      
--witness_data output file structure:
--  #var's (incl. homog. var.!!)
--  #nonempty codims   
--  blocks by codim (1 block per codim):
--    codim 
--    total #points in this codim (over all irred. comps.)
--    blocks by points (1 block per point):
--      max prec used
--      coords (proj!!)
--      max prec used (useless!) 
--      last approx of point on path before convergence to t=0 (useless!)
--      cond_num
--      corank (useless!)
--      smallest nonzero sing val (useless!)
--      largest zero sing val (useless!)
--      type
--      multiplicity
--      component number
--      deflations needed for this point
--    -1 (at end of blocks)

--  junk at end is the matrix of slice coefficients and such.

    l = lines get (dir|"/witness_data"); -- grabs all lines of the file
    numVars = value(first l);  l=drop(l,1);
    numCodims = value(first l); l=drop(l,1);
    wList = {};  --list of witness sets
    listOfCodims = {};  --keeps track of codimension of each witness set; 
    --needed since we add slice data later.

    for codimNum from 1 to numCodims do (
	pts := {};  --for each codim, we store all points and 
	--all codims (next line), then sort after gathering all points in the codim
        compNums := {};
        maxCompNum := 0;  --keeps track of max component number in this codim
        codimen = value(first l); l=drop(l,1);
        ptsInCodim = value(first l); l=drop(l,1);

        for ptNum from 1 to ptsInCodim do (
	    maxPrec := value(first l);
            l = drop(l,1);
            coords = {};
            for j from 1 to numVars do ( -- grab each coordinate
              -- use regexp to get the two numbers from the string
	      coord = select("[0-9.e+-]+", cleanupOutput(first l));  
              coords = join(coords, {toCC(maxPrec, value(coord#0),value(coord#1))});  
	      -- NOTE: we convert to maxPrec bits complex type
              l = drop(l,1);
              );

            l = drop(l,numVars+1);  -- don't need second copy of point or 
	    --extra copy of maxPrec

            -- now we dehomogenize, assuming the first variable is the hom coord:
            
	    dehomCoords = {};
	    if o.IsProjective==-1 then (
		for j from 1 to numVars-1 do (
		    dehomCoords = join(dehomCoords, {coords#j / coords#0});
                    ) 
		)
	        else for j from 0 to numVars-1 do (
		    dehomCoords = join(dehomCoords, {coords#j });
                    );
       
	    condNum = value(cleanupOutput(first l)); l=drop(l,4);
            ptType = value(first l); l=drop(l,1);
            ptMult = value(first l); l=drop(l,1);
            compNum = value(first l); l=drop(l,1);
            numDeflations = value(first l); l=drop(l,1);
            pt = new Point;
            pt.Coordinates = dehomCoords;
            pts = join(pts,{pt});
            compNums = join(compNums,{compNum});
            if (compNum > maxCompNum) then maxCompNum=compNum; 
            ); 
        	 
	for j from 0 to maxCompNum do ( 
	    --loop through the component numbers in this codim 
	    --to break them into witness sets
	    ptsInWS := {}; --stores all points in the same witness set
            
	    for k from 0 to #pts-1 do (
	      --save the point if its in the current component (component j)
	      if (compNums#k == j) then ptsInWS = join(ptsInWS,{pts#k}); 	      
	      );
            
	    N = map(CC^0,CC^(numVars+1),0);
	    
	    if AllowStrings===-1 then ws = witnessSet(ideal F,N, ptsInWS) 
	      --turn these points into a witness set
    	      else (
		  ws = witnessSet(ideal(1),N,ptsInWS);
		  ws.Equations=F
	        );

           -- ws = witnessSet(ideal F,N, ptsInWS); --turn these points into a witness set
            ws.ComponentNumber=j;
	    ws.WitnessDataFileName=dir|"/witness_data";
	    wList = join(wList, {ws}); --add witness set to list
            listOfCodims = join(listOfCodims, {codimNum});
            );        
	);

-- now we grab the slice data, at the end of the witness_data file, 
--to be inserted into the witnessSets with dim>0
    l = drop(l,3); -- -1, blank line, MPType
    randDims = select("[0-9]+", first l);  -- grabs #rows, 
    
    --#cols for the matrix used to randomize the system 
    l = drop(l,1);
    
    numRands = value(randDims#0) * value(randDims#1);  -- numRands is the 
    --number of random numbers we want to skip next
    
    l = drop(l,numRands+1);   -- includes blank line after rands    
    
    -- next we have the same number of integers 
    --(degrees needed ot keep homogenization right)
    l = drop(l,numRands);
    
    -- next we have an integer and a list of row vectors 
    --(the number of which is the initial integer).  Again related to 
    --homogenization.
    numToSkip = select("[0-9]+", first l);
    
    l = drop(l,value(numToSkip#0)+3); -- dropping all those, 
    --plus line containing integer (before), then blank line, and one more line
    
    --finally, we have the number of linears and the number of coefficients per linear
    linCoeffDims = select("[0-9-]+", first l);
    l = drop(l,1);

    --now we just read in the matrix
    numLinCoeffs = value(linCoeffDims#0) * value(linCoeffDims#1);
    rw = {};
    mat = {};
    
    for i from 1 to value(linCoeffDims#1) do ( 
	for j from 1 to value(linCoeffDims#0) do (
            coefParts = select("[0-9-]+/[0-9-]+", first l);
            rw = join(rw, {toCC(53,value(coefParts#0)) + 
		    ii*toCC(53,value(coefParts#1))});  
	    -- definitely losing data here, going from rational number to float!
            l = drop(l,1);
            );
        mat = join(mat, {rw});  
        rw = {};
        );
    
    M = matrix(mat);

    -- Finally, we can cycle through the witness sets in nv 
    -- and add the slice data.
    -- There are length listOfCodims witness sets, 
    -- the first of which uses the full set of slices (all of M).
    -- The higher codimensions need higher-dimensional hyperplane sections, 
    -- so fewer slices (part of M).
    -- The lowest slice is kept longest.  
    -- Ex:  If there is a codim 1 set with a 2x4 matrix of slice data, 
    -- a subsequent codim 2 set would have a 
    -- 1x4 matrix of slice data consists of the second (not first) 
    -- line of the codim 1 slice data.
        	
    for codimNum from 0 to length listOfCodims - 1 do (
	 coeffList := {};  
	 
	 --We store the cols of M needed for this particular codimNum in coeffList, 
	 --then turn it into a matrix and store it the witness set.
         
	 colsToSkip = listOfCodims#codimNum - listOfCodims#0;  
         
	 for i from colsToSkip to numgens source M - 1 do (
	     coeffCol := {};
	     for j from 0 to numgens target M - 1 do ( 
		 coeffCol = join(coeffCol, {M_(j,i)});
                 ); 
             coeffList = join(coeffList, {coeffCol});
             );
          
	  if (#coeffList > 0) then N = matrix(coeffList) 
	      else N = map(CC^0,CC^(numVars+1),0); 
	  
	   -- rearrange columns so slice from NAGtypes
	  --returns the correct linear functional
	  
	  firstCol:=N_{0};
	  N=(submatrix'(N, ,{0})|firstCol);   
	   
	  (wList#codimNum).Slice = N;
 	  
          );

    nv = numericalVariety wList;
    nv.WitnessDataFileName=dir|"/witness_data";
    nv.Equations=F;
    return nv
    )

----- start Sample

  else if (o.runType == 3) then (      
       l = lines get (dir|"/sample_points"); -- grabs all lines of the file
       var's := gens ring F#0; -- variables 
       ---Should this be changed to getting the number of 
       -- vars directly from main_data? 3/6/14
       numVars = #var's;
       numberOfSolutions := value(first l);
       l = drop(l,1);
       
       --Now we go through all blocks of solutions 
       -- (each block contains the coordinates of the solution and other stuff).
       
       solNum = 1;
       
       pts={}; 
       
       while solNum <= numberOfSolutions do ( 
       -- -1 in solNum position (top of solution block) is key to 
       -- end of solutions.
     	    solNum=solNum+1;
	    maxPrec = value(first l);
            l = drop(l,1);
            coords = {};
            for j from 1 to numVars do ( -- grab each coordinate
	      -- use regexp to get the two numbers from the string
              coord = select("[0-9.e+-]+", cleanupOutput(first l));  
              coords = join(coords, {toCC(53, value(coord#0),value(coord#1))});  
	      -- NOTE: we convert to a 53 bit floating point complex type 
	      -- beware that we might be losing data here!!!
              l = drop(l,1);
	      );
	 
            pt = new Point;
            pt.Coordinates = coords; --we want to output these
	    pts=join(pts,{pt})
            );
       
       solNum=1;
       return pts  
       )

-- component membership

  else if (o.runType==4) then (
       NV := o.NVariety;
       firstl := lines get (dir | "/witness_data"); 
       numVars = value(first firstl)-1;
       coDims := {};
       comps := {};
       l = lines get (dir | "/incidence_matrix"); 
       -- grabs lines of incidence_matrix file
       numCoDims := value first l;
       l=drop(l,1);
       
       for coDimNum from 1 to numCoDims do ( --get co-dimensions of components
           coDims = append(coDims, value ("{"|replace (" ", ",", l_0)|"}"));
           l=drop(l,1)
           );
       
       wSets := {}; --list of lists of witness sets for each point
       l = drop(l,3);
       for i from 1 to #o.StartSolutions do (
          --getting row from incidence matrix and dropping extra space
	 testVector := drop(value ("{"|replace (" ", ",", l_0)|"}"), -1); 
	 witSets'forOnePoint := {};
         for j from 0 to numCoDims-1 do(	  
           subTestVector := take(testVector, coDims_j_1);  
	   --get component numbers that with positive result
	   compNums := positions(subTestVector, k->k==1); 
	    --grabs witness sets in this component
           possWitSets := NV#(numVars-coDims_j_0);
	   --select witness sets with positive result
	   witSets := select(possWitSets, k->member(k.ComponentNumber, compNums)); 
    	   witSets'forOnePoint = witSets'forOnePoint | witSets;
	   testVector=drop(testVector, coDims_j_1);
	   );
 	 
	 wSets = append(wSets,witSets'forOnePoint); --append to larger list that we will output      
         );
       return wSets    
  )

  else if o.runType == 8 then ( 
       l = lines get (dir|"/raw_solutions");
       
       while #l>0 and #separate(" ", l#0) < 2 do l = drop(l,1);
       
       while #l>0 do (
	    if DBG>=10 then << "------------------------------" << endl;
	    coords = {};
	    while #l>0 and #separate(" ", l#0) >= 2 do ( 
	      	 coords = coords | {(
		   	   a = separate(" ",  cleanupOutput(first l));	 
		   	   (value a#0)+ii*(value a#1)
	      	   	   )};
    	      	 l = drop(l,1);
	      	 );
	    while #l>0 and #separate(" ", l#0) < 2 do l = drop(l,1);
            if DBG>=10 then << coords << endl;
	    s = s | {{coords}};
	    return s 
	    );     

    ) 
  
  else error "unknown output file";  
  )

-------------------------------------------------------
---functions used by bertiniSolve, makeBertiniInput, 
---and readBertiniSolutions----------------------------
-------------------------------------------------------

cleanupOutput = method(TypicalValue=>String)
cleanupOutput String := s -> (
  t := replace("E", "e", s);
  t = replace("[(,)]","", t);
  t = replace("e\\+","e",t)
  )

stageTwoParameterRun = method(TypicalValue=>Nothing,Options=>{Verbose=>true, 
	MultiplicityTol=>1e-6, AllowStrings=>-1,
	ConditionNumTol=>1e10, Parameters=>null,ParameterValues=>null,
	StartSystem=>{},StartSolutions=>{},RawData=>null,WitnessData=>null,
	NVariety=>null,MPType=>-1,PRECISION=>-1,IsProjective=>-1,ODEPredictor=>-1,
	TrackTolBeforeEG=>-1,TrackTolDuringEG=>-1,FinalTol=>-1,MaxNorm=>-1,
	MinStepSizeBeforeEG=>-1,MinStepSizeDuringEG=>-1,ImagThreshold=>-1,
	CoeffBound=>-1,DegreeBound=>-1,CondNumThreshold=>-1,RandomSeed=>-1,
	SingValZeroTol=>-1,EndGameNum=>-1,UseRegeneration=>-1,SecurityLevel=>-1,
	ScreenOut=>-1,OutputLevel=>-1,StepsForIncrease=>-1,MaxNewtonIts=>-1,
	MaxStepSize=>-1,MaxNumberSteps=>-1,MaxCycleNum=>-1,RegenStartLevel=>-1,
	dimen=>-1,compnum=>-1,numpts=>-1,Points=>{},digits=>-1,runType=>0,
	PathVariable=>null})  
stageTwoParameterRun (String, List) := o -> (dir, F) -> (
  copyFile(dir|"/nonsingular_solutions",dir|"/start");
  moveFile(dir|"/nonsingular_solutions",dir|"/nonsingular_solutions_stage1");
  sols:={};
  for i from 0 to #o.ParameterValues-1 do (
  --writing parameter values to file 
     f:= openOut (dir|"/final_parameters"); -- the only name for Bertini's final 
     --parameters file 
     f << #(o.Parameters) << endl << endl;
     scan((o.ParameterValues)_i, s->(
       scan(s, c-> f << realPart c << " " << imaginaryPart c << ";" << endl );
       f << endl;
       ));
     close f;
     run("cd "|dir|"; "|BERTINIexe|" >bertini_session.log");  
     solution :=readSolutionsBertini(dir, F, o);
     sols = append(sols, solution);
     moveFile(dir|"/nonsingular_solutions",dir|"/nonsingular_solutions"|toString(i));
     );
  return sols
  )

checkConditionNumber=(listOfPoints, tolerance)->(
    for i in listOfPoints do (
      if i.ConditionNumber>tolerance 
	  and i.SolutionStatus=!=FailedPath 
          and i.SolutionStatus=!=RefinementFailure
          then i.SolutionStatus=Singular)
      )

checkMultiplicity=(listOfPoints)->(
    for i in listOfPoints do
      if i.Multiplicity>1 and i.SolutionStatus=!=FailedPath 
        and i.SolutionStatus=!=RefinementFailure
        then i.SolutionStatus=Singular)

---- November 2014 additions
--FUNCTION 1: makeB'InputFile
--the input of makeB'InputFile is a string of the directory where we want to write the files.



makeB'InputFile = method(TypicalValue => String, Options=>{
	StorageFolder=>null,
	NameB'InputFile=>"input",  --This option allows us to change the name of the input file that we will make.
	B'Configs=>{}, --This option is a list of pairs of strings or options. These will be written in the CONFIG part of the Bertini input file. 
--For different functions using Bertini one must state "homogeneous variable groups", "affine variable groups", "parameters", "variables", or "path variables".
	HomVariableGroup=>{}, --A list  of homogeneous variable groups or a list of list of homogeneous variable groups
	AffVariableGroup=>{}, --A list  of affine variable groups or a list of list of affine variable groups.
    	ParameterGroup=>{}, --A list of parameters or list of list of parameters.
    	VariableList=>{}, --A list of variables or a list of list of variables.  
    	PathVariable=>{}, --A list of path variables or a list of list of path variables.  
    	RandomComplex=>{}, --A list or a list of list of symbols that denote random complex numbers.
    	RandomReal=>{}, --A list or a list of list of symbols that denote random real numbers.
	B'Constants=>{},--A list of pairs. Each pair consists of a symbol that will be set to a string and a number. 
	NamePolynomials=>{}, --A list of names (names are always strings) of the polynomials which we want to find the common zero set of.
	B'Polynomials=>{},--A list  of polynomials we want to solve.   	
	B'Functions=>{},--A list of pairs consisting of a name and a polynomial.  
	Verbose=>1--Set to 0 to silence additional outputs. 
	})
makeB'InputFile(String) := o ->(IFD)->(    
    IFD=addSlash(IFD);
--Warnings are printed here.     
     if #o.B'Polynomials===0 and #o.NamePolynomials===0 then (print "Warning: NamePolynomials and B'Polynomials are both empty.");
     if #o.B'Polynomials=!=0 and #o.NamePolynomials=!=0 then (print "Warning: NamePolynomials and B'Polynomials are both non-empty.");     
     if #o.VariableList===0 and #o.AffVariableGroup===0 and #o.HomVariableGroup===0 then stdio << "Warning: VariableList, AffVariableGroup, and HomVariableGroup are all empty." <<endl<<endl;     
--Errors are printed here. 
     for onePair to #o.B'Constants-1 do if  class((o.B'Constants)_onePair)===List and #((o.B'Constants)_onePair)=!=2 then  error ("B'Constants is not a list of pairs because of element "|onePair);
     for onePair to #o.B'Functions-1 do if #((o.B'Functions)_onePair)=!=2  and class ((o.B'Functions)_onePair)=!=B'Section and class ((o.B'Functions)_onePair)=!=B'Slice and class ((o.B'Functions)_onePair)=!=Option then  error ("B'Functions is not a list of pairs because of element "|onePair);
     for onePair to #o.B'Functions-1 do if  class ((o.B'Functions)_onePair)===B'Section and not member(NameB'Section,keys ((o.B'Functions)_onePair)) then  error ("B'Functions contains an unnamed B'Section because of element "|onePair|". Set the NameB'Section option.");
     for onePair to #o.B'Functions-1 do if  class ((o.B'Functions)_onePair)===B'Slice and not member(NameB'Slice,keys ((o.B'Functions)_onePair)) then  error ("B'Functions contains an unnamed B'Slice because of element "|onePair|". Set the NameB'Slice option. ");
--Now we write the file. The first thing we do is create a file named "input" by default (this default is changed by the NameB'InputFile option).
     if o.StorageFolder=!=null 
     then (
	 filesGoHere:=addSlash(IFD|o.StorageFolder);
	 if o.Verbose>0 then print filesGoHere;
	 if not fileExists(filesGoHere) then mkdir(filesGoHere))
     else filesGoHere=IFD;
     openedInputFile:= openOut(filesGoHere|o.NameB'InputFile);
     openedInputFile <<  endl  << "% This input file was written with the Bertini.m2 Macaulay2 package." << endl<<endl;
--The first part of a Bertini input file is the configurations.  We write the configuratiosn followed by a line "%%%ENDCONFIG;". We use this line as marker to write configurations after writing the initial file. 
     openedInputFile << "CONFIG" << endl << endl;
     for oneConfig in o.B'Configs do (
       if class oneConfig===Option 
       then openedInputFile << toString((toList oneConfig)_0) << " : " << toString((toList oneConfig)_1) << " ; " << endl
       else if class oneConfig===List then openedInputFile << toString(oneConfig_0) << " : " << toString(oneConfig_1) << " ; " << endl	 
       else error("B'Config has an unreadable element: "|toString oneConfig));
     openedInputFile <<  endl << "%%%ENDCONFIG;" << endl;
     openedInputFile << "END;" << endl << endl;
--The second part of a Bertini input file is the INPUT.     
     openedInputFile << "INPUT" << endl << endl;
-----Write the Variable groups, parameters, and constants.
--Write the homogeneous variable groups
     if o.HomVariableGroup=!={} and o.AffVariableGroup=!={} then print "Warning: The HomVariableGroup is written first and then the AffVariableGroup is written second.";
     if #o.HomVariableGroup=!=0 and class ((o.HomVariableGroup)_0 )=!=List then theHomVariableGroup:={o.HomVariableGroup} else theHomVariableGroup=o.HomVariableGroup;
     if #theHomVariableGroup=!=0 then 
     for oneGroup in theHomVariableGroup do (
--	 openedInputFile << "hom_variable_group "  ;
--	 for j to #oneGroup-2 do (openedInputFile <<toString (oneGroup_j)  << ", ");
--	 openedInputFile << toString(oneGroup_(-1)) << " ; "<< endl
	 writeNamedListToB'InputFile("hom_variable_group",oneGroup,openedInputFile)
	 );
--Write the affine variable groups
     if #o.AffVariableGroup=!=0 and class ((o.AffVariableGroup)_0 )=!=List then theAffVariableGroup:={o.AffVariableGroup} else theAffVariableGroup=o.AffVariableGroup;
     if #theAffVariableGroup=!=0 then 
     for oneGroup in theAffVariableGroup do (
	 --openedInputFile << "variable_group "  ;
	 --for j to #oneGroup-2 do (openedInputFile <<toString (oneGroup_j)  << ", ");
	 --openedInputFile << toString(oneGroup_(-1)) << " ; "<< endl
	 writeNamedListToB'InputFile("variable_group",oneGroup,openedInputFile)
	 );
     openedInputFile <<endl;
--Write  variable groups
     if #o.VariableList=!=0 and class ((o.VariableList)_0 )=!=List then theVariableList:={o.VariableList} else theVariableList=o.VariableList;
     if #theVariableList=!=0 then 
     for oneGroup in theVariableList do (
	 openedInputFile << "variable "  ;
	 for j to #oneGroup-2 do (openedInputFile <<toString (oneGroup_j)  << ", ");
	 openedInputFile << toString(oneGroup_(-1)) << " ; "<< endl);
     openedInputFile <<endl;
--Write the parameters
     if #o.ParameterGroup=!=0 and class ((o.ParameterGroup)_0 )=!=List then theParameterGroup:={o.ParameterGroup} else theParameterGroup=o.ParameterGroup;
     if #theParameterGroup=!=0 then 
     for oneGroup in theParameterGroup do (
--	 openedInputFile << "parameter "  ;
--	 for j to #oneGroup-2 do (openedInputFile <<toString (oneGroup_j)  << ", ");
--	 openedInputFile << toString(oneGroup_(-1)) << " ; "<< endl
	 writeNamedListToB'InputFile("parameter",oneGroup,openedInputFile)
	 );
     openedInputFile <<endl;
--write the path variable
     if #o.PathVariable=!=0 and class ((o.PathVariable) )=!=List then thePathVariable:={o.PathVariable} else thePathVariable=o.PathVariable;
     if #thePathVariable=!=0 
     then (
       openedInputFile << "pathvariable "  ;
       for j to #thePathVariable-2 do (openedInputFile <<toString (thePathVariable_j)  << ", ");
       openedInputFile << toString(thePathVariable_(-1)) << " ; "<< endl);
     openedInputFile <<endl;     
--Write the random complex constants 
     if #o.RandomComplex=!=0 then (
       if class(o.RandomComplex_0)=!=List 
       then theRandomComplex:={o.RandomComplex}
       else theRandomComplex=o.RandomComplex;
       for aGroup in theRandomComplex do(
     	 --openedInputFile << "random "  ;
     	 --for j to #aGroup-2 do (openedInputFile <<toString (aGroup_j)  << ", ");
     	 --openedInputFile << toString(aGroup_(-1)) << " ; "<< endl
	 writeNamedListToB'InputFile("random",aGroup,openedInputFile)
	 ));
--Write the random real constants
     if #o.RandomReal=!=0 then (
       if class(o.RandomReal_0)=!=List 
       then theRandomReal:={o.RandomReal}
       else theRandomReal=o.RandomReal;
       for aGroup in theRandomReal do(
--     	 openedInputFile << "random_real "  ;
--     	 for j to #aGroup-2 do (openedInputFile <<toString (aGroup_j)  << ", ");
--     	 openedInputFile << toString(aGroup_(-1)) << " ; "<< endl
	 writeNamedListToB'InputFile("random_real",aGroup,openedInputFile)
	 ));
--Write the  constants and also the constant ii=I
     if #o.B'Constants=!=0 then (
     openedInputFile << "constant "  ;
     openedInputFile << "ii"  << ", ";
     pairsB'Constants:=for i in o.B'Constants list 
       if class i ===List then i else if class i===Option then toList i else error"B'Constants has an invalid element.";
     for j to #(pairsB'Constants)-2 do (openedInputFile <<toString ((pairsB'Constants)_j_0)  << ", ");
     openedInputFile << (pairsB'Constants_(-1))_0 << " ; "<< endl;
     openedInputFile << "ii = I"  << "; "<<endl;
     for onePair in (pairsB'Constants) do (
	 openedInputFile << toString(onePair_0) << " = " <<toString(onePair_1) <<" ; "<<endl
	 ));
--write just the constant "ii = I"
     if #o.B'Constants===0 then (
     openedInputFile << "constant "  ;
     openedInputFile << "ii"  << "; "<<endl;
     openedInputFile << "ii = I"  << "; "<<endl);       
     openedInputFile <<endl;
--
--We write the names of the polynomials we want to solve.
-- if B'Polynomials is not used then we do the following to name the polynomials.
     if #o.B'Polynomials===0 and #o.NamePolynomials=!=0 then   (  
     openedInputFile << "function "  ;
     for j to #(o.NamePolynomials)-2 do (openedInputFile <<toString ((o.NamePolynomials)_j)  << ", ");
     openedInputFile << (o.NamePolynomials_(-1)) << " ; "<< endl);
--if B'Polynomials is used then we do the following to name the polynomials.
     if #o.B'Polynomials=!=0  then (  
     openedInputFile << "function "  ;
     for j to #(o.B'Polynomials)-2 do (openedInputFile << "jade"|j  << ", ");
     openedInputFile << "jade"|toString(#(o.B'Polynomials)-1) << " ; "<< endl);      
--
     openedInputFile <<endl;
--Now we write B'Functions followed by the B'Polynomials.
--write the B'Functions
    if #o.B'Functions=!=0 then (
    for onePair in  o.B'Functions do (
      if class onePair===List 
      then openedInputFile << toString(onePair_0) << " = "<<toString(onePair_1)<< " ; "<<endl << endl;
      if class onePair===Option 
      then openedInputFile << toString( (toList onePair)_0) << " = "<<toString( (toList onePair)_1)<< " ; "<<endl << endl;
      if class onePair===B'Section 
      then (openedInputFile << toString(onePair#NameB'Section) << " = "<<par'String(onePair#B'SectionString)<< " ; "<<endl << endl ); 
      if class onePair===B'Slice 
      then for aSection to #(onePair#B'SectionString)-1 do  
        (openedInputFile << toString((onePair#NameB'Slice)_aSection) << " = "<<par'String((onePair#B'SectionString)_aSection)<< " ; "<<endl << endl ) 
 	    );
	openedInputFile << endl);
--Write the B'Polynomials
    if #o.B'Polynomials=!=0 then (
    for onePolynomialIndex to  #o.B'Polynomials-1 do (
      if class ((o.B'Polynomials)_onePolynomialIndex)===B'Section 
      then (
	if  member(NameB'Section,keys ((o.B'Polynomials)_onePolynomialIndex)) then print ("Warning: Element "|onePolynomialIndex|" of B'Polynomials is a B'Section with a set NameB'Section option that will be ignored. ");
	if not member(B'SectionString,keys ((o.B'Polynomials)_onePolynomialIndex)) then error("Element "|onePolynomialIndex|" of B'Polynomials is a B'Section with an unset B'SectionString option. ");
	openedInputFile << "jade"|toString(onePolynomialIndex) << " = "<<((o.B'Polynomials)_onePolynomialIndex)#B'SectionString<< " ; "<<endl << endl   
	)
      else if class ((o.B'Polynomials)_onePolynomialIndex)===B'Slice 
      then error("Element "|onePolynomialIndex|" of B'Polynomials is a B'Slice. B'Slice's must be converted to a list of B'Sections. ")
      else openedInputFile << "jade"|toString(onePolynomialIndex) << " = "<<toString((o.B'Polynomials)_onePolynomialIndex)<< " ; "<<endl << endl 
	    );
	openedInputFile << endl);    
    openedInputFile << "END;" << endl << endl;
    close openedInputFile        		);

writeNamedListToB'InputFile=(nameList,oneList,openedInputFile)->(
    openedInputFile << nameList|" "  ;
    for j to #oneList-2 do (openedInputFile <<toString (oneList_j)  << ", ");
    openedInputFile << toString(oneList_(-1)) << " ; "<< endl;
    openedInputFile <<endl;     
    )


makeWitnessSetFiles = method(TypicalValue => Nothing, Options=>{
	NameWitnessSliceFile=>"linear_slice_file",
    	NameSolutionsFile=>"witness_solutions_file",
	NameB'InputFile=>"input",
	SpecifyComponent=>-2,
	StorageFolder=>null,
	Verbose=>1
		})
makeWitnessSetFiles(String,Number) := o ->(IFD,theDim)->(
    IFD=addSlash(IFD);
    if o.StorageFolder=!=null 
    then (
	 filesGoHere:=addSlash(IFD|o.StorageFolder);
	 if not fileExists(filesGoHere) then mkdir(filesGoHere))
    else filesGoHere=addSlash(IFD);
    if not fileExists(filesGoHere|"witness_data") then error"witness_data file does not exist. ";
    s:= run("sed -i -e 's/%%%ENDCONFIG/TRACKTYPE : 4; %%%ENDCONFIG/' "|IFD|o.NameB'InputFile);
    tempfileName:="JADE_tracktype4_1";
    PFile:= openOut(filesGoHere|tempfileName); 
    PFile << toString(theDim) << endl ;
    PFile << toString(o.SpecifyComponent) << endl ;
    PFile << toString(o.NameSolutionsFile) << endl ;
    PFile << toString(o.NameWitnessSliceFile) << endl ;
    close PFile;
    runBertini(filesGoHere,TextScripts=>tempfileName,Verbose=>o.Verbose);
    removeFile(filesGoHere|tempfileName);    
        )

addSlash=(aString)->(
    if aString_-1===" " then error (aString|" cannot end with whitespace.");
    if aString_-1=!="/" then aString=aString|"/";
    return aString    )

makeSampleSolutionsFile = method(TypicalValue => Nothing, Options=>{
	NameSolutionsFile=>"sample_solutions_file",
	NameB'InputFile=>"input",
	StorageFolder=>null,
	SpecifyComponent=>{},
	Verbose=>1
		})
makeSampleSolutionsFile(String,Number) := o ->(IFD,aNumber)->(    
    IFD=addSlash(IFD);
    if o.StorageFolder=!=null 
    then (
	 filesGoHere:=addSlash(IFD|o.StorageFolder);
	 if not fileExists(filesGoHere) then mkdir(filesGoHere))
    else filesGoHere=addSlash(IFD);
    theNumberOfPoints:=aNumber;
    if o.SpecifyComponent==={} 
    then error"SpecifyComponent option must be set to a point or a list {dimension,component number}.";
    if  class o.SpecifyComponent===List     then (    
      theDim:=(o.SpecifyComponent)_0;
      theComponent:=(o.SpecifyComponent)_1) else if class o.SpecifyComponent===Point then(
      theDim=(o.SpecifyComponent)#Dimension;
      theComponent=(o.SpecifyComponent)#ComponentNumber);         
    if theNumberOfPoints<1 then error" The number of sample points should be positive. ";
    if not fileExists(filesGoHere|"witness_data") then error"witness_data file does not exist. ";
    s:= run("sed -i -e 's/%%%ENDCONFIG/TRACKTYPE : 2; %%%ENDCONFIG/' "|IFD|o.NameB'InputFile);
    tempfileName:="JADE_tracktype2_1";
    PFile:= openOut(filesGoHere|tempfileName); 
    PFile << toString(theDim) << endl ;
    PFile << toString(theComponent) << endl ;
    PFile << toString(theNumberOfPoints) << endl ;
    PFile << "0" << endl ;    
    PFile << toString(o.NameSolutionsFile) << endl ;
    close PFile;
    runBertini(IFD,TextScripts=>tempfileName,StorageFolder=>o.StorageFolder,Verbose=>o.Verbose);
    removeFile(filesGoHere|tempfileName)            )


makeMembershipFile = method(TypicalValue => Nothing, Options=>{
	NameSolutionsFile=>"member_points",
	NameB'InputFile=>"input",
	StorageFolder=>null,
	TestSolutions=>{},
	M2Precision=>53,
	Verbose=>1
		})
makeMembershipFile(String) := o ->(IFD)->(
    IFD=addSlash(IFD);
    if o.StorageFolder=!=null 
    then (
	 filesGoHere:=addSlash(IFD|o.StorageFolder);
	 if not fileExists(filesGoHere) then mkdir(filesGoHere))
    else filesGoHere=addSlash(IFD);
    if o.TestSolutions=!={} 
    then writeStartFile(IFD,o.TestSolutions,
	NameStartFile=>o.NameSolutionsFile,
	M2Precision=>o.M2Precision	);	
    if not fileExists(IFD|o.NameSolutionsFile) then error("The file "|o.NameSolutionsFile|" does not exist in "|IFD|". ");
    if o.Verbose>0 then print (filesGoHere);
    if o.Verbose>0 then print o.NameSolutionsFile;
    moveB'File(IFD,o.NameSolutionsFile,"member_points");    
    if not fileExists(filesGoHere|"witness_data") then error"witness_data file does not exist. ";
    s:= run("sed -i -e 's/%%%ENDCONFIG/TRACKTYPE : 3; %%%ENDCONFIG/' "|IFD|o.NameB'InputFile);
    runBertini(IFD,StorageFolder=>o.StorageFolder,Verbose=>o.Verbose)   
    )




--To do a trace test we need to evaluate the points using a Bertini Tracktype 4. 
--the number of variables in this input file equals the number of points times the number of coordinates.
makeB'TraceInput = method(TypicalValue => Nothing, Options=>{
	NameB'InputFile=>"inputTT",  --This option allows us to change the name of the input file that we will prodcue. (This imput file is super simple).
	B'Configs=>{}, --This option is a list of pairs of strings. These will be written in the CONFIG part of the Bertini input file. 
	})
makeB'TraceInput(String,Number,Number) := o ->(filesGoHere,NumberOfPoints,NumberOfCoordinates)->(    
    theVars:= for aSol  to NumberOfPoints-1 list for aCoordinate to NumberOfCoordinates-1 list ("jadeTT"|aSol|"v"|aCoordinate);
    makeB'InputFile(filesGoHere,
	NameB'InputFile=>o.NameB'InputFile,
	B'Configs=>o.B'Configs|{{"TRACKTYPE",-4}},
	AffVariableGroup=>theVars,
	B'Polynomials=>for aGroup in transpose theVars list ((makeB'Section(aGroup,B'NumberCoefficients=>for i in aGroup list 1/NumberOfPoints))#B'SectionString)
	))

replaceFirstLine = method(TypicalValue => Nothing, Options=>{
	})
replaceFirstLine(String,String,Thing) := o ->(filesGoHere,fileName,aString)->(
    if toString(filesGoHere)_-1==="/" then aDir:=filesGoHere else aDir=filesGoHere|"/";
    run("sed -i -e "|toExternalString("1s/.*/")|toString(aString)|toExternalString("/")|" "|aDir|fileName)
    )    

calculateB'Trace = method(TypicalValue=>Nothing,Options=>{
	NameStartFile=>"start",---we will read these start points.
	NameFunctionFile=>"calculatedTrace",---the traces will be written to this file.
	NameB'InputFile=>"inputTT"---this file should be created prior to calling the calculateB'Trace function.
	})
calculateB'Trace(String) := o ->(
    filesGoHere) ->(
     if filesGoHere_-1===" " then error (filesGoHere|" cannot end with whitespace.");
     if filesGoHere_-1=!="/" then filesGoHere=filesGoHere|"/";     
     if not fileExists(filesGoHere|o.NameStartFile) then error("The file "|o.NameStartFile|" does not exist in the directory.");
     if o.NameStartFile=!="start" then copyFile(filesGoHere|o.NameStartFile,filesGoHere|"start");
     replaceFirstLine(filesGoHere,"start",1);
     runBertini(filesGoHere,NameB'InputFile=>o.NameB'InputFile,Verbose=>o.Verbose);--maybe an error because of the backslash at the end. 
     if o.NameFunctionFile=!="function" then moveFile(filesGoHere|"function",filesGoHere|o.NameFunctionFile));      







----------------------------------------------------------------------------------
--Assume we are given a parameter 2 input file.
--We only change the FIRST PARAMETER.
--We import the start parameter P and fix a Gamma. 
--We take the trace of solutions for the parameter at P, P+Gamma, P-Gamma 
b'TraceTestImage=method(TypicalValue=>Thing,Options=>{ --assuming the directory contains a start file and start parameters and parameter homotopy file with one parameter
	NameB'InputFile=>"input",
	SubFolder=>false,
	B'Exe=>BERTINIexe,
	RandomGamma=>.27890+.31712*ii,
	StartPoints=>false,
	StartParameters=>false,
	MapPoints=>false,--(List of polynomials or a matrix of polynomials, list of variables)
	OnlyCalculateTrace=>false,
	M2Precision=>53,
	SubIntoCC=>true,
	StopBeforeTest=>false,
    	Verbose=>1
		})
b'TraceTestImage(String) := o ->(storeFiles)->(
    if storeFiles_-1===" " then error (storeFiles|" cannot end with whitespace.");
    if storeFiles_-1=!="/" then storeFiles=storeFiles|"/";
    IFD:=storeFiles;    
--
    if o.SubFolder=!=false then (
      storeFiles=IFD|"/"|o.SubFolder;
      if fileExists(storeFiles)===false then makeDirectory(storeFiles)
      );
    if o.StartPoints===false then(
      if false===fileExists(storeFiles|"/"|"start") then 
        error"The option StartPoints needs to be set or the Bertini 'start' file is in the wrong directory.");
    if class o.StartPoints===String then( 
      if false===fileExists(storeFiles|"/"|o.StartPoints) then error"The file "|storeFiles|"/"|o.StartPoints|" does not exist ";
      if o.StartPoints=!="start" then moveB'File(storeFiles,o.StartPoints,"start",CopyB'File=>true)
      );
    if class o.StartPoints===List then writeStartFile(storeFiles,o.StartPoints, M2Precision=>o.M2Precision);
    if o.StartParameters=!=false then writeParameterFile(storeFiles,o.StartParameters,M2Precision=>o.M2Precision,NameParameterFile=>"start_parameters");
    if o.StartParameters===false then (
      if false===fileExists(storeFiles|"/"|"start_parameters") then error"The file "|storeFiles|"/"|"start_parameters"|" does not exist "
      );
    startParameters:=importParameterFile(storeFiles,NameParameterFile=>"start_parameters",M2Precision=>o.M2Precision);
    if OnlyCalculateTrace=!=true then (
      writeParameterFile(storeFiles,{first startParameters+o.RandomGamma}|drop(startParameters,1), NameParameterFile=>"final_parameters",M2Precision=>o.M2Precision);
      runBertini(storeFiles,Verbose=>o.Verbose);
      moveB'File(storeFiles,"nonsingular_solutions","traceF");--F is for Forward
      writeParameterFile(storeFiles,{first startParameters-o.RandomGamma}|drop(startParameters,1), NameParameterFile=>"final_parameters",M2Precision=>o.M2Precision);
      runBertini(storeFiles,Verbose=>o.Verbose);
      moveB'File(storeFiles,"nonsingular_solutions","traceB");--B is for Backward
      );
    solsF:=importSolutionsFile(storeFiles,NameSolutionsFile=>"traceF",M2Precision=>o.M2Precision);
    solsC:=importSolutionsFile(storeFiles,NameSolutionsFile=>"start",M2Precision=>o.M2Precision);
    solsB:=importSolutionsFile(storeFiles,NameSolutionsFile=>"traceB",M2Precision=>o.M2Precision);
    if o.MapPoints=!=false then (
      functionMapPoints:=(o.MapPoints)_0;
      varsMapPoints:=(o.MapPoints)_1;    
      afterMapTrace:=({
        for oneSol in solsB list apply(functionMapPoints,i->subPoint(i,varsMapPoints,oneSol)),
        for oneSol in solsC list apply(functionMapPoints,i->subPoint(i,varsMapPoints,oneSol)),
        for oneSol in solsF list apply(functionMapPoints,i->subPoint(i,varsMapPoints,oneSol))
        }/sum)
      ) else afterMapTrace=({solsB,solsC,solsF}/sum);
    if o.StopBeforeTest===false then return ((afterMapTrace_0-afterMapTrace_1)-(afterMapTrace_1-afterMapTrace_2)) else return afterMapTrace
    )
------------------------------------------------------------------------------


--run("sed -i -e "|toExternalString("1s/.*/")|toString(STuFF)|toExternalString("/")|" "|theDir|"/input") 
readFile = method(TypicalValue => Nothing, Options=>{
	})
readFile(String,String,Number) := o ->(filesGoHere,fileName,aInteger)->(
    if toString(filesGoHere)_-1==="/" then aDir:=filesGoHere else aDir=filesGoHere|"/";
    aFile:=openIn(aDir|fileName);
    s:=read(aFile,aInteger);
    close aFile;
    return s
    );  
readFile(String,Number) := o ->(filesGoHere,aInteger)->(
    if toString(filesGoHere)_-1==="/" then aDir:=filesGoHere else aDir=filesGoHere|"/";
    aFile:=openIn(aDir|"bertini_session.log");
    s:=read(aFile,aInteger);
    close aFile;
    return s
    );
readFile(String) := o ->(filesGoHere)->(
    if toString(filesGoHere)_-1==="/" then aDir:=filesGoHere else aDir=filesGoHere|"/";
    aFile:=openIn(aDir|"bertini_session.log");
    s:=read(aFile,10000);
    close aFile;
    return s
    );  
  
 
 

valueBM2=method(TypicalValue=>String,Options=>{
	M2Precision=>53})
valueBM2(String) := o->(aString)->(
    if class aString =!=String 
    then error"Input should be a string. ";
    sepSpaces:=select("[0-9e.+-]+",aString);
    if #sepSpaces===2 
    then  (
      coordRealPart:=select("[0-9.+-]+",sepSpaces_0);
      coordImagPart:=select("[0-9.+-]+",sepSpaces_1);
      if #coordRealPart===1 then coordRealPart=append(coordRealPart,"0");
      if #coordImagPart===1 then coordImagPart=append(coordImagPart,"0");
      oneCoord:={coordRealPart_0,coordRealPart_1,coordImagPart_0,coordImagPart_1};
      return (value((oneCoord_0)|"p"|o.M2Precision|"e"|toString(value(oneCoord_1)))+
	ii*value((oneCoord_2)|"p"|o.M2Precision|"e"|toString(value(oneCoord_3)))
	  ))
    else if #sepSpaces===1 
    then (
      coordRealPart=select("[0-9.+-]+",sepSpaces_0);
      if #coordRealPart===1 then coordRealPart=append(coordRealPart,"0");
      oneCoord={coordRealPart_0,coordRealPart_1};
      return	(value((oneCoord_0)|"p"|o.M2Precision|"e"|toString(value(oneCoord_1)))
	  ))
    else error"String formatted incorrectly. "
    );

  
importSliceFile=method(TypicalValue=>String,Options=>{
	NameWitnessSliceFile=>"linear_slice_file",
 	Verbose=>1}    )
importSliceFile(String) := o->(aString)->(
    if aString_-1=!="/" then aString=aString|"/";
    allInfo:=lines get(aString|o.NameWitnessSliceFile);
    theConstants:={};
    theLinearSystems:={};
    for aLine in allInfo do (
      sepLine:=separate("=",aLine);
      if o.Verbose>1 then print sepLine;
      if o.Verbose>1 then print ( #sepLine);
      if #sepLine==2 then (
	if #select("const",sepLine_0)==1
	then theConstants=append(theConstants,{sepLine_0,
	  replace(";","",replace("I","ii",sepLine_1))});
	if #select("linear",sepLine_0)==1
	then theLinearSystems=append(theLinearSystems,{sepLine_0,replace(";","",sepLine_1)})	
	)	);
    return{theConstants,theLinearSystems}       
    )
  
importMainDataFile=method(TypicalValue=>String,Options=>{
	M2Precision=>53,
	NameMainDataFile=>"main_data",
	SpecifyDim=>false,
	Verbose=>1
	})
importMainDataFile(String) := o->(aString)->(
    aString=addSlash aString;
    allInfo:=lines get(aString|o.NameMainDataFile);
    theNumberOfVariables:=value ( (separate(" ",allInfo_0))_3);
    theVariables:=drop(separate(" ",allInfo_1),1);
    zeroDimCase:=false;
    posDimCase:=false;
    regenZeroDimCase:=false;
    if replace("-","",allInfo_3)=!=allInfo_3 
    then zeroDimCase=true
    else if replace("is being used","",allInfo_4)=!=allInfo_4 
    then regenZeroDimCase=true
    else posDimCase= true;
    if (zeroDimCase or regenZeroDimCase)
    then (
    if zeroDimCase then allInfo=drop(allInfo,4);
    if regenZeroDimCase then allInfo=drop(allInfo,7);
    linesPerSolutions:=theNumberOfVariables+13;
    theListOfPoints:={};
    while #select("Solution",allInfo_0)=!=0 do(
      if o.Verbose>1 then print "win";
      aNewPoint:=new Point;
      --Sol. Number and path number      
      theLine0:=separate(" ",allInfo_0);      
      aNewPoint.SolutionNumber=value (theLine0_1);
      if o.Verbose>1 then print theLine0;
      aNewPoint.PathNumber=value replace(")","",(theLine0_4));
      --Estimated condition number
      theLine1:=separate(":",allInfo_1);
      aNewPoint.ConditionNumber=valueBM2(theLine1_1);
      --FunctionResidual
      theLine2:=separate(":",allInfo_2);
      aNewPoint.FunctionResidual=valueBM2(theLine2_1);
      --NewtonResidual
      theLine3:=separate(":",allInfo_3);
      aNewPoint.NewtonResidual=valueBM2(theLine3_1);
      --FinalTvalue
      theLine4:=separate(":",allInfo_4);
      aNewPoint.FinalTValue=valueBM2(theLine4_1);
      --MaxPrecisionUtilized
      theLine5:=separate(":",allInfo_5);
      aNewPoint.MaxPrecisionUtilized=valueBM2(theLine5_1);
      --PrecisionIncreased
      theLine6:=separate(":",allInfo_6);
      aNewPoint.PrecisionIncreased=valueBM2(theLine6_1);
      --Accuracy Estimate1
      theLine7:=separate(":",allInfo_7);
      aNewPoint.AccuracyEstInternal=valueBM2(theLine7_1);
      --Accuracy Estimate2 
      theLine8:=separate(":",allInfo_8);
      if theLine8_1===replace("infinity","",theLine8_1) 
      then aNewPoint.AccuracyEst=valueBM2(theLine8_1)
      else aNewPoint.AccuracyEst= infinity;
      --CycleNumber
      theLine9:=separate(":",allInfo_9);
      aNewPoint.CycleNumber=valueBM2(theLine9_1);
      --coordinaes
      theCoords:={};
      for i to theNumberOfVariables-1 do(
	  theCoords=append(theCoords,valueBM2(allInfo_(i+10),M2Precision=>o.M2Precision) ) );
      aNewPoint.Coordinates=theCoords;
      --paths with same endpoint
      theLineX:=separate(":",allInfo_(10+theNumberOfVariables));
      aNewPoint.PathsWithSameEndpoint=drop(drop(separate("  ",theLineX_1),1),-1);--Why the double space? --Do we want all paths or other paths????
      --multiplicity
      theLineY:=separate(":",allInfo_(10+theNumberOfVariables+1));
      aNewPoint.Multiplicity=value(theLineY_1);
      theListOfPoints=append(theListOfPoints,aNewPoint);
      if o.Verbose>1 then   print linesPerSolutions;
      allInfo=drop(allInfo,linesPerSolutions);
      if o.Verbose>1 then print allInfo
      );
    return theListOfPoints);
    if posDimCase 
    then   (
    if o.Verbose>1 then print 1;
    allInfo=drop(allInfo,4);
    linesPerSolutions=theNumberOfVariables+6;
    theListOfPoints={};
    while #select("reproduce",allInfo_0)=!=1 do(
      if o.Verbose>1 then print 2;
      if #select("DIMENSION",allInfo_0)=!=0
      then (
	if o.Verbose>1 then print 3;
	theDim:=value (select("[0-9]+",allInfo_0))_0;
        if o.SpecifyDim=!=false and o.SpecifyDim=!=theDim then dimFlag:=false else dimFlag=true;
	allInfo=drop(allInfo,1))
      else if #select("NONSINGULAR",allInfo_0)=!=0 and #select("UNCLASSIFIED",allInfo_0)===0 
      then (
	if o.Verbose>1 then print 4;
	solUnclassified:=0;
	theSolutionType:="NONSINGULAR";
	allInfo=drop(allInfo,1))	
      else if #select("SINGULAR",allInfo_0)=!=0 and #select("NON",allInfo_0)===0 and #select("UNCLASSIFIED",allInfo_0)===0   
      then (
	if o.Verbose>1 then print 5;
	solUnclassified=0;
	theSolutionType="SINGULAR";
	allInfo=drop(allInfo,1))
      else if #select("UNCLASSIFIED NONSINGULAR",allInfo_0)=!=0  
      then (
	if o.Verbose>1 then print 5.1;
	solUnclassified=1;
	theSolutionType="NONSINGULAR";
	allInfo=drop(allInfo,1))
      else if #select("UNCLASSIFIED SINGULAR",allInfo_0)=!=0  
      then (
	if o.Verbose>1 then print 5.2;
    	solUnclassified=1;
	theSolutionType="SINGULAR";
	allInfo=drop(allInfo,1))
      else if #select("---------------",allInfo_0)=!=0 
      then (
	if dimFlag 
	then (
	aNewPoint=new Point;
	aNewPoint.Dimension=theDim;
	aNewPoint.SolutionType=theSolutionType;
	aNewPoint.PathNumber=value ((separate(":",allInfo_1))_1);
--
	if solUnclassified===0 
	then  aNewPoint.ComponentNumber=value ((separate(":",allInfo_2))_1)
        else aNewPoint.ComponentNumber=-1;
	aNewPoint.ConditionNumber=valueBM2((separate(":",allInfo_(3-solUnclassified)))_1);
      	theCoords={};
      	for i to theNumberOfVariables-1 do(
	  theCoords=append(theCoords,valueBM2(allInfo_(i+4-solUnclassified)) ) );
        aNewPoint.Coordinates=theCoords;
      --multiplicity
        aNewPoint.Multiplicity=value( (separate(":",allInfo_(4+theNumberOfVariables-solUnclassified)))_1);
        aNewPoint.DeflationsNeeded=value( (separate(":",allInfo_(4+theNumberOfVariables+1-solUnclassified)))_1);      
      	theListOfPoints=append(theListOfPoints,aNewPoint);
      	--print linesPerSolutions;
      	allInfo=drop(allInfo,linesPerSolutions-solUnclassified))
        else (allInfo=drop(allInfo,linesPerSolutions); print "1"	))
      else allInfo=drop(allInfo,1));
    return theListOfPoints
    ))
    


--FUNCTION 2 runBertini
--To run bertini we need to say where we want to output the files. 
--Additional options are speciifying the location of the input file (the default is that the input file is located where we output the files)
--B'Exe is how we call Bertini. The default option is how Bertini is usually called in M2 in the init file. 
--InputFileName is default to be input. But we can change this if we wanted to. 
runBertini= method(TypicalValue => String, Options=>{
	NameB'InputFile=>"input",
    	StorageFolder=>null,
	PreparePH2=>false,
	B'Exe=>BERTINIexe,
	TextScripts=>"",
	Verbose=>1})
runBertini(String) := o ->(IFD)->(--IFD=input file directory
    	IFD=addSlash(IFD);
    	if o.StorageFolder=!=null 
    	then (
	  filesGoHere:=addSlash(IFD|o.StorageFolder);
	  if not fileExists(filesGoHere) then mkdir(filesGoHere))
        else filesGoHere=addSlash(IFD);
    	if o.TextScripts=!="" then theTS:=" < "|o.TextScripts else theTS="";
    	if o.Verbose>0 then print o.B'Exe;
    	runSuccess:=run("cd "|filesGoHere|"; "|(o.B'Exe)|" "|IFD|o.NameB'InputFile|theTS|" >bertini_session.log");
    	if runSuccess=!=0 
	then (
	  print fileExists(filesGoHere|"bertini_session.log");
	  print readFile(filesGoHere,"bertini_session.log",10000);
	  error"Bertin run failed. ");
	if o.PreparePH2=!=false and runSuccess===0
	then (
	  s:= run("sed -i -e 's/%%%ENDCONFIG/	 PARAMETERHOMOTOPY : 2; %%%ENDCONFIG/' "|IFD|o.NameB'InputFile);
	  moveFile(filesGoHere|"nonsingular_solutions",filesGoHere|"start"));		   
	);

--Helper function
convertRealNumber=(aNumber)->(
    realPartSeparate:=separate("p",toExternalString ( aNumber));
    realPartMantissa:=realPartSeparate_0;
    if 1=!=#realPartSeparate 
    then (separateExponent:=separate("e",realPartSeparate_1);
    	if 1==#separateExponent
    	then realPartExponent:="0"
    	else realPartExponent=(separateExponent)_1;
    	return(realPartMantissa|"e"|realPartExponent))
    else return(realPartMantissa|"e0"));


--takes a number and outputs a string to write in a bertini file: ###e# ###e#
NumberToB'String= method(TypicalValue => Thing, Options=>{
	M2Precision=>53})
NumberToB'String(Thing) := o ->(aNumber)->(
    if class aNumber ===String then print "Warning: String may not  be converted correctly.";
    if class aNumber ===QQ then print "Warning: rational numbers will be converted to floating point.";
    if class aNumber ===String then return aNumber;
    aCNumber:=sub(aNumber,CC_(o.M2Precision));
    return(convertRealNumber(realPart aCNumber)|" "|convertRealNumber(imaginaryPart aCNumber))
    )	;  

--takes a number and outputs a string to write in a bertini file: ###e# ###e#
importParameterFile= method(TypicalValue => String, Options=>{
	M2Precision=>53,
	NameParameterFile=>"final_parameters",
	StorageFolder=>null})
importParameterFile(String) := o ->(aString)->(
    aString=addSlash aString;
    if o.StorageFolder=!=null 
    then aString=addSlash(aString|o.StorageFolder);
    if class o.NameParameterFile===String then NPF:=o.NameParameterFile;
    if o.NameParameterFile===1 then NPF="start_parameters";
    if o.NameParameterFile===2 then NPF="final_parameters";
    if o.NameParameterFile===3 then NPF="random_values";    
    aString=aString|NPF;
    if false===fileExists aString 
    then error"The file "|NPF|" does not exist at "|aString|". ";
    getLines:=apply(lines get (aString),i->select("[0-9e.+-]+",i)); -- grabs all lines of the solution file and selects desired words
    expectedNumberOfParameters:=value (getLines_0_0);
    getLines=drop(getLines,2);
    collectedCoordinates:={};
    for i in getLines do (
    if #i==2 then  (
	coordRealPart:=select("[0-9.+-]+",i_0);
	coordImagPart:=select("[0-9.+-]+",i_1);
	if #coordRealPart===1 then coordRealPart=append(coordRealPart,"0");
	if #coordImagPart===1 then coordImagPart=append(coordImagPart,"0");
	oneCoord:={coordRealPart_0,coordRealPart_1,coordImagPart_0,coordImagPart_1};
	collectedCoordinates=append(collectedCoordinates,
	    value((oneCoord_0)|"p"|o.M2Precision|"e"|toString(value(oneCoord_1)))+
	    ii*value((oneCoord_2)|"p"|o.M2Precision|"e"|toString(value(oneCoord_3)))
		   )) else
    if  #i>2  then print ("Warning, a line was not parsed: "|i_0|"...");
    if  #i===1 then   print ("Warning, a line was not parsed: "|i_0|"...");
    );
    if #collectedCoordinates=!= expectedNumberOfParameters then 
    print("Warning: Expected "|expectedNumberOfParameters|" parameter(s) but found "|toString(#collectedCoordinates)|" parameter(s)."); 
    return collectedCoordinates);  --This needs to be documented


writeParameterFile = method(TypicalValue=>Nothing,Options=>{
	NameParameterFile=>"final_parameters",
	M2Precision=>53,
	StorageFolder=>null
	})
writeParameterFile(String,List) := o ->(IFD,listParameters)->(
     IFD=addSlash IFD;
     if o.StorageFolder=!=null 
     then (
	 filesGoHere:=addSlash(IFD|o.StorageFolder);
	 if not fileExists(filesGoHere) then mkdir(filesGoHere))
     else filesGoHere=addSlash(IFD);
     PFile:= openOut(filesGoHere|o.NameParameterFile); 
     PFile << toString(length listParameters) << endl << endl;
     for c in listParameters do (
	 	 PFile <<NumberToB'String(c,M2Precision=>o.M2Precision) <<endl
	 );
     PFile << endl;      
     close PFile);      

writeStartFile = method(TypicalValue=>Nothing,Options=>{
	NameStartFile=>"start",
	M2Precision=>53,
    	StorageFolder=>null	
	})
writeStartFile(String,List) := o ->(IFD,listOfListCoords) ->(    
     IFD=addSlash(IFD);
     if o.StorageFolder=!=null 
     then (
	 filesGoHere:=addSlash(IFD|o.StorageFolder);
	 if not fileExists(filesGoHere) then mkdir(filesGoHere))
     else filesGoHere=addSlash(IFD);
     PFile:= openOut(filesGoHere|o.NameStartFile); 
     PFile << toString(length listOfListCoords) << endl ;
     for listCoords in listOfListCoords do (
	 PFile<<endl;
	 for c in listCoords do(
             PFile <<NumberToB'String(c,M2Precision=>o.M2Precision) <<endl
	 ));
     PFile << endl;      
     close PFile);      



importSolutionsFile= method(TypicalValue=>Nothing,Options=>{
	NameSolutionsFile=>"raw_solutions",
	M2Precision=>53, OrderPaths=>false,
	StorageFolder=>null,
	Verbose=>1 })
importSolutionsFile(String) := o -> (importFrom)-> (
    importFrom=addSlash importFrom;
    if o.StorageFolder=!=null 
    then importFrom=addSlash(importFrom|o.StorageFolder);
    if  class o.NameSolutionsFile===String then NSF:=o.NameSolutionsFile;
    if  o.NameSolutionsFile===0 then NSF="nonsingular_solutions";
    if  o.NameSolutionsFile===1 then NSF="real_finite_solutions";
    if  o.NameSolutionsFile===2 then NSF="infinite_solutions";
    if  o.NameSolutionsFile===3 then NSF="finite_solutions";        
    if  o.NameSolutionsFile===4 then NSF="start";        
    if  o.NameSolutionsFile===5 then NSF="raw_solutions";        
    importFrom=importFrom|NSF;
    if false=== fileExists importFrom then error ("File "|NSF|" does not exist.");
    importedFileLines := apply(lines get (importFrom),i->select("[0-9.e+-]+",i)); -- grabs all lines of the solution file and selects desired words.
    if o.Verbose>1 then for i in importedFileLines do print i;
    numberOfsolutionsInFile:=value(importedFileLines_0_0);--the first line of the solution file gives the number of solutions in the file
    if numberOfsolutionsInFile==0 then return {};
    importedFileLines=drop(importedFileLines,1);--drop the first  line
    storeSolutions:={};---We will store the solutions we specified and return this in the end
    collectedCoordinates:={};
    if o.Verbose>1 then print collectedCoordinates;
    if o.OrderPaths===false then(
    for i in importedFileLines do(
    	if o.Verbose>1 then print( "i",i);
	if #i==2 then  (
	    coordRealPart:=select("[0-9.+-]+",i_0);
	    coordImagPart:=select("[0-9.+-]+",i_1);
	    if #coordRealPart===1 then coordRealPart=append(coordRealPart,"0");
	    if #coordImagPart===1 then coordImagPart=append(coordImagPart,"0");
	    oneCoord:={coordRealPart_0,coordRealPart_1,coordImagPart_0,coordImagPart_1};
--	    print oneCoord;
	    collectedCoordinates=append(collectedCoordinates,
	    	value((oneCoord_0)|"p"|o.M2Precision|"e"|toString(value(oneCoord_1)))+
	    	ii*value((oneCoord_2)|"p"|o.M2Precision|"e"|toString(value(oneCoord_3)))
		   ));
--	print collectedCoordinates;
    	if  #i>2  then error ("Line was not parsed: "|i_0|"...")));
    if o.OrderPaths===true then(
    solutionCount:=0;
    for i in importedFileLines do(
      if #i==1 then (
	collectedCoordinates=append(collectedCoordinates,value(i_0));
	solutionCount=solutionCount+1);
      if #i==2 then (
        coordRealPart:=select("[0-9.+-]+",i_0);
	coordImagPart:=select("[0-9.+-]+",i_1);
	if #coordRealPart===1 then coordRealPart=append(coordRealPart,"0");
	if #coordImagPart===1 then coordImagPart=append(coordImagPart,"0");
	oneCoord:={coordRealPart_0,coordRealPart_1,coordImagPart_0,coordImagPart_1};
	if o.Verbose>1 then print oneCoord;
	collectedCoordinates=append(collectedCoordinates,
	  value((oneCoord_0)|"p"|o.M2Precision|"e"|toString(value(oneCoord_1)))+
	    ii*value((oneCoord_2)|"p"|o.M2Precision|"e"|toString(value(oneCoord_3)))
	      ));
	if o.Verbose>1 then print collectedCoordinates;
    	if  #i>2  then error ("Line was not parsed: "|i_0|"...")));
    	numberOfCoordinates:=numerator(#collectedCoordinates/numberOfsolutionsInFile);
	if o.Verbose>1 then print numberOfCoordinates;
    	storeSolutions=for i to numberOfsolutionsInFile-1 list 
	  for j to numberOfCoordinates-1 list collectedCoordinates_(i*numberOfCoordinates+j);
    	if o.OrderPaths===true then(
	  if o.Verbose>1 then print "inLoop";
	  sortStoreSolutions:=sort storeSolutions;
	  storeSolutions=for i in sortStoreSolutions list drop(i,1);   
    	  if o.Verbose>1 then for i in sortStoreSolutions do print i_0;
    	  if #storeSolutions=!=solutionCount then print "Warning: Unexpected solution count. OrderPaths option should only be set to 'true' when importing solution files with path numbers."
	    );	
    return storeSolutions    );



importIncidenceMatrix= method(TypicalValue=>Nothing,Options=>{
	NameIncidenceMatrixFile=>"incidence_matrix",
	StorageFolder=>null,
	Verbose=>1 })
importIncidenceMatrix(String) := o -> (importFrom)-> (
    if  class o.NameIncidenceMatrixFile===String 
    then NSF:=o.NameIncidenceMatrixFile;
    importFrom=addSlash importFrom;
    if o.StorageFolder=!=null 
    then importFrom=addSlash(importFrom|o.StorageFolder);
    importFrom=importFrom|NSF;
    if false=== fileExists importFrom then error ("File "|NSF|" does not exist.");
    importedFileLines := apply(lines get (importFrom),i->select("[0-9.e+-]+",i)); -- grabs all lines of the file and selects desired words.
    if o.Verbose>1 then for i in importedFileLines do print i;
    numberOfNonEmptyCodims:=value(importedFileLines_0_0);--the first line of the incident_matrix file gives the number of non-empty codims. see page p.299 of [NSPSB] 
    importedFileLines=drop(importedFileLines,1);--drop the first  line
    indexListForComponents:={};---We will index the components by codimension and component number. 
    for i to numberOfNonEmptyCodims-1 do (
      currentCodim:=value(importedFileLines_i_0);
      numberOfComponentsInCurrentCodim:=value(importedFileLines_i_1);
      for i to numberOfComponentsInCurrentCodim-1 do indexListForComponents=append(indexListForComponents,(currentCodim,i))
      );
    totalNumComponents:=#indexListForComponents;
    importedFileLines=drop(importedFileLines,numberOfNonEmptyCodims+1);--the plus one is for an empty line that we need to drop
    numberOfTestPoints:=value(importedFileLines_0_0);
    importedFileLines=drop(importedFileLines,1+1);--the plus one is for an empty line that we need to drop
    incidenceList:={};
    for i to numberOfTestPoints-1 do(
      inComponent:={};
      for oneTest to totalNumComponents-1 do if value(importedFileLines_i_oneTest)===1 then inComponent=append(inComponent,indexListForComponents_oneTest);
      incidenceList=append(incidenceList,inComponent)
      );
    return incidenceList    );


---
--This function takes as its input a directory and a list of list of parameter values.
b'PHSequence=method(TypicalValue=>Thing,Options=>{
	NameB'InputFile=>"input",
	NameStartFile=>"start",
	NameParameterFile=>"start_parameters",
	NameSolutionsFile=>"nonsingular_solutions",
    	StorageFolder=>null,
	SaveData=>false, 
	B'Exe=>BERTINIexe,
	SolutionFileStyle=>"simple"	,
	Verbose=>1
	})
b'PHSequence(String,List) := o ->(IFD,listOfListOfParameterValues)->(
    IFD=addSlash IFD;
    if fileExists(IFD|o.NameB'InputFile)===false then error "input file does not exist in correct directory.";
    if fileExists(IFD|o.NameStartFile)===false then error "start file does not exist in correct directory.";
    if fileExists(IFD|o.NameParameterFile)===false then error "start_parameters file does not exist in correct directory.";        
--
    if o.StorageFolder=!=null 
    then (
      storeFiles:=addSlash(IFD|o.StorageFolder);
      if not fileExists(storeFiles) then mkdir(storeFiles))
    else storeFiles=addSlash(IFD);
    if o.NameStartFile=!="start" or null=!=o.StorageFolder 
    then moveB'File(IFD,o.NameStartFile,"start",SubFolder=>o.StorageFolder,CopyB'File=>true);
--
    if o.NameParameterFile=!="start_parameters" or null=!=o.StorageFolder  
    then moveB'File(IFD,o.NameParameterFile,"start_parameters",SubFolder=>o.StorageFolder,CopyB'File=>true);
--
    if  null=!=o.StorageFolder  
    then moveB'File(IFD,o.NameB'InputFile,o.NameB'InputFile,SubFolder=>o.StorageFolder,CopyB'File=>true);
----a check to see if we keep all solutions NEED to be made. 
    runCount:=0;
    sfIn:=openIn(storeFiles|"start");
    NumPathsToTrack:=value(read(sfIn,1));
    close sfIn;
    if o.Verbose>1 then print (NumPathsToTrack, "paths");
    for listPV in listOfListOfParameterValues do(
	runCount=1+runCount;
    	if o.Verbose>1 then print ("rc seq",runCount);
    	writeParameterFile(storeFiles,listPV,NameParameterFile=>"final_parameters");--writes final parameter file
	if o.Verbose>1 then print listPV;
	runBertini(storeFiles,Verbose=>o.Verbose,
	    NameB'InputFile=>o.NameB'InputFile,B'Exe=>o.B'Exe);
	if o.Verbose>1 then print "-rc";
    	if fileExists(storeFiles|o.NameSolutionsFile)===false and o.NameSolutionsFile=!="simple_raw_solutions" then (
	  start0pnts:= openOut(storeFiles|"start");  
     	  start0pnts << "0" << endl << endl;
	  close start0pnts;
	  print ("Warning: No paths in this seqence were successfuly tracked: Run count "|toString(runCount));
	  break);
	if o.NameSolutionsFile==="simple_raw_solutions" then  (
	  simplifyRawSolutions(storeFiles));	  
	if o.NameSolutionsFile==="raw_solutions" 
	then  error "NameSolutionsFiles should not be set as raw_solutions, instead set as simple_raw_solutions";	  
	if o.SaveData then (
	    moveB'File(storeFiles,o.NameSolutionsFile,o.NameSolutionsFile|toString(runCount),CopyB'File=>true);
	    moveB'File(storeFiles,"final_parameters","start_parameters"|toString(runCount),CopyB'File=>true)
	    );
	if o.Verbose>1 then print o.NameSolutionsFile;
	if o.Verbose>1 then print ("Number of solutions: "|toString(#importSolutionsFile(storeFiles,NameSolutionsFile=>o.NameSolutionsFile|toString(runCount))));
	moveB'File(storeFiles,"final_parameters","start_parameters");
	moveB'File(storeFiles,o.NameSolutionsFile,"start");
    	sfIn=openIn(storeFiles|"start");
	NumPathsTracked:=value(read(sfIn,1));
	close sfIn;
	if NumPathsToTrack=!=NumPathsTracked then print ("Warning: The number of paths tracked in this seqence dropped at iteration "|toString(runCount));
	if 0===NumPathsTracked then print ("Warning: There are no paths to track at iteration "|toString(runCount));
    	if o.Verbose>1 then print "seq";
	if o.Verbose>1 then print(importSolutionsFile(storeFiles,NameSolutionsFile=>"start"));
    	));
 
simplifyRawSolutions=(aDirectory)->(
    importedFileLines := apply(lines get (aDirectory|"raw_solutions"),i->select("[0-9.e+-]+",i)); -- grabs all lines of the solution file and selects desired words.
    numberOfsolutionsInFile:=value(importedFileLines_0_0);--the first line of the solution file gives the number of solutions in the file
    importedFileLines=drop(importedFileLines,1);--drop the first  line
    collectedCoordinates:={};
    solutionCount:=0;
    for i in importedFileLines do(
      if #i==1 then (
	collectedCoordinates=append(collectedCoordinates,value(i_0));
	solutionCount=solutionCount+1);
      if #i==2 then (
        coordRealPart:=select("[0-9.+-]+",i_0);
	coordImagPart:=select("[0-9.+-]+",i_1);
	if #coordRealPart===1 then coordRealPart=append(coordRealPart,"0");
	if #coordImagPart===1 then coordImagPart=append(coordImagPart,"0");
	oneCoord:={coordRealPart_0,coordRealPart_1,coordImagPart_0,coordImagPart_1};
        collectedCoordinates=append(collectedCoordinates,
	  (oneCoord_0)|"e"|toString(value(oneCoord_1))|" "|(oneCoord_2)|"e"|toString(value(oneCoord_3)))
		   );
      if  #i>2  then error ("Line was not parsed: "|i_0|"..."));
    numberOfCoordinates:=numerator(#collectedCoordinates/numberOfsolutionsInFile); --This also counts the path number as a coordinate.
    storeSolutions:=for i to numberOfsolutionsInFile-1 list 
      for j to numberOfCoordinates-1 list collectedCoordinates_(i*numberOfCoordinates+j);
    sortStoreSolutions:=sort storeSolutions;
    storeSolutions=for i in sortStoreSolutions list drop(i,1);   
    if #storeSolutions=!=solutionCount then print "Warning: Unexpected solution count. OrderPaths option should only be set to 'true' when importing solution files with path numbers.";
    orderedStartFile:=openOut (aDirectory|"simple_raw_solutions");
    orderedStartFile << toString numberOfsolutionsInFile <<endl;
    for oneSolution in storeSolutions do (
	orderedStartFile<<endl;
	for aString in oneSolution do (orderedStartFile <<aString<<endl) );
    close orderedStartFile;
--    print ("Wrote simple_raw_solutions to "|aDirectory|"simple_raw_solutions");
--    print "22222222222222222222";
    return storeSolutions    );
     

b'PHMonodromyCollect=method(TypicalValue=>Thing,Options=>{
	NameB'InputFile=>"input",
	NameStartFile=>"start",
	NameParameterFile=>"start_parameters",
	NameSolutionsFile=>"simple_raw_solutions",
    	StorageFolder=>null,
	SaveData=>false,
	B'Exe=>BERTINIexe,
	MonodromyStartPoints=>false,
	MonodromyStartParameters=>false,
  	NumberOfLoops=>1,
  	NumSolBound=>infinity,
	SpecifyLoops=>false,
	Verbose=>1
	})
b'PHMonodromyCollect(String) := o ->(IFD)->(
    IFD=addSlash(IFD);
    if o.Verbose>0 then print IFD;
--
    if o.MonodromyStartPoints=!=false then writeStartFile(IFD,o.MonodromyStartPoints);--write a start file.
    if o.MonodromyStartParameters=!=false then writeParameterFile(IFD,o.MonodromyStartParameters,NameParameterFile=>"start_parameters");--write a start_parameter file.
    if fileExists(IFD|o.NameB'InputFile)===false then error "input file does not exist in correct directory.";
    if fileExists(IFD|o.NameStartFile)===false then error "start file does not exist in correct directory or MonodromyStartPoints needs to be set.";
    if fileExists(IFD|o.NameParameterFile)===false then error "start_parameters file does not exist in correct directory or MonodromyStartParameters needs to be set.";        
--
    if o.StorageFolder=!=null 
    then (
      storeFiles:=addSlash(IFD|o.StorageFolder);
      if false===fileExists storeFiles then mkdir storeFiles)
    else storeFiles=addSlash(IFD);
    if o.NameStartFile=!="start" or null=!=o.StorageFolder 
    then moveB'File(IFD,o.NameStartFile,"start",SubFolder=>o.StorageFolder,CopyB'File=>true);
    if o.NameParameterFile=!="start_parameters" or null=!=o.StorageFolder  
    then moveB'File(IFD,o.NameParameterFile,"start_parameters",SubFolder=>o.StorageFolder,CopyB'File=>true);
    if null=!=o.StorageFolder  
    then moveB'File(IFD,o.NameB'InputFile,o.NameB'InputFile,SubFolder=>o.StorageFolder,CopyB'File=>true);
--
    bP:=importParameterFile(storeFiles,NameParameterFile=>"start_parameters");---we pull the base parameters
    solCollection:= importSolutionsFile(storeFiles,NameSolutionsFile=>"start");
    loopCount:=0;
    if o.Verbose>1 then print solCollection;
    breakLoop:=false;
    if o.SpecifyLoops=!=false then listsOfListsOfParameterValues:=o.SpecifyLoops;
    while not breakLoop do(
      if (o.SpecifyLoops===false) then listsOfListsOfParameterValues={for i to 2-1 list for j to #bP-1 list (2*random(CC)-random(CC))};
      for listsOfParameterValues in listsOfListsOfParameterValues do(
	loopCount=loopCount+1;
	if o.Verbose>0 then print ("Monodromy loop number",loopCount);
--    	print (	    append(listsOfParameterValues,bP));	
        if fileExists(storeFiles|"start")===false then error "start file for b'PHSequence is missing.";
        if fileExists(storeFiles|o.NameParameterFile)===false then error "start_parameters file for b'PHSequence is missing.";        
    	b'PHSequence(storeFiles,
	    append(listsOfParameterValues,bP),
	    B'Exe=>o.B'Exe,
	    NameSolutionsFile=>o.NameSolutionsFile,---ProblemLine
       	    NameB'InputFile=>o.NameB'InputFile);
	if o.Verbose>1 then print ".5";
    	if o.Verbose>1 then print (importSolutionsFile(storeFiles,NameSolutionsFile=>"start"));
	preSolCollection:=sortSolutions(
	    solCollection|importSolutionsFile(storeFiles,NameSolutionsFile=>"start"));
    	if o.Verbose>1 then print "1";
    	if o.Verbose>1 then print preSolCollection;
--removing multiplicities 
    	if #preSolCollection>=2 then (
	    solCollection={};
	    for i to #preSolCollection-2 do 
	    if (not areEqual(preSolCollection_i,preSolCollection_(i+1),Tolerance=>1e-6)) 
	    then (
--    	      print (preSolCollection_i,preSolCollection_(i+1));
--	      print areEqual(preSolCollection_i,preSolCollection_(i+1)) ;
	      solCollection=append(solCollection,preSolCollection_i));
	    solCollection=append(solCollection,preSolCollection_-1));
    	if #preSolCollection<2 then solCollection=preSolCollection;
	if o.Verbose>1 then print ("Number of solutions found", #solCollection));
      writeStartFile(storeFiles,solCollection);
      if loopCount>=o.NumberOfLoops then (
	breakLoop=true;
	if o.Verbose>1 then print "NumberOfLoops has been reached.");
      if #solCollection>= o.NumSolBound then (
	breakLoop=true;
	if o.Verbose>0 then print (#solCollection);
	if o.Verbose>1 then print ("Number of loops: "|toString loopCount);
	if o.Verbose>1 then print "NumSolBound has been reached");      
      );
    return solCollection);
	    
----

radicalList=method(TypicalValue=>Thing,Options=>{
	})
radicalList(List,Number) := o ->(aList,aTolerance)->(
    newList:={aList_0};
    for i to #aList-1 do (
	appendToList:=true;
	for j in newList do if (abs(j-aList_i)<aTolerance) then appendToList=false;
	if appendToList then newList=append(newList,aList_i));
    return newList)
radicalList(List) := o ->(aList)->(
    aTolerance:=1e-10;
    newList:={aList_0};
    for i to #aList-1 do (
	appendToList:=true;
	for j in newList do if (abs(j-aList_i)<aTolerance) then appendToList=false;
	if appendToList then newList=append(newList,aList_i));
    return newList)     

b'PHGaloisGroup=method(TypicalValue=>Thing,Options=>{
    	LoopRadius=>{},
	NameB'InputFile=>"input",  --this is the input file that allows us to do the parameter homotopies
	NameStartFile=>"start",  --this start file will be the solutions we start with
	NameParameterFile=>"start_parameters", --this file is the start_parameters for our start solutions
	NameSolutionsFile=>"simple_raw_solutions",--this file tells us which solutions to consider at the end
	SaveData=>false,--if true then we save the NameSolutionsFile's that we produce after the completetion of each loop
	B'Exe=>BERTINIexe,
	MonodromyStartPoints=>false,--This will write a start file in the StartFileDirectory with the name NameStartFile
	MonodromyStartParameters=>false,--This will write a start_parameters file in the StartParametersFileDirectory with the name NameParameterFile
  	NumberOfLoops=>1,--This is the number of loops we will perform. 
	BranchPoints=>{.12415+.34636*ii},
	NameGaloisGroupGeneratorFile=>"gggFile",
	M2Precision=>52,
	ReturnGaloisGroupGeneratorFile=>true,
    	StorageFolder=>null,
	Verbose=>1	
--	SpecifyLoops=>false
	})
b'PHGaloisGroup(String) := o ->(IFD)->(
    IFD=addSlash(IFD);
--
    if o.StorageFolder=!=null 
    then (
      storeFiles:=addSlash(IFD|o.StorageFolder);
      if false === fileExists(storeFiles) then mkdir storeFiles) 
    else storeFiles=addSlash(IFD);
    --For b'PHGaloisGroup to run we need to have a start file that consists of solutions for a general choice of parameters. 
    --There are two ways to get this start file. The first way is to tell M2 where to find the start file by specifying NameStartFile option. 
    --The other way is by specifying MonodromyStartPoints to a list of points or solutions. 
    --THe following lines check to see if the configurations to run b'PHGaloisGroup are set correctly in regards to the start file. 
    if o.MonodromyStartPoints=!=false then writeStartFile(IFD,o.MonodromyStartPoints);--write a start file.
    if fileExists(IFD|o.NameB'InputFile)===false then error "input file does not exist in correct directory.";
    if o.MonodromyStartPoints===false and fileExists(IFD|o.NameStartFile)===false then error "MonodromyStartPoints needs to be set.";
    --For b'PHGaloisGroup to run we need to have a start_parameter file that consists of a general choice of parameters. 
    --There are two ways to get this start_parameter file. The first way is to tell M2 where to find the start_parameter file by specifying NameParameterFile options. 
    --The other way is by specifying MonodromyStartParameters to a list of parameters. 
    --THe following lines check to see if the configurations to run b'PHGaloisGroup are set correctly in regards to the start_parameter file. 
    if o.MonodromyStartParameters===false and fileExists(IFD|o.NameParameterFile)===false then error "start_parameters file does not exist in correct directory or MonodromyStartParameters needs to be set.";        
    if o.MonodromyStartParameters=!=false then writeParameterFile(IFD,o.MonodromyStartParameters,NameParameterFile=>"start_parameters");--write a start_parameter file. 
    --Now we want to do our computations in a the directory specified by storeFiles. 
    --So we copy files from IFD to the directory given by storeFiles. 
    if o.Verbose>1 then print 1;
    if o.NameStartFile=!="start" or null=!=o.StorageFolder 
    then moveB'File(IFD,o.NameStartFile,"start",SubFolder=>o.StorageFolder,CopyB'File=>true);
    if o.Verbose>1 then print 2;
    if o.NameParameterFile=!="start_parameters" or null=!=o.StorageFolder 
    then moveB'File(IFD,o.NameParameterFile,"start_parameters",SubFolder=>o.StorageFolder,CopyB'File=>true);
    if o.Verbose>1 then print 3;
    if null=!=o.StorageFolder 
    then moveB'File(IFD,o.NameB'InputFile,o.NameB'InputFile,SubFolder=>o.StorageFolder,CopyB'File=>true);
    if o.Verbose>1 then print "3.1";
    --We save the start points in a text file by copying it to "ggStartJade" and also in memory as solCollection.
    moveB'File(IFD,"start","ggStartJade",SubFolder=>o.StorageFolder,CopyB'File=>true);
    --Now all computations are done 
    solCollection:= importSolutionsFile(storeFiles,NameSolutionsFile=>"ggStartJade",M2Precision=>o.M2Precision);
    --We save the start points' parameters as bP in memory rather than a text file. 
    basePointT:=(importParameterFile(storeFiles,NameParameterFile=>"start_parameters",M2Precision=>o.M2Precision));
--    print basePointT;
    if #basePointT=!=1 then error "The base point downstairs can only have one coordinate. Parameter space should be restricted to a line parameterized by one copy of complex numbers.";
    basePointT=basePointT_0;
--    print basePointT;
    --Now we will perform monodromy loops. We keep track of the number of loops we have performed by loopCount.
    loopCount:=0;
    breakLoop:=false;
    if class o.BranchPoints=!=String 
    then branchPointsT:=(o.BranchPoints)
    else branchPointsT =radicalList(    flatten flatten importSolutionsFile(IFD,
	    NameSolutionsFile=>o.BranchPoints),1e-12);
    --put a radical list warning here:
    gggFile:= openOut(storeFiles|o.NameGaloisGroupGeneratorFile); 
    gggFile << "[" << endl;    	    
    solCollection= importSolutionsFile(storeFiles,NameSolutionsFile=>"ggStartJade",M2Precision=>o.M2Precision);
--    writeParameterFile(storeFiles,{basePointT},NameParameterFile=>"ggStartParametersJade");
--    print branchPointsT;
    loopFailures:=0;      
    if #branchPointsT===1 and o.LoopRadius==={} 
    then theLoopRadius:=.1;
--
    if o.LoopRadius=!={} 
    then theLoopRadius=o.LoopRadius;
--
    if #branchPointsT=!=1 and o.LoopRadius==={} 
    then theLoopRadius=1/2*min flatten(for i to #branchPointsT-2 list for j from i+1 to #branchPointsT-1 list abs(branchPointsT_i-branchPointsT_j));
--
    loopPointsT:={};
    if o.Verbose>1 then print 4;
    for oneBranchPointT in branchPointsT  do (
    	--NWSWE
      if imaginaryPart(oneBranchPointT-basePointT)<0 
      then(if o.Verbose>1 then print "below p";
	         loopPointsT={
	  oneBranchPointT+ii*theLoopRadius,
	  oneBranchPointT-theLoopRadius,
	  oneBranchPointT-ii*theLoopRadius,
	  oneBranchPointT+theLoopRadius,
	  basePointT}) 
  ---SENW 
      else if imaginaryPart(oneBranchPointT-basePointT)>0 
      then(if o.Verbose>1 then print "above p";
	         loopPointsT={
	  oneBranchPointT-ii*theLoopRadius,
	  oneBranchPointT+theLoopRadius,
	  oneBranchPointT+ii*theLoopRadius,
	  oneBranchPointT-theLoopRadius,
	  basePointT}) 
      else print "An error occurred while creating the loops."      ;       
      if o.Verbose>1 then print loopPointsT;
      if o.Verbose>1 then   print for i in loopPointsT list {i};
      if o.Verbose>1 then   print 6;
      b'PHSequence(storeFiles,for i in loopPointsT list {i},
	  Verbose=>o.Verbose,
	  B'Exe=>o.B'Exe,
	  NameB'InputFile=>o.NameB'InputFile,
	  SaveData=>true,
	  NameSolutionsFile=>"simple_raw_solutions"--"simple_raw_solutions"--this needs to be raw to keep the correct ordering
	    );
	permutedSols:=importSolutionsFile(storeFiles,NameSolutionsFile=>"start");
    	copyFile(storeFiles|"ggStartJade",storeFiles|"start");
        trackingSucess:=(#solCollection==#permutedSols);
        if not trackingSucess then (
	  loopFailures=loopFailures+1;
	  if o.Verbose>.5 then print ("Warning: There was a path tracking failure durin loop "|toString(loopCount))
	  );
        if trackingSucess then(    	
          if o.SaveData==true then writeStartFile(storeFiles,permutedSols,NameStartFile=>("newSols"|toString(loopCount)));
    	  ggGenerator:={};
--    	  for i to #solCollection-1 do for j to #permutedSols-1 do if areEqual({solCollection_i_0,solCollection_i_-1},{permutedSols_j_0,permutedSols_j_-1},Tolerance=>1e-10) then ggGenerator=append(ggGenerator,j+1);
    	  for i to #solCollection-1 do for j to #permutedSols-1 do
	    if areEqual( for k to #solCollection_0-1 list solCollection_i_k,
		for l to #solCollection_0-1 list permutedSols_j_l,Tolerance=>1e-10) then ggGenerator=append(ggGenerator,j+1);
    	  if #solCollection=!=#ggGenerator then (
    	    if o.Verbose>.5 then print (#ggGenerator, #solCollection);
	    loopFailures=loopFailures+1;
	    if o.Verbose>.5 then print ("Warning: Monodrompy loop "|loopCount|" found unexpected new solutions or a numerical error involving tolerances occurred.")
	    );
    	  if o.Verbose>.5 then print ("gg",new Array from ggGenerator);--check that this works.
	  gggFile << toString (new Array from ggGenerator);
	  if loopCount=!=#branchPointsT-1 then gggFile << "," << endl;
	  loopCount=loopCount+1	)      );
    print ("There were "|toString loopFailures|" loop failures.");
    gggFile << "]" << endl;
    close gggFile;
    print toString (storeFiles|o.NameGaloisGroupGeneratorFile);
    if o.ReturnGaloisGroupGeneratorFile===true then return value get (storeFiles|o.NameGaloisGroupGeneratorFile) );


	    

convertToGap=(aList)->(new Array from for i in aList list  new Array from toList i+for j in i list 1);




-------MULTIPROJECTIVE POINTS AND SLICES 
--B'MultiProjectivePoint=new Type of MutableHashTable;
B'Section=new Type of MutableHashTable;
B'Slice= new Type of MutableHashTable;
--B'WitnessSet= new Type of MutableHashTable;




par'String=(aString)->("("|toString(aString)|")");
makeB'Section = method(TypicalValue=>Nothing,Options=>{
	ContainsPoint=>{},
	B'NumberCoefficients=>{},
    	B'Homogenization=>1,
	RandomCoefficientGenerator=>(()->(2*random(CC)-random(CC))),
	NameB'Section=>null
	 })
makeB'Section(List) := o -> (oneVariableGroup)-> (
    theSection:=new B'Section;
    theSectionString:="";
    theNumberCoefficients:={};
    createsNumbers:=o.RandomCoefficientGenerator;
    theSpecifiedCoefficients:={};
    if o.B'NumberCoefficients=!={} then (
      theSpecifiedCoefficients=o.B'NumberCoefficients;
      theNumberCoefficients=o.B'NumberCoefficients);
    for aVar to #oneVariableGroup-1 do (
      if theSpecifiedCoefficients==={} then (
	theCoefficient:=createsNumbers();
	theNumberCoefficients=append(theNumberCoefficients,theCoefficient))
      else  theCoefficient=theSpecifiedCoefficients_aVar;      
      theSectionString=theSectionString|par'String theCoefficient;
      if oneVariableGroup_aVar=!=null then  (
	theSectionString=theSectionString|"*";
    	if parent class o.ContainsPoint===MutableHashTable
	then theContainsPoint:=(o.ContainsPoint#Coordinates)
	else  theContainsPoint=o.ContainsPoint;
	if o.ContainsPoint==={}
	then theSectionString=theSectionString|par'String oneVariableGroup_aVar
	else theSectionString=theSectionString|"("|toString(oneVariableGroup_aVar)|"-"|par'String(o.B'Homogenization)|"*"|par'String ( (theContainsPoint)_aVar)|")"
	);
      if #oneVariableGroup-1=!=aVar then theSectionString=theSectionString|"+");
    theSection.B'SectionString=theSectionString;
    theSection.B'NumberCoefficients=theNumberCoefficients;
    if o.ContainsPoint=!={} then theSection.B'Homogenization=o.B'Homogenization;
    if o.NameB'Section=!=null then theSection.NameB'Section=toString(o.NameB'Section);
    return theSection
  )



makeB'Slice = method(TypicalValue=>Nothing,Options=>{
	ContainsMultiProjectivePoint=>{},
    	ContainsPoint=>{},
	B'NumberCoefficients=>{},
	B'Homogenization=>{},
	RandomCoefficientGenerator=>(()->(2*random(CC)-random(CC))),
	NameB'Slice=>null,
	 })
makeB'Slice(Thing,List) := o ->(sliceType,multipleVariableGroups)->(
--      
    if class sliceType===ZZ then (
      numberOfSections:=sliceType;
      numSliceTypes:=1;
      AssumeOneGroup:=true);
    if class sliceType===List then (
      numberOfSections=sum sliceType;
      numSliceTypes=#sliceType;
      AssumeOneGroup=false);
    if multipleVariableGroups==={} then error "An empty list is not a valid input.";
---------- 
    if AssumeOneGroup===true then (
      if class multipleVariableGroups_0===List then error"If sliceType is an integer the second input cannot be a list of lists.";
      multipleVariableGroups={multipleVariableGroups};
      if o.B'Homogenization=!={} and class o.B'Homogenization===List then error"If sliceType is an integer then B'Homogenization must be {} or not a list.";
      if o.B'Homogenization==={} then theHomogenization:={1};
      if o.B'NumberCoefficients=!={} then(
	if class ((o.B'NumberCoefficients)_0_0)===List then error"When sliceType is an integer B'NumberCoefficients cannot be a list of lists. ";
	if class ((o.B'NumberCoefficients)_0_0)=!=List then  theCoefs:=o.B'NumberCoefficients));
---------- 
    if AssumeOneGroup===false then (
      if class o.B'Homogenization=!=List then error"When sliceType is a list, B'Homogenization should be a list.";
      if o.B'Homogenization==={} then theHomogenization=for i in multipleVariableGroups list 1;
      if o.B'Homogenization=!={} then theHomogenization=o.B'Homogenization;
      if o.B'NumberCoefficients=!={} then(
	if class ((o.B'NumberCoefficients)_0)=!=List then error"When sliceType is a list B'NumberCoefficients should be a list of lists. ";
	if class ((o.B'NumberCoefficients)_0)===List then  theCoefs=o.B'NumberCoefficients));          
---------- 
    if o.B'NumberCoefficients==={} then theCoefs=for i to numberOfSections-1 list {};
    print numberOfSections;
    print theCoefs;
    if #theCoefs=!=numberOfSections then error "The number of sets of coefficients of B'NumberCoefficients does not match the number of sections to be made. ";
    if #theHomogenization=!=#multipleVariableGroups then error "B'Homogenization does not match the number of variable groups. ";
    if class o.NameB'Slice===List and #o.NameB'Slice=!=numberOfSections then error"When NameB'Slice is a list, the number of elements should equal the number of sections being made. ";
--
    createsNumbers:=o.RandomCoefficientGenerator;
    if class sliceType===ZZ then sliceType={sliceType};
--
    theSlice:= new B'Slice;
    listSections:={};
--
    if o.ContainsMultiProjectivePoint=!={} and parent class o.ContainsMultiProjectivePoint ===MutableHashTable then  theMultiProjectivePoint:=o.ContainsMultiProjectivePoint#Coordinates;
    if o.ContainsMultiProjectivePoint=!={} and parent class o.ContainsMultiProjectivePoint ===VisibleList then  theMultiProjectivePoint=o.ContainsMultiProjectivePoint;
    if o.ContainsPoint=!={} and parent class o.ContainsPoint===MutableHashTable then  theMultiProjectivePoint={o.ContainsPoint#Coordinates};
    if o.ContainsPoint=!={} and parent class o.ContainsPoint===VisibleList then  theMultiProjectivePoint={o.ContainsPoint};
    if o.ContainsPoint==={} and o.ContainsMultiProjectivePoint==={} then theMultiProjectivePoint=for i to numberOfSections list {};
--    print theMultiProjectivePoint;
    sliceCount:=0;
    for useGroup to numSliceTypes-1 do(
      for oneSlice to (sliceType_useGroup)-1 do(
	oneVariableGroup:=multipleVariableGroups_(useGroup);
      	oneSetCoefs:=theCoefs_sliceCount;
      	theNameB'Section:=if o.NameB'Slice===null 
	then null else(
          if class o.NameB'Slice===List 
	  then ((o.NameB'Slice)_sliceCount) 
	  else (toString (o.NameB'Slice)|toString sliceCount));
      listSections=append(listSections,makeB'Section(oneVariableGroup,
	  B'Homogenization=>theHomogenization_useGroup,
	  ContainsPoint=>theMultiProjectivePoint_(useGroup),
	  B'NumberCoefficients=>oneSetCoefs,
	  RandomCoefficientGenerator=>createsNumbers,
	  NameB'Section=>theNameB'Section	  
	  ));
      sliceCount=sliceCount+1) );
    theSlice.ListB'Sections=listSections;
    theSlice.B'SectionString=for i in theSlice#ListB'Sections list i#B'SectionString;    
    theSlice.B'NumberCoefficients=for i in theSlice#ListB'Sections list i#B'NumberCoefficients;    
    if o.B'Homogenization=!={} then theSlice.B'Homogenization=for i in theSlice#ListB'Sections list i#B'Homogenization;    
    theSlice.NameB'Slice=if o.NameB'Slice=!=null then for i in theSlice#ListB'Sections list i#NameB'Section;    
    return theSlice)
  

sortMainDataComponents = method(TypicalValue=>List,Options=>{
	 })
sortMainDataComponents(List) := o ->(importedMD)->(
    organizedData:={};
    while #importedMD>0 do(
      firstPoint:=importedMD_0;
      oneComponent:={};
      for onePoint in importedMD do(
	if (firstPoint#Dimension)==(onePoint#Dimension) and 
	(firstPoint#ComponentNumber==onePoint#ComponentNumber) then (
	  oneComponent=append(oneComponent,onePoint);
          importedMD=delete(onePoint,importedMD)));
      organizedData=append(organizedData,oneComponent));
  return organizedData)   



subPoint = method(TypicalValue=>List,Options=>{
	SpecifyVariables=>false,
	SubIntoCC=>false,
	M2Precision=>53
	 })
subPoint(Thing,List,Thing) := o ->(polyOrMatrix,listVars,aPoint)->(
    if o.SubIntoCC===true and o.SpecifyVariables=!=false then (
      if #o.SpecifyVariables=!=listVars then print"Warning: SubIntoCC may set unassigned variables to be zero." );
    if class aPoint===Point then coords:=aPoint#Coordinates else
    if class aPoint===Matrix then coords=flatten entries aPoint else
    if class aPoint===List then coords=aPoint else print "class of "|toString aPoint|" is not recognized.";     
    if false=== o.SpecifyVariables then selectedVars:=listVars else selectedVars=o.SpecifyVariables;
    afterSub:=sub(polyOrMatrix,flatten for i to #listVars-1 list 
      if member(listVars_i,selectedVars) then listVars_i=>coords_i else {}
    );
    if o.SubIntoCC===true then 
      return sub(afterSub,CC_(o.M2Precision)) else if
      o.SubIntoCC===false then return afterSub else error"SubIntoCC should be set to true or false.")
 

moveB'File = method(TypicalValue=>List,Options=>{
    	SubFolder=>null,
	MoveToDirectory=>null,
  	CopyB'File=>false
	 })
moveB'File(String,String,String) := o ->(storeFiles,originalName,newName)->(
    if o.SubFolder=!=null and o.MoveToDirectory=!=null then error"SubFolder and MoveToDirectory cannot both be set.";
--
    storeFiles=addSlash(storeFiles);
--
    if o.SubFolder=!=null then finalDirectory:=storeFiles|o.SubFolder;        
    if o.MoveToDirectory=!=null then finalDirectory=o.MoveToDirectory;    
    if o.MoveToDirectory===null and o.SubFolder===null then finalDirectory=storeFiles; 
-- 
    if finalDirectory_-1===" " then error ("MoveToDirectory nor SubFolder cannot end with whitespace.");
    if finalDirectory_-1=!="/" then finalDirectory=finalDirectory|"/";    
--
  if (storeFiles|originalName)=!=(finalDirectory|newName) 
  then(
    if o.CopyB'File===false then moveFile(storeFiles|originalName,finalDirectory|newName);
    if o.CopyB'File===true then copyFile(storeFiles|originalName,finalDirectory|newName))    
)



    


--apply(lines     get (theDir|"/finite_solutions"),i->select("[0-9e.+-]+",i))

--importB'Coordinates gets the solutions from a bertini file such as nonsingular_solutions to store them as a list of coordinates in M2
--the input is a string, giving the location of the file, and an integer giving the number of coordinates 
--the output is a list of points
///
importPoints = method(TypicalValue=>Nothing,Options=>{
	SpecifyPoints=>{},
	SpecifyCoordinates=>{} })
importPoints(String,ZZ) := o -> (importFrom,numberOfCoordinates)-> (
    importedFileLines := lines     get (importFrom); -- grabs all lines of the solution file
    numberOfsolutionsInFile:=value(importedFileLines_0);--the first line of the solution file gives the number of solutions in the file
    importedFileLines=drop(importedFileLines,1);--drop the first line
    storeSolutions:={};---We will store the solutions we specified and return this in the end
    for i to numberOfsolutionsInFile-1 do (
	linesForOnePoint:=for indexLines to numberOfCoordinates+1-1 list importedFileLines_indexLines;--we read a blank line and the coordinates of one solution
	importedFileLines=drop(importedFileLines,numberOfCoordinates+1);--we drop the lines we just read
	if  member(i,o.SpecifyPoints) or #o.SpecifyPoints==0 -- We proceed to turn the text file into a point to be stored in M2 if it is a specified solution
	then(
      	  cAS:=collectAPointIP(linesForOnePoint,numberOfCoordinates,o.SpecifyCoordinates);
     	  storeSolutions= append(storeSolutions,cAS)));
     return storeSolutions);

--collectAPointIP is a subfunction for importPoints
collectAPointIP=(linesToRead,numberOfCoordinates,specifyCoordinates)->(
     collectedCoordinates:={};
     linesToRead=drop(linesToRead,1);--drops an empty line
     for j to numberOfCoordinates-1 do (
	  if member(j,specifyCoordinates) or #specifyCoordinates==0 then (
	       oneCoord:=select("[0-9.+-]+",first(linesToRead));
	       collectedCoordinates=append(collectedCoordinates,value((oneCoord_0)|"p300")*10^(value(oneCoord_1))+ii*
			      value((oneCoord_2)|"p300")*10^(value(oneCoord_3)));
     	       linesToRead=drop(linesToRead,1)) else (
	  linesToRead=drop(linesToRead,1)));--drops coordinates you don't care about
     return  point {collectedCoordinates});
///







--##########################################################################--
-- TESTS
--##########################################################################--

TEST///
load concatenate(Bertini#"source directory","./Bertini/TST/bertiniIsProjective.tst.m2")
///

TEST///
load concatenate(Bertini#"source directory","./Bertini/TST/bertiniParameterHomotopy.tst.m2")
/// 

TEST///
load concatenate(Bertini#"source directory","./Bertini/TST/bertiniPosDimSolve.tst.m2")
/// 

TEST///
load concatenate(Bertini#"source directory","./Bertini/TST/bertiniRefineSols.tst.m2")
/// 

TEST///
load concatenate(Bertini#"source directory","./Bertini/TST/bertiniSample-bertiniComponentMemberTest.tst.m2")
/// 

TEST///
load concatenate(Bertini#"source directory","./Bertini/TST/bertiniTrackHomotopy.tst.m2")
/// 

TEST///
load concatenate(Bertini#"source directory","./Bertini/TST/bertiniZeroDimSolve.tst.m2")
/// 

TEST///
load concatenate(Bertini#"source directory","./Bertini/TST/makeBInputFile.tst.m2")
///

TEST///
load concatenate(Bertini#"source directory","./Bertini/TST/runBertini.tst.m2")
///

TEST///
load concatenate(Bertini#"source directory","./Bertini/TST/importSolutionsFile.tst.m2")
///

TEST///
load concatenate(Bertini#"source directory","./Bertini/TST/importMainDataFile.tst.m2")
///

TEST///
load concatenate(Bertini#"source directory","./Bertini/TST/bPHSequence.tst.m2")
///

TEST///
load concatenate(Bertini#"source directory","./Bertini/TST/bPHMonodromyCollect.tst.m2")
///

TEST///
load concatenate(Bertini#"source directory","./Bertini/TST/makeBSection.tst.m2")
///

TEST///
load concatenate(Bertini#"source directory","./Bertini/TST/makeBSlice.tst.m2")
///

TEST///
load concatenate(Bertini#"source directory","./Bertini/TST/makeWitnessSetFiles.tst.m2")
///

TEST///
load concatenate(Bertini#"source directory","./Bertini/TST/makeSampleSolutions.tst.m2")
///

TEST///
load concatenate(Bertini#"source directory","./Bertini/TST/makeMembershipFile.tst.m2")
///

TEST///
load concatenate(Bertini#"source directory","./Bertini/TST/bTraceTestImage.tst.m2")
///

TEST///
load concatenate(Bertini#"source directory","./Bertini/TST/subPoint.tst.m2")
///

TEST///
load concatenate(Bertini#"source directory","./Bertini/TST/moveBFile.tst.m2")
///

TEST///
load concatenate(Bertini#"source directory","./Bertini/TST/bPHGaloisGroup.tst.m2")
///

TEST///
load concatenate(Bertini#"source directory","./Bertini/TST/radicalList.tst.m2")
///


---newtst

--##########################################################################--
-- DOCUMENTATION
--##########################################################################--

beginDocumentation()

load "./Bertini/doc.m2";
end


--##########################################################################--
--DEPRACATED FUNCTIONS
--##########################################################################--

b'TraceTest=method(TypicalValue=>Thing,Options=>{ --assuming the directory contains a start file and start parameters and parameter homotopy file with one parameter
	NameB'InputFile=>"input",
	NameStartFile=>"start",
	NameParameterFile=>"start_parameters",
    	InputFileDirectory=>{},
--	NameSolutionsFile=>"nonsingular_solutions",		
	B'Exe=>BERTINIexe,
	ParameterValues=>{0,.5,1},
	UseStartPointsFirst=>false	})
b'TraceTest(String,Number,Number) := o ->(storeFiles,NumberOfPoints,NumberOfCoordinates)->(
    if storeFiles_-1===" " then error (storeFiles|" cannot end with whitespace.");
    if storeFiles_-1=!="/" then storeFiles=storeFiles|"/";    
--
    if o.InputFileDirectory==={} then IFD:=storeFiles else IFD=o.InputFileDirectory;
    if IFD_-1===" " then error (IFD|" cannot end with whitespace.");
    if IFD_-1=!="/" then IFD=IFD|"/";    
--
    if fileExists(IFD|o.NameB'InputFile)===false then error "input file does not exist in correct directory.";
    if fileExists(storeFiles|o.NameStartFile)===false then error "start file does not exist in correct directory.";
    if fileExists(storeFiles|o.NameParameterFile)===false then error "start_parameters file does not exist in correct directory.";        
--
    print "tt1";
    makeB'TraceInput(storeFiles,NumberOfPoints,NumberOfCoordinates,NameB'InputFile=>"inputTTjade");
    print "tt2";
    runCount:=1;
    if o.UseStartPointsFirst===true then (
      print "tt3";
      moveFile(storeFiles|"start",storeFiles|"startPHjade");
      calculateB'Trace(storeFiles,NameStartFile=>"startPHjade",
	NameFunctionFile=>"trace"|toString(runCount),
	NameB'InputFile=>"inputTTjade");
      moveFile(storeFiles|"startPHjade",storeFiles|"start");      
      runCount=runCount+1);
    print "tt4"; 
    for aParameter in o.ParameterValues do(
      writeParameterFile(storeFiles,{aParameter});
      print "tt5Loop";
      runBertini(IFD,NameB'InputFile=>o.NameB'InputFile,Verbose=>o.Verbose);
      print readFile(storeFiles,"bertini_session.log",10000);
      print "tt6Loop";
      moveFile(storeFiles|"start",storeFiles|"startPHjade");
      print "tt7Loop";
      calculateB'Trace(storeFiles,NameStartFile=>"nonsingular_solutions",--need a check to make sure we don't lose solutions
	NameFunctionFile=>"trace"|toString(runCount),
	NameB'InputFile=>"inputTTjade");
      print "tt8Loop";
      runCount=runCount+1;      
      moveFile(storeFiles|"startPHjade",storeFiles|"start")      
	);
    print "tt9";
    return for i from 1 to runCount-1 list ((importSolutionsFile(storeFiles,NameSolutionsFile=>"trace"|toString i))_0)    
     );
 --##########################################################################--
 
 


























--##############

--To add next version:

---------------------------
-- bertiniSegmentHomotopy--
---------------------------

bertiniSegmentHomotopy = method(TypicalValue => List, Options=>{ --StartSystem=>{},-- StartSolutions=>{}
	  gamma=>1.0,CheckConditionNum=>1,MPType=>-1,PRECISION=>-1,IsProjective=>-1,ODEPredictor=>-1,TrackTolBeforeEG=>-1,TrackTolDuringEG=>-1,FinalTol=>-1,MaxNorm=>-1,MinStepSizeBeforeEG=>-1,MinStepSizeDuringEG=>-1,ImagThreshold=>-1,CoeffBound=>-1,DegreeBound=>-1,CondNumThreshold=>-1,RandomSeed=>-1,SingValZeroTol=>-1,EndGameNum=>-1,UseRegeneration=>-1,SecurityLevel=>-1,ScreenOut=>-1,OutputLevel=>-1,StepsForIncrease=>-1,MaxNewtonIts=>-1,MaxStepSize=>-1,MaxNumberSteps=>-1,MaxCycleNum=>-1,RegenStartLevel=>-1})
bertiniSegmentHomotopy (List, List,List) := o -> (S,F,Sols) -> (
--F is the list of polynomials
--S is the start system
--Sols are the start solutions
         L := {runType=>1};
         o2 := new OptionTable from L;
         o3 := o ++ o2;
	 o4 := o3 ++ {StartSystem=>S};
	 o5 := o4 ++ {StartSolutions=>Sols};
         bertiniSolve(F,o5)
         )
     

----------------------------------
--NEW FUNCTIONS FOR FEBRUARY 2014
---------------------------------



bertiniRealNumber=(aNumber)->(
    if class aNumber===QQ then print "WARNING: final parameters cannot have type QQ." else
    realPartSeparate:=separate("p",toExternalString ( aNumber));
    realPartMantissa:=realPartSeparate_0;
    if 1=!=#realPartSeparate 
    then (separateExponent:=separate("e",realPartSeparate_1);
    	if 1==#separateExponent
    	then realPartExponent:="0"
    	else realPartExponent=(separateExponent)_1;
    	return(realPartMantissa|"e"|realPartExponent))
    else return(realPartMantissa|"e0"));
    
bertiniComplexNumber=(aCNumber)->{bertiniRealNumber(realPart aCNumber),
    bertiniRealNumber(imaginaryPart aCNumber)};

--IMPORT POINTS
--importPoints gets the solutions from a bertini solutions file to store them as points in M2
--the input is a string, giving the location of the file, and an integer giving the number of coordinates 
--the output is a list of points
importPoints = method(TypicalValue=>Nothing,Options=>{
	SpecifyPoints=>{},
	SpecifyCoordinates=>{} })
importPoints(String,ZZ) := o -> (importFrom,numberOfCoordinates)-> (
    importedFileLines := lines     get (importFrom); -- grabs all lines of the solution file
    numberOfsolutionsInFile:=value(importedFileLines_0);--the first line of the solution file gives the number of solutions in the file
    importedFileLines=drop(importedFileLines,1);--drop the first line
    storeSolutions:={};---We will store the solutions we specified and return this in the end
    for i to numberOfsolutionsInFile-1 do (
	linesForOnePoint:=for indexLines to numberOfCoordinates+1-1 list importedFileLines_indexLines;--we read a blank line and the coordinates of one solution
	importedFileLines=drop(importedFileLines,numberOfCoordinates+1);--we drop the lines we just read
	if  member(i,o.SpecifyPoints) or #o.SpecifyPoints==0 -- We proceed to turn the text file into a point to be stored in M2 if it is a specified solution
	then(
      	  cAS:=collectAPointIP(linesForOnePoint,numberOfCoordinates,o.SpecifyCoordinates);
     	  storeSolutions= append(storeSolutions,cAS)));
     return storeSolutions);

--collectAPointIP is a subfunction for importPoints
collectAPointIP=(linesToRead,numberOfCoordinates,specifyCoordinates)->(
     collectedCoordinates:={};
     linesToRead=drop(linesToRead,1);--drops an empty line
     for j to numberOfCoordinates-1 do (
	  if member(j,specifyCoordinates) or #specifyCoordinates==0 then (
	       oneCoord:=select("[0-9.+-]+",first(linesToRead));
	       collectedCoordinates=append(collectedCoordinates,value((oneCoord_0)|"p300")*10^(value(oneCoord_1))+ii*
			      value((oneCoord_2)|"p300")*10^(value(oneCoord_3)));
     	       linesToRead=drop(linesToRead,1)) else (
	  linesToRead=drop(linesToRead,1)));--drops coordinates you don't care about
     return  point {collectedCoordinates});


--   INPUT of phMonodromy 
--String should be a directory (no "/" at then end) that contains start files for bertini parameter homotopy
----start files needed: input, start_parameters, start	
--ZZ equals number of parameters 
--ZZ equals the number of coordinates of the points.
--List is a a list of lists of numbers not of type QQ. 
----Each entry of List are parameters for a parameter homotopy. 



sortSolutions2=(solutionSet,tolerance)->(
    S:={};
    for i in solutionSet do S=insertInList(S,i,tolerance);
    return S)

isSameSolution=(aPoint,bPoint,tolerance)->(
    if (class tolerance)=!=List  then tolerance=for i to #(coordinates aPoint) list  tolerance;
    aPoint=coordinates aPoint;
    bPoint=coordinates bPoint;
    for i to #aPoint-1 list if abs (realPart aPoint_i-realPart bPoint_i)>tolerance_i or
    abs (imaginaryPart aPoint_i-imaginaryPart bPoint_i)>tolerance_i then 
    if   abs (realPart aPoint_i-realPart bPoint_i)>tolerance_i then 
    	if (realPart aPoint_i-realPart bPoint_i)>0 then return 1 else return -1
    else if (imaginaryPart aPoint_i-imaginaryPart bPoint_i)>0 then return 1 else return -1;    
    return true)
   
insertInList=(setS,aPoint,theTolerances)->(
    if #setS==0 then (
	print 0;
	 return {aPoint}) else
    lowerBound:=0;
    upperBound:=#setS-1;
    if isSameSolution(setS_lowerBound,aPoint,theTolerances)===true then (
	print "A";
	return setS);
    if isSameSolution(setS_upperBound,aPoint,theTolerances)===true then (
	print "B";
	return setS); 
    if isSameSolution(setS_lowerBound,aPoint,theTolerances)==-1 then (
	print 1;
	return prepend(aPoint,setS));
    if isSameSolution(setS_upperBound,aPoint,theTolerances)==1 then (
	print 2;
	return append(setS,aPoint));    
    while lowerBound<=upperBound do (
	if isSameSolution(setS_lowerBound,aPoint,theTolerances)===true then (
	    print "A1";
	    return setS);
        if isSameSolution(setS_upperBound,aPoint,theTolerances)===true then (
	    print "A2";
	    return setS);
	if isSameSolution(setS_lowerBound,aPoint,theTolerances)==-1 then (
	    print "B1";
	    return insert(lowerBound,aPoint,setS));
    	if isSameSolution(setS_upperBound,aPoint,theTolerances)==1 then (
	    print "B2";
	    return insert(upperBound+1,aPoint,setS));    
        midpoint:=floor((upperBound+lowerBound)/2);
	if isSameSolution(setS_midpoint,aPoint,theTolerances)===true then (
	    print "C";
	    return setS) else
    	if isSameSolution(setS_midpoint,aPoint,theTolerances)==1 then (
--	    print (midpoint,midpoint+1);
	    lowerBound=midpoint+1);
    	if isSameSolution(setS_midpoint,aPoint,theTolerances)==-1 then (
--	    print (midpoint,midpoint-1);
	    upperBound=midpoint-1)  ;
    	print isSameSolution(setS_midpoint,aPoint,theTolerances);
    	print (lowerBound,upperBound,midpoint);
	print "whileLoop");
    print "fail";
    return insert(lowerBound,aPoint,setS))
	  
  

--PARAMETERHOMOTOPYPOSTPROCESS
--This function takes a directory as its input where a bertini run has alreaddy been made.
--The purpose is so that Alice can email a folder to Bob, and Bob can easily manipulate the data with the Bertini.m2 interface
phPostProcess = method(TypicalValue=>Nothing,Options=>{
	PrintNotes=>-1,--if printNotes is not -1 then  "notes" from Alice is printed for Bob instead of Bertini being called
--    	InputFilesName=>"input",
	OutputLocation=>-1,
      	SolutionType=>"nonsingular_solutions",
	B'InputFile=>"input",
    	B'StartFile=>"start",
    	B'StartParameters=>"start_parameters",
	SpecifyCoordinates=>{},
	SpecifyPoints=>{}
	})
phPostProcess(String,List,ZZ) := o -> (
    inputLocation,postParameters,numberOfCoordinates)-> (
    if o.PrintNotes==1 then print  get (inputLocation|"/notes")    
    else(
	if o.OutputLocation=!=-1 
	then (OL:=o.OutputLocation; 
	    copyFile(inputLocation|"/"|o.B'StartFile, OL|"/start");
	    copyFile(inputLocation|"/"|o.B'StartParameters,
		OL|"/start_parameters"))
        else OL=inputLocation;
	writeParameters(OL,postParameters);   
    	callBertini(OL,BERTINIexe,inputLocation,o.B'InputFile);---call Bertini 
    importPoints(OL|"/"|o.SolutionType,numberOfCoordinates,
	SpecifyPoints=>o.SpecifyPoints,
	SpecifyCoordinates=>o.SpecifyCoordinates)    ));
    
---writeParameters is a subfunction for parameterHomotopyPostProcess    
writeParameters=(filesGoHere,listParameters)->(
     finalParameterFile:= openOut(filesGoHere|"/final_parameters"); -- the only name for Bertini's final parameters file 
     finalParameterFile << toString(length listParameters) << endl << endl;
     for c in listParameters do (
	 cString:=bertiniComplexNumber(c);
	 finalParameterFile <<cString_0 << " " <<cString_1 <<endl
	 );
     finalParameterFile << endl;      
     close finalParameterFile);      


--writeParameters=(filesGoHere,listParameters)->(
  --writing parameter values to file 
  --   finalParameterFile:= openOut(filesGoHere|"/final_parameters"); -- the only name for Bertini's final parameters file 
    -- finalParameterFile << #(listParameters) << endl << endl;
    -- scan(listParameters, c-> finalParameterFile << (separate("p",toExternalString (realPart c)))_0 << " " << (separate("p",toExternalString (imaginaryPart c)))_0 << " " << endl );
     --  finalParameterFile << endl;      
      -- close finalParameterFile);

---helper functions for writeParameters
bertiniRealNumber=(aNumber)->(
    if class aNumber===QQ then error "final parameters cannot have type QQ" else
    realPartSeparate:=separate("p",toExternalString ( aNumber));
    realPartMantissa:=realPartSeparate_0;
    if 1=!=#realPartSeparate 
    then (separateExponent:=separate("e",realPartSeparate_1);
    	if 1==#separateExponent
    	then realPartExponent:="0"
    	else realPartExponent=(separateExponent)_1;
    	return(realPartMantissa|"e"|realPartExponent))
    else return(realPartMantissa|"e0"));
    
bertiniComplexNumber=(aCNumber)->{bertiniRealNumber(realPart aCNumber),
    bertiniRealNumber(imaginaryPart aCNumber)};

callBertini=(inDirectory,BERTINIexe,inputLocation,inputFilesName)->(
    run("cd "|inDirectory|"; "|BERTINIexe|" "|inputLocation|"/"|inputFilesName|" >bertini_session.log"));  
--     run("cd "|filesGoTo|"; "|BERTINIexe|" "|fileLocation|"/"|inputFilesName|" >bertini_session.log");  

--exportPoints--This function should export the coordinates of the points
--saveFolder--This function should copy a temporary directory to a location specified by the user
--call bertini in a specified folder     
     
----------------------------------------------------
--NEW FUNCTIONS FOR FEBRUARY 2014 -- FROM JULY 2014 --
--MAY HAVE DIFFERENT THINGS THAN ABOVE
----------------------------------------------------


--IMPORT POINTS
--importPoints gets the solutions from a bertini solutions file to store them as points in M2
--the input is a string, giving the location of the file, and an integer giving the number of coordinates 
--the output is a list of points
importPoints = method(TypicalValue=>Nothing,Options=>{
	SpecifyPoints=>{},
	SpecifyCoordinates=>{} })
importPoints(String,ZZ) := o -> (importFrom,numberOfCoordinates)-> (
    importedFileLines := lines     get (importFrom); -- grabs all lines of the solution file
    numberOfsolutionsInFile:=value(importedFileLines_0);--the first line of the solution file gives the number of solutions in the file
    importedFileLines=drop(importedFileLines,1);--drop the first line
    storeSolutions:={};---We will store the solutions we specified and return this in the end
    for i to numberOfsolutionsInFile-1 do (
	linesForOnePoint:=for indexLines to numberOfCoordinates+1-1 list importedFileLines_indexLines;--we read a blank line and the coordinates of one solution
	importedFileLines=drop(importedFileLines,numberOfCoordinates+1);--we drop the lines we just read
	if  member(i,o.SpecifyPoints) or #o.SpecifyPoints==0 -- We proceed to turn the text file into a point to be stored in M2 if it is a specified solution
	then(
      	  cAS:=collectAPointIP(linesForOnePoint,numberOfCoordinates,o.SpecifyCoordinates);
     	  storeSolutions= append(storeSolutions,cAS)));
     return storeSolutions);

--collectAPointIP is a subfunction for importPoints
collectAPointIP=(linesToRead,numberOfCoordinates,specifyCoordinates)->(
     collectedCoordinates:={};
     linesToRead=drop(linesToRead,1);--drops an empty line
     for j to numberOfCoordinates-1 do (
	  if member(j,specifyCoordinates) or #specifyCoordinates==0 then (
	       oneCoord:=select("[0-9.+-]+",first(linesToRead));
	       collectedCoordinates=append(collectedCoordinates,value((oneCoord_0)|"p300")*10^(value(oneCoord_1))+ii*
			      value((oneCoord_2)|"p300")*10^(value(oneCoord_3)));
     	       linesToRead=drop(linesToRead,1)) else (
	  linesToRead=drop(linesToRead,1)));--drops coordinates you don't care about
     return  point {collectedCoordinates});


--   INPUT of phMonodromy 
--String should be a directory (no "/" at then end) that contains start files for bertini parameter homotopy
----start files needed: input, start_parameters, start	
--ZZ equals number of parameters 
--ZZ equals the number of coordinates of the points.
----Each entry of List are parameters for a parameter homotopy. 

monPre=30;
phMonodromy = method(TypicalValue=>Nothing,Options=>{
	PrintNotes=>-1,--if printNotes is not -1 then  "notes" from Alice is printed for Bob instead of Bertini being called
    	B'InputFile=>"input",
    	B'StartFile=>"nonsingular_solutions",
    	B'StartParameters=>"start_parameters",
	OutputLocation=>-1,
      	SolutionType=>"nonsingular_solutions",
	SpecifyCoordinates=>{},
	SpecifyPoints=>{},
	ParameterValues=>{}
		})
phMonodromy(String,ZZ,ZZ) := o -> (--parameterValues is a list of list of complex numbers OR an empty list
    inputLocation,numberOfParameters,numberOfCoordinates)-> (
    if o.PrintNotes==1 then print  get (inputLocation|"/notes")    
    else(
	--Set OL to be the location of files bertini will create
	--Default location is the same as the location of the input files
	if o.OutputLocation=!=-1 then OL:=o.OutputLocation else OL=inputLocation;  
	if fileExists(inputLocation|"/"|o.B'StartFile) then (
	    if o.B'StartFile=!="nonsingular_solutions" then copyFile(inputLocation|"/"|o.B'StartFile, OL|"/nonsingular_solutions"))
	else (print o.B'StartFile; error "B'StartFile does not exist."); 
	if fileExists(inputLocation|"/"|o.B'InputFile) then (	
	    if o.B'InputFile=!="input" then copyFile(inputLocation|"/"|o.B'InputFile, OL|"/input"))
	else error "B'InputFile does not exist."; 
	if fileExists(inputLocation|"/"|o.B'StartParameters) then (
	    if o.B'StartParameters=!="start_parameters" then  copyFile(inputLocation|"/"|o.B'StartParameters,OL|"/start_parameters"))
	else error "B'StartParameters does not exist.";
	copyFile(inputLocation|"/"|"start_parameters",OL|"/base_parametersADEJ"); --save the base parameters of the monodromy so we can come back later
    	if o.ParameterValues=!={} then PV:=o.ParameterValues else PV=for i to 2-1 list for j to numberOfParameters-1 list 2*random(RR_monPre)-1+ii*(2*random(RR_monPre)-1);
	for anH in PV do (   
	    writeParameters(OL,anH);
	    if fileExists(OL|"/nonsingular_solutions") then copyFile( OL|"/nonsingular_solutions",OL|"/start")
	    else (print "should break loop now";return {};print "fail");
       	    callBertini(OL,BERTINIexe,inputLocation,o.B'InputFile);---call Bertini
--    	    print"BERTINI called!";
	    copyFile(OL|"/final_parameters", OL|"/start_parameters")
	    );
	copyFile(inputLocation|"/base_parametersADEJ",OL|"/final_parameters");  ---Go back to the base parameters
	if fileExists(OL|"/nonsingular_solutions") then copyFile( OL|"/nonsingular_solutions",OL|"/start")
	else (print "should break loop now2";return {};print "fail");
        callBertini(OL,BERTINIexe,inputLocation,o.B'InputFile);---call Bertini
	copyFile(OL|"/base_parametersADEJ", OL|"/start_parameters");	
--    	print"BERTINI called!!";
    	if not fileExists(OL|"/"|o.SolutionType) then (print "Warning: no solutions found. FS";return{});
    	print "Successful Loop!";
	return importPoints(OL|"/"|o.SolutionType,numberOfCoordinates,
	    SpecifyCoordinates=>o.SpecifyCoordinates,
	    SpecifyPoints=>o.SpecifyPoints)));

phMonodromySolve = method(TypicalValue=>Nothing,Options=>{
	PrintNotes=>-1,--if printNotes is not -1 then  "notes" from Alice is printed for Bob instead of Bertini being called
    	B'InputFile=>"input",
    	B'StartFile=>"nonsingular_solutions",
    	B'StartParameters=>"start_parameters",
	OutputLocation=>-1,
      	SolutionType=>"nonsingular_solutions",
--	SpecifyCoordinates=>{},
--	SpecifyPoints=>{},
	ParameterValues=>{},
	MonodromyStart=>{},
	NumberOfLoops=>1,
	MonodromyTolerance=>1e-6,
	NumberOfWrites=>1,
	MonodromyUpperBound=>0	
		})
phMonodromySolve(String,ZZ,ZZ) := o -> (
    inputLocation,numberOfParameters,numberOfCoordinates)->(
    theTolerances:=o.MonodromyTolerance;
    setS:=o.MonodromyStart;
    if o.OutputLocation=!=-1 
    then OL:=o.OutputLocation --Set OL to be the location of files bertini will create
    else OL=inputLocation;  --Default location is the same as the location of the input files
    if not fileExists(inputLocation|"/"|o.B'StartFile) then  error "B'StartFile does not exist. solve."; 
    if not fileExists(inputLocation|"/"|o.B'InputFile) then  error "B'InputFile does not exist. solve."; 
    if not fileExists(inputLocation|"/"|o.B'StartParameters)  then error "B'StartParameters does not exist. solve.";
    if o.B'StartFile=!="nonsingular_solutions" then copyFile(inputLocation|"/"|o.B'StartFile, OL|"/nonsingular_solutions"); 
    escapeLoops:=false;
    for rounds to o.NumberOfWrites-1 do if not escapeLoops then (
    	copyFile(OL|"/nonsingular_solutions",OL|"/nonsingular_solutionsADEJ"); 
    	if o.B'InputFile=!="input" then copyFile(inputLocation|"/"|o.B'InputFile, OL|"/input"); 
    	if o.B'StartParameters=!="start_parameters" then  copyFile(inputLocation|"/"|o.B'StartParameters,OL|"/start_parameters");
    	if o.B'StartParameters=!="start_parameters" then  copyFile(inputLocation|"/"|o.B'StartParameters,OL|"/start_parametersADEJ");
    	setS=sortSolutions2 (setS,theTolerances);
	--    print "1";
	for i to o.NumberOfLoops-1 do if not escapeLoops then (
	    --	print "2";
            for bPoint in phMonodromy(OL,numberOfParameters,numberOfCoordinates) do (
	    	setS=insertInList(setS,bPoint,theTolerances));
	    if (o.MonodromyUpperBound=!=0 and #setS>=o.MonodromyUpperBound) then (	    
    	    	escapeLoops=true;
--		print setS;
	    	writePoints(OL,"nonsingular_solutions",setS);
	    	print ("MonodromyUpperBound on the number of solutions has been reached."));
--	    print "fail?";
	    if not fileExists(OL|"/nonsingular_solutions") then (
	    	print "Warning: A phMonodromy run did not find any solutions.";
	    	copyFile(OL|"/nonsingular_solutionsADEJ",OL|"/nonsingular_solutions");
	    	copyFile(OL|"/start_parametersADEJ",OL|"/start_parameters")));
    	print (toString(#setS)|" points found!");
    	writePoints(OL,"nonsingular_solutions",setS));
    return setS) 

sortSolutions2=(solutionSet,tolerance)->(
    print "sort.";
    S:={};
    for i in solutionSet do S=insertInList(S,i,tolerance);
    print "sort!";
    return S)

isSameSolution=(aPoint,bPoint,tolerance)->(
    if (class tolerance)=!=List  then tolerance=for i to #(coordinates aPoint) list  tolerance;
    aPoint=coordinates aPoint;
    bPoint=coordinates bPoint;
    for i to #aPoint-1 list if abs (realPart aPoint_i-realPart bPoint_i)>tolerance_i or
    abs (imaginaryPart aPoint_i-imaginaryPart bPoint_i)>tolerance_i then 
    if   abs (realPart aPoint_i-realPart bPoint_i)>tolerance_i then 
    	if (realPart aPoint_i-realPart bPoint_i)>0 then return 1 else return -1
    else if (imaginaryPart aPoint_i-imaginaryPart bPoint_i)>0 then return 1 else return -1;    
    return true)
   
insertInList=(setS,aPoint,theTolerances)->(
    if #setS==0 then (
--	print 0;
	 return {aPoint}) else
    lowerBound:=0;
    upperBound:=#setS-1;
    if isSameSolution(setS_lowerBound,aPoint,theTolerances)===true then (
--	print "A";
	return setS);
    if isSameSolution(setS_upperBound,aPoint,theTolerances)===true then (
--	print "B";
	return setS); 
    if isSameSolution(setS_lowerBound,aPoint,theTolerances)==-1 then (
--	print 1;
	return prepend(aPoint,setS));
    if isSameSolution(setS_upperBound,aPoint,theTolerances)==1 then (
--	print 2;
	return append(setS,aPoint));    
    while lowerBound<=upperBound do (
	if isSameSolution(setS_lowerBound,aPoint,theTolerances)===true then (
--	    print "A1";
	    return setS);
        if isSameSolution(setS_upperBound,aPoint,theTolerances)===true then (
--	    print "A2";
	    return setS);
	if isSameSolution(setS_lowerBound,aPoint,theTolerances)==-1 then (
--	    print "B1";
	    return insert(lowerBound,aPoint,setS));
    	if isSameSolution(setS_upperBound,aPoint,theTolerances)==1 then (
--	    print "B2";
	    return insert(upperBound+1,aPoint,setS));    
        midpoint:=floor((upperBound+lowerBound)/2);
	if isSameSolution(setS_midpoint,aPoint,theTolerances)===true then (
--	    print "C";
	    return setS) else
    	if isSameSolution(setS_midpoint,aPoint,theTolerances)==1 then (
--	    print (midpoint,midpoint+1);
	    lowerBound=midpoint+1);
    	if isSameSolution(setS_midpoint,aPoint,theTolerances)==-1 then (
--	    print (midpoint,midpoint-1);
	    upperBound=midpoint-1)   )  ;
--    	print isSameSolution(setS_midpoint,aPoint,theTolerances);
--    	print (lowerBound,upperBound,midpoint);
--	print "whileLoop");
    print "fail";
    return insert(lowerBound,aPoint,setS))
	  
  

    
      






--PARAMETERHOMOTOPYPOSTPROCESS
--This function takes a directory as its input where a bertini run has alreaddy been made.
--The purpose is so that Alice can email a folder to Bob, and Bob can easily manipulate the data with the Bertini.m2 interface
phPostProcess = method(TypicalValue=>Nothing,Options=>{
	PrintNotes=>-1,--if printNotes is not -1 then  "notes" from Alice is printed for Bob instead of Bertini being called
--    	InputFilesName=>"input",
	OutputLocation=>-1,
      	SolutionType=>"nonsingular_solutions",
	B'InputFile=>"input",
    	B'StartFile=>"start",
    	B'StartParameters=>"start_parameters",
	SpecifyCoordinates=>{},
	SpecifyPoints=>{}
	})
phPostProcess(String,List,ZZ) := o -> (
    inputLocation,postParameters,numberOfCoordinates)-> (
    if o.PrintNotes==1 then print  get (inputLocation|"/notes")    
    else(
	if o.OutputLocation=!=-1 
	then (OL:=o.OutputLocation; 
	    copyFile(inputLocation|"/"|o.B'StartFile, OL|"/start");
	    copyFile(inputLocation|"/"|o.B'StartParameters,
		OL|"/start_parameters"))
        else OL=inputLocation;
	writeParameters(OL,postParameters);   
    	callBertini(OL,BERTINIexe,inputLocation,o.B'InputFile);---call Bertini 
    importPoints(OL|"/"|o.SolutionType,numberOfCoordinates,
	SpecifyPoints=>o.SpecifyPoints,
	SpecifyCoordinates=>o.SpecifyCoordinates)    ));
    
---writeParameters is a subfunction for parameterHomotopyPostProcess    
writeParameters=(filesGoHere,listParameters)->(
     finalParameterFile:= openOut(filesGoHere|"/final_parameters"); -- the only name for Bertini's final parameters file 
     finalParameterFile << toString(length listParameters) << endl << endl;
     for c in listParameters do (
	 cString:=bertiniComplexNumber(c);
	 finalParameterFile <<cString_0 << " " <<cString_1 <<endl
	 );
     finalParameterFile << endl;      
     close finalParameterFile);      


---writeParameters is a subfunction for parameterHomotopyPostProcess    
writePoints=(filesGoHere,nameOfFile,listPoints)->(
     startPointsFile:= openOut(filesGoHere|"/"|nameOfFile); 
     startPointsFile << toString(length listPoints) << endl << endl;
     for aPoint in listPoints do (
	 for c in coordinates aPoint do (
	     cString:=bertiniComplexNumber(c);
	     startPointsFile <<cString_0 << " " <<cString_1 <<endl
	     );
	 startPointsFile << " "<<endl);     	 
     startPointsFile << endl;      
     close startPointsFile);      

---helper functions for writeParameters
bertiniRealNumber=(aNumber)->(
    if class aNumber===QQ then error "final parameters cannot have type QQ" else
    realPartSeparate:=separate("p",toExternalString ( aNumber));
    realPartMantissa:=realPartSeparate_0;
    if 1=!=#realPartSeparate 
    then (separateExponent:=separate("e",realPartSeparate_1);
    	if 1==#separateExponent
    	then realPartExponent:="0"
    	else realPartExponent=(separateExponent)_1;
    	return(realPartMantissa|"e"|realPartExponent))
    else return(realPartMantissa|"e0"));
    
bertiniComplexNumber=(aCNumber)->{bertiniRealNumber(realPart aCNumber),
    bertiniRealNumber(imaginaryPart aCNumber)};




       
callBertini=(inDirectory,BERTINIexe,inputLocation,inputFilesName)->(
    run("cd "|inDirectory|"; "|BERTINIexe|" "|inputLocation|"/"|inputFilesName|" >bertini_session.log"));  
--     run("cd "|filesGoTo|"; "|BERTINIexe|" "|fileLocation|"/"|inputFilesName|" >bertini_session.log");


---Comments: each functions input contains a directory. 
--This directory determines where bertini will put files. 
--One can then copy and move the files later. 
--One can call input files and start files and start parameter files from other directorys. 

--one can NameInputFile NameStartFile NameSolutionFile NameParametersFile


