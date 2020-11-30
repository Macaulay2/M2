bertiniPresent := run ("type bertini >/dev/null 2>&1") === 0

newPackage(
  "Bertini",
  Version => "2.1.2.3",
  Date => "July 2020",
  Authors => {
    {Name => "Elizabeth Gross",
     Email=> "elizabeth.gross@sjsu.edu",
     HomePage => "http://math.sjsu.edu/~egross"},
    {Name => "Jose Israel Rodriguez",
     Email => "Jose@math.wisc.edu",
     HomePage =>"https://www.math.wisc.edu/~jose/"},
    {Name => "Dan Bates",
     Email => "bates@math.colostate.edu",
     HomePage => "http://www.math.colostate.edu/~bates"},
    {Name => "Anton Leykin",
     Email => "leykin@math.gatech.edu",
     HomePage => "http://www.math.gatech.edu/~leykin"}
  },
  Headline => "interface to Bertini",
  Keywords => {"Numerical Algebraic Geometry", "Interfaces"},
  Configuration => { "BERTINIexecutable"=>"bertini" },
  AuxiliaryFiles => true,
  PackageExports => {"NAGtypes"},
  PackageImports => {"NAGtypes"},
  CacheExampleOutput => true,
  OptionalComponentsPresent => bertiniPresent
)

exportMutable{"storeBM2Files"
  }

export {
  "SetParameterGroup",
  "bertiniUserHomotopy",
  "ReturnPoints",
  "PrintMidStatus",
  "OutputStyle",--TODO remove this option
  "TopDirectory",
  "StorageFolder",
  "RandomGamma",
  "SubFolder",
  "StartParameters",
  "StartPoints",
  "subPoint",
  "OrderPaths",
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
  "BertiniInputConfiguration", --This option is a list of pairs of strings. These will be written in the CONFIG part of the Bertini input file.
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
  "importSolutionsFile",
  "importIncidenceMatrix",
  "SaveData",
  "SolutionFileStyle",
  "radicalList",
--  "B'MultiProjectivePoint",
  "makeB'Section",
  "makeB'Slice",
  "ContainsPoint",
  "B'NumberCoefficients",
  "B'Homogenization",
  "RandomCoefficientGenerator",
  "B'SectionString",
  "B'Section",
  "NameB'Section",
  "ContainsMultiProjectivePoint",--Eventually we will want to have multiprojective points.
  "NameB'Slice",
  "ListB'Sections",
  "makeB'TraceInput",
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

--##########################################################################--
-- GLOBAL VARIABLES
--##########################################################################--

DBG = 0 -- debug level (10=keep temp files)
BERTINIexe=(options Bertini).Configuration#"BERTINIexecutable"
--needsPackage"NAGtypes"
needsPackage "SimpleDoc"
     storeBM2Files = temporaryFileName();
     makeDirectory storeBM2Files
-- Bertini interface for M2
-- used by ../NumericalAlgebraicGeometry.m2

--BertiniVariety = new Type of MutableHashTable


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
bertiniZeroDimSolve = method(TypicalValue => List, Options=>{
  IsProjective =>-1,
  UseRegeneration =>-1,
  OutputStyle=>"OutPoints",--{"OutPoints","OutSolutions","OutNone"}--The output can be lists of Points (A muteable hash table), or lists of Solutions (list of complex numbers that are coordinates), or can be None (All information is stored on as a text file in the directory where the computation was ran).
  TopDirectory=>storeBM2Files,
	BertiniInputConfiguration=>{},
	AffVariableGroup=>{},
	HomVariableGroup=>{},
  RandomComplex=>{}, --A list or a list of list of symbols that denote random complex numbers.
  RandomReal=>{}, --A list or a list of list of symbols that denote random real numbers.
  B'Constants=>{},--A list of pairs. Each pair consists of a symbol that will be set to a string and a number.
  B'Functions=>{},--A list of pairs consisting of a name and a polynomial.
  NameSolutionsFile=>"raw_solutions",
  NameMainDataFile=>"main_data",
	M2Precision=>53,
  Verbose=>false
	} )
bertiniZeroDimSolve(Ideal) := o -> (I) ->bertiniZeroDimSolve( I_*,o )
bertiniZeroDimSolve(List) := o -> (myPol) ->(
    --myPol are your polynomial system that you want to solve. If empty return empty.
  if myPol=={} then error"Polynomial system is the empty list. ";
--%%--Bertini is text based. So directories have to be specified to store these text files which are read by Bertini.
--%%%%--When loading Bertini.m2 a temporary directory is made where files are stored by default: storeBM2Files.
--%%%%--To change the default directory, set the TopDirectory option to the directory you would like.
  myTopDir:=o.TopDirectory;
--%%-- We set AffVariableGroup and HomVariableGroup. If the user does not specify these groups then AffVariableGroup is taken to be the generators of the ring the first element of myPol.
  myAVG:= o.AffVariableGroup;
  myHVG:= o.HomVariableGroup;
--%%-- If the user does not specifiy variable groups then myAVG is set to the generators of the ring of the first polynomial.
  if myAVG==={} and myHVG==={}
  then (
    if not member (class first myPol,{String,B'Section,B'Slice,Product,Symbol})
    then (
	if o.IsProjective==-1 
    	then (myAVG=gens ring first myPol)
	else (myHVG=gens ring first myPol))
  else error"AffVariableGroup or HomVariableGroup need to be set. "    );
--%%-- Verbose set greater than 1 will print the variable groups.
--  if o.Verbose then print myAVG;
--  if o.Verbose then  print myHVG;
--%%--We need to set the CONFIGS of the Bertini input file.
--%%%%--These CONFIGS come in two flavors:
--%%%%--If the same configuration is set twice then Bertini will use the one set last.
--%%%%--The first is in BertiniInputConfiguration where we just list the configurations.
  myConfigs:=(o.BertiniInputConfiguration);
  if o.UseRegeneration===1 then myConfigs=myConfigs|{"UseRegeneration"=>1};
--  TODO: Regeneraetion test R=QQ[x]; length(bertiniZeroDimSolve({x^2}))==1;  bertiniZeroDimSolve({x^2},UseRegeneration=>1)=={}
--  print myConfigs;
--%%-- We use the makeB'InputFile method to write a Bertini file.
  makeB'InputFile(myTopDir,
    B'Polynomials=>myPol,
    AffVariableGroup=>myAVG,
    HomVariableGroup=>myHVG,
--%%--These are extra options the user can specify. For more information refer to their documentation.
    BertiniInputConfiguration=>myConfigs,
    RandomComplex=>o.RandomComplex,--A list or a list of list of symbols that denote random complex numbers.
    RandomReal=>o.RandomReal, --A list or a list of list of symbols that denote random real numbers.
    B'Constants=>o.B'Constants,--A list of pairs. Each pair consists of a symbol that will be set to a string and a number.
    B'Functions=>o.B'Functions--A list of pairs consisting of a name and a polynomial.
    );
--%%--Check for some errors.
--%%%%--
  if o.NameSolutionsFile=!="raw_solutions" and o.OutputStyle=!="OutSolutions"
  then error"If NameSolutionsFile is set then OutputStyle should be set to OutSolutions. ";
--%%--We call Bertini and solve the zero dimensional system.
    successRun:=runBertini(myTopDir,Verbose=>o.Verbose);
--    print successRun;
--%%--After completing the Bertini runs we import the results into Macaulay2; this is the list called theSols below.
--%%%%--Depending on the OutputStyle option we import nothing, main_data files to give Points, or raw_solutions files.
    if o.OutputStyle==="OutPoints"
    then theSols:=importMainDataFile(myTopDir,NameMainDataFile=>o.NameMainDataFile,M2Precision=>o.M2Precision);
    if o.OutputStyle==="OutSolutions"
    then theSols=importSolutionsFile(myTopDir,NameSolutionsFile=>o.NameSolutionsFile,OrderPaths=>true,M2Precision=>o.M2Precision);
--
    if o.OutputStyle=!="OutNone"
    then return theSols)

--For zero dim solve OutStyle and NameSolutionsFile need to both be changed.
--Do an error for this.

bertiniPosDimSolve = method(TypicalValue => NumericalVariety, Options=>{
  BertiniInputConfiguration=>{},
	Verbose=>false,
	IsProjective=>-1
  })
bertiniPosDimSolve List := o -> F -> (
--F is the list of polynomials
   L := {runType=>2};
   o2 := new OptionTable from L;
   o3 := o ++ o2;
   bertiniSolve(F,o3)
   )
bertiniPosDimSolve Ideal := o -> I -> bertiniPosDimSolve(I_*, o)


bertiniSample = method(TypicalValue => List, Options=>{Verbose=>false,
	BertiniInputConfiguration=>{},
	IsProjective=>-1
  })
bertiniSample (ZZ, WitnessSet) := o -> (n, W) -> (
  --W is a witness set
  -- n is the number of points to sample
  L := {runType=>3,dimen=>dim W, compnum => W.ComponentNumber,numpts => n, WitnessData=>W.WitnessDataFileName};
  o2 := new OptionTable from L;
  o3 := o ++ o2 ;
  bertiniSolve(flatten entries gens (W.Equations),o3)
  )


bertiniComponentMemberTest = method(TypicalValue => List, Options=>{Verbose=>false,
  BertiniInputConfiguration=>{},
	IsProjective=>-1})
bertiniComponentMemberTest (List, NumericalVariety) := o -> (pts, NV) -> (
--pts, list of pts to test
--NV, numerical variety
	 L := {
     BertiniInputConfiguration=>o.BertiniInputConfiguration,
     runType=>4,
     StartSolutions=>pts,
     WitnessData=>NV.WitnessDataFileName,
	   NVariety=>NV};
     o2 := new OptionTable from L;
     o3 := o ++ o2;
     bertiniSolve(NV.Equations, o3)
     )

bertiniRefineSols = method(TypicalValue => List, Options=>{Verbose=>false,
  BertiniInputConfiguration=>{},
  IsProjective=>-1
  })
bertiniRefineSols (ZZ, List, List) := o -> (d, F,p) -> (
  --d, number of digits
  --F is the list of polynomials.
  --p, list of points to sharpen
   L := {BertiniInputConfiguration=>o.BertiniInputConfiguration,
     runType=>5,
     StartSolutions=>p,
     digits=>d};
   o2 := new OptionTable from L;
   o3 := o ++ o2;
   bertiniSolve(F, o3)
   )


bertiniTrackHomotopy = method(TypicalValue => List, Options=>{
  Verbose=>false,
  BertiniInputConfiguration=>{},
  IsProjective=>-1} )
bertiniTrackHomotopy (RingElement, List, List) := o -> (t, H, S1) -> (
  --t, path variable
  --H, homotopy
  --S1, solutions to start system
   L := {BertiniInputConfiguration=>o.BertiniInputConfiguration,
     runType=>6,
     StartSolutions=>S1,
     PathVariable=>t};
   o2 := new OptionTable from L;
   o3 := o ++ o2;
   bertiniSolve(H,o3)
   )

--This is a type 2 user-defined homotopy
bertiniUserHomotopy = method(TypicalValue => List, Options=>{
  Verbose=>false,
	OutputStyle=>"OutPoints",--{"OutPoints","OutSolutions","OutNone"}--The output can be lists of Points (A muteable hash table), or lists of Solutions (list of complex numbers that are coordinates), or can be None (All information is stored on as a text file in the directory where the computation was ran).
	TopDirectory=>storeBM2Files,
	B'Functions=>{},
	BertiniInputConfiguration=>{},
	AffVariableGroup=>{},
	HomVariableGroup=>{},
	RandomComplex=>{},
	RandomReal=>{},
	B'Constants=>{},--A list of pairs. Each pair consists of a symbol that will be set to a string and a number.
	B'Functions=>{},--A list of pairs consisting of a name and a polynomial.
	M2Precision=>53
--	IsProjective=>-1
	--NonPolynomial=>false
	} )
bertiniUserHomotopy(Thing,List, List, List) := o -> (pathT, SPG, myPol, S1) -> (
--%%--Bertini is text based. So directories have to be specified to store these text files which are read by Bertini.
--%%%%--When loading Bertini.m2 a temporary directory is made where files are stored by default: storeBM2Files.
--%%%%--To change the default directory, set the TopDirectory option to the directory you would like.
  myTopDir:=o.TopDirectory;
--if o.NonPolynomial===false then()
--%%-- We set AffVariableGroup and HomVariableGroup. If the user does not specify these groups then AffVariableGroup is taken to be the generators of the ring the first element of myPol with myParams deleted.
  myAVG:= o.AffVariableGroup;
  myHVG:= o.HomVariableGroup;
  myParams:= for i in SPG list if class i===Option then first i else i;
  if myAVG==={} and myHVG==={}
  then (
    if not member (class first myPol,{String,B'Section,B'Slice,Product,Symbol})
    then (myAVG=gens ring first myPol;
      for i in flatten myParams do myAVG=delete(i,myAVG);
      myAVG=delete(pathT,myAVG))
  else error"AffVariableGroup or HomVariableGroup need to be set. ");
--%%-- We use the bWriteInputFile method to write a Bertini file.
  makeB'InputFile(myTopDir,
    SetParameterGroup=>SPG,
    B'Polynomials=>myPol,
    AffVariableGroup=>myAVG,
    HomVariableGroup=>myHVG,
    PathVariable=>{pathT},
--%%--These are extra options the user can specify. For more information refer to their documentation.
    BertiniInputConfiguration=>({{"UserHomotopy",2}}|o.BertiniInputConfiguration),
    RandomComplex=>o.RandomComplex,--A list or a list of list of symbols that denote random complex numbers.
    RandomReal=>o.RandomReal, --A list or a list of list of symbols that denote random real numbers.
    B'Constants=>o.B'Constants,--A list of pairs. Each pair consists of a symbol that will be set to a string and a number.
    B'Functions=>o.B'Functions--A list of pairs consisting of a name and a polynomial.
    );
--  print 1;
    writeStartFile(myTopDir,S1);
    runBertini(myTopDir,Verbose=>o.Verbose);
--    print 2;
--%%%%--Depending on the OutputStyle option, the style of this text file can be main_data or a list of coordinates.
--%%--After completing the Bertini runs we import the results into Macaulay2; this is the list called allSols below.
--%%%%--Depending on the OutputStyle option we import nothing, main_data files to give Points, or raw_solutions files.
    allSols:={};
    if o.OutputStyle==="OutPoints"
    then  allSols=importMainDataFile(myTopDir,M2Precision=>o.M2Precision,NameMainDataFile=>"main_data");
    if o.OutputStyle==="OutSolutions"
    then allSols=importSolutionsFile(myTopDir,NameSolutionsFile=>"raw_solutions",OrderPaths=>true,M2Precision=>o.M2Precision);
    if o.OutputStyle=!="OutNone"
    then return allSols)
--bertiniUserHomotopy(RingElement, List, List) := o -> (pathT, myPol, S1) -> bertiniUserHomotopy(pathT,{},myPol,S1)




bertiniParameterHomotopy = method(TypicalValue => List, Options=>{
	OutputStyle=>"OutPoints",--{"OutPoints","OutSolutions","OutNone"}--The output can be lists of Points (A muteable hash table), or lists of Solutions (list of complex numbers that are coordinates), or can be None (All information is stored on as a text file in the directory where the computation was ran).
	TopDirectory=>storeBM2Files,
	B'Functions=>{},
	BertiniInputConfiguration=>{},
	AffVariableGroup=>{},
	HomVariableGroup=>{},
	RandomComplex=>{}, --A list or a list of list of symbols that denote random complex numbers.
	RandomReal=>{}, --A list or a list of list of symbols that denote random real numbers.
	B'Constants=>{},--A list of pairs. Each pair consists of a symbol that will be set to a string and a number.
	B'Functions=>{},--A list of pairs consisting of a name and a polynomial.
	M2Precision=>53,
	Verbose=>false
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
    if not member (class first myPol,{String,B'Section,B'Slice,Product,Symbol})
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
    BertiniInputConfiguration=>({{ParameterHomotopy,1}}|o.BertiniInputConfiguration),
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
--%%%%--Depending on the OutputStyle option, the style of this text file can be main_data or a list of coordinates.
    runNumber:=0;
    for i in myParValues do(
      writeParameterFile(myTopDir,i);
      runBertini(myTopDir,Verbose=>o.Verbose);
      if o.OutputStyle==="OutPoints" then moveB'File(myTopDir,"main_data","ph_jade_"|runNumber);
      if o.OutputStyle==="OutNone" then moveB'File(myTopDir,"raw_solutions","ph_jade_"|runNumber);
      if o.OutputStyle==="OutSolutions" then moveB'File(myTopDir,"raw_solutions","ph_jade_"|runNumber);
      runNumber=runNumber+1
      );
--%%--After completing the Bertini runs we import the results into Macaulay2; this is the list called allSols below.
--%%%%--Depending on the OutputStyle option we import nothing, main_data files to give Points, or raw_solutions files.
    allSols:={};
    if o.OutputStyle==="OutPoints"
    then for i from 0 to #myParValues-1 do allSols=allSols|{importMainDataFile(myTopDir,M2Precision=>o.M2Precision,NameMainDataFile=>"ph_jade_"|i)};
    if o.OutputStyle==="OutSolutions"
    then for i from 0 to #myParValues-1 do allSols=allSols|{importSolutionsFile(myTopDir,NameSolutionsFile=>"ph_jade_"|i,OrderPaths=>true,M2Precision=>o.M2Precision)};
--
    if o.OutputStyle=!="OutNone"
    then return allSols)


---------------------------------------------------
-- bertiniSolve: This is the main control function:
---------------------------------------------------

bertiniSolve = method(TypicalValue => List, Options=>{
  BertiniInputConfiguration => {},
	AllowStrings=>-1,
  MultiplicityTol=>1e-6,
	Verbose=>false,
  IsProjective=>-1,Parameters=>null,ParameterValues=>null,StartSystem=>{},
	StartSolutions=>{},NVariety=>null, RawData=>null,WitnessData=>null,
	dimen=>-1,compnum=>-1,numpts=>-1,Points=>{},digits=>-1,runType=>0,
	PathVariable=>null})
bertiniSolve List := o -> F -> (  -- F is the list of polynomials
	  dir := makeBertiniInput(F,o);   -- creates the input file
          if o.Verbose then stdio << "The version of Bertini
	    you have installed on your computer
	    was used for this run. \nBertini is under ongoing development by
	    D. Bates, J. Hauenstein, A. Sommese, and C. Wampler.\n\n";

	  --if o.WriteOnly=!=-1 then break "Write Only";

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

         readSolutionsBertini(dir,F,o) -- o contains runType,
	 --so we can switch inside readSolutionsBertini
         )


-------------------
-- makeBertiniInput
-------------------

makeBertiniInput = method(TypicalValue=>Nothing,Options=>{
  BertiniInputConfiguration=>{},
	AllowStrings=>-1,
  MultiplicityTol=>1e-6,
	Verbose=>false,
  Parameters=>null,ParameterValues=>null,StartSystem=>{},
	StartSolutions=>{},RawData=>null,WitnessData=>null,NVariety=>null,
	IsProjective=>-1,
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

    dir := temporaryFileName(); -- build a directory to store temporary data
    makeDirectory dir;
    f := openOut (dir|"/input"); -- typical (but not only possible) name for
    --Bertini's input file

    -- The following block is the config section of the input file

    f << "CONFIG\n\n";-- starting the config section of the input file

    -- for each user-provided option, we write the appropriate config to the file:
    scan(o.BertiniInputConfiguration,i->f<<(toString first i) <<": "<<(toString last i)<<" ;\n");
    -- now we handle the various runType options:

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
      else ((  -- refine sols runs: write out polys and some other stuff
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

	  if o.Verbose then stdio
	       << "Temporary directory for input and output files:" << dir << endl << endl;

      dir
      )



-----------------------
-- readSolutionsBertini
-----------------------

readSolutionsBertini = method(TypicalValue=>NumericalVariety, Options=>{
  BertiniInputConfiguration=>{},
  MultiplicityTol=>1e-6,
	Verbose=>false,
  AllowStrings=>-1,
	IsProjective=>-1,Parameters=>null,
	ParameterValues=>null, StartSystem=>{},NVariety=>null,
	StartSolutions=>{},RawData=>null,WitnessData=>null,
	dimen=>-1,compnum=>-1,numpts=>-1,Points=>{},
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
  if (member(o.runType,{0,8}))
  then (
    sessionLog:= lines get (dir|"/bertini_session.log"); -- get contents of session log
    --and check for rank error
    --TODO incorporate this error in bertiniZeroDimSolve
    scan(sessionLog, i->if i=="The system has no zero dimensional solutions based on its rank!" then
	     error  "The system has no zero dimensional solutions based on its rank!");
    failedPaths := lines get (dir|"/failed_paths"); -- get contents of failed paths file and check if non-empty
    if  failedPaths=!={""} then (
	  if o.Verbose then stdio << "Warning: Some paths failed, the set of solutions may be incomplete" <<endl<<endl) ;
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
    	if o.IsProjective==-1
      then for j from 1 to numVars-1 do (
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
    	l = drop(l,1);
      pt.SolutionNumber = value(first l);
      solNum = pt.SolutionNumber;
      l = drop(l,1);
      pt.Coordinates = dehomCoords; --we want to output these
    	pts = join(pts,{pt});
    	);
      pts = solutionsWithMultiplicity(pts, Tolerance=>o.MultiplicityTol);
      if o#?UseRegeneration then(
          if o.UseRegeneration==1 then return pts
          );
      checkMultiplicity(pts);
      checkConditionNumber(pts, 1e10);--TODO: 1e10 specifies a condition number tolerance that should be an option.
      for i in pts do (
        if (i.SolutionStatus=!=Singular
          and i.SolutionStatus=!=FailedPath
      	  and i.SolutionStatus=!=RefinementFailure)
        then i.SolutionStatus=Regular);
    	return pts
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
    	if where'is'e===null
      then value(s|"p" | toString P)
  	   else (
         pos := first first where'is'e;
         value (substring((0,pos),s) | "p" | toString P | substring((pos,#s-pos),s))
         ));
    while solNum > -1 do (
  	-- -1 in solNum position (top of solution block) is key to end of solutions.
    	maxPrec = value(first l);
    	l = drop(l,1);
    	bitPrec := ceiling((log 10/log 2)*o.digits);
    	coords = {};
    	for j from 1 to numVars do ( -- grab each coordinate
    	    -- use regexp to get the two numbers from the string
    	    coord = select("[0-9.e+-]+", cleanupOutput(first l));
    	    if (o.runType==1 or o.runType==6)
          then (
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
    	if(value(first l)=!=1) and o.runType==5
      then pt.SolutionStatus=RefinementFailure else pt.SolutionStatus=null;
    	if(value(first l)=!=1) and o.runType=!=5 then  pt.SolutionStatus=FailedPath;
    	l=drop(l,1);
      pt.SolutionNumber = value(first l);
     	solNum=pt.SolutionNumber;
      l = drop(l,1);
      pt.Coordinates = coords; --we want to output these
    	pts=join(pts,{pt})
      );
    pts=solutionsWithMultiplicity(pts, Tolerance => o.MultiplicityTol);
    if o#?UseRegeneration then(
        if o.UseRegeneration==1 then return pts
        );
    checkMultiplicity(pts);
    checkConditionNumber(pts, 1e10);--TODO: 1e10 specifies a condition number tolerance that should be an option.
    for i in pts do (
      if (i.SolutionStatus=!=Singular
        and i.SolutionStatus=!=FailedPath
        and i.SolutionStatus=!=RefinementFailure)
      then i.SolutionStatus=Regular);
    return pts ) else
  --if PosDim, we read in the output from witness_data
   if (o.runType == 2) then (
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
  	    if o.IsProjective==-1
            then (
	        for j from 1 to numVars-1 do (
  		    dehomCoords = join(dehomCoords, {coords#j / coords#0});
          	    ))
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
  	    N = map(CC^0,CC^numVars,0); -- this is a dummy, will grab slice data later
  	    ws = if o.IsProjective===1 then ( 
		W := projectiveWitnessSet(ideal F, N -* fake affine chart *-, N, ptsInWS); 
		W.AffineChart = null; -- !!! this is a hack
		W
		) else witnessSet(ideal F, N, ptsInWS);
	    ws.IsIrreducible = true;
	    --turn these points into a witness set
	    -- ws = witnessSet(ideal F,N, ptsInWS); --turn these points into a witness set
            ws.ComponentNumber=j;
	    ws.WitnessDataFileName=dir|"/witness_data";
	    wList = join(wList, {ws}); --add witness set to list
	    listOfCodims = join(listOfCodims, {codimen});
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


    for i from 1 to value(linCoeffDims#0) do ( 
	for j from 1 to value(linCoeffDims#1) do (
            coefParts = select("[0-9-]+/[0-9-]+", first l);
            rw = join(rw, {toCC(53,value(coefParts#0)) + 
		    ii*toCC(53,value(coefParts#1))});  
	    -- definitely losing data here, going from rational number to float!
            l = drop(l,1);
            );
        mat = join(mat, {rw});  
        rw = {};
        );
    
    M = if #mat>0 then transpose matrix(mat) else map(CC^(numVars+1),CC^0,0); --stores all slices

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
	--We store the cols of M needed for this particular codimNum in coeffList,
	--then turn it into a matrix and store it the witness set.
	colsToSkip = listOfCodims#codimNum - listOfCodims#0;
	N = transpose submatrix(M,,colsToSkip..numcols M - 1);
	if o.IsProjective===1 then N = map(CC^(numrows N),CC^1,0)|N; -- constant terms are 0xb
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
  else error "unknown output file";
  )



-*
restart
path
path=prepend("/Users/jo/Documents/GoodGit/AntonM2/M2/Macaulay2/packages",path)
needsPackage"Bertini"
debug Bertini
R = CC[x,y,z,t]
I = ideal(x + 3, y+1)
I = ideal(x*(x + 3), x*(y+1)*(z-t^2))
I = ideal(x^2*(x + 3), x^2*(y+1)*(z-t^2))
I = ideal(x,y,z)
I = ideal(x,2*z-t,x-2*y-1)

nv = bertiniPosDimSolve(I_*, Verbose => true)
w = first components nv
F = polySystem slice w
pts2 = w#Points
pts2 / (p -> norm evaluate(F,p)) -- this value is >> 0
nv#WitnessDataFileName
PWD  = new MutableHashTable from {IsProjective=>-1}
A = parseWitnessDataFile(PWD,first separate("w",nv#WitnessDataFileName),"witness_data")
peek PWD
peek PWD#"WS"#0
peek PWD#"WS"#1
peek PWD#"WS"#2

(matrix{{1_CC}}|sub(vars R,    matrix PWD#"WS"#0#0))* transpose PWD#"SliceData" 
PWD#"Directory"

R = CC[x,y,z,t];I = ideal(x,y);
nv = bertiniPosDimSolve(I_*, Verbose => true,IsProjective=>1)
PWD  = new MutableHashTable from {IsProjective=>1}
A = parseWitnessDataFile(PWD,first separate("w",nv#WitnessDataFileName),"witness_data")
PWD#"RemainingFile"
sub(vars R,    matrix PWD#"WS"#0#0)*transpose  PWD#"SliceData" 


R = CC[x,y,z,t]
I = ideal(x + 3, y+1)
IP =-1
nv = bertiniPosDimSolve(I_*, Verbose => true)
PWD  = new MutableHashTable from {IsProjective=>IP}
A = parseWitnessDataFile(PWD,first separate("w",nv#WitnessDataFileName),"witness_data")
PWD#"RemainingFile"
(matrix{{1}}|sub(vars R,    matrix PWD#"WS"#0#0)) * transpose PWD#"SliceData" 

PWD#"SliceData"
PWD#"WS"#1//toList/(i->i#"ComponentNumber")
PWD#"WS"#1//toList/(i->i#"Multiplicity")
PWD#"WS"#0//toList/(i->i#"Multiplicity")
*-
--This method is used for debugging parsing witness data files. 
parseWitnessDataFile = method(TypicalValue=>MutableHashTable)
parseWitnessDataFile (MutableHashTable,String,String) := (PWD,dir,name) -> (
    --PWD :=new MutableHashTable from {};
    PWD#"Directory"=dir;
    PWD#"Name"=name;
    if  dir_-1=!="/" then dir =dir|"/";
    l := lines get (dir|name); -- grabs all lines of the file
    numVars := value(first l);  
    PWD#"NumVars"=numVars;
    l = drop(l,1);
    maxCodim := value(first l); 
    PWD#"MaxCodim"=maxCodim;--Number of equidimensional witness sets
    l=drop(l,1);    
    --list of witness sets indexed by codimension
    wList := new MutableList from for i to maxCodim-1 list null;
    --keeps track of codimension of each witness set; 
    trueCodimension := new MutableList from for i to maxCodim-1 list null;     
    --componentIndex#i number of components in codimension i.        
    componentIndex := new MutableList from for i to maxCodim-1 list null;  
    --numPoints#i number of pts in codimension i.
    numPoints := new MutableList from for i to maxCodim-1 list null;  
    scan(PWD#"MaxCodim",
	ic->(
	    print 1;
	    trueCodimension#ic = value(first l); 
	    l=drop(l,1);
            if componentIndex#ic===null then componentIndex#ic={};
	    numPoints#ic = value(first l);
	    l=drop(l,1);
	    pts := new MutableList from for i to numPoints#ic-1 list null ;  
	    -- We now construct a new point using the type Point.
    	    print"numPoints#ic loop";
--
            scan(numPoints#ic,
		ptNum->(
            	    pt := new Point;
	    	    maxPrec := value(first l);
            	    l = drop(l,1);
	    	    pt#"MaxPrecisionBits"=maxPrec;
            	    coords := new MutableList from for i to numVars-1 list null;
    	    	    print"numVars loop";
            	    scan(numVars,
			j->( -- grab each coordinate
              		    -- use regexp to get the two numbers from the string
	      		    coord := select("[0-9.e+-]+", cleanupOutput(first l));  
	      		    -- NOTE: we convert to maxPrec bits complex type
              		    coords#j = toCC(maxPrec, value(coord#0),value(coord#1));  
              		    l = drop(l,1);
              		    )
			);
    	    	    --If we have an affine variety, we homogenize by the first coordinate. 
    	    	    pt#"ProjectiveCoordinates"=coords;
            	    l = drop(l,numVars+1);  -- don't need second copy of point or extra copy of maxPrec
	    	    if PWD.IsProjective===1 
		    then pt.Coordinates = toList coords 
            	    -- If we have an affine variety we dehomogenize, assuming the first variable is the hom coord:
	    	    else pt.Coordinates =(1/coords#0)*toList drop(coords,1);    	    
	    	    condNum := value(cleanupOutput(first l)); 
	    	    pt#"ConditionNumber"=condNum;
	    	    l=drop(l,4);
    	    	    --What is type?
            	    ptType := value(first l); l=drop(l,1);
	    	    pt#"PointType"=ptType;
            	    ptMult := value(first l); l=drop(l,1);
            	    pt#"Multiplicity"=ptMult;
    	    	    compNum := value(first l); l=drop(l,1);
	    	    pt#"ComponentNumber"=compNum;
            	    numDeflations := value(first l); l=drop(l,1);
    	    	    pt#"NumDeflations"=numDeflations;
    	    	    --Append pt to pts
    	    	    print pt.Coordinates;
            	    pts#ptNum = pt;
    	    	    print (componentIndex#ic);
            	    if not member(compNum,componentIndex#ic)
	    	    then componentIndex#ic = append(componentIndex#ic,compNum)            	     
		    )
		);
	    wList#ic =  pts
	    )
	);
    PWD#"WS"=wList;
    -- now we grab the slice data, at the end of the witness_data file, 
    --to be inserted into the witnessSets with dim>0
    l = drop(l,2); -- These are the lines {-1, blank line} 
    --MPType line
    PWD#"MPType"=first l; 
    l=drop(l,1);

    --#cols for the matrix used to randomize the system 
    randDims := select("[0-9]+", first l);  -- grabs #rows,     
    l = drop(l,1);
    
    -- numRands is the number of random numbers we want to skip next    
    numRands := value(randDims#0) * value(randDims#1);  
    
    l = drop(l,numRands+1);   -- includes blank line after rands    
    
    -- next we have the same number of integers 
    --(degrees needed to keep homogenization right)
    l = drop(l,numRands);
    
    -- next we have an integer and a list of row vectors 
    --(the number of which is the initial integer).  Again related to homogenization.    
    numToSkip := select("[0-9]+", first l);
    
    l = drop(l,value(numToSkip#0)+3); -- dropping all those, 
    --plus line containing integer (before), then blank line, and one more line
    
    --finally, we have the number of linears and the number of coefficients per linear
    (numberOfLinears,numberOfCoefficientsPerLinear) := toSequence select("[0-9-]+", first l);
    l = drop(l,1);

    --now we just read in the matrix
    numLinCoeffs := value(numberOfLinears) * value(numberOfCoefficientsPerLinear);
    rw := {};
    mat := {};
    PWD#"NumberOfLinears" =value(numberOfLinears);
    PWD#"NumberOfCoefficientsPerLinear" =value(numberOfCoefficientsPerLinear);    
    for i from 1 to PWD#"NumberOfLinears"  do ( 
	for j from 1 to PWD#"NumberOfCoefficientsPerLinear"  do (
            coefParts := select("[0-9-]+/[0-9-]+", first l);
            rw = join(rw, {toCC(53,value(coefParts#0)) + 
		    ii*toCC(53,value(coefParts#1))});  
	    -- definitely losing data here, going from rational number to float!
            l = drop(l,1);
            );
        mat = join(mat, {rw});  
        rw = {};
        );    
    M := matrix(mat);
    PWD#"SliceData"=M;   
    PWD#"RemainingFile"=l;
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
    PWD);


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


pairTypes={List,Option}
makeB'InputFile = method(TypicalValue => String, Options=>{
	StorageFolder=>null,
	NameB'InputFile=>"input",  --This option allows us to change the name of the input file that we will make.
	BertiniInputConfiguration=>{}, --This option is a list of pairs of strings or options. These will be written in the CONFIG part of the Bertini input file.
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
	Verbose=>false,
	SetParameterGroup=>{}
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
	 if o.Verbose then print filesGoHere;
	 if not fileExists(filesGoHere) then mkdir(filesGoHere))
     else filesGoHere=IFD;
     openedInputFile:= openOut(filesGoHere|o.NameB'InputFile);
     openedInputFile <<  endl  << "% This input file was written with the Bertini.m2 Macaulay2 package." << endl<<endl;
--The first part of a Bertini input file is the configurations.  We write the configuratiosn followed by a line "%%%ENDCONFIG;". We use this line as marker to write configurations after writing the initial file.
     openedInputFile << "CONFIG" << endl << endl;
     for oneConfig in o.BertiniInputConfiguration do (
       if class oneConfig===Option
       then openedInputFile << toUpper toString((toList oneConfig)_0) << " : " << toString((toList oneConfig)_1) << " ; " << endl
       else if class oneConfig===List then openedInputFile << toString(oneConfig_0) << " : " << toString(oneConfig_1) << " ; " << endl
       else error("BertiniInputConfiguration has an unreadable element: "|toString oneConfig));
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
--If userdefined homotopy then we write the parameters and in terms of the path variable.
     if #o.PathVariable=!=0 then(
     if #o.SetParameterGroup=!=0 and not member( class((o.SetParameterGroup)_0 ),pairTypes) then error"Parameters should be set in terms of the pathvariable, e.g., x=>t,y=>t^2. ";
     oneGroupNames:=for i in o.SetParameterGroup list if class i ===List then first i else if class i===Option then first toList i;
     writeNamedListToB'InputFile("parameter",oneGroupNames,openedInputFile);
     for onePair in o.SetParameterGroup do (
       if class onePair===List
       then openedInputFile << toString(onePair_0) << " = "<<toString(onePair_1)<< " ; "<<endl << endl;
       if class onePair===Option
       then openedInputFile << toString( (toList onePair)_0) << " = "<<toString( (toList onePair)_1)<< " ; "<<endl << endl;
 	    );
       openedInputFile << endl; );
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


addSlash=(aString)->(
    if aString_-1===" " then error (aString|" cannot end with whitespace.");
    if aString_-1=!="/" then aString=aString|"/";
    return aString    )

makeSampleSolutionsFile = method(TypicalValue => Nothing, Options=>{
	NameSolutionsFile=>"sample_solutions_file",
	NameB'InputFile=>"input",
	StorageFolder=>null,
	SpecifyComponent=>{},
	Verbose=>false
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
	Verbose=>false
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
    if o.Verbose then print (filesGoHere);
    if o.Verbose then print o.NameSolutionsFile;
    moveB'File(IFD,o.NameSolutionsFile,"member_points");
    if not fileExists(filesGoHere|"witness_data") then error"witness_data file does not exist. ";
    s:= run("sed -i -e 's/%%%ENDCONFIG/TRACKTYPE : 3; %%%ENDCONFIG/' "|IFD|o.NameB'InputFile);
    runBertini(IFD,StorageFolder=>o.StorageFolder,Verbose=>o.Verbose)
    )





replaceFirstLine = method(TypicalValue => Nothing, Options=>{
	})
replaceFirstLine(String,String,Thing) := o ->(filesGoHere,fileName,aString)->(
    if toString(filesGoHere)_-1==="/" then aDir:=filesGoHere else aDir=filesGoHere|"/";
    run("sed -i -e "|toExternalString("1s/.*/")|toString(aString)|toExternalString("/")|" "|aDir|fileName)
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

importMainDataFile=method(TypicalValue=>String,Options=>{
	M2Precision=>53,
	NameMainDataFile=>"main_data",
	SpecifyDim=>false,
	Verbose=>false
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
      if o.Verbose then print "win";
      aNewPoint:=new Point;
      --Sol. Number and path number
      theLine0:=separate(" ",allInfo_0);
      aNewPoint.SolutionNumber=value (theLine0_1);
      if o.Verbose then print theLine0;
      aNewPoint.PathNumber=value replace("\\)","",(theLine0_4));
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
      if o.Verbose then   print linesPerSolutions;
      allInfo=drop(allInfo,linesPerSolutions);
      if o.Verbose then print allInfo
      );
    return theListOfPoints);
    if posDimCase
    then   (
    if o.Verbose then print 1;
    allInfo=drop(allInfo,4);
    linesPerSolutions=theNumberOfVariables+6;
    theListOfPoints={};
    while #select("reproduce",allInfo_0)=!=1 do(
      if o.Verbose then print 2;
      if #select("DIMENSION",allInfo_0)=!=0
      then (
	if o.Verbose then print 3;
	theDim:=value (select("[0-9]+",allInfo_0))_0;
        if o.SpecifyDim=!=false and o.SpecifyDim=!=theDim then dimFlag:=false else dimFlag=true;
	allInfo=drop(allInfo,1))
      else if #select("NONSINGULAR",allInfo_0)=!=0 and #select("UNCLASSIFIED",allInfo_0)===0
      then (
	if o.Verbose then print 4;
	solUnclassified:=0;
	theSolutionType:="NONSINGULAR";
	allInfo=drop(allInfo,1))
      else if #select("SINGULAR",allInfo_0)=!=0 and #select("NON",allInfo_0)===0 and #select("UNCLASSIFIED",allInfo_0)===0
      then (
	if o.Verbose then print 5;
	solUnclassified=0;
	theSolutionType="SINGULAR";
	allInfo=drop(allInfo,1))
      else if #select("UNCLASSIFIED NONSINGULAR",allInfo_0)=!=0
      then (
	if o.Verbose then print 5.1;
	solUnclassified=1;
	theSolutionType="NONSINGULAR";
	allInfo=drop(allInfo,1))
      else if #select("UNCLASSIFIED SINGULAR",allInfo_0)=!=0
      then (
	if o.Verbose then print 5.2;
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
	Verbose=>false})
runBertini(String) := o ->(IFD)->(--IFD=input file directory
    	IFD=addSlash(IFD);
    	if o.StorageFolder=!=null
    	then (
	  filesGoHere:=addSlash(IFD|o.StorageFolder);
	  if not fileExists(filesGoHere) then mkdir(filesGoHere))
        else filesGoHere=addSlash(IFD);
    	if o.TextScripts=!="" then theTS:=" < "|o.TextScripts else theTS="";
    	if o.Verbose then print o.B'Exe;
    	runSuccess:=run("cd "|filesGoHere|"; "|(o.B'Exe)|" "|IFD|o.NameB'InputFile|theTS|" >bertini_session.log");
    	if runSuccess=!=0
	then (
	  print fileExists(filesGoHere|"bertini_session.log");
	  print readFile(filesGoHere,"bertini_session.log",10000);
	  error"Bertini run failed. ");
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
     if class first listOfListCoords ===Point then listOfListCoords=listOfListCoords/coordinates;
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
	Verbose=>false })
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
    if o.Verbose then for i in importedFileLines do print i;
    numberOfsolutionsInFile:=value(importedFileLines_0_0);--the first line of the solution file gives the number of solutions in the file
    if numberOfsolutionsInFile==0 then return {};
    importedFileLines=drop(importedFileLines,1);--drop the first  line
    storeSolutions:={};---We will store the solutions we specified and return this in the end
    collectedCoordinates:={};
    if o.Verbose then print collectedCoordinates;
    if o.OrderPaths===false then(
    for i in importedFileLines do(
    	if o.Verbose then print( "i",i);
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
	if o.Verbose then print oneCoord;
	collectedCoordinates=append(collectedCoordinates,
	  value((oneCoord_0)|"p"|o.M2Precision|"e"|toString(value(oneCoord_1)))+
	    ii*value((oneCoord_2)|"p"|o.M2Precision|"e"|toString(value(oneCoord_3)))
	      ));
	if o.Verbose then print collectedCoordinates;
    	if  #i>2  then error ("Line was not parsed: "|i_0|"...")));
    	numberOfCoordinates:=numerator(#collectedCoordinates/numberOfsolutionsInFile);
	if o.Verbose then print numberOfCoordinates;
    	storeSolutions=for i to numberOfsolutionsInFile-1 list
	  for j to numberOfCoordinates-1 list collectedCoordinates_(i*numberOfCoordinates+j);
    	if o.OrderPaths===true then(
	  if o.Verbose then print "inLoop";
	  sortStoreSolutions:=sort storeSolutions;
	  storeSolutions=for i in sortStoreSolutions list drop(i,1);
    	  if o.Verbose then for i in sortStoreSolutions do print i_0;
    	  if #storeSolutions=!=solutionCount then print "Warning: Unexpected solution count. OrderPaths option should only be set to 'true' when importing solution files with path numbers."
	    );
    return storeSolutions    );






importIncidenceMatrix= method(TypicalValue=>Nothing,Options=>{
	NameIncidenceMatrixFile=>"incidence_matrix",
	StorageFolder=>null,
	Verbose=>false })
importIncidenceMatrix(String) := o -> (importFrom)-> (
    if  class o.NameIncidenceMatrixFile===String
    then NSF:=o.NameIncidenceMatrixFile;
    importFrom=addSlash importFrom;
    if o.StorageFolder=!=null
    then importFrom=addSlash(importFrom|o.StorageFolder);
    importFrom=importFrom|NSF;
    if false=== fileExists importFrom then error ("File "|NSF|" does not exist.");
    importedFileLines := apply(lines get (importFrom),i->select("[0-9.e+-]+",i)); -- grabs all lines of the file and selects desired words.
    if o.Verbose then for i in importedFileLines do print i;
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
	NameB'Slice=>null
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
--    print numberOfSections;
--    print theCoefs;
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

--TODO: radicalList needs a more descriptive name
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
load concatenate(Bertini#"source directory","./Bertini/TST/makeBSection.tst.m2")
///

TEST///
load concatenate(Bertini#"source directory","./Bertini/TST/makeBSlice.tst.m2")
///

TEST///
load concatenate(Bertini#"source directory","./Bertini/TST/makeMembershipFile.tst.m2")
///

TEST///
load concatenate(Bertini#"source directory","./Bertini/TST/subPoint.tst.m2")
///

TEST///
load concatenate(Bertini#"source directory","./Bertini/TST/moveBFile.tst.m2")
///

TEST///
load concatenate(Bertini#"source directory","./Bertini/TST/radicalList.tst.m2")
///

TEST///
load concatenate(Bertini#"source directory","./Bertini/TST/bertiniUserHomotopy.tst.m2")
///

--##########################################################################--
-- DOCUMENTATION
--##########################################################################--

beginDocumentation()

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
      F = {x^2-1,y^2-2}
      solns = bertiniZeroDimSolve(F)
///;
------------------------------------------------------
------MAIN METHODS ------------
------------------------------------------------------
--bertiniZeroDimSolve,bertiniPosDimSolve,bertiniParameterHomotopy,bertiniUserHomotopy,bertiniComponentMemberTest,bertiniSample
doc ///
  Key
    bertiniZeroDimSolve
    (bertiniZeroDimSolve,Ideal)
    (bertiniZeroDimSolve,List)
  Headline
    a main method to solve a zero-dimensional system of equations
  Usage
    S = bertiniZeroDimSolve F
    S = bertiniZeroDimSolve I
    S = bertiniZeroDimSolve(I, UseRegeneration=>1)
  Inputs
    F:List
      a list of ring elements (system need not be square)
    I:Ideal
      an ideal defining a variety
  Outputs
    S:List
      a list of points that are contained in the variety of F
  Description
    Text
      This method finds isolated solutions to the system F via numerical polynomial homotopy continuation
      by (1) building a Bertini input file from the system F,
      (2) calling Bertini on this input file,
      (3) returning solutions from a machine readable file that is an output from Bertini.
    Example
      R = CC[x,y];
      F = {x^2-1,y^2-2};
      S = bertiniZeroDimSolve F
    Text
      Each solution is of type @TO Point@.  Additional information about the solution can be accessed by using @TO peek@.
    Example
      peek S_0
    Text
      Bertini uses a multihomogeneous homotopy as a default, but regeneration can be deployed with the option UseRegeneration=>1 .
    Example
      R = CC[x];
      F = {x^2*(x-1)};
      S = bertiniZeroDimSolve F
      B = bertiniZeroDimSolve(F,UseRegeneration=>1)
    Text
      Variables must begin with a letter (lowercase or capital) and
      can only contain letters, numbers, underscores, and square brackets.
      Regeneration in bertiniZeroDimSolve only finds nonsingular isolated points.
///

--Options
doc ///
  Key
    TopDirectory
    [bertiniParameterHomotopy, TopDirectory]
    [bertiniZeroDimSolve,TopDirectory]
    [bertiniUserHomotopy,TopDirectory]
  Headline
    Option to change directory for file storage.
  Usage
    bertiniParameterHomotopy(...,TopDirectory=>String)
    bertiniZeroDimSolve(...,TopDirectory=>String)
    bertiniUserHomotopy(...,TopDirectory=>String)
  Description
    Text
      This option specifies a directory to store Bertini output files.
///

doc ///
  Key
    UseRegeneration
  Headline
    an option specifying when to use regeneration
  Usage
    bertiniParameterHomotopy(...,TopDirectory=>String)
    bertiniZeroDimSolve(...,TopDirectory=>String)
    bertiniUserHomotopy(...,TopDirectory=>String)
  Description
    Text
      This option is set to 1 to have Bertini use regeneration when solving a polynomial system.

///

doc ///
  Key
    bertiniPosDimSolve
    (bertiniPosDimSolve,Ideal)
    (bertiniPosDimSolve,List)
  Headline
    a main method that is used to produce witness sets
  Usage
    V = bertiniPosDimSolve I
    V = bertiniPosDimSolve F
  Inputs
    F:List
      a list of ring elements defining a variety
  Outputs
    V:NumericalVariety
      a numerical irreducible decomposition of the variety defined by F
  Description
    Text
      The method {\tt bertiniPosDimSolve} calls  {\tt Bertini} to find
      a numerical irreducible decomposition of the zero-set of F.  The decomposition is
      returned as the @TO NumericalVariety@ NV.  Witness sets of NV contain approximations
      to solutions of the system F=0.
      Bertini (1) writes the system to temporary files,
      (2) invokes {\tt Bertini}'s solver with {\tt TrackType => 1},
      (3) Bertini uses a cascade homotopy to find witness supersets in each dimension,
      (4) removes extra points using a membership test or local dimension test,
      (5) deflates singular witness points, and finally
      (6) decomposes using a combination of monodromy and a linear trace test
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
///

doc ///
  Key
    bertiniSample
    (bertiniSample, ZZ, WitnessSet)
  Headline
    a main method to sample points from an irreducible component of a variety
  Usage
    V = bertiniSample (n, W)
  Inputs
    n:ZZ
      an integer specifying the number of desired sample points
    W:WitnessSet
      a witness set for an irreducible component
  Outputs
    L:List
      a list of sample points
  Description
    Text
      Samples points from an irreducible component of a variety using Bertini.  The irreducible
      component needs to be in its numerical form as a @TO WitnessSet@.  The method
      @TO bertiniPosDimSolve@ can be used to generate a witness set for the component.
      Bertini (1) writes the witness set to a temporary file,
      (2) invokes {\tt Bertini}'s solver with option {\tt TrackType => 2}, and
      (3 moves the hyperplanes defined in the @TO WitnessSet@ W within the space until the desired points are sampled,
      (4) stores the output of {\tt Bertini} in a temporary file, and finally
      (5) parses and outputs the solutions.
    Example
      R = CC[x,y,z]
      F = { (y^2+x^2+z^2-1)*x, (y^2+x^2+z^2-1)*y }
      NV = bertiniPosDimSolve(F)
      W = NV#1_0 --z-axis
      bertiniSample(4, W)
///


doc ///
  Key
    bertiniTrackHomotopy
    (bertiniTrackHomotopy, RingElement, List, List)
  Headline
    a main method to track using a user-defined homotopy
  Usage
    S0=bertiniTrackHomotopy(t, H, S1)
  Inputs
    t:RingElement
      a path variable
    H:List
      a list polynomials that define the homotopy with respect to the path variable
    S1:List
      a list of solutions to the start system
  Outputs
    S0:List
      a list of solutions to the target system
  Description
    Text
      This method calls {\tt Bertini} to track a user-defined homotopy.  The
      user needs to specify the homotopy H, the path variable t, and a list
      of start solutions S1.
      Bertini (1) writes the homotopy and start solutions to temporary files,
      (2) invokes {\tt Bertini}'s solver with configuration keyword {\tt UserHomotopy => 1}
      in the affine case and {\tt UserHomotopy => 2} in the projective situation,
      (3) stores the output of {\tt Bertini} in a temporary file, and
      (4) parses a machine readable file to output a list of solutions.
    Example
      R = CC[x,t]; -- include the path variable in the ring
      H = { (x^2-1)*t + (x^2-2)*(1-t)};
      sol1 = point {{1}};
      sol2 = point {{-1}};
      S1= { sol1, sol2  };--solutions to H when t=1
      S0 = bertiniTrackHomotopy (t, H, S1) --solutions to H when t=0
      peek S0_0
    Text
      In the previous example, we solved $x^2-2$ by moving
      from $x^2-1$ with a linear homotopy. {\tt Bertini} tracks homotopies starting at
      $t=1$ and ending at $t=0$. Final solutions are of the type Point.
    Example
      R=CC[x,y,t]; -- include the path variable in the ring
      f1=(x^2-y^2);
      f2=(2*x^2-3*x*y+5*y^2);
      H = { f1*t + f2*(1-t)}; --H is a list of polynomials in x,y,t
      sol1=    point{{1,1}}--{{x,y}} coordinates
      sol2=    point{{ -1,1}}
      S1={sol1,sol2}--solutions to H when t=1
      S0=bertiniTrackHomotopy(t, H, S1, IsProjective=>1) --solutions to H when t=0
    Text
      Variables must begin with a letter (lowercase or capital) and
      can only contain letters, numbers, underscores, and square brackets.
///

doc ///
  Key
    bertiniUserHomotopy
    (bertiniUserHomotopy, Thing, List, List, List)
  Headline
    a main method to track a user-defined homotopy
  Usage
    S0=bertiniUserHomotopy(t, P, H, S1)
  Inputs
    t:RingElement
      a path variable
    P: List
      a list of options that set the parameters
    H:List
      a list of polynomials that define the homotopy
    S1:List
      a list of solutions to the start system
  Outputs
    S0:List
      a list of solutions to the target system
  Description
    Text
      This method calls {\tt Bertini} to track a user-defined homotopy.  The
      user needs to specify the homotopy H, the path variable t, and a list
      of start solutions S1.
      Bertini (1) writes the homotopy and start solutions to temporary files,
      (2) invokes {\tt Bertini}'s solver with configuration keyword {\tt UserHomotopy => 2},
      (3) stores the output of {\tt Bertini} in a temporary file, and
      (4) parses a machine readable file to output a list of solutions.
    Example
      R = CC[x,a,t]; -- include the path variable in the ring
      H = { (x^2-1)*a + (x^2-2)*(1-a)};
      sol1 = point {{1}};
      sol2 = point {{-1}};
      S1= { sol1, sol2  };--solutions to H when t=1
      S0 = bertiniUserHomotopy (t,{a=>t}, H, S1) --solutions to H when t=0
      peek S0_0
    Example
      R=CC[x,y,t,a]; -- include the path variable in the ring
      f1=(x^2-y^2);
      f2=(2*x^2-3*x*y+5*y^2);
      H = { f1*a + f2*(1-a)}; --H is a list of polynomials in x,y,t
      sol1=    point{{1,1}}--{{x,y}} coordinates
      sol2=    point{{ -1,1}}
      S1={sol1,sol2}--solutions to H when t=1
      S0=bertiniUserHomotopy(t,{a=>t}, H, S1, HomVariableGroup=>{x,y}) --solutions to H when t=0
///


doc ///
  Key
    bertiniComponentMemberTest
    (bertiniComponentMemberTest, List, NumericalVariety)
  Headline
    a main method to test whether points lie on a given variety
  Usage
    L = bertiniComponentMemberTest (pts, NV)
  Inputs
    pts:List
      a list of points to test
    NV:NumericalVariety
      a numerical variety to test if points lie on a given irreducible component
  Outputs
    L:List
      entries are lists of witness sets containing the test point
  Description
    Text
      This method checks whether the test points pts lie on NV using {\tt Bertini} by
      (1) writing the witness set information of NV and the test points to temporary files,
      (2) invokes {\tt Bertini}'s solver with option {\tt TRACKTYPE => 3},
      (3) stores output of {\tt Bertini} in temporary file,
      (4) parses and outputs the solutions.
    Example
      R = CC[x,y,z];
      F = {(y^2+x^2+z^2-1)*x,(y^2+x^2+z^2-1)*y};
      NV = bertiniPosDimSolve(F)
      pts = {{0,0,0}} --z-axis
      bertiniComponentMemberTest(pts, NV)
    Text
      In the current implementation, at most one witness set is listed for each test point although the point may lie on more than one component.
///

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
      an integer specifying the number of digits of precision
    F:List
      a list of polynomials (system need not be square)
    l:List
      a list of points to be sharpened
  Outputs
    S:List
      a list of solutions of type Point
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
    Text
      @TO bertiniRefineSols@ will only refine non-singular solutions and does not currently work for homogeneous systems.
///

doc ///
  Key
    bertiniParameterHomotopy
    (bertiniParameterHomotopy,List,List,List)
  Headline
    a main method to perform a parameter homotopy in Bertini
  Usage
    S = bertiniParameterHomotopy(F,P,T)
  Inputs
    F:List
      a list of polynomials
    P:List
      a list of parameter indeterminants
    T:List
      a list of lists of target parameter values
  Outputs
    S:List
      a list whose entries are lists of solutions for each target system
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
      bPH2=bertiniParameterHomotopy( {"x^2-u1"}, {u1},finParamValues,AffVariableGroup=>{x},OutputStyle=>"OutSolutions")
      class bPH1_0_0
      class bPH2_0_0
    Example
      dir1 := temporaryFileName(); -- build a directory to store temporary data
      makeDirectory dir1;
      bPH5=bertiniParameterHomotopy( {"x^2-u1"}, {u1},{{1},{2}},AffVariableGroup=>{x},OutputStyle=>"OutNone",TopDirectory=>dir1)
      B0=importSolutionsFile(dir1,NameSolutionsFile=>"ph_jade_0")
      B1=importSolutionsFile(dir1,NameSolutionsFile=>"ph_jade_1")
///

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
      The user can specify CONFIGS for the file using the BertiniInputConfiguration option.
      The user should specify variable groups with the AffVariableGroup (affine variable group) option or HomVariableGroup (homogenous variable group) option.
      The user should specify the polynomial system they want to solve with the  B'Polynomials option or B'Functions option.
      If B'Polynomials is not used then the user should use the  NamePolynomials option.
    Example
      R=QQ[x1,x2,y]
      theDir = temporaryFileName();
      makeDirectory theDir
      makeB'InputFile(theDir,
	      BertiniInputConfiguration=>{MPType=>2},
     	  AffVariableGroup=>{{x1,x2},{y}},
	      B'Polynomials=>{y*(x1+x2+1)^2+1,x1-x2+1,y-2});
    Example
      R=QQ[x1,x2,y,X]
      makeB'InputFile(theDir,
	      BertiniInputConfiguration=>{MPType=>2},
     	  AffVariableGroup=>{{x1,x2},{y}},
	      NamePolynomials=>{f1,f2,f3},
	      B'Functions=>{
  	     {X,x1+x2+1},
  	     {f1,y*X^2+1},
  	     {f2,x1-x2+1},
  	     {f3,y-2}});
    Example
      R=QQ[x1,x2,y,X]
      makeB'InputFile(theDir,
	       BertiniInputConfiguration=>{MPype=>2},
     	   AffVariableGroup=>{{x1,x2},{y}},
	        B'Polynomials=>{y*X^2+1,x1-x2+1,y-2},
	         B'Functions=>{
	            {X,x1+x2+1}});
    Text
      Variables must begin with a letter (lowercase or capital) and can only
      contain letters, numbers, underscores, and square brackets.
      "jade" should not be used in any expression.
      "I" can only be used to represent the complex number sqrt(-1).
///


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
      writeStartFile(storeBM2Files,coordinatesOfTwoPnts);
///




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
	 B'Polynomials=>{x^2-1,y^3-1});
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
     writeParameterFile(storeBM2Files,{1,2},NameParameterFile=>"final_parameters");
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
       BertiniInputConfiguration=>{{TrackType,1}},
       B'Polynomials=>{"(x^2+y^2+z^2-1)*y"});
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
    	BertiniInputConfiguration=>{{TrackType,1}},    AffVariableGroup=>{x,y,z},    B'Polynomials=>{"z*((x+y+z)^3-1)","z*(y^2-3+z)"}    );
    runBertini(storeBM2Files)
    makeSampleSolutionsFile(storeBM2Files,2,SpecifyComponent=>{1,0})
    makeMembershipFile(storeBM2Files,NameSolutionsFile=>"sample_solutions_file")
    theIM=importIncidenceMatrix(storeBM2Files)

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
      When set to 1, this option indicates that the input system is homogenized and the output should be given in projective space.
    Example
      R = CC[x,y,z];
      f = {(x^2+y^2-z^2)*(z-x),(x^2+y^2-z^2)*(z+y)};
      bertiniPosDimSolve(f,IsProjective=>1);
///;


doc ///
  Key
    "Variable groups"
    AffVariableGroup
    HomVariableGroup
    [bertiniParameterHomotopy, AffVariableGroup]
    [bertiniParameterHomotopy, HomVariableGroup]
    [makeB'InputFile, AffVariableGroup]
    [makeB'InputFile, HomVariableGroup]
    [bertiniZeroDimSolve,HomVariableGroup]
    [bertiniZeroDimSolve,AffVariableGroup]
    [bertiniUserHomotopy,AffVariableGroup]
  Headline
    an option to group variables and use multihomogeneous homotopies
  Description
    Text
      Grouping the variables has Bertini solve zero dimensional systems using multihomogeneous homotopies.
    Example
      R = CC[x,y];
      F1 = {x*y+1,2*x*y+3*x+4*y+5};
      bertiniZeroDimSolve(F1, AffVariableGroup=>{{x},{y}});
      hR =CC[x0,x1,y0,y1]
      F2 = {x1*y1+x0*y0,2*x1*y1+3*x1*y0+4*x0*y1+5*x0*y0};
      bertiniZeroDimSolve(F2,HomVariableGroup=>{{x0,x1},{y0,y1}});

///;

doc ///
  Key
    "Bertini input configuration"
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
    SecurityLevel
    ScreenOut
    OutputLevel
    StepsForIncrease
    MaxNewtonIts
    MaxStepSize
    MaxNumberSteps
    MaxCycleNum
    RegenStartLevel
  Headline
    a configuration option for a Bertini input file
  Description
    Text
      Refer to Appendix E of SIAM Bertini book for full details and list of options.

      MPType: Type of precision (0=double, 1=fixed higher, 2=adaptive).

      PRECISION: Precision, in bits, when used MPType=1. Precision is capitalized here to not conflict with Precision.

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
      bertiniZeroDimSolve(F,BertiniInputConfiguration=>{RandomSeed=>0,TrackTolBeforeEG=>1e-6,FinalTol=>1e-100})
    Text
      (2) Store your frequently used favorites in an OptionTable
      and pass it as the last argument in each function call:
    Example
      opts = new OptionTable from {BertiniInputConfiguration=>{RandomSeed=>0,TrackTolBeforeEG=>1e-6,FinalTol=>1e-100}}
      G = {x^2+y^2-1};
      bertiniPosDimSolve(G,opts)
///;

doc///
 Key
 	 "Bertini input file declarations: random numbers"
   RandomReal
   RandomComplex
   [makeB'InputFile, RandomReal]
   [makeB'InputFile, RandomComplex]
   [bertiniParameterHomotopy,RandomComplex]
   [bertiniParameterHomotopy,RandomReal]
   [bertiniZeroDimSolve,RandomComplex]
   [bertiniZeroDimSolve,RandomReal]
 Headline
   an option which designates symbols/strings/variables that will be set to be a random real number or random complex number
 Description
   Text
     This option should be set to a list of symbols, strings, or variables.
     Elements of this list will be fixed to random real/complex numbers when Bertini is called.
   Example
     R=QQ[x,y,c1,c2]
     makeB'InputFile(storeBM2Files,
	      AffVariableGroup=>{{x,y}},
	      RandomReal=>{c1,c2},--c1=.1212, c2=.4132 may be written to the input file.
	      B'Polynomials=>{x-c1,y-c2});
   Example
     R=QQ[x,y,c1,c2]
     makeB'InputFile(storeBM2Files,
	      AffVariableGroup=>{{x,y}},
	      RandomComplex=>{c1,c2},--c1=.1212+ii*.1344, c2=.4132-ii*.2144 are written to the input file.
	      B'Polynomials=>{x-c1,y-c2});
   Text
     AFTER Bertini is run, the random values are stored in a file named "random_values".

///;



doc///
 Key
   B'Constants
   [makeB'InputFile, B'Constants]
   [bertiniParameterHomotopy,B'Constants]
   [bertiniZeroDimSolve,B'Constants]
 Headline
   an option to designate the constants for a Bertini Input file
 Description
   Text
     This option should be set to a list of options X=>v with X denoting the
     constant as an indeterminant and v as it's value.
   Example
     R=QQ[z,a,b,c]
     makeB'InputFile(storeBM2Files,
	      BertiniInputConfiguration=>{MPType=>2},
	      AffVariableGroup=>{{z}},
	       B'Constants=>{a=>2,b=>3+2*ii,c=>3/2},
	        B'Polynomials=>{a*z^2+b*z+c});
///;


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
    [importIncidenceMatrix,Verbose]
    [importMainDataFile,Verbose]
    [importSolutionsFile,Verbose]
    [runBertini,Verbose]
    [makeSampleSolutionsFile,Verbose]
    [bertiniUserHomotopy,Verbose]
  Headline
    Option to silence additional output
  Usage
    bertiniTrackHomotopyVerbose(...,Verbose=>Boolean)
    bertiniUserHomotopyVerbose(...,Verbose=>Boolean)
    bertiniPosDimSolve(...,Verbose=>Boolean)
    bertiniRefineSols(...,Verbose=>Boolean)
    bertiniSample(...,Verbose=>Boolean)
    bertiniZeroDimSolve(...,Verbose=>Boolean)
    bertiniParameterHomotopy(...,Verbose=>Boolean)
    makeB'InputFile(...,Verbose=>Boolean)
    makeMembershipFile(...,Verbose=>Boolean)
    b'PHGaloisGroup(...,Verbose=>Boolean)
    b'PHMonodromyCollect(...,Verbose=>Boolean)
    importIncidenceMatrix(...,Verbose=>Boolean)
    importMainDataFile(...,Verbose=>Boolean)
    importSliceFile(...,Verbose=>Boolean)
    importSolutionsFile(...,Verbose=>Boolean)
    runBertini(...,Verbose=>Boolean)
  Description
    Text
      Use {\tt Verbose=>false} to silence additional output.
///;



doc ///
 Key
   moveB'File
   (moveB'File,String,String,String)
   [moveB'File,MoveToDirectory]
   [moveB'File,SubFolder]
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
     writeParameterFile(storeBM2Files,{2,3,5,7});
     fileExists(storeBM2Files|"/final_parameters")
     moveB'File(storeBM2Files,"final_parameters","start_parameters")
     fileExists(storeBM2Files|"/final_parameters")
     fileExists(storeBM2Files|"/start_parameters")
     moveB'File(storeBM2Files,"start_parameters","backup",CopyB'File=>true)
     fileExists(storeBM2Files|"/start_parameters")
     fileExists(storeBM2Files|"/backup")
   Text
     The options MoveToDirectory and SubFolder give greater control for where to move the file.
   Example
     Dir1 = temporaryFileName();
     makeDirectory Dir1
     writeParameterFile(storeBM2Files,{2,3,5,7});
     moveB'File(storeBM2Files,"final_parameters","start_parameters",MoveToDirectory=>Dir1)
     fileExists(Dir1|"/start_parameters")
   Example
     makeDirectory (storeBM2Files|"/Dir2")
     writeParameterFile(storeBM2Files,{2,3,5,7});
     moveB'File(storeBM2Files,"final_parameters","start_parameters",SubFolder=>"Dir2")
     fileExists(storeBM2Files|"/Dir2/start_parameters")

///;

doc ///
 Key
   CopyB'File
   [moveB'File, CopyB'File]
 Headline
   an optional argument to specify whether make a copy of the file.
 Description
   Text
     When set to true, a file is copy of the file is made rather than just moved. The default in moveB'File is set to false.
///;


doc ///
 Key
   B'Section
 Headline
   a mutable hash table that gives information about a hyperplane used to slice a numerical variety.
 Description
   Text
     B'Section is a type of mutable hash table. It can be created using makeB'Section.
///;

doc ///
 Key
   B'Slice
 Headline
   a mutable hash table that gives information about a linear space used to slice a numerical variety.
 Description
   Text
     B'Slice is a type of mutable hash table. It can be created using makeB'Slice.
///;

doc ///
 Key
   makeB'Section
   (makeB'Section,List)
   [makeB'Slice,B'Homogenization]
   [makeB'Section,B'NumberCoefficients]
   [makeB'Section,NameB'Section]
   [makeB'Section,B'Homogenization]
   [makeB'Section,ContainsPoint]
   [makeB'Section,RandomCoefficientGenerator]
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
       B'Polynomials=>{f,s1});
     runBertini(storeBM2Files)
     #importSolutionsFile(storeBM2Files)==3

///;


doc ///
 Key
   makeB'Slice
   NameB'Slice
   (makeB'Slice,Thing,List)
   [makeB'Slice,B'NumberCoefficients]
   [makeB'Slice,ContainsMultiProjectivePoint]
   [makeB'Slice,ContainsPoint]
   [makeB'Slice,NameB'Slice]
   [makeB'Slice,RandomCoefficientGenerator]
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
     makeB'InputFile(storeBM2Files,AffVariableGroup=>{x,y,z},B'Functions=>{aSlice},NamePolynomials=>{"f0","f1","f2"});
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
    	 B'Polynomials=>{f1,f2}|xxSlice#ListB'Sections);
     runBertini(storeBM2Files)
     xxDegree=#importSolutionsFile(storeBM2Files)
     makeB'InputFile(storeBM2Files,
    	 HomVariableGroup=>variableGroups,
    	 B'Polynomials=>{f1,f2}|xySlice#ListB'Sections);
     runBertini(storeBM2Files)
     xyDegree=#importSolutionsFile(storeBM2Files)
     makeB'InputFile(storeBM2Files,
    	 HomVariableGroup=>variableGroups,
    	 B'Polynomials=>{f1,f2}|yySlice#ListB'Sections);
     runBertini(storeBM2Files)
     yyDegree=#importSolutionsFile(storeBM2Files)

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

undocumented {
  SetParameterGroup,
ReturnPoints,
B'SectionString,
ContainsMultiProjectivePoint,
ListB'Sections,
PrintMidStatus,
OutputStyle,--TODO remove this option
StorageFolder,
RandomGamma,
SubFolder,
StartParameters,
StartPoints,
OrderPaths,
MultiplicityTol,
ConditionNumTol,
ParameterValues,
NameB'InputFile,--This option allows us to change the name of the input file.
NameParameterFile,
NameSolutionsFile,
NameIncidenceMatrixFile,
NameStartFile,
NameFunctionFile,
--
BertiniInputConfiguration, --This option is a list of pairs of strings. These will be written in the CONFIG part of the Bertini input file.
ParameterGroup,
VariableList,
PathVariable,
B'Polynomials, --a list of polynomials whose zero set we want to solve; when used then the NamePolynomials option is disabled and the polynomials are automatically named jade
NamePolynomials, --A list of names of the polynomials which we want to find the common zero set of.
B'Functions, --A list of list of pairs.
--
runBertini,
InputFileDirectory,
StartFileDirectory,
StartParameterFileDirectory,
B'Exe,
M2Precision,--needs doc
writeParameterFile,
SaveData,
SolutionFileStyle,
--  B'MultiProjectivePoint,
ContainsPoint,
B'NumberCoefficients,
B'Homogenization,
RandomCoefficientGenerator,
NameB'Section,
makeB'TraceInput,
replaceFirstLine,
PreparePH2,
readFile,
NameMainDataFile,
--  linesPerSolutions,
PathNumber,
FinalTValue,
MaxPrecisionUtilized,
PrecisionIncreased,
AccuracyEstInternal,
AccuracyEst,
PathsWithSameEndpoint,
CycleNumber,
FunctionResidual,
Dimension,
SolutionType,
DeflationsNeeded,
--  B'WitnessSet,
SpecifyDim,
NameWitnessSliceFile,
importSliceFile,
TextScripts,
NameWitnessSolutionsFile,
SpecifyComponent,
makeWitnessSetFiles,
makeSampleSolutionsFile,
NameSampleSolutionsFile,
TestSolutions,
makeMembershipFile,
ComponentNumber,
sortMainDataComponents,
MoveToDirectory,
SpecifyVariables,
SubIntoCC
}
end
installPackage("Bertini",RemakeAllDocumentation=>true)

--load "./Bertini/doc.m2";

end
makeWitnessSetFiles = method(TypicalValue => Nothing, Options=>{
	NameWitnessSliceFile=>"linear_slice_file",
    	NameSolutionsFile=>"witness_solutions_file",
	NameB'InputFile=>"input",
	SpecifyComponent=>-2,
	StorageFolder=>null,
	Verbose=>false
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

        --TODO fix this test
        TEST///
        load concatenate(Bertini#"source directory","./Bertini/TST/makeSampleSolutions.tst.m2")
        ///
