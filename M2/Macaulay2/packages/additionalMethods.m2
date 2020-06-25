
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
		     	Verbose=>false
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


--New method
-*
 importSliceFile=method(TypicalValue=>String,Options=>{
 	NameWitnessSliceFile=>"linear_slice_file",
  	Verbose=>false}    )
 importSliceFile(String) := o->(aString)->(
     if aString_-1=!="/" then aString=aString|"/";
     allInfo:=lines get(aString|o.NameWitnessSliceFile);
     theConstants:={};
     theLinearSystems:={};
     for aLine in allInfo do (
       sepLine:=separate("=",aLine);
       if o.Verbose then print sepLine;
       if o.Verbose then print ( #sepLine);
       if #sepLine==2 then (
 	if #select("const",sepLine_0)==1
 	then theConstants=append(theConstants,{sepLine_0,
 	  replace(";","",replace("I","ii",sepLine_1))});
 	if #select("linear",sepLine_0)==1
 	then theLinearSystems=append(theLinearSystems,{sepLine_0,replace(";","",sepLine_1)})
 	)	);
     return{theConstants,theLinearSystems}
     )
*-



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
		 	Verbose=>false
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
		     if o.Verbose then print (NumPathsToTrack, "paths");
		     for listPV in listOfListOfParameterValues do(
		 	runCount=1+runCount;
		     	if o.Verbose then print ("rc seq",runCount);
		     	writeParameterFile(storeFiles,listPV,NameParameterFile=>"final_parameters");--writes final parameter file
		 	if o.Verbose then print listPV;
		 	runBertini(storeFiles,Verbose=>o.Verbose,
		 	    NameB'InputFile=>o.NameB'InputFile,B'Exe=>o.B'Exe);
		 	if o.Verbose then print "-rc";
		     	if fileExists(storeFiles|o.NameSolutionsFile)===false and o.NameSolutionsFile=!="simple_raw_solutions" then (
		 	  start0pnts:= openOut(storeFiles|"start");
		      	  start0pnts << "0" << endl << endl;
		 	  close start0pnts;
		 	  print ("Warning: No paths in this sequence were successfuly tracked: Run count "|toString(runCount));
		 	  break);
		 	if o.NameSolutionsFile==="simple_raw_solutions" then  (
		 	  simplifyRawSolutions(storeFiles));
		 	if o.NameSolutionsFile==="raw_solutions"
		 	then  error "NameSolutionsFiles should not be set as raw_solutions, instead set as simple_raw_solutions";
		 	if o.SaveData then (
		 	    moveB'File(storeFiles,o.NameSolutionsFile,o.NameSolutionsFile|toString(runCount),CopyB'File=>true);
		 	    moveB'File(storeFiles,"final_parameters","start_parameters"|toString(runCount),CopyB'File=>true)
		 	    );
		 	if o.Verbose then print o.NameSolutionsFile;
		 	if o.Verbose then print ("Number of solutions: "|toString(#importSolutionsFile(storeFiles,NameSolutionsFile=>o.NameSolutionsFile|toString(runCount))));
		 	moveB'File(storeFiles,"final_parameters","start_parameters");
		 	moveB'File(storeFiles,o.NameSolutionsFile,"start");
		     	sfIn=openIn(storeFiles|"start");
		 	NumPathsTracked:=value(read(sfIn,1));
		 	close sfIn;
		 	if NumPathsToTrack=!=NumPathsTracked then print ("Warning: The number of paths tracked in this seqence dropped at iteration "|toString(runCount));
		 	if 0===NumPathsTracked then print ("Warning: There are no paths to track at iteration "|toString(runCount));
		     	if o.Verbose then print "seq";
		 	if o.Verbose then print(importSolutionsFile(storeFiles,NameSolutionsFile=>"start"));
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
		 	Verbose=>false,
		 	PrintMidStatus=>true--Set to false to silence additional output.
		 	})
		 b'PHMonodromyCollect(String) := o ->(IFD)->(
		     IFD=addSlash(IFD);
		     if o.Verbose then print IFD;
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
		     if o.Verbose then print solCollection;
		     breakLoop:=false;
		     if o.SpecifyLoops=!=false then listsOfListsOfParameterValues:=o.SpecifyLoops;
		     while not breakLoop do(
		       if (o.SpecifyLoops===false) then listsOfListsOfParameterValues={for i to 2-1 list for j to #bP-1 list (2*random(CC)-random(CC))};
		       for listsOfParameterValues in listsOfListsOfParameterValues do(
		 	loopCount=loopCount+1;
		 	if o.Verbose or o.PrintMidStatus then print ("Starting monodromy loop number: "|toString(loopCount)|".");
		 --    	print (	    append(listsOfParameterValues,bP));
		         if fileExists(storeFiles|"start")===false then error "start file for b'PHSequence is missing.";
		         if fileExists(storeFiles|o.NameParameterFile)===false then error "start_parameters file for b'PHSequence is missing.";
		     	b'PHSequence(storeFiles,
		 	    append(listsOfParameterValues,bP),
		 	    B'Exe=>o.B'Exe,
		 	    NameSolutionsFile=>o.NameSolutionsFile,---ProblemLine
		        	    NameB'InputFile=>o.NameB'InputFile);
		 --	if o.Verbose then print ".5";
		 --    	if o.Verbose then print (importSolutionsFile(storeFiles,NameSolutionsFile=>"start"));
		 	preSolCollection:=sortSolutions(
		 	    solCollection|importSolutionsFile(storeFiles,NameSolutionsFile=>"start"));
		 --    	if o.Verbose then print "1";
		 --    	if o.Verbose then print preSolCollection;
		 --removing multiplicities
		     	if #preSolCollection>=2 then (
		 	    solCollection={};
		 	    for i to #preSolCollection-2 do
		 	    if (not areEqual(preSolCollection_i,preSolCollection_(i+1),Tolerance=>1e-10))
		 	    then (
		 --    	      print (preSolCollection_i,preSolCollection_(i+1));
		 --	      print areEqual(preSolCollection_i,preSolCollection_(i+1)) ;
		 	      solCollection=append(solCollection,preSolCollection_i));
		 	    solCollection=append(solCollection,preSolCollection_-1));
		     	if #preSolCollection<2 then solCollection=preSolCollection;
		 	if o.Verbose or o.PrintMidStatus then print ("Number of solutions found: "|toString( #solCollection)|"."));
		       writeStartFile(storeFiles,solCollection);
		       if loopCount>=o.NumberOfLoops then (
		 	breakLoop=true;
		 	if o.Verbose or o.PrintMidStatus then print "NumberOfLoops has been reached.");
		       if #solCollection>= o.NumSolBound then (
		 	breakLoop=true;
		 	if o.Verbose then print (#solCollection);
		 	if o.Verbose or o.PrintMidStatus then print ("Number of loops: "|toString loopCount|".");
		 	if o.Verbose or o.PrintMidStatus then print "NumSolBound has been reached.");
		       );
		     return solCollection);

		 bertiniImageMonodromyCollect=method(TypicalValue=>Thing,Options=>{
		 	NameB'InputFile=>"input",
		 	MonodromyStartPoints=>null,--Set this option if the start points come from a list.
		 	NameStartFile=>"start",--Set this option if the start points come from a file.
		 	NameParameterFile=>"start_parameters",
		 	NameSolutionsFile=>"simple_raw_solutions",
		     	StorageFolder=>null,
		 	SaveData=>false,
		 	B'Exe=>BERTINIexe,
		 	MonodromyStartParameters=>null,
		   	NumberOfLoops=>1,
		   	NumSolBound=>infinity,
		 	SpecifyLoops=>null,
		 	Verbose=>false,
		 	AffVariableGroup=>{},
		 	B'Functions=>{},
		 	B'Constants=>{},
		 	PrintMidStatus=>true,--Set to false to silence additional output.
		 	ImageCoordinates=>{},--ImagePolys,
		 	GeneralCoordinate=>null,--This is given by a list of random numbers. The length of the list is the number of image coordinates.
		 	OnlyMoveParameters=>null,--Set to a list, e.g. {0,2} so that the 0th and 2nd parameters are the only ones that move.
		     	EquivalentCoordinates=>((a,b)->(if abs(a-b)<1e-10 then true else false)),
		 	M2Precision=>52,
		 	ReturnPoints=>true,
		 	ContinueLoop=>true
		 	})
		 bertiniImageMonodromyCollect(String) := o ->(IFD)->(
		     IFD=addSlash(IFD);
		     if o.Verbose then print IFD;
		 --Write start_parameter and start files.
		     if o.SpecifyLoops=!=null and o.OnlyMoveParameters=!=null then print("Warning: SpecifyLoops is set and OnlyMoveParameters will be ignored.");
		 --
		     if o.MonodromyStartPoints=!=null and o.NameStartFile=!="start" then print("Warning: MonodromyStartPoints and NameStartFile should not be set at the same time. ");
		     if o.MonodromyStartPoints=!=null then writeStartFile(IFD,o.MonodromyStartPoints);--write a start file.
		 --
		     if o.MonodromyStartParameters=!=null and o.NameParameterFile=!="start_parameters" then print("Warning: MonodromyStartParameters and NameStartParameterFile should not be set at the same time. ");
		     if o.MonodromyStartParameters=!=null then writeParameterFile(IFD,o.MonodromyStartParameters,NameParameterFile=>"start_parameters");--write a start_parameter file.
		     if fileExists(IFD|o.NameB'InputFile)===false then error "input file does not exist in correct directory.";
		     if fileExists(IFD|o.NameStartFile)===false then error "start file does not exist in correct directory or MonodromyStartPoints needs to be set.";
		     if fileExists(IFD|o.NameParameterFile)===false then error "start_parameters file does not exist in correct directory or MonodromyStartParameters needs to be set.";
		 --Move files to storage folder if necessary.
		     if o.StorageFolder=!=null
		     then (
		       storeFiles:=addSlash(IFD|o.StorageFolder);
		       if false===fileExists storeFiles then mkdir storeFiles)
		     else storeFiles=addSlash(IFD);
		 --Copy files to have default names.
		     if o.NameStartFile=!="start" or null=!=o.StorageFolder
		     then moveB'File(IFD,o.NameStartFile,"start",SubFolder=>o.StorageFolder,CopyB'File=>true);
		     if o.NameParameterFile=!="start_parameters" or null=!=o.StorageFolder
		     then moveB'File(IFD,o.NameParameterFile,"start_parameters",SubFolder=>o.StorageFolder,CopyB'File=>true);
		     if null=!=o.StorageFolder
		     then moveB'File(IFD,o.NameB'InputFile,o.NameB'InputFile,SubFolder=>o.StorageFolder,CopyB'File=>true);
		 --
		 --INSERT SHARPEN POINTS and REMOVE DUPLICATES OPTION HERE--
		 --For now assume we are given a good start solution.
		 --Maybe change general coordinate to weighted coordinate.
		     moveB'File(storeFiles,"start","start_Fiber_JADE",CopyB'File=>true);
		 --Import base parameters and fiber.
		     bP:=importParameterFile(storeFiles,NameParameterFile=>"start_parameters");---we pull the base parameters
		     if o.ImageCoordinates==={}     then imagePoly:=flatten (o.AffVariableGroup)    else imagePoly=o.ImageCoordinates;
		     --print imagePoly;
		     numImageCoords:=#imagePoly;
		     numOriginalCoords:=#flatten o.AffVariableGroup;
		 --Set the general coordinate:
		     inputImageMapName:="input_Image_Map_JADE";
		     if o.GeneralCoordinate=!=null then   gcMap:=o.GeneralCoordinate;
		     if o.GeneralCoordinate===null then   (
		       gcMap=for i to numImageCoords-1 list random CC);
		     theWeights:=for i to numImageCoords-1 list "weightJADE"|i=>gcMap_i;
		     makeB'InputFile(storeFiles,
		       NameB'InputFile=>inputImageMapName,      AffVariableGroup=>o.AffVariableGroup,
		       B'Constants=>o.B'Constants|theWeights,      B'Functions=>o.B'Functions,      B'Configs=>{{"SecurityLevel",1},{"Tracktype",-4}},
		       B'Polynomials=>{makeB'Section(imagePoly,B'NumberCoefficients=>theWeights/toList/first)});
		     runBertini(storeFiles,NameB'InputFile=>inputImageMapName);
		     startFiberGenCoord:= flatten importSolutionsFile(storeFiles,NameSolutionsFile=>"function");
		     startImageGC:= startFiberGenCoord;
		     numStartPoints:=#startImageGC;
		     loopCount:=0;
		     continueLoop:=true;
		     breakLoop:=false;
		 --If the loops are not specified by a lists of lists of parameter values then the default is to take 'triangle-loops' given by lists of list of 2 parameter values.
		     loopGenerator:=(aLoopCount)->(
		       if o.SpecifyLoops=!=null
		       then oneLoop:=((o.SpecifyLoops)_aLoopCount);
		       if o.SpecifyLoops===null
		       then oneLoop=for i to 2-1 list for j to #bP-1 list
		 	if o.OnlyMoveParameters===null
		 	then (2*random(CC)-random(CC))
		 	else if  member(j,o.OnlyMoveParameters)
		 	then (2*random(CC)-random(CC)) else bP_j;
		       return oneLoop);
		 --This for loop tracks one round of monodromy.--listsOfParameterValues=pointsOnLoop--listsOfListsOfParameterValues=listOfLoops
		     while not breakLoop do(
		       --print first loopGenerator(loopCount);
		       ------If monodromy-loops are not specified then at each interation of the while loop we will create a new list of loops but only has one element.
		       theCurrentLoop:=loopGenerator(loopCount);
		       loopCount=loopCount+1;
		       if o.Verbose or o.PrintMidStatus then print ("Starting monodromy loop number: "|toString(loopCount)|".");
		 --    	print (	    append(listsOfParameterValues,bP));
		       if fileExists(storeFiles|"start")===false then error "start file for b'PHSequence is missing.";
		       if fileExists(storeFiles|o.NameParameterFile)===false then error "start_parameters file for b'PHSequence is missing.";
		       b'PHSequence(storeFiles,        append(theCurrentLoop,bP),
		 	B'Exe=>o.B'Exe,	NameSolutionsFile=>o.NameSolutionsFile,	NameB'InputFile=>o.NameB'InputFile,	SaveData=>true);
		 --    	print (importSolutionsFile(storeFiles,NameSolutionsFile=>"start"));
		       openStartFileToAppend:=openOutAppend(addSlash(storeFiles)|"start_Fiber_JADE");
		 --    	print get openStartFileToAppend;
		 -----INSERT CONTINUE OPTION.
		       runBertini(storeFiles,NameB'InputFile=>inputImageMapName);
		       startImageGC= flatten importSolutionsFile(storeFiles,NameSolutionsFile=>"function");
		       stringSolutions:= lines get  (addSlash(storeFiles)|"start");
		       numStartPoints=value first stringSolutions;
		       stringSolutions=append(drop(stringSolutions,2),"");
		 --    	print stringSolutions;
		     	--Need a check for the standardization of Bertini solutions file.
		     	--Assume the structure of the file is solution then whiteline where each line of solution is a coordinate.
		       for onePoint to #startImageGC-1 do (
		 	foundNewSolution:=true;
		 	indexStarFiberGenCoord:=0;
		 	while  foundNewSolution and indexStarFiberGenCoord<#startFiberGenCoord do(
		 	  foundNewSolution=not o.EquivalentCoordinates(startImageGC_onePoint,(startFiberGenCoord_indexStarFiberGenCoord));
		 	  indexStarFiberGenCoord=indexStarFiberGenCoord+1	);
		 	if foundNewSolution
		 	then(
		 --append to fiber file
		 	  for c to numOriginalCoords do(openStartFileToAppend <<stringSolutions_c <<endl);
		 	  startFiberGenCoord=append(startFiberGenCoord,startImageGC_onePoint) ;
		 	  numStartPoints=numStartPoints+1
		 	    );
		 --Drop lines
		 	stringSolutions=drop(stringSolutions,numOriginalCoords+1);
		 		);
		         close openStartFileToAppend;
		 	print ("Current fiber size: "|toString numStartPoints);
		 	replaceFirstLine(storeFiles,"start_Fiber_JADE",toString numStartPoints)      ;
		       moveB'File(storeFiles,"start_Fiber_JADE","start",CopyB'File=>true);
		 ----Now we check if we have finished the computation by breaking the while loop.
		       if loopCount>=o.NumberOfLoops then (
		 	breakLoop=true;
		 	if o.Verbose or o.PrintMidStatus then print "NumberOfLoops has been reached.");
		 --      print startFiberGenCoord;
		       if #startFiberGenCoord>= o.NumSolBound then (
		 	breakLoop=true;
		 	if o.Verbose then print (#startFiberGenCoord);
		 	if o.Verbose or o.PrintMidStatus then print ("Number of loops: "|toString loopCount|".");
		 	if o.Verbose or o.PrintMidStatus then print "NumSolBound has been reached.");
		       );
		     if o.ReturnPoints then return importSolutionsFile(storeFiles,NameSolutionsFile=>"start"));


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
		 	Verbose=>false
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
		     if o.Verbose then print 1;
		     if o.NameStartFile=!="start" or null=!=o.StorageFolder
		     then moveB'File(IFD,o.NameStartFile,"start",SubFolder=>o.StorageFolder,CopyB'File=>true);
		     if o.Verbose then print 2;
		     if o.NameParameterFile=!="start_parameters" or null=!=o.StorageFolder
		     then moveB'File(IFD,o.NameParameterFile,"start_parameters",SubFolder=>o.StorageFolder,CopyB'File=>true);
		     if o.Verbose then print 3;
		     if null=!=o.StorageFolder
		     then moveB'File(IFD,o.NameB'InputFile,o.NameB'InputFile,SubFolder=>o.StorageFolder,CopyB'File=>true);
		     if o.Verbose then print "3.1";
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
		     if o.Verbose then print 4;
		     for oneBranchPointT in branchPointsT  do (
		     	--NWSWE
		       if imaginaryPart(oneBranchPointT-basePointT)<0
		       then(if o.Verbose then print "below p";
		 	         loopPointsT={
		 	  oneBranchPointT+ii*theLoopRadius,
		 	  oneBranchPointT-theLoopRadius,
		 	  oneBranchPointT-ii*theLoopRadius,
		 	  oneBranchPointT+theLoopRadius,
		 	  basePointT})
		   ---SENW
		       else if imaginaryPart(oneBranchPointT-basePointT)>0
		       then(if o.Verbose then print "above p";
		 	         loopPointsT={
		 	  oneBranchPointT-ii*theLoopRadius,
		 	  oneBranchPointT+theLoopRadius,
		 	  oneBranchPointT+ii*theLoopRadius,
		 	  oneBranchPointT-theLoopRadius,
		 	  basePointT})
		       else print "An error occurred while creating the loops."      ;
		       if o.Verbose then print loopPointsT;
		       if o.Verbose then   print for i in loopPointsT list {i};
		       if o.Verbose then   print 6;
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
		 	  if o.Verbose then print ("Warning: There was a path tracking failure durin loop "|toString(loopCount))
		 	  );
		         if trackingSucess then(
		           if o.SaveData==true then writeStartFile(storeFiles,permutedSols,NameStartFile=>("newSols"|toString(loopCount)));
		     	  ggGenerator:={};
		 --    	  for i to #solCollection-1 do for j to #permutedSols-1 do if areEqual({solCollection_i_0,solCollection_i_-1},{permutedSols_j_0,permutedSols_j_-1},Tolerance=>1e-10) then ggGenerator=append(ggGenerator,j+1);
		     	  for i to #solCollection-1 do for j to #permutedSols-1 do
		 	    if areEqual( for k to #solCollection_0-1 list solCollection_i_k,
		 		for l to #solCollection_0-1 list permutedSols_j_l,Tolerance=>1e-10) then ggGenerator=append(ggGenerator,j+1);
		     	  if #solCollection=!=#ggGenerator then (
		     	    if o.Verbose then print (#ggGenerator, #solCollection);
		 	    loopFailures=loopFailures+1;
		 	    if o.Verbose then print ("Warning: Monodrompy loop "|loopCount|" found unexpected new solutions or a numerical error involving tolerances occurred.")
		 	    );
		     	  if o.Verbose then print ("gg",new Array from ggGenerator);--check that this works.
		 	  gggFile << toString (new Array from ggGenerator);
		 	  if loopCount=!=#branchPointsT-1 then gggFile << "," << endl;
		 	  loopCount=loopCount+1	)      );
		     print ("There were "|toString loopFailures|" loop failures.");
		     gggFile << "]" << endl;
		     close gggFile;
		     print toString (storeFiles|o.NameGaloisGroupGeneratorFile);
		     if o.ReturnGaloisGroupGeneratorFile===true then return value get (storeFiles|o.NameGaloisGroupGeneratorFile) );


		 convertToGap=(aList)->(new Array from for i in aList list  new Array from toList i+for j in i list 1);
