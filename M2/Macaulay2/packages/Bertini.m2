needsPackage "NAGtypes"

newPackage(
  "Bertini",
  Version => "0.1", 
  Date => "11 June 2012",
  Authors => {
    {Name => "Dan Bates",
     Email => "bates@math.colostate.edu",
     HomePage => "http://www.math.colostate.edu/~bates"},
    {Name => "Contributing Author: Anton Leykin",
     HomePage => "http://www.math.gatech.edu/~leykin"}
  },
  Headline => "Interface to Bertini",
  Configuration => { 
    "path" => "",
    "BERTINIexe"=>"bertini", 
    "keep files" => true
  },
  DebuggingMode => false,
  AuxiliaryFiles => false,
  CacheExampleOutput => true
)

export { 
  "bertiniSolve",
  "bertiniZeroDimSolve",
  "bertiniParameterHomotopy",
  "bertiniPosDimSolve",
  "bertiniSample",
  "bertiniComponentMemberTest",
  "bertiniRefineSols",
  "StartSystem",  
  "StartSolutions",  
  "gamma",
  "MPTYPE",  
  "PRECISION",  
  "ODEPREDICTOR",  
  "TRACKTOLBEFOREEG",  
  "TRACKTOLDURINGEG",
  "FINALTOL",  
  "MAXNORM",  
  "MINSTEPSIZEBEFOREEG",  
  "MINSTEPSIZEDURINGEG",  
  "IMAGTHRESHOLD",
  "COEFFBOUND",  
  "DEGREEBOUND",  
  "CONDNUMTHRESHOLD",  
  "RANDOMSEED",  
  "SINGVALZEROTOL",
  "ENDGAMENUM", 
  "USEREGENERATION",  
  "SECURITYLEVEL",  
  "SCREENOUT",  
  "OUTPUTLEVEL",
  "STEPSFORINCREASE",  
  "MAXNEWTONITS",  
  "MAXSTEPSIZE",  
  "MAXNUMBERSTEPS",  
  "MAXCYCLENUM",
  "REGENSTARTLEVEL",
  "runType",
  "compnum",
  "dimen",
  "numpts",
  "digits"
--  "points"
}

needsPackage "NAGtypes"

--##########################################################################--
-- GLOBAL VARIABLES 
--##########################################################################--

 DBG = 0; -- debug level (10=keep temp files)
 path'BERTINI = (options Bertini).Configuration#"path";
 BERTINIexe=path'BERTINI|(options Bertini).Configuration#"BERTINIexe"; 

-- QUESTION: do we need to prepend "rootPath" to all the file names to resolve issues with cygwin??? (question from PHCpack interface)

needsPackage "SimpleDoc"

-- Bertini interface for NAG4M2
-- used by ../NumericalAlgebraicGeometry.m2

-- The following six methods are just front ends for various Bertini functions.
-- Each just calls bertiniSolve() with the appropriate input data and toggle (corresp. to the type of run).
-- bertiniSolve then does all the work of building the input file, calling bertini, and calling the appropriate output parser. 

bertiniZeroDimSolve = method(TypicalValue => List, Options=>{MPTYPE=>-1,PRECISION=>-1,ODEPREDICTOR=>-1,TRACKTOLBEFOREEG=>-1,TRACKTOLDURINGEG=>-1,FINALTOL=>-1,MAXNORM=>-1,MINSTEPSIZEBEFOREEG=>-1,MINSTEPSIZEDURINGEG=>-1,IMAGTHRESHOLD=>-1,COEFFBOUND=>-1,DEGREEBOUND=>-1,CONDNUMTHRESHOLD=>-1,RANDOMSEED=>-1,SINGVALZEROTOL=>-1,ENDGAMENUM=>-1,USEREGENERATION=>-1,SECURITYLEVEL=>-1,SCREENOUT=>-1,OUTPUTLEVEL=>-1,STEPSFORINCREASE=>-1,MAXNEWTONITS=>-1,MAXSTEPSIZE=>-1,MAXNUMBERSTEPS=>-1,MAXCYCLENUM=>-1,REGENSTARTLEVEL=>-1})
bertiniZeroDimSolve List := o -> F -> (  
--F is the list of polynomials.
         L := {runType=>0};
         o2 := new OptionTable from L;
         o3 := o ++ o2;
         bertiniSolve(F,o3)
         ) 


bertiniParameterHomotopy = method(TypicalValue => List, Options=>{StartSystem=>{},StartSolutions=>{},gamma=>1.0+ii,MPTYPE=>-1,PRECISION=>-1,ODEPREDICTOR=>-1,TRACKTOLBEFOREEG=>-1,TRACKTOLDURINGEG=>-1,FINALTOL=>-1,MAXNORM=>-1,MINSTEPSIZEBEFOREEG=>-1,MINSTEPSIZEDURINGEG=>-1,IMAGTHRESHOLD=>-1,COEFFBOUND=>-1,DEGREEBOUND=>-1,CONDNUMTHRESHOLD=>-1,RANDOMSEED=>-1,SINGVALZEROTOL=>-1,ENDGAMENUM=>-1,USEREGENERATION=>-1,SECURITYLEVEL=>-1,SCREENOUT=>-1,OUTPUTLEVEL=>-1,STEPSFORINCREASE=>-1,MAXNEWTONITS=>-1,MAXSTEPSIZE=>-1,MAXNUMBERSTEPS=>-1,MAXCYCLENUM=>-1,REGENSTARTLEVEL=>-1})
bertiniParameterHomotopy List := o -> F -> (
--F is the list of polynomials
         L := {runType=>1};
         o2 := new OptionTable from L;
         o3 := o ++ o2;
         bertiniSolve(F,o3)
         )

--  The following will NOT be funcitonal until we have (and include as output) a numerical irreducible decomposition data type. 
bertiniPosDimSolve = method(TypicalValue => List, Options=>{MPTYPE=>-1,PRECISION=>-1,ODEPREDICTOR=>-1,TRACKTOLBEFOREEG=>-1,TRACKTOLDURINGEG=>-1,FINALTOL=>-1,MAXNORM=>-1,MINSTEPSIZEBEFOREEG=>-1,MINSTEPSIZEDURINGEG=>-1,IMAGTHRESHOLD=>-1,COEFFBOUND=>-1,DEGREEBOUND=>-1,CONDNUMTHRESHOLD=>-1,RANDOMSEED=>-1,SINGVALZEROTOL=>-1,ENDGAMENUM=>-1,USEREGENERATION=>-1,SECURITYLEVEL=>-1,SCREENOUT=>-1,OUTPUTLEVEL=>-1,STEPSFORINCREASE=>-1,MAXNEWTONITS=>-1,MAXSTEPSIZE=>-1,MAXNUMBERSTEPS=>-1,MAXCYCLENUM=>-1,REGENSTARTLEVEL=>-1})
bertiniPosDimSolve List := o -> F -> (  
--F is the list of polynomials
         L := {runType=>2};
         o2 := new OptionTable from L;
         o3 := o ++ o2;
         bertiniSolve(F,o3)
         ) 

bertiniSample = method(TypicalValue => List, Options=>{MPTYPE=>-1,PRECISION=>-1,ODEPREDICTOR=>-1,TRACKTOLBEFOREEG=>-1,TRACKTOLDURINGEG=>-1,FINALTOL=>-1,MAXNORM=>-1,MINSTEPSIZEBEFOREEG=>-1,MINSTEPSIZEDURINGEG=>-1,IMAGTHRESHOLD=>-1,COEFFBOUND=>-1,DEGREEBOUND=>-1,CONDNUMTHRESHOLD=>-1,RANDOMSEED=>-1,SINGVALZEROTOL=>-1,ENDGAMENUM=>-1,USEREGENERATION=>-1,SECURITYLEVEL=>-1,SCREENOUT=>-1,OUTPUTLEVEL=>-1,STEPSFORINCREASE=>-1,MAXNEWTONITS=>-1,MAXSTEPSIZE=>-1,MAXNUMBERSTEPS=>-1,MAXCYCLENUM=>-1,REGENSTARTLEVEL=>-1,dimen=>-1,compnum=>-1,numpts=>-1})
bertiniSample List := o -> F -> (  
--F is the list of polynomials
         L := {runType=>3};
         o2 := new OptionTable from L;
         o3 := o ++ o2;
         bertiniSolve(F,o3)
         ) 

--  The following will NOT be functional until we have (and include in the arg list) a numerical irreducible decomposition data type.
bertiniComponentMemberTest = method(TypicalValue => List, Options=>{MPTYPE=>-1,PRECISION=>-1,ODEPREDICTOR=>-1,TRACKTOLBEFOREEG=>-1,TRACKTOLDURINGEG=>-1,FINALTOL=>-1,MAXNORM=>-1,MINSTEPSIZEBEFOREEG=>-1,MINSTEPSIZEDURINGEG=>-1,IMAGTHRESHOLD=>-1,COEFFBOUND=>-1,DEGREEBOUND=>-1,CONDNUMTHRESHOLD=>-1,RANDOMSEED=>-1,SINGVALZEROTOL=>-1,ENDGAMENUM=>-1,USEREGENERATION=>-1,SECURITYLEVEL=>-1,SCREENOUT=>-1,OUTPUTLEVEL=>-1,STEPSFORINCREASE=>-1,MAXNEWTONITS=>-1,MAXSTEPSIZE=>-1,MAXNUMBERSTEPS=>-1,MAXCYCLENUM=>-1,REGENSTARTLEVEL=>-1,Points=>{}})
bertiniComponentMemberTest List := o -> F -> (  
--F is the list of polynomials.
         L := {runType=>4};
         o2 := new OptionTable from L;
         o3 := o ++ o2;
         bertiniSolve(F,o3)
         ) 

bertiniRefineSols = method(TypicalValue => List, Options=>{MPTYPE=>-1,PRECISION=>-1,ODEPREDICTOR=>-1,TRACKTOLBEFOREEG=>-1,TRACKTOLDURINGEG=>-1,FINALTOL=>-1,MAXNORM=>-1,MINSTEPSIZEBEFOREEG=>-1,MINSTEPSIZEDURINGEG=>-1,IMAGTHRESHOLD=>-1,COEFFBOUND=>-1,DEGREEBOUND=>-1,CONDNUMTHRESHOLD=>-1,RANDOMSEED=>-1,SINGVALZEROTOL=>-1,ENDGAMENUM=>-1,USEREGENERATION=>-1,SECURITYLEVEL=>-1,SCREENOUT=>-1,OUTPUTLEVEL=>-1,STEPSFORINCREASE=>-1,MAXNEWTONITS=>-1,MAXSTEPSIZE=>-1,MAXNUMBERSTEPS=>-1,MAXCYCLENUM=>-1,REGENSTARTLEVEL=>-1,Points=>{},digits=>-1})
bertiniRefineSols List := o -> F -> ( 
--F is the list of polynomials.
         L := {runType=>5};
         o2 := new OptionTable from L;
         o3 := o ++ o2;
         bertiniSolve(F,o3)
         ) 


---------------------------------------------------
-- bertiniSolve: This is the main control function:
---------------------------------------------------

bertiniSolve = method(TypicalValue => List, Options=>{StartSystem=>{},StartSolutions=>{},gamma=>1.0+ii,MPTYPE=>-1,PRECISION=>-1,ODEPREDICTOR=>-1,TRACKTOLBEFOREEG=>-1,TRACKTOLDURINGEG=>-1,FINALTOL=>-1,MAXNORM=>-1,MINSTEPSIZEBEFOREEG=>-1,MINSTEPSIZEDURINGEG=>-1,IMAGTHRESHOLD=>-1,COEFFBOUND=>-1,DEGREEBOUND=>-1,CONDNUMTHRESHOLD=>-1,RANDOMSEED=>-1,SINGVALZEROTOL=>-1,ENDGAMENUM=>-1,USEREGENERATION=>-1,SECURITYLEVEL=>-1,SCREENOUT=>-1,OUTPUTLEVEL=>-1,STEPSFORINCREASE=>-1,MAXNEWTONITS=>-1,MAXSTEPSIZE=>-1,MAXNUMBERSTEPS=>-1,MAXCYCLENUM=>-1,REGENSTARTLEVEL=>-1,dimen=>-1,compnum=>-1,numpts=>-1,Points=>{},digits=>-1,runType=>0})

bertiniSolve List := o -> F -> (  -- F is the list of polynomials
  	  dir := makeBertiniInput(F,o);   -- creates the input file
          stdio << "The version of Bertini you have installed on your computer was used for this run. \nBertini is under ongoing development by D. Bates, J. Hauenstein, A. Sommese, and C. Wampler.\n\n";
          if o.runType == 0 then ( -- ZeroDim 
    	    run("cd "|dir|"; "|BERTINIexe|" >bertini_session.log");  -- runs Bertini, storing screen output to bertini_session.log
            );
          if o.runType == 1 then ( -- param homotopy
    	    run("cd "|dir|"; "|BERTINIexe|" >bertini_session.log");  -- runs Bertini, storing screen output to bertini_session.log
            );
          if o.runType == 2 then ( -- PosDim 
    	    run("cd "|dir|"; "|BERTINIexe|" >bertini_session.log");  -- runs Bertini, storing screen output to bertini_session.log
            );
          if o.runType == 3 then ( -- Sample 
            stdio << "Run terminated.  Sampling will be available via this M2/Bertini interface once witness sets are handled correctly in the interface.\n\n"
    	    --run("cd "|dir|"; "|BERTINIexe|" < sample_script >bertini_session.log");  -- runs Bertini, storing screen output to bertini_session.log
            );
          if o.runType == 4 then ( -- Membership  
            stdio << "Run terminated.  Component membership will be available via this M2/Bertini interface once witness sets are handled correctly in the interface.\n"
    	    --run("cd "|dir|"; "|BERTINIexe|" >bertini_session.log");  -- runs Bertini, storing screen output to bertini_session.log
            );
          if o.runType == 5 then ( -- Refine/Sharpen 
    	    run("cd "|dir|"; "|BERTINIexe|" < sharpen_script >bertini_session.log");  -- runs Bertini, storing screen output to bertini_session.log
            );
          readSolutionsBertini(dir,F,o) -- o contains runType, so we can switch inside readSolutionsBertini
          )



-- ISSUES TO CONSIDER:
--   how to handle multiple variable groups (building input AND reading output)???
--   return homogeneous coords or nonhom -- probably need to determine what sort of input user has provided!!!???


-------------------
-- makeBertiniInput
-------------------

makeBertiniInput = method(TypicalValue=>Nothing, Options=>{StartSystem=>{},StartSolutions=>{},gamma=>1.0+ii,MPTYPE=>-1,PRECISION=>-1,ODEPREDICTOR=>-1,TRACKTOLBEFOREEG=>-1,TRACKTOLDURINGEG=>-1,FINALTOL=>-1,MAXNORM=>-1,MINSTEPSIZEBEFOREEG=>-1,MINSTEPSIZEDURINGEG=>-1,IMAGTHRESHOLD=>-1,COEFFBOUND=>-1,DEGREEBOUND=>-1,CONDNUMTHRESHOLD=>-1,RANDOMSEED=>-1,SINGVALZEROTOL=>-1,ENDGAMENUM=>-1,USEREGENERATION=>-1,SECURITYLEVEL=>-1,SCREENOUT=>-1,OUTPUTLEVEL=>-1,STEPSFORINCREASE=>-1,MAXNEWTONITS=>-1,MAXSTEPSIZE=>-1,MAXNUMBERSTEPS=>-1,MAXCYCLENUM=>-1,REGENSTARTLEVEL=>-1,dimen=>-1,compnum=>-1,numpts=>-1,Points=>{},digits=>-1,runType=>0})  
makeBertiniInput List := o -> T -> ( -- T=polynomials of target system

  v := gens ring T#0; -- variables
  dir := temporaryFileName(); -- build a directory to store temporary data 
  makeDirectory dir; 
  f := openOut (dir|"/input"); -- typical (but not only possible) name for Bertini's input file 

  -- The following block is the config section of the input file 
  f << "CONFIG\n\n"; -- starting the config section of the input file 

  -- for each user-provided option, we write the appropriate config to the file IF it's not just the (meaningless) default:
  if o.MPTYPE =!= -1 then
    f << "MPTYPE: " << o.MPTYPE << ";\n";
  if o.PRECISION =!= -1 then
    f << "PRECISION: " << o.PRECISION << ";\n";
  if o.ODEPREDICTOR =!= -1 then
    f << "ODEPREDICTOR: " << o.ODEPREDICTOR << ";\n";
  if o.TRACKTOLBEFOREEG =!= -1 then
    f << "TRACKTOLBEFOREEG: " << o.TRACKTOLBEFOREEG << ";\n";
  if o.TRACKTOLDURINGEG =!= -1 then
    f << "TRACKTOLDURINGEG: " << o.TRACKTOLDURINGEG << ";\n";
  if o.FINALTOL =!= -1 then
    f << "FINALTOL: " << o.FINALTOL << ";\n";
  if o.MAXNORM =!= -1 then
    f << "MAXNORM: " << o.MAXNORM << ";\n";
  if o.MINSTEPSIZEBEFOREEG =!= -1 then
    f << "MINSTEPSIZEBEFOREEG: " << o.MINSTEPSIZEBEFOREEG << ";\n";
  if o.MINSTEPSIZEDURINGEG =!= -1 then
    f << "MINSTEPSIZEDURINGEG: " << o.MINSTEPSIZEDURINGEG << ";\n";
  if o.IMAGTHRESHOLD =!= -1 then
    f << "IMAGTHRESHOLD: " << o.IMAGTHRESHOLD << ";\n";
  if o.COEFFBOUND =!= -1 then
    f << "COEFFBOUND: " << o.COEFFBOUND << ";\n";
  if o.DEGREEBOUND =!= -1 then
    f << "DEGREEBOUND: " << o.DEGREEBOUND << ";\n";
  if o.CONDNUMTHRESHOLD =!= -1 then
    f << "CONDNUMTHRESHOLD: " << o.CONDNUMTHRESHOLD << ";\n";
  if o.RANDOMSEED =!= -1 then
    f << "RANDOMSEED: " << o.RANDOMSEED << ";\n";
  if o.SINGVALZEROTOL =!= -1 then
    f << "SINGVALZEROTOL: " << o.SINGVALZEROTOL << ";\n";
  if o.ENDGAMENUM =!= -1 then
    f << "ENDGAMENUM: " << o.ENDGAMENUM << ";\n";
  if o.USEREGENERATION =!= -1 then
    f << "USEREGENERATION: " << o.USEREGENERATION << ";\n";
  if o.SECURITYLEVEL =!= -1 then
    f << "SECURITYLEVEL: " << o.SECURITYLEVEL << ";\n";
  if o.SCREENOUT =!= -1 then
    f << "SCREENOUT: " << o.SCREENOUT << ";\n";
  if o.OUTPUTLEVEL =!= -1 then
    f << "OUTPUTLEVEL: " << o.OUTPUTLEVEL << ";\n";
  if o.STEPSFORINCREASE =!= -1 then
    f << "STEPSFORINCREASE: " << o.STEPSFORINCREASE << ";\n";
  if o.MAXNEWTONITS =!= -1 then
    f << "MAXNEWTONITS: " << o.MAXNEWTONITS << ";\n";
  if o.MAXSTEPSIZE =!= -1 then
    f << "MAXSTEPSIZE: " << o.MAXSTEPSIZE << ";\n";
  if o.MAXNUMBERSTEPS =!= -1 then
    f << "MAXNUMBERSTEPS: " << o.MAXNUMBERSTEPS << ";\n";
  if o.MAXCYCLENUM =!= -1 then
    f << "MAXCYCLENUM: " << o.MAXCYCLENUM << ";\n";
  if o.REGENSTARTLEVEL =!= -1 then
    f << "REGENSTARTLEVEL: " << o.REGENSTARTLEVEL << ";\n";

  -- now we handle the various runType options:
  if o.runType == 1 then --param run -- startSystem and startSolutions should be nonempty...add sanity check???
    f << "USERHOMOTOPY: 1;\n";
  if o.runType == 2 then --pos dim run
    f << "TRACKTYPE: 1;\n";
  if o.runType == 3 then --sample component -- need dim and compnum from user, along with witness_data file (in file or data type???), then need to create short script to handle interactive session 
    f << "TRACKTYPE: 2;\n";
  if o.runType == 4 then --membership test -- need to create file from points (o.Points should be nonempty!)
    f << "TRACKTYPE: 3;\n";
  if o.runType == 5 then --refine solutions -- need to create file from points (o.Points should be nonempty, and digits should be specified by user)
    f << "SHARPENONLY: 1;\n";
 
  f << endl << "END;\n\n";  -- end of config section

  -- The following block is the input section of the input file
  f << "INPUT" << endl << endl;
  if o.runType==1 then  -- if user-defined, declaration type of vars is "variable"
    f << "variable "
  else f << "variable_group "; -- if not user-defined, dec type of vars if "variable_group"
  scan(#v, i->  -- now we list the variables in a single list  ...  What about an mhom structure???
       if i<#v-1 
       then f << toString v#i << ", "
       else f << toString v#i << ";" << endl
       );
  f << "function "; -- "function" section
  scan(#T, i-> -- here are the function names
       if i<#T-1
       then f << "f" << i << ", "
       else f << "f" << i << ";" << endl << endl
      );
  bertiniNumbers := p->if class p === CC 
  then toString realPart p | "+" | toString imaginaryPart p | "*I"
  else ( L := toExternalString p; 
       L = replace("p"|toString precision p, "", L);
       L = replace("\\bii\\b", "I", L); 
       L = replace("([0-9])e([0-9])", "\\1E\\2", L);
       L
       );
--  bertiniNumbers := p->( L := toString p; -- bertiniNumbers is a method that takes in "p" (list of polynomials) and returns them with ii replaced with I, e replaced with E (don't know why the latter)???
--       L = replace("ii", "I", L); 
--       L = replace("([0-9])e([0-9])", "\\1E\\2", L);
--       L = replace("e", "E", L);
--       L
--       );
  if (o.runType!=1) -- non-param runs: just write out the polynomials
    then scan(#T, i -> f << "f" << i << " = " << bertiniNumbers T#i << ";" << endl) 
  else (  -- param runs: write out polys AND other junk (see next several lines!)
       if #o.StartSystem != #T then error "expected equal number of equations in start and target systems";
       f << "pathvariable t;\n" 
         << "parameter s;\n"
         << "s = t;\n\n";  -- need to make gamma a random number here !!!???
       scan(#T, i -> f << "f" << i 
	    << " = (" << bertiniNumbers T#i << ")*(1-s)+s*("<< bertiniNumbers o.gamma << ")*(" << bertiniNumbers o.StartSystem#i << ");" << endl 
	   );
       );
  f << endl << "END;" << endl << endl;
  close f;

  --Now we build auxilary files for various sorts of runs:
  if (o.runType==1) then ( -- writing out start file in the case of a param run
       f = openOut (dir|"/start"); -- the only name for Bertini's start solutions file 
       f << #o.StartSolutions << endl << endl;
       scan(o.StartSolutions, s->(
		 scan(s, c-> f << realPart c << " " << imaginaryPart c << ";" << endl );
		 f << endl;
		 ));
       close f;
       );

  if (o.runType==4) then ( -- writing out file with points in the case of a membership run
       f = openOut (dir|"/member_points"); -- the only name for Bertini's membership points file 
       f << #o.StartSolutions << endl << endl;
       scan(o.StartSolutions, s->(
		 scan(s, c-> f << realPart c << " " << imaginaryPart c << ";" << endl );
		 f << endl;
		 ));
       close f;
       );


  if (o.runType==5) then ( -- writing out file with points in the case of a refine run
       f = openOut (dir|"/points"); -- the only name for Bertini's sharpening points file 
       f << #o.StartSolutions << endl << endl;
       scan(o.StartSolutions, s->(
		 scan(s, c-> f << realPart c << " " << imaginaryPart c << ";" << endl );
		 f << endl;
		 ));
       close f;

       f = openOut (dir|"/sharpen_script"); -- writing out file with query responses in case of a refine/sharpen run
       f << "5" << endl << o.digits << endl << "2" << endl << "points" << endl;
       close f;
       );

  if (o.runType==3) then ( -- writing out file with query responses in case of a sample run
       f = openOut(dir|"/sample_script");
       f << o.dimen << endl << o.compnum << endl << o.numpts << endl << "0" << endl << "sample_points" << endl;  -- sampled points will be written file named sample_points
       close f;
       );

  stdio << "Temporary directory for input and output files:" << dir << endl << endl;
  dir
  )

cleanupOutput = method(TypicalValue=>String)
cleanupOutput String := s -> (
-- cleanup output (Bertini and hom4ps2)
  t := replace("E", "e", s);
  t = replace("[(,)]","", t);
  t = replace("e\\+","e",t)
  )


-----------------------
-- readSolutionsBertini
-----------------------


readSolutionsBertini = method(TypicalValue=>NumericalVariety, Options=>{StartSystem=>{},StartSolutions=>{},gamma=>1.0+ii,MPTYPE=>-1,PRECISION=>-1,ODEPREDICTOR=>-1,TRACKTOLBEFOREEG=>-1,TRACKTOLDURINGEG=>-1,FINALTOL=>-1,MAXNORM=>-1,MINSTEPSIZEBEFOREEG=>-1,MINSTEPSIZEDURINGEG=>-1,IMAGTHRESHOLD=>-1,COEFFBOUND=>-1,DEGREEBOUND=>-1,CONDNUMTHRESHOLD=>-1,RANDOMSEED=>-1,SINGVALZEROTOL=>-1,ENDGAMENUM=>-1,USEREGENERATION=>-1,SECURITYLEVEL=>-1,SCREENOUT=>-1,OUTPUTLEVEL=>-1,STEPSFORINCREASE=>-1,MAXNEWTONITS=>-1,MAXSTEPSIZE=>-1,MAXNUMBERSTEPS=>-1,MAXCYCLENUM=>-1,REGENSTARTLEVEL=>-1,dimen=>-1,compnum=>-1,numpts=>-1,Points=>{},digits=>-1,runType=>0})

readSolutionsBertini (String,List) := o -> (dir,F) -> (  -- dir=directory holding the output files, options are same as bertiniSolve

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
  var's := gens ring F#0; -- variables
  R = QQ[var's]; --setting the ring

  if (o.runType == 0) then ( --raw_data, for zeroDim 
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
--    NOTE:  # paths ending at same point is NOT reported in this file -- needs to be computed...only available in human-readable main_data!!! 
--  -1 (at end of blocks)
--  junk at end is the matrix of patch coefficients -- MPTYPE on first line, then number or rows & columns on second, then the coeffs

       l := lines get (dir|"/raw_data"); -- grabs all lines of the file
       numVars = value(first l);
       l = drop(l,2);
       solNum = value(first l);
       l = drop(l,1);
       --Now we go through all blocks of solutions (each block contains the coordinates of the solution and a bunch of other stuff.
       --stdio << "Solutions:" << endl << endl;

       wList := {}; --list of witness sets
       pts:={}; 

       while solNum > -1 do ( -- -1 in solNum position (top of solution block) is key to end of solutions.
            maxPrec := value(first l);
            l = drop(l,1);

            coords = {};
            for j from 1 to numVars do ( -- grab each coordinate
              coord = select("[0-9.+-]+e[0-9+-]+", cleanupOutput(first l));  -- use regexp to get the two numbers from the string
              coords = join(coords, {toCC(53, value(coord#0),value(coord#1))});  -- NOTE: we convert to a 53 bit floating point complex type -- beware that we might be losing data here!!!???
              l = drop(l,1);
              --print coords; --remove me
	      );
	 
	
	 
    
            -- now we dehomogenize, assuming the first variable is the hom coord:
            dehomCoords = {};
            for j from 1 to numVars-1 do (
	      dehomCoords = join(dehomCoords, {coords#j / coords#0});
              );

            --print dehomCoords; --remove me

            funcResid = value(cleanupOutput(first l)); l=drop(l,1);
            condNum = value(cleanupOutput(first l)); l=drop(l,1);
            newtonResid = value(cleanupOutput(first l)); l=drop(l,1);
            lastT = value(cleanupOutput(first l)); l=drop(l,3);
            cycleNum = value(first l); l=drop(l,1);
            success = value(first l); l=drop(l,1);
            solNum = value(first l);
            l = drop(l,1); 

            pt = new Point;
            pt.Coordinates = dehomCoords; --we want to output these
            --N = map(CC^0,CC^4,0);
            --ws = witnessSet(ideal F, N, {pt});
	    --wList = join(wList, {ws});
	    pts=join(pts,{pt})
            );
       pts
         --nv = numericalVariety wList;
       )


  else if (o.runType == 2) then ( --if PosDim, we read in the output from witness_data : -1 for now for development purposes!!!??? 
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
       listOfCodims = {};  --keeps track of codimension of each witness set; needed since we add slice data later.

       for codimNum from 1 to numCodims do (

         -- WARNING!!!!!????? 
         -- For now, this data is overwritten in each pass through the codimNum loop: need to store codim's worth of data at end of loop (later, when witness data type is stabilized/understood)

         pts := {};  --for each codim, we store all points and all codims (next line), then sort after gathering all points in the codim
         compNums := {};
         maxCompNum := 0;  --keeps track of max component number in this codim

         codimen = value(first l); l=drop(l,1);
         ptsInCodim = value(first l); l=drop(l,1);

         for ptNum from 1 to ptsInCodim do (
            maxPrec := value(first l);
            l = drop(l,1);
            coords = {};
            for j from 1 to numVars do ( -- grab each coordinate
              coord = select("[0-9.+-]+e[0-9+-]+", cleanupOutput(first l));  -- use regexp to get the two numbers from the string
              coords = join(coords, {toCC(53, value(coord#0),value(coord#1))});  -- NOTE: we convert to a 53 bit floating point complex type -- beware that we might be losing data here!!!???
              l = drop(l,1);
              );

            l = drop(l,numVars+1);  -- don't need second copy of point or extra copy of maxPrec

            -- now we dehomogenize, assuming the first variable is the hom coord:
            dehomCoords = {};
            for j from 1 to numVars-1 do (
              dehomCoords = join(dehomCoords, {coords#j / coords#0});
              );

--print dehomCoords;


            
	    condNum = value(cleanupOutput(first l)); l=drop(l,4);
            ptType = value(first l); l=drop(l,1);
            ptMult = value(first l); l=drop(l,1);
            compNum = value(first l); l=drop(l,1);
            numDeflations = value(first l); l=drop(l,1);
--print(codimNum, ptNum, compNum);

            pt = new Point;
            pt.Coordinates = dehomCoords;
            pts = join(pts,{pt});
            compNums = join(compNums,{compNum});
--print compNums;
            if (compNum > maxCompNum) then maxCompNum=compNum; 
          ); 
	 
	  for j from 0 to maxCompNum do ( --loop through the component numbers in this codim to break them into witness sets
 	    ptsInWS := {}; --stores all points in the same witness set
            for k from 0 to #pts-1 do (
	      --print (j,k); 
	      if (compNums#k == j) then ptsInWS = join(ptsInWS,{pts#k}); --save the point if its in the current component (component j)
	    );
            N = map(CC^0,CC^4,0);
            ws = witnessSet(ideal F,N, ptsInWS); --turn these points into a witness set
            wList = join(wList, {ws}); --add witness set to list
            listOfCodims = join(listOfCodims, {codimNum});
          );
        );

        -- now we grab the slice data, at the end of the witness_data file, to be inserted into the witnessSets with dim>0
	l = drop(l,3); -- -1, blank line, MPTYPE
        randDims = select("[0-9]+", first l);  -- grabs #rows, #cols for the matrix used to randomize the system 
	l = drop(l,1);
        numRands = value(randDims#0) * value(randDims#1);  -- numRands is the number of random numbers we want to skip next
        l = drop(l,numRands+1);   -- includes blank line after rands
	-- next we have the same number of integers (degrees needed ot keep homogenization right)
        l = drop(l,numRands);
	-- next we have an integer and a list of row vectors (the number of which is the initial integer).  Again related to homogenization.
 	numToSkip = select("[0-9]+", first l);
        l = drop(l,value(numToSkip#0)+3); -- dropping all those, plus line containing integer (before), then blank line, and one more line
	--finally, we have the number of linears and the number of coefficients per linear
        linCoeffDims = select("[0-9]+", first l);
        l = drop(l,1);

        --now we just read in the matrix
        numLinCoeffs = value(linCoeffDims#0) * value(linCoeffDims#1);
        rw = {};
        mat = {};
        for i from 1 to value(linCoeffDims#1) do 
        { 
          for j from 1 to value(linCoeffDims#0) do 
          {
            coefParts = select("[0-9]+/[0-9]+", first l);
            rw = join(rw, {toCC(53,value(coefParts#0)) + ii*toCC(53,value(coefParts#1))});  -- definitely losing data here, going from rational number to float!!
            l = drop(l,1);
          };
          mat = join(mat, {rw});  
          rw = {};
        };
        M = matrix(mat);
        --print M;

        -- Finally, we can cycle through the witness sets in nv and add the slice data.
        -- There are length listOfCodims witness sets, the first of which uses the full set of slices (all of M).
        -- The higher codimensions need higher-dimensional hyperplane sections, so fewer slices (part of M).
        -- The lowest slice is kept longest.  Ex:  If there is a codim 1 set with a 2x4 matrix of slice data, a subsequent codim 2 set would have a 
        -- 1x4 matrix of slice data consists of the second (not first) line of the codim 1 slice data.
        for codimNum from 0 to length listOfCodims - 1 do
        {
          coeffList := {};  --We store the cols of M needed for this particular codimNum in coeffList, then turn it into a matrix and store it the witness set.
          colsToSkip = listOfCodims#codimNum - listOfCodims#0;  
          -- print numgens target M; -- gives the number of rows
          -- print numgens source M; -- gives the number of cols
          for i from colsToSkip to numgens source M - 1 do
          {
            coeffCol := {};
	    for j from 0 to numgens target M - 1 do
            { 
              coeffCol = join(coeffCol, {M_(j,i)});
            }; 
            coeffList = join(coeffList, {coeffCol});
          };
--print coeffList;
          if (#coeffList > 0) then
            N = matrix(coeffList)
          else
            N = map(CC^0,CC^4,0); 
          (wList#codimNum).Slice = N;
 	  
        };

        nv = numericalVariety wList;

      )




--  else if (o.runType == -1) then ( --if ZeroDim, we actually want to read raw_data, but this works until that is fully implemented. 
--       l = lines get (dir|"/finite_solutions");
--       nsols := value first separate(" ", l#0);
--       l = drop(l,2);
--       while #s < nsols do (	 
--	    coords = {};
--	    while #(first l) > 2 do ( 
--	      	 coords = coords | {(
--		   	   a := separate(" ",  cleanupOutput(first l));	 
--		   	   (value a#0)+ii*(value a#1)
--	      	   	   )};
--    	      	 l = drop(l,1);
--	      	 );	
--	    l = drop(l,1);
--            if DBG>=10 then << coords << endl;
--	    s = s | {{coords}};
--	    );	
--       ) 

  else if o.runType == 8 then ( --runType is never 8...this block reads in raw_solutions, which I think won't be so important (???).
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
	    );     

    ) else error "unknown output file";  
--  pts;
  if o.runType==0 then return pts else return nv;
 
  )

--trackBertini = method(TypicalValue => List)
--trackBertini (List,List,List,MutableHashTable) := List => (S,T,solsS,o) -> (
     -- tempdir := temporaryFileName() | "NumericalAlgebraicGeometry-bertini";
     -- mkdir tempdir; 	  
--     dir := makeBertiniInput(T, StartSystem=>S, StartSolutions=>solsS, gamma=>o.gamma);
--     compStartTime := currentTime();      
--     run("cd "|dir|"; "|BERTINIexe|" >bertini_session.log");
--     if DBG>0 then << "Bertini's computation time: " << currentTime()-compStartTime << endl;
--     readSolutionsBertini(dir, "raw_solutions")
--     )



--##########################################################################--
-- DOCUMENTATION
--##########################################################################--

beginDocumentation()

--doc ///
--  Key
--    Bertini
--  Headline
--    software for numerical algebraic geometry
--  Description
--    Text
--      Interfaces the functionality of the software {\tt Bertini}
--      to solve polynomial systems and perform calculations in
--      {\em numerical algebraic geometry}.  The software is available at
--      @HREF"http://www.nd.edu/~sommese/bertini/"@.
--      The site currently provides only executable versions named {\tt bertini} or {\tt bertini.exe} (for Cygwin).
--      The user must have the executable program {\tt phc} available,
--      preferably in the executation path.
--
--      Below is a simple example using the most popular function,
--      a basic zero-dimensional solve with no special options.
--    Example
--      R = CC[x,y]
--      F = {x^2-1,y^2-1}
--      solns = bertiniSolve(F)
--///;

--doc ///
--  Key 
--    bertiniSolve
--    (bertiniSolve,List)
--    (bertiniSolve,List,HashTable)
--  Headline
--    temporarily a method for doing a zero-dimensional Bertini run, soon to be deprecated
--  Usage
--    S = bertiniSolve F
--  Inputs
--    F:List
--      whose entries are polynomials (system need not be square) 
--  Outputs
--    S:List
--      whose entries are the solutions found by Bertini, with junk points removed if system is not square
--  Description
--    Text
--      For now, this function simply builds a Bertini style input file from F and calls Bertini on 
--      this input file.  Solutions are currently pulled from machine readable file finitesolutions
--      and returned as a list.
--    Example
--      R = CC[x,y]
--      F = {x^2-1,y^2-1}
--      S = bertiniSolve F
--///;

-- slices
-- dehom
-- doc

end
