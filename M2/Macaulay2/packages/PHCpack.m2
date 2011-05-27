needsPackage "NAGtypes"

newPackage(
  "PHCpack",
  Version => "0.9", 
  Date => "23 May 2011",
  Authors => {
    {Name => "Elizabeth Gross",
     Email => "lizgross@math.uic.edu",
     HomePage => "http://www.math.uic.edu/~lizgross"},
    {Name => "Sonja Petrovic", 
     Email => "petrovic@math.uic.edu",
     HomePage => "http://www.math.uic.edu/~petrovic"},
    {Name => "Jan Verschelde", 
     Email => "jan@math.uic.edu",
     HomePage => "http://www.math.uic.edu/~jan"},
    {Name => "Contributing Author: Anton Leykin"}
  },
  Headline => "Interface to PHCpack",
  Configuration => { 
    "path" => "",
    "PHCexe"=>"phc", 
    "keep files" => true
  },
  DebuggingMode => false,
  AuxiliaryFiles => true,
  CacheExampleOutput => true
)

--##########################################################################--
-- UPDATE AND CHANGE LOG:
-- 23 May 2011 (pm)- SP:
-- 	returned comment lines about temporary file names to various methods in the package
-- 	updated headlines in documentation to be consitend and reference phc
-- 23 May 2011 (am)- SP:
--     wrote notes as comments in the cascade method to summarize the one last change 
-- 	we need for version 1.0 of the package!!
-- 19 May 2011 - SP: 
--    documentation updates listed in email. 
--    changed "phc -" executable to PHCexe|" -" where necessary (2 instances)
--    modified mixedVolume so it no longer incorrectly specifies TypicalValue=>Sequence.
--
--  19 May 2011 - JV:
--    documentation is in the file PHCpackDoc.m2, loaded below
--    removed refineSoln, documented refineSolutions with example
--    placed (other, older?) versions of cascade after comments
--    added options stableMV and startSystem to mixedVolume
--    needed new internal function "startSystemFromFile" for startSystem
--    added options gamma and tDegree to trackPaths with documentation
--    added internal method isPointReal and exported method RealFilter
--  20 April 2010. ~SP: draft of package.
--  5 May 2010 - SP: 
--    finished w/ mixed volume; black box; incld. slack variables. 
--  16 May 2010 -EG: 
--    documentation of mixed volume and black box.  
--    Need to include slack variables info and tests.
--  9 June 2010 ~SP: 
--    some housekeeping and clean-up of mixed volume and black box solver;
--    added a few things to documentation, mostly about dimension of the 
--    system and slack vars; 
--    many draft-like things still there. Please keep for now (at least 
--    as comments) for future reference.
--    rational input allowed for the black-box solver;
--    added "convertToPoly": a function that takes a rational system and
--    turns it into laurent (natural environment to M2, acceptable for PHC) .
--  24 June 2010 -EG:
--    added tests for phcSolve() and mixedVolume()
--  ? August 2010 ~Anton visited UIC and transfered over a few functions; 
--    we made a decision on what should be done next.
--  3 Oct 2010 -SP:
--    after Jan has added the new functions on 30 Sep 2010, I have cleaned 
--  up some of the comments in the file. No time for further changes. 
--##########################################################################--

export { 
  phcSolve,
  mixedVolume,
  stableMV,
  startSystem,
  convertToPoly, 
  refineSolutions,
  trackPaths,
  gamma,
  tDegree,
  realFilter,
  zeroFilter,
  nonZeroFilter,
  cascade
}

protect ErrorTolerance, protect addSlackVariables, protect Iterations,
protect generalEquations, protect Bits, protect ResidualTolerance, 
protect Append

needsPackage "NAGtypes"

--##########################################################################--
-- GLOBAL VARIABLES 
--##########################################################################--

DBG = 0; -- debug level (10=keep temp files)
path'PHC = (options PHCpack).Configuration#"path";
PHCexe=path'PHC|(options PHCpack).Configuration#"PHCexe"; 
-- this is the executable string we need to make sure that calls to PHCpack actually run:
-- NOTE: the absolute path should be put into the init-PHCpack.m2 file 

-- QUESTION: do we need to prepend "rootPath" to all the file names to resolve issues with cygwin??

needsPackage "SimpleDoc"

--##########################################################################--
-- INTERNAL METHODS
--##########################################################################--

----------------------------------
--- File read/write operations ---
----------------------------------

getFilename = () -> (
  filename := temporaryFileName();
  while fileExists(filename) 
    or fileExists(filename|"PHCinput") 
    or fileExists(filename|"PHCoutput") do filename = temporaryFileName();
  filename
)

systemToFile = method(TypicalValue => Nothing)
systemToFile (List,String) := (F,name) -> (
  file := openOut name;
  file << #F << " " << numgens ring F#0 << endl;
  scan(F, f->( 
    L := toExternalString f;
    L = replace("ii", "I", L);
    L = replace("e", "E", L);
    L = replace("p53","",L);
    L = replace("p[0-9]+","",L);
    file << L << ";" << endl;
  ));
  close file;
) 

startSystemFromFile = method(TypicalValue => List)
startSystemFromFile (String) := (name) -> (
  -- IN: file name starting with a random coefficient start system
  -- OUT: list of polynomials in a ring with coefficients in CC
  -- REQUIRED: the format of the system on file is a random coefficient
  --   system produced by phc -m, every term starts on a separate line
  --   with the "+" sign.  The coefficient field must be CC.
  s := get name;
  s = replace("i","ii",s);
  s = replace("E","e",s);
  L := lines(s);
  n := value L_0;
  result := {};
  i := 0; j := 1;
  local stop;
  local term;
  local p;
  while i < n do (
    stop = false; p = 0;
    while not stop do (
      if #L_j != 0 then (
        -- we have to bite off the first "+" sign of the term
        term = value substring(1,#L_j-1,L_j); p = p + term;
        if (L_j_(#L_j-1) == ";") then (
          term = value substring(1,#L_j-2,L_j); p = p + term;
          stop = true; result = result | {p}
        );
      ); j = j + 1;
      stop = stop or (j >= #L);
    );
    i = i + 1; 
  );
  result
)

parseSolutions = method(TypicalValue => Sequence, Options => {Bits => 53})
parseSolutions (String,Ring) := o -> (s,R) -> (
  -- parses solutions in PHCpack format 
  -- IN:  s = string of solutions in PHCmaple format 
  --      V = list of variable names
  -- OUT: {list of solutions, list of multiplicities} 
-----and more stuff, too!!! ~~sonja
  oldprec := defaultPrecision;
  defaultPrecision = o.Bits;
  L := get s; 
  L = replace("=", "=>", L);
  L = replace("I", "ii", L);
  L = replace("E\\+","e",L);
  L = replace("E", "e", L);
  L = replace("time", "\"time\"", L);
  L = replace("rco", "\"rco\"", L);
  L = replace("multiplicity", "\"mult\"", L);
  L = replace("res", "\"residual\"", L);
  L = replace("resolution", "\"residual\"", L);
  -- because M2 automatically thinks "res"=resolution
  use R; 	  
  sols := toList apply(value L, x->new HashTable from toList x);
  defaultPrecision = oldprec;
  apply(sols, x->point( {apply(gens R, v->x#v)} | outputToPoint x ))
)

solutionsToFile = method(TypicalValue => Nothing, Options => {Append => false})
solutionsToFile (List,Ring,String) := o -> (S,R,name) -> (
  -- writes solutions to file in PHCpack format
  -- IN: S, list of solutions;
  --     R, ring (with symbols for the variables);
  --     name, string with file name.
  file := if o.Append then openOutAppend name else openOut name;
  if o.Append then 
    file << endl << "THE SOLUTIONS :" << endl;
  file << #S << " " << numgens R << endl << 
  "===========================================================" << endl;
  scan(#S, i->( 
    file << "solution " << i << " :" << endl <<
    "t :  0.00000000000000E+00   0.00000000000000E+00" << endl <<
    "m :  1" << endl <<
    "the solution for t :" << endl;
     scan(numgens R, v->(
       L := " "|toString R_v|" :  "|
       format(0,-1,9,9,realPart toCC S#i#Coordinates#v)|"  "|
       format(0,-1,9,9,imaginaryPart toCC S#i#Coordinates#v);
       file << L << endl; 
    ));
    file <<  "== err :  0 = rco :  1 = res :  0 ==" << endl;
  ));
  close file;
) 


----------------------------------
--- conversion to Point        ---
----------------------------------
outputToPoint = method()
outputToPoint HashTable := (H)->{
  SolutionStatus => if H#"mult" == 1 then Regular else Singular, 
  ConditionNumber => (H#"rco")^(-1), 
  LastT => H#"time"
}

--------------------------------------
--- functions to filter solutions ----
--------------------------------------

isPointReal = method(TypicalValue => Boolean)
isPointReal (Point,RR) := (sol,tol) -> (
  -- IN: sol, a solution of a polynomial system;
  --     tol, tolerance for the imaginary part.
  -- OUT: true, if all variables have imaginary part less than tol;
  --      false, otherwise.
  L := sol#Coordinates;
  result := true;
  scan(L,x->( if(abs(imaginaryPart(x)) > tol) then result = false));
  return result;
)

isCoordinateZero = method(TypicalValue => Boolean)
isCoordinateZero (Point,ZZ,RR) := (sol,k,tol) -> (
  -- IN: sol, a solution of a polynomial system;
  --     k, index to a coordinate must be within range;
  --     tol, tolerance for the absolute value of the k-th coordinate.
  -- OUT: true, if the k-th coordinate has abs less than tol;
  --      false, otherwise.
  L := sol#Coordinates;
  --if(abs(L_k) > tol)
  -- then return false
  -- else return true;
  -- SP: the above 3 lines could just be written as one line: 
  return abs(L_k)<=tol; 
)


--##########################################################################--
-- EXPORTED METHODS
--
-- NOTE:
-- trackPaths and refineSolutions are methods adapted from  NumericalAlgebraicGEometry/PHCpack.interface.m2
--##########################################################################--

-----------------------------------
------ POLY SYSTEM CONVERTER ------
-----------------------------------
convertToPoly = method(TypicalValue => List)
convertToPoly List := List => system -> (
  -- IN: system (or a poly) which is rational 
  --     (i.e. lives in some field of fractions of a polynomial ring)
  -- OUT: same system converted to a laurent polynomial system,
  --      where denominators are replaced with new variables.
  R := ring ideal system;
  P := R.baseRings_(#R.baseRings-1); 
  -- P is the polynomial ring whose field of fractions the system lives in
  counter := 0;
  var := local var;
  scan(system, f-> (
    if instance(class f, FractionField) then
     --if f is already polynomial, don't do anything!
       if liftable(f,P) then
     --if it can be lifted to P, then do so and update the system
	 system = system-set{f} | {lift(f,P)}
       else (  
     -- add one new variable "var_counter", and define the 
     -- appropriate Laurent polynomial ring: 
          P = newRing(P, Variables=>flatten entries vars P | {var_counter},
                      Inverses=>true,MonomialOrder=>RevLex);
     -- add the new laurent polynomial to replace the rational equation:
          newvar := P_(numgens P - 1);
	  system = system - set{f} | {sub(numerator(f),P)*newvar^(-1)};
	  system = system | {newvar - sub(denominator(f),P)}; 
	  counter = counter+1; 
       )   -- "sub" is there to make sure everyone is living in the same ring
     )
  ); -- at the end of it all, we haven't touched things 
     -- that were polynomial to begin with.  
     -- For consistency, we make sure that everyone lives in the Laurent 
     -- polynomial ring; so let us do one final ring change. 
     -- This is not necessary for PHCpack, 
     -- but for any further M2 calculations for the system, it is.
  system=apply(system,f-> sub(f,P)); 
  -- now everyone lives in the same L.poly ring P.
  system
)

----------------------------------
------ THE BLACKBOX SOLVER -------
----------------------------------
phcSolve = method(TypicalValue => List)
phcSolve  List := List => system -> (
  -- IN:  system = list of polynomials in the system 
  -- OUT: solutions to the system = a list (?sequence?) of hashtables
  --   with keys being the solutions, and entries info on those solns
  --   (such as multiplicity, etc.)
  filename := getFilename();
  << "using temporary files " << filename|"PHCinput" << " and " << filename|"PHCoutput" << endl;
  infile := filename|"PHCinput";
  outfile := filename|"PHCoutput";
  solnsfile := filename|"PHCsolns";
  R := ring ideal system;
  n := #system;
  if n < numgens R then
    error "the system is underdetermined (positive-dimensional)"; 
  --let's add slack variables if needed (i.e. if system is overdetermined)
  if n > numgens R then (
    nSlacks := n - numgens R;
    slackVars := apply(nSlacks, i->getSymbol("S"|toString i));
    newR := QQ[gens R, slackVars];-- newR := CC[gens R, slackVars];
    rM := random(QQ^n,QQ^nSlacks);-- rM := random(CC^n,CC^nSlacks);
    system = apply(#system, i->sub(system#i,newR)
      +(rM^{i}*transpose submatrix'(vars newR,toList(0..numgens R - 1)))_(0,0));
  );
  -- before moving on, check whether the system is rational (not only 
  -- polynomial), and convert it to laurent polynomials, which PCHpack
  -- can accept: 
  if instance(ring ideal system, FractionField) 
    then system = convertToPoly(system);
  --"there are denominators! i will call the conversion method.";
  -- writing data to the corresponding files:    
  systemToFile(system,infile);
  -- launching blackbox solver:
  execstr := PHCexe|" -b " |infile|" "|outfile;
  ret := run(execstr);
  if ret =!= 0 then 
    error "error occurred while executing PHCpack command: phc -b";
  execstr = PHCexe|" -z "|infile|" " |solnsfile;
  ret = run(execstr);
  if ret =!= 0 then 
    error "error occurred while executing PHCpack command: phc -z";
  -- parse and output the solutions:
  result := parseSolutions(solnsfile, ring first system);
  result
)

-----------------------------------
-------- MIXED VOLUME -------------
-----------------------------------
mixedVolume = method(Options => {stableMV => false, startSystem => false})
mixedVolume  List := Sequence => opt -> system -> (
  -- IN:  system = list of polynomials in the system 
  -- OUT: mixed volume of the system. if optional inputs specified, then output is
  --      a sequence containing a subset of: mixed volume, stable mixed volume, start system, and solutions to start system.
  -- Calls an Ada translation of ACM TOMS Algorithm 846: "MixedVol: a software package for mixed-volume computation" by Tangan Gao, T. Y. Li, Mengnien Wu, ACM TOMS 31(4):555-560, 2005.
  R := ring ideal system;
  n := #system;
  if n < numgens R then error "the system is underdetermined";
  filename := getFilename();
  << "using temporary files " << filename|"PHCinput" << " and " << filename|"PHCoutput" << endl;
  infile := filename|"PHCinput";
  outfile := filename|"PHCoutput";
  cmdfile := filename|"PHCcommands";
  sesfile := filename|"PHCsession";
  if opt.startSystem then startfile := filename|"PHCstart";
  -- let's add slack variables if needed (i.e. if system is overdetermined)
  if n > numgens R then (
    nSlacks := n - numgens R;
    slackVars := apply(nSlacks, i->getSymbol("S"|toString i));
    newR := QQ[gens R, slackVars];-- newR := CC[gens R, slackVars];
    rM := random(QQ^n,QQ^nSlacks);-- rM := random(CC^n,CC^nSlacks);
    system = apply(#system, i->sub(system#i,newR)
      +(rM^{i}*transpose submatrix'(vars newR,toList(0..numgens R - 1)))_(0,0));
    );
  -- writing data to the corresponding files
  file := openOut cmdfile; 
  file << "4" << endl; -- call MixedVol in PHCpack
  if opt.stableMV
   then (file << "y" << endl)  -- stable mixed volume wanted
   else (file << "n" << endl); -- no stable mixed volume 
  file << "n" << endl; -- no mixed-cell configuration on file
  if opt.startSystem then (
    file << "y" << endl; -- random coefficient start system wanted
    file << startfile << endl;
    file << "0" << endl << "1" << endl;
   )
   else (file << "n" << endl); -- no random coefficient start system
  close file;
  systemToFile(system,infile);
  -- launching mixed volume calculator :
  -- << "launching mixed volume calculator" << endl;
  execstr := PHCexe|" -m "|infile|" "|outfile|" < "|cmdfile|" > "|sesfile;
  ret := run(execstr);
  if ret =!= 0 then 
    error "error occurred while executing PHCpack command: phc -m";
  F := get outfile; 
  -- search lines of outfile for:  " mixed volume : "
  -- once found, extract just the number and return its value:
  local mixvol;
  scanLines(line ->  
    if substring(0,21,line) == "common mixed volume :" then (
      mixvol = value replace("common mixed volume : ","",line);
      break
   ), outfile);
  local stabmv;
  if opt.stableMV then (
    scanLines(line ->  
      if substring(0,21,line) == "stable mixed volume :" then (
        stabmv = value replace("stable mixed volume : ","",line);
        break
      ), outfile);
  );
  local result;
  if not opt.startSystem then (
    if opt.stableMV then result = (mixvol, stabmv) else result = mixvol;
  )
  else (
    solsfile := startfile | ".sols";
    p := startSystemFromFile(startfile);
    execstr = PHCexe|" -z "|startfile|" "|solsfile;
    ret = run(execstr);
    if ret =!= 0 then
      error "error occurred while executing PHCpack command: phc -z";
    sols := parseSolutions(solsfile, ring ideal system);
    if opt.stableMV
      then result = (mixvol,stabmv,p,sols)
      else result = (mixvol,p,sols);
  ); 
  result
)

----------------------------------
--- PATH TRACKER               ---
----------------------------------

trackPaths = method(TypicalValue => List, Options=>{gamma=>0, tDegree=>2})
trackPaths (List,List,List) := List => o -> (T,S,Ssols) -> (
  -- IN: T, target system to be solved;
  --     S, start system with solutions in Ssols;
  --     Ssols, solutions at the start of the paths.
  -- OUT: Tsols, solutions at the end of the paths.
  R := ring first T;
  n := #T;
  targetfile := temporaryFileName() | "PHCtarget";
  startfile := temporaryFileName() | "PHCstart";
  outfile := temporaryFileName() | "PHCoutput";
  Ssolsfile := temporaryFileName() | "PHCstartsols";
  Tsolsfile := temporaryFileName() | "PHCtargetsols";
  batchfile := temporaryFileName() | "PHCbat";
  << "using temporary files " << outfile << " and " << Tsolsfile << endl;
  --{targetfile, startfile, outfile, Ssolsfile, Tsolsfile, batchfile } / (f->if fileExists f then removeFile f);
  -- writing data to the corresponding files
  if n < numgens R then error "the system is underdetermined";
  if n > numgens R then (
    nSlacks := n - numgens R;
    slackVars := apply(nSlacks, i->getSymbol("S"|toString i));
    newR := CC[gens R, slackVars];
    rM := random(CC^n,CC^nSlacks);
    S = apply(#S, i->sub(S#i,newR)
    +(rM^{i}*transpose submatrix'(vars newR,toList(0..numgens R - 1)))_(0,0));
    rM = random(CC^n,CC^nSlacks);
    T = apply(#T, i->sub(T#i,newR)
    +(rM^{i}*transpose submatrix'(vars newR,toList(0..numgens R - 1)))_(0,0));
    Ssols = apply(Ssols, s->s|toList(nSlacks:0_CC)); 
  )
  else newR = R;
  systemToFile(T,targetfile);
  systemToFile(S,startfile);
  solutionsToFile(Ssols,newR,Ssolsfile);	  
  -- making batch file
  bat := openOut batchfile;
  bat << targetfile << endl << outfile << endl <<"n"<< endl 
  << startfile << endl << Ssolsfile << endl;
  -- first menu with settings of the construction of the homotopy
     bat << "k" << endl << o.tDegree << endl; 
  if o.gamma != 0 then (
    bat << "a" << endl << realPart o.gamma << endl;
    bat << imaginaryPart o.gamma << endl;
  );
  bat << "0" << endl;
  -- second menu 
  bat << "0" << endl; -- exit for now
  -- third menu
  bat << "0" << endl; -- exit for now
  -- fourth menu
  bat << "0" << endl; -- exit for now
  close bat;
  run(PHCexe|" -p <"|batchfile|" >phc_session.log");
  run(PHCexe|" -z "|outfile|" "|Tsolsfile);
  -- parse and output the solutions
  result := parseSolutions(Tsolsfile, newR);
  if n > numgens R then (
    result = apply(result, s->(
      if any(drop(first s, numgens R), x->abs x > 0.01) 
      then error "slack value is nonzero";
      {take(first s, numgens R)}|drop(s,1)
    ));
    totalN := #result;
    scan(result, s->(
      if s#1#"mult">1 then error "mutiple root encountered";
      if s#1#"mult"<0 then error "negative mutiplicity";
    ));			 
    result = select(result, 
      s->--s#1#"mult">0 and 
      max(s#0/abs)<10000 -- path failed and/or diverged
    );
    if DBG>0 and #result < totalN 
    then  -- error "discarded!" 
    << "track[PHCpack]: discarded "<< 
    totalN-#result << " out of " << totalN << " solutions" << endl;
  );
  -- clean up
  --if DBG<10 then {targetfile, startfile, outfile, Ssolsfile, Tsolsfile, batchfile } / removeFile ;
  return result;
)

-----------------------------------
----- REFINING SOLUTIONS ----------
-----------------------------------
refineSolutions = method()
refineSolutions (List,List,ZZ) := (f,sols,dp) -> (
  -- IN: f, a polynomial system;
  --     sols, initial approximations for the solutions of f;
  --     dp, number of decimal places in the working precision.
  -- OUT: a list of refined solutions.
  PHCinputFile := temporaryFileName() | "PHCinput";
  PHCoutputFile := temporaryFileName() | "PHCoutput";
  PHCbatchFile := temporaryFileName() | "PHCbatch";
  PHCsessionFile := temporaryFileName() | "PHCsession";
  PHCsolutions := temporaryFileName() | "PHCsolutions";
  -- stdio << "writing input system to " << PHCinputFile << endl;
  systemToFile(f,PHCinputFile);
  -- stdio << "appending solutions to " << PHCinputFile << endl;
  R := ring first f;
  solutionsToFile(sols,R,PHCinputFile,Append=>true);
  -- stdio << "preparing input data for phc -v in " << PHCbatchFile << endl;
  s := concatenate("3\n",PHCinputFile);
  s = concatenate(s,"\n");
  s = concatenate(s,PHCoutputFile);
  s = concatenate(s,"\n3\n1.0E-");
  s = concatenate(s,toString(dp-4));  -- tolerance for correction term
  s = concatenate(s,"\n4\n1.0E-");
  s = concatenate(s,toString(dp+16));  -- tolerance for residual
  s = concatenate(s,"\n6\n");
  nit := ceiling(dp/10.0);
  s = concatenate(s,toString(nit));   -- number of Newton iterations
  s = concatenate(s,"\n7\n");
  s = concatenate(s,toString(dp));    -- decimal places in working precision
  s = concatenate(s,"\n0\n");
  bat := openOut PHCbatchFile;
  bat << s;
  close bat;
  -- stdio << "running phc -v, writing output to " << PHCsessionFile << endl;
  run(PHCexe|" -v < " | PHCbatchFile | " > " | PHCsessionFile);
  stdio << "using temporary file " << PHCoutputFile << " for storing refined solutions " << endl;
  -- stdio << "running phc -z on " << PHCoutputFile << endl;
  -- stdio << "solutions in Maple format in " << PHCsolutions << endl;
  run(PHCexe|" -z " | PHCoutputFile | " " | PHCsolutions);
  -- stdio << "parsing file " << PHCsolutions << " for solutions" << endl;
  b := ceiling(log_2(10^dp));
  result := parseSolutions(PHCsolutions,R,Bits=>b);
  result
)

--------------------------------------
------- FILTERING SOLUTIONS   --------
--------------------------------------

realFilter = method(TypicalValue => List)
realFilter (List,RR) := (sols,tol) -> (
  -- IN: sols, solutions of a polynomial system;
  --     tol, tolerance for the imaginary part.
  -- OUT: list of real solutions, solutions in sols where
  --      all variables have imaginary part less than tol.
  return select(sols,t->isPointReal(t,tol));
)

zeroFilter = method(TypicalValue => List)
zeroFilter (List,ZZ,RR) := (sols,k,tol) -> (
  -- IN: sols, solutions of a polynomial system;
  --     k, index to a coordinate of a solution, must be within range
  --     tol, tolerance for the absolute value of k-th coordinate
  -- OUT: list of solutions in sols where the k-th coordinate
  --      is less than the tolerance.
  return select(sols,t->isCoordinateZero(t,k,tol));
)

nonZeroFilter = method(TypicalValue => List)
nonZeroFilter (List,ZZ,RR) := (sols,k,tol) -> (
  -- IN: sols, solutions of a polynomial system;
  --     k, index to a coordinate of a solution, must be within range
  --     tol, tolerance for the absolute value of k-th coordinate
  -- OUT: list of solutions in sols where the k-th coordinate
  --      is less than the tolerance.
  return select(sols,t->(not isCoordinateZero(t,k,tol)));
)

-----------------------------------------------
------------------ CASCADE --------------------
-----------------------------------------------
cascade = method(TypicalValue=>List)
cascade Ideal := List => (I) -> (
     -- returns a list of WitnessSet's ...
     --
     -- SP 23May2011:
     --
     -- TO DO: Need this method to output a list of types WitnessSet
     --for each dimension. E.g. if there are two components of dim 5 and 7,
     --the method should return a List of the following format:
     --
     --{{5,WitnessSet},{7,WitnessSet}} --if we prefer lisf of lists
     --or
     --{{5=>WitnessSet},{7=>WitnessSet}}  --if we prefer hashtable
     --
     --Each WitnessSet contains the following:
     --WitnessSet = {Equations, Slice, Points}
     --where Equations = the system; Points are what we already output right now;
     --and Slice is what *needs to be done*
     --
     --To get each slice:
     --for dimension d
     --take the file target_sw"d"
     --grab the last d equations that are at the start of the file.
     --
     -- So: the one thing to do is to just parse the files for these last d equations.
     infile := temporaryFileName() |      "PHCmonodromy";
     targetfile := temporaryFileName() | "PHCtarget";
     batchfile := temporaryFileName() | "PHCbat";
     solsfile :=  temporaryFileName() | "PHCsolfile";
     << "using temporary files " << targetfile|", " << infile|", " << solsfile << endl;
     {infile, targetfile, solsfile, batchfile} / (f->if fileExists f then removeFile f);
     systemToFile(I_*, infile);
     -- making batch file (for phc -c)
     bat := openOut batchfile;
     bat << "0" << endl;
     bat << "y" << endl;
     bat << infile << endl; -- name of input file
     bat << targetfile << endl; -- name of output file
     bat << numgens ring I - 1 << endl;
     bat << "n" << endl;
     bat << "0" << endl;
     close bat;
     compStartTime := currentTime();      
     run(PHCexe|" -c <"|batchfile|" >phc_session.log");
     if DBG>0 then << "PHCpack computation time: " << currentTime()-compStartTime << endl;
     -- Now we have to grab the files and get the points
     i := numgens ring I - 1;     	  
     filnames:={};
     while i >=0 do (
     fil := (targetfile|"_sw"|i);
     if fileExists fil then filnames=append (filnames,i=>fil);  
       i=i-1);
     result:={};
     for ff in filnames do (
	  (j,f) := toSequence ff;
     	  --  1. read file, and make sure that there are solutions.  If not, return null
     	  S := get f;
     	  if not match("THE SOLUTIONS", S) 
	  then null
	  else  (
     	   --  2. now clean the equations
     	   --if fileExists solsfile then removeFile solsfile;
     	   run(PHCexe|"  -z "|f|" "|solsfile);
	   result=append(result,ff => parseSolutions(solsfile, ring I))
	  ));
     --
     --insert code to grab the slices here:
     -- 
     --insert code that creates each witness set like this:
     --witnessSet(system,slice,points)
     --then store the above witness set in the 'result' which we'll output.
     result
     )

-----------------------------------------------
--------- MONODROMY BREAKUP    ----------------
-----------------------------------------------

monodromyBreakup = method(Options => {})
monodromyBreakup WitnessSet := o -> (W) -> (
     -- Input: a witness set (i.e. numerical equidimensional set)
     -- Output: a list of witness sets, probably the irreducible
     --  decomposition of W.
     W = addSlackVariables generalEquations W;
     infile := temporaryFileName() | 
     "PHCmonodromy";
     targetfile := temporaryFileName() | 
     "PHCtarget";
     batchfile := temporaryFileName() | 
     "PHCbat";
     solsfile :=  temporaryFileName() | 
     "PHCsolfile";
     {infile, targetfile, batchfile} / (f->if fileExists f then removeFile f);
     -- writing data to the corresponding files                                                                                                                                                                           
     systemToFile(W.Equations_* | W.Slice,infile);
     solutionsToFile(W.Points,ring W,infile, Append=>true);	  
     -- making batch file (for phc -f)
     bat := openOut batchfile;
     bat << "2" << endl; -- option for newton's method using multiprecision
     bat << infile << endl; -- name of input file
     bat << targetfile << endl; -- name of output file
     bat << if degree W < 15 then "2" else "1" << endl; -- this 15 is a problem
     bat << "0" << endl;
     close bat;
     compStartTime := currentTime();      
     run(PHCexe|" -f <"|batchfile|" >phc_session.log");
     if DBG>0 then << "PHCpack computation time: " << currentTime()-compStartTime << endl;
     -- Now we have to grab the files and get the points
     i := 1;
     filnames := while (
	  fil := (infile|"_f"|i);
     	  fileExists fil
	  ) list fil do i=i+1;
     for f in filnames list (
	  if fileExists solsfile then removeFile solsfile;
	  run(PHCexe|" -z "|f|" "|solsfile);
	  witnessSet(W.Equations, ideal W.Slice, parseSolutions(solsfile, ring W))
	  )
     )

--##########################################################################--
-- DOCUMENTATION
--##########################################################################--

beginDocumentation()

load "./PHCpackDoc.m2";





--##########################################################################--
-- TESTS
--##########################################################################


-----------------------------------
-- cascade
-----------------------------------
TEST/// 
      R=QQ[x11,x22,x21,x12,x23,x13,x14,x24]
      I=ideal(x11*x22-x21*x12,x12*x23-x22*x13,x13*x24-x23*x14)
      assert{ # cascade I == 1}--there is one component of dim.5.
///;

-----------------------------------
-- convertToPoly
-----------------------------------
TEST///
      QQ[x,y,z];
      sys = {y-x^2, z-x^3, (x+y+z-1)/x};
      convertedSys = convertToPoly(sys);
      R=ring ideal convertedSys 
      assert(isPolynomialRing(R)) --make sure it is a poly ring
      O=options(R); 
      assert(O#Inverses) -- want to make sure this is a Laurent ring
///;

-----------------------------------
-- mixedVolume
-----------------------------------
TEST/// 
     R=QQ[x,y,z] --- the example needs to be meaningful; tested against by-hand output
     S={y-x^2,z-x^3,x+y+z-1}
     m=mixedVolume(S) --value returned by the function we are testing
     assert(m==3) --I know the answer is 3.
     R=QQ[x,y,z]
     S1={y^3+z^2+3,z^2+x^4+x^4*z^2+4,x^4+y^3+x^4*y^3+5}
     M=mixedVolume(S1)
     assert(M==48)--testing output against by-hand calculation
     R=QQ[x,y]
     S2={x^2+x*y+y^2+x+y+1,x^5+x^4*y+x^3*y^2+x^2*y^3+x*y^4+y^5} --another example
     M=mixedVolume(S2)
     assert(M==10)--testing output against by-hand calculation 
///;


-----------------------------------
-- phcSolve
-----------------------------------
TEST/// 
     R=QQ[x,y,z]
     S={x^2-y*z-3,y^2-x*z-4,z^2-x*y-5}
     L=phcSolve(S)
     n=# L
     assert(n==2) --testing phc output against by-hand calculation
     sol1={11/6.,-1/6.,-13/6.}
     sol2={-11/6.,1/6.,13/6.}
     assert((abs((sol1-L_0_0)_0)<.00000000001 and abs((sol1-L_0_0)_1)<.00000000001 and abs((sol1-L_0_0)_2)<.00000000001) or 
     (abs((sol1-L_1_0)_0)<.00000000001 and abs((sol1-L_1_0)_1)<.00000000001 and abs((sol1-L_1_0)_2)<.00000000001))--since phc output is 
     --numerical and not exact, comparision is done by looking at the modulus of the difference between the output and expected answer 
     --(is there an easier way to code this?)
     assert((abs((sol1-L_0_0)_0)<.00000000001 and abs((sol1-L_0_0)_1)<.00000000001 and abs((sol1-L_0_0)_2)<.00000000001) or 
     (abs((sol1-L_1_0)_0)<.00000000001 and abs((sol1-L_1_0)_1)<.00000000001 and abs((sol1-L_1_0)_2)<.00000000001))
     --should I include second test with more difficult system that compares phc output with solution obtained through Macaulay 2 (if possible)
     --or another package of Macaulay 2? (eg)
///;


-----------------------------------
-- realFilter
-----------------------------------
TEST/// 
     R = QQ[x,y]; 
     f = {x^2 + 4*y^2 - 4, 2*y^2 - x}; 
     fSols = phcSolve(f);
     realSols = realFilter(fSols,1.0e-10)
     assert{# realSols == 2} --cannot use isReal because the output has small imaginary parts!!
///;

-----------------------------------
-- refineSolutions
-----------------------------------
TEST/// 
      R = QQ[x,y]; 
      S = {x^2 - 1/3, x*y - 1}; 
      roots = phcSolve(S);
      r0 = roots#0#Coordinates#1
      newRoots = refineSolutions(S,roots,64)
      newRoots#0 -- recall that solutions are of type Point
      assert(newRoots#0#Coordinates#1 == 1.73205080756887729352744634150587236694280525381038062805580698)
///;

-----------------------------------
-- trackPaths
-----------------------------------
TEST/// 
     R = CC[x,y]; 
     f = { x^3*y^5 + y^2 + x^2*y, x*y + x^2 - 1};
     (m,q,qsols) = mixedVolume(f,startSystem=>true);
     fsols = trackPaths(f,q,qsols)
     assert(# fsols == 8)
///;


-----------------------------------
-- zeroFilter
-----------------------------------
TEST/// 
     R = QQ[x,y]; 
     f = { x^3*y^5 + y^2 + x^2*y, x*y + x^2 - 1};
     fSols = phcSolve(f);
     zeroSols = zeroFilter(fSols,1,1.0e-10);
     assert(zeroSols_0#Coordinates == {-1,0})
     assert(zeroSols_1#Coordinates == {1,0})
///;


-----------------------------------
-- nonZeroFilter
-----------------------------------
TEST/// 
     R = QQ[x,y]; 
     f = { x^3*y^5 + y^2 + x^2*y, x*y + x^2 - 1};
     fSols = phcSolve(f);
     nonzeroSols = nonZeroFilter(fSols,0,1.0e-10);
     assert{# nonzeroSols ==10} -- this is just complement of zeroFilter which is tested in more detail. Here we are just counting the number of solutions to make sure the method ran.
///;



--##########################################################################--
end   -- terminate reading ... 
--##########################################################################--






restart
uninstallPackage "PHCpack"

installPackage("PHCpack", RemakeAllDocumentation=>true, UserMode=>true,DebuggingMode => true)
viewHelp PHCpack
-- RerunExamples=>true,

options PHCpack
loadPackage ("PHCpack", Configuration=>{"path"=>"/Users/petrovic/PHCpack/./phc"})
loadPackage ("PHCpack", Configuration=>{"path"=>"/Users/petrovic/","PHCexe"=>"./phc"})


restart
debug loadPackage "PHCpack"

--##########################################################################--






///
-- WitnessSet and monodromyBreakupPHC

R = QQ[x,y,z]
I = ideal"x+y-2,y-z+3"
J = ideal"x2+y2+z2-1,xy-z2"
L = trim intersect(I,J)
RC = CC[gens R]
L = sub(L,RC)
W = witnessSet L
--W1 = generalEquations W
--W2 = addSlackVariables W1
W3s = monodromyBreakupPHC W
apply(W3s, points)
W3s/degree
peek W2
netList (ideal W2)_*
peek oo
///

///
-- cascade interface
restart
debug loadPackage "PHCpack"

R = QQ[x,y,z,w]
I = ideal"x+y-2,y2-z+3"
J = ideal"x2+y2+z2-1,xy-z2,x2+w2"
L = trim intersect(I,J)
RC = CC[gens R]
L = sub(L,RC)
cascadePHC L

W = witnessSet L
--W1 = generalEquations W
--W2 = addSlackVariables W1
W3s = monodromyBreakupPHC W
apply(W3s, points)
W3s/degree
peek W2
see ideal W2
peek oo
///

