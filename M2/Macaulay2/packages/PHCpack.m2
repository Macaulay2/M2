--------------------------------------------------------------
--------------------------------------------------------------
-- authors: Elizabeth Gross, Sonja Petrovic, Jan Verschelde --
--
-- UPDATE AND CHANGE LOG:
--     	    20 April 2010. ~SP: draft of package.
--     	    5 May 2010 - SP: 
--     	    	      	   finished w/ mixed volume; black box; incld. slack variables. 
--     	    	      	   did not do documentation.
--     	    16 May 2010 -EG: 
--     	    	      	   documentation of mixed volume and black box.  
--     	    	      	   Need to include slack variables info and tests.
--     	    9 June 2010 ~SP: 
--     	    	      	   some housekeeping and clean-up of mixed volume and black box solver;
--     	    	      	   added a few things to documentation, mostly about dimension of the system and slack vars; 
--     	    	      	   	many draft-like things still there. Please keep for now (at least as comments) for future reference.
--     	    	      	   rational input allowed for the black-box solver;
--     	    	      	   added "convertToPoly": a function that takes a rational system and turns it into laurent (natural environment to M2, acceptable for PHC) .
--     	    24 June 2010 -EG:
--     	    	      	   added tests for solveBlackBox() and mixedVol()
--     	    TO DO list:
--     	    	 - decide the name for solveBlackBox(). I don't think we can use "solve". How about numSolve, or something? or phcSolve? or solvePHC?
--     	    	 - optional input: precision --for a script example, see "toricMarkov" code from FourTiTwo.m2 --and documentation
--     	    	 - refineSoln() --draft is there
--     	    	 - "phc -f" to filter solutions close to zero
--     	    	 - tests!
--     	    	 - some more references in documentation; update documentation for the main package page - write a short story with an example. :)
--     	    
--------------------------------------------------------------
--------------------------------------------------------------
needsPackage "NAGtypes"

newPackage(
	"PHCpack",
    	Version => "0.1", 
    	Date => "19 April 2010",
    	Authors => {
	     {Name => "Elizabeth Gross", Email => "lizgross@math.uic.edu",  HomePage => "http://www.math.uic.edu/~lizgross"},
	     {Name => "Sonja Petrovic", Email => "petrovic@math.uic.edu", HomePage => "http://www.math.uic.edu/~petrovic"},
	     {Name => "Jan Verschelde", Email => "jan@math.uic.edu", HomePage => "http://www.math.uic.edu/~jan"}
	     },
    	Headline => "Interface to PHCpack",
	Configuration => { 
	     "path" => "",
	     "PHCexe"=>"phc", 
	     "keep files" => true
	      },
    	DebuggingMode => true, 	-- DebuggingMode should be true while developing a package,  but false after it is done
	AuxiliaryFiles=>true,
	CacheExampleOutput => true
	)

-- Any symbols or functions that the user is to have access to must be placed in the following  list:
export { 
     solveBlackBox,
     mixedVolume,
     convertToPoly, 
     refineSolutions,
     refineSoln,
     trackPaths
     }
-- convertToPoly: this function could be useful, so export???

protect ErrorTolerance, protect addSlackVariables, protect Iterations,
protect generalEquations, protect tDegree, protect Bits, protect gamma, 
protect ResidualTolerance, protect Append

needsPackage "NAGtypes"

-- GLOBAL VARIABLES ----------------------------------
DBG = 0; -- debug level (10=keep temp files)
path'PHC = (options PHCpack).Configuration#"path";
-- NOTE: the absolute path should be put into the init-PHCpack.m2 file for PHCpack inside Library -> Application Support -> Macaulay2 ->.
-- if path'PHC == "" then path'PHC =  prefixDirectory | currentLayout#"programs"
PHCexe=path'PHC|(options PHCpack).Configuration#"PHCexe"; --is the executable string we need to make sure that calls to PHCpack actually run:
--PHCexe="../../../.././phc";
--PHCexe="/Users/petrovic/PHCpack/./phc";   
--PHCexe="/Users/petrovic/./phc"; 
-- NOTE: the absolute path should be put into the init-PHCpack.m2 file for PHCpack inside Library -> Application Support -> Macaulay2 ->.

----------------------------------------------------------------------
--QUESTION:
--should i concatenate  rootPath to the beginning of each filename??
----------------------------------------------------------------------



needsPackage "SimpleDoc"




----------------------------------------------------------------------
--***********************  internal methods ************************--
----------------------------------------------------------------------

----------------------------------
--- File read/write operations ---
----------------------------------

getFilename = () -> (
     filename := temporaryFileName();
     while fileExists(filename) or fileExists(filename|"PHCinput") or fileExists(filename|"PHCoutput") do filename = temporaryFileName();
     filename)

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

----------------------------------
--- conversion to Point        ---
----------------------------------
outputToPoint = method()
outputToPoint HashTable := (H)->{
     SolutionStatus => if H#"mult" == 1 then Regular else Singular, 
     RCondition => H#"rco", 
     LastT => H#"time"
     }

parseSolutions = method(TypicalValue => Sequence, Options => {Bits => 53})
parseSolutions (String,Ring) := o -> (s,R) -> (
     -- parses solutions in PHCpack format 
     -- IN:  s = string of solutions in PHCmaple format 
     --      V = list of variable names
     -- OUT: {list of solutions, list of multiplicities} -----and more stuff, too!!! ~~sonja
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
     L = replace("resolution", "\"residual\"", L);--because M2 automatically thinks "res"=resolution
     use R; 	  
     sols := toList apply(value L, x->new HashTable from toList x);
     defaultPrecision = oldprec;
     apply(sols, x->point( {apply(gens R, v->x#v)} | outputToPoint x ))
     )

-- copied from PHCpack.interface.m2 -- check if correct!!!
solutionsToFile = method(TypicalValue => Nothing,
     Options => {Append => false})
solutionsToFile (List,Ring,String) := o -> (S,R,name) -> (
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
			 format(0,-1,9,9,realPart toCC S#i#v)|"  "|
			 format(0,-1,9,9,imaginaryPart toCC S#i#v);
			 file << L << endl; 
			 ));
	       file <<  "== err :  0 = rco :  1 = res :  0 ==" << endl;
	       ));
     close file;
     ) 

----------------------------------------------------------------------
--***********************  EXPORTED METHODS  ************************--
----------------------------------------------------------------------

-----------------------------------
------ POLY SYSTEM CONVERTER ------
-----------------------------------
--****--
--**** THIS PROBABLY NEEDS A NAME CHANGE?? I AM NOT CREATIVE ENOUGH (or, I'm too creative). FEEL FREE TO CHANGE, AS LONG AS THERE IS NO CONFLICT WITH CURRENT M2 FUNCTION NAMES.
--****--
convertToPoly = method(TypicalValue => List)
convertToPoly   List := List => system -> (
     -- IN: system (or a poly) which is rational (i.e. lives in some field of fractions of a polynomial ring)
     -- OUT: same system converted to a laurent polynomial system, where denominators are replaced with new variables.
     R:=ring ideal system;
     P:=R.baseRings_(#R.baseRings-1); --this is the polynomial ring whose field of fractions the system is living in.
     counter:=0;
     var := local var;
     scan(system, f-> (
	       if 	  instance(class f, FractionField) --if f is already polynomial, don't do anything!
	       then if liftable(f,P) --if it can be lifted to P, then do so and update the system
	       then system = system-set{f} | {lift(f,P)}  else 
	       (    --add one new variable "var_counter", and define the appropriate Laurent polynomial ring: 
		    P = newRing(P, Variables=>flatten entries vars P | {var_counter},Inverses=>true,MonomialOrder=>RevLex);
     	       	    --add the new laurent polynomial to replace the rational equation:
		    system = system - set{f} | {sub(numerator(f),P)*var_counter^(-1)};
		    system = system | {var_counter - sub(denominator(f),P)}; 
		    counter=counter+1; 
	       )    -- "sub" is there to make sure everyone is living in the same ring. 
	  )
     );--at the end of it all, we haven't touched things that were polynomial to begin with.  
     --For consistency, we make sure that everyone lives in the Laurent polynomial ring; so let us do one final ring change. 
     --This is not necessary for PHCpack, but for any further M2 calculations for the system, it is.
     system=apply(system,f-> sub(f,P)); -- now everyone lives in the same L.poly ring P.
     system
     )

-----------------------------------
------ THE BLACK-BOX SOLVER -------
-----------------------------------
solveBlackBox = method(TypicalValue => List)
solveBlackBox  List := List => system -> (
     -- IN:  system = list of polynomials in the system 
     -- OUT: solutions to the system = a list (?sequence?) of hashtables with keys being the solutions, and entries info on those solns (such as multiplicity, etc.)
     -- !!! problem with temporaryFileName: cygwin's /tmp is different from Windows' /tmp  ~~anton
     -- check the "path" discussion in FourTiTwo.m2 !! ~~sonja
     -- -----I THINK this will fix CYGWIN PROBLEMS: prepending "rootPath" to filenames SHOULD fix this interface on cygwin!
     filename:=getFilename();
     << "using temporary file name " << filename|"PHCinput" << endl;
     infile := filename|"PHCinput";
     outfile := filename|"PHCoutput";
     solnsfile := filename| "PHCsolns";
     R:=ring ideal system;
     n := #system;
     if n < numgens R then error "the system is underdetermined (positive-dimensional)"; 
     --let's add slack variables if needed (i.e. if system is overdetermined)
     if n > numgens R then (
      nSlacks := n - numgens R;
      slackVars := apply(nSlacks, i->getSymbol("S"|toString i));
      newR := QQ[gens R, slackVars];-- newR := CC[gens R, slackVars];
      rM := random(QQ^n,QQ^nSlacks);-- rM := random(CC^n,CC^nSlacks);
      system = apply(#system, i->sub(system#i,newR)+(rM^{i}*transpose submatrix'(vars newR,toList(0..numgens R - 1)))_(0,0));
      );
     --before moving on, check whether the system is rational (not only polynomial), and convert it to laurent polynomials, which PCHpack can accept: 
     if instance(ring ideal system, FractionField) then  system=convertToPoly(system); --"there are denominators! i will call the conversion method.";
     -- writing data to the corresponding files:    
     systemToFile(system,infile);
     -- launching blackbox solver:
     execstr := PHCexe|" -b "|infile|" "|outfile;
     --	    execstr = path'PHC|"phc -b" |infile|" "|rootPath |outfile; 
     --     execstr = PHCexe|" -b" |rootPath |infile|" "|rootPath |outfile;
     ret := run(execstr);
     if ret =!= 0 then error "error occurred while executing PHCpack command: phc -b";
     execstr = PHCexe|" -z "|infile|" " |solnsfile;
     ret = run(execstr);
     if ret =!= 0 then error "error occurred while executing PHCpack command: phc -z";
     -- parse and output the solutions:                                                                                                                                                                                  
     result := parseSolutions(solnsfile, ring first system);
     result
     )

-----------------------------------
-------- MIXED VOLUME -------------
-----------------------------------
mixedVolume = method(TypicalValue => ZZ)  
mixedVolume  List := ZZ => system -> (
     -- IN:  system = list of polynomials in the system 
     -- OUT: integer = mixed volume of the system, IF the system is 0-dim'l.
     R:=ring ideal system;
     n := #system;
     if n < numgens R then error "the system is underdetermined (not 0-dimensional)";
     filename:=getFilename();
     << "using temporary file name " << filename|"PHCinput" << endl;
     infile := filename|"PHCinput";
     outfile := filename|"PHCoutput";
     --let's add slack variables if needed (i.e. if system is overdetermined)
     if n > numgens R then (
      nSlacks := n - numgens R;
      slackVars := apply(nSlacks, i->getSymbol("S"|toString i));
      newR := QQ[gens R, slackVars];-- newR := CC[gens R, slackVars];
      rM := random(QQ^n,QQ^nSlacks);-- rM := random(CC^n,CC^nSlacks);
      system = apply(#system, i->sub(system#i,newR)+(rM^{i}*transpose submatrix'(vars newR,toList(0..numgens R - 1)))_(0,0));
      );
     -- writing data to the corresponding files                                                                                                                                                                           
     systemToFile(system,infile);
     -- launching blackbox solver:
     execstr := PHCexe|" -m -b "|infile|" "|outfile;
     ret := run(execstr);
     if ret =!= 0 then error "error occurred while executing PHCpack command: phc -m -b";
     F := get outfile; 
     --search lines of outfile for:  " mixed volume : "
     --once found, extract just the number and return its value:
     local result;
     scanLines(line ->  
	  if  substring(0,14,line) == "mixed volume :" 
	  then (
	       result = value replace("mixed volume : ","",line);
	       break
	       ), 
	  outfile);
     result      
     )


-----------------------------------
-------- REFINING SOLNS -----------
-----------------------------------
refineSoln = method(TypicalValue => List)  
refineSoln  List := List => soln -> (
     -- IN: 
     -- OUT:
     << "this function still remains to be written" << endl;
     )

--here is a sample method from FirstPackage.m2:
-- firstFunction = method(TypicalValue => String)
--note: typical value is the type of the output!
-- firstFunction ZZ := String => n -> if n == 1 then "Hello World!" else "D'oh!"
--note: ZZ = input type, String = output type. 

----------------------------------------------------------------------
-- Functions moved from NumericalAlgebraicGEometry/PHCpack.interface.m2
-- trackPaths and refineSolution have been "wrapped" already 
----------------------------------------------------------------------

trackPaths = method(TypicalValue => List, Options=>{gamma=>1, tDegree=>2})
trackPaths (List,List,List) := List => o -> (S,T,solsS) -> (
     R := ring first T;
     n := #T;
     targetfile := temporaryFileName() | 
     "PHCtarget";
     startfile := temporaryFileName() | 
     "PHCstart";
     outfile := temporaryFileName() | 
     "PHCoutput";
     solsSfile := temporaryFileName() | 
     "PHCstartsols";
     solsTfile := temporaryFileName() | 
     "PHCtargetsols";
     batchfile := temporaryFileName() | 
     "PHCbat";
     {targetfile, startfile, outfile,
	  solsSfile, solsTfile, batchfile } / (f->if fileExists f then removeFile f);
     -- writing data to the corresponding files                                                                                                                                                                           
     if n < numgens R then error "the system is underdetermined";
     if n > numgens R then (
	  nSlacks := n - numgens R;
	  slackVars := apply(nSlacks, i->getSymbol("S"|toString i));
	  newR := CC[gens R, slackVars];
	  rM := random(CC^n,CC^nSlacks);
	  S = apply(#S, i->sub(S#i,newR)+(rM^{i}*transpose submatrix'(vars newR,toList(0..numgens R - 1)))_(0,0));
	  rM = random(CC^n,CC^nSlacks);
	  T = apply(#T, i->sub(T#i,newR)+(rM^{i}*transpose submatrix'(vars newR,toList(0..numgens R - 1)))_(0,0));
	  solsS = apply(solsS, s->s|toList(nSlacks:0_CC)); 
	  ) else newR = R;
     systemToFile(T,targetfile);
     systemToFile(S,startfile);
     solutionsToFile(solsS,newR,solsSfile);	  
     -- making batch file
     bat := openOut batchfile;
     bat << targetfile << endl << outfile << endl <<"n"<< endl 
     << startfile << endl << solsSfile << endl;
     -- first menu
     bat << "k" << endl << o.tDegree << endl; 
     bat << "a" << endl << realPart o.gamma << endl << imaginaryPart o.gamma << endl;
     bat << "0" << endl;
     -- second menu 
     bat << "0" << endl; -- exit for now
     -- third menu
     bat << "0" << endl; -- exit for now
     -- fourth menu
     bat << "0" << endl; -- exit for now
     close bat;
     compStartTime := currentTime();      
     run(PHCexe|" -p <"|batchfile|" >phc_session.log");
     if DBG>0 then << "PHCpack computation time: " << currentTime()-compStartTime << endl;
     run(PHCexe|" -z "|outfile|" "|solsTfile);
     -- parse and output the solutions                                                                                                                                                                                    
     result := parseSolutions(solsTfile, newR);
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
     if DBG<10 then {targetfile, startfile, outfile,
	  solsSfile, solsTfile, batchfile } / removeFile ;
     return result;
     )

refineSolutions = method(TypicalValue => List, Options => {
	  ResidualTolerance => 0, 
	  ErrorTolerance => 1e-10,
	  Iterations => null,
	  Bits => 300
	  }
	  )
refineSolutions (List,List) := List => o -> (T,sols) -> (
     -- T, a target system, is a list of polynomials over CC
     -- sols is a list of solutions
     --  each solution is a list {point, other stuff after that}
     R := ring first T;
     n := #T;
     targetfile := temporaryFileName() | 
     "PHCtarget";
     outfile := temporaryFileName() | 
     "PHCoutput";
     solsTfile := temporaryFileName() | 
     "PHCsols";
     batchfile := temporaryFileName() | 
     "PHCbat";
     {targetfile, outfile, batchfile, solsTfile} / (f->if fileExists f then removeFile f);
     -- writing data to the corresponding files                                                                                                                                                                           
     systemToFile(T,targetfile);
     solutionsToFile(sols,R,targetfile, Append=>true);	  
     -- making batch file (for phc -v)
     bat := openOut batchfile;
     bat << "3" << endl; -- option for newton's method using multiprecision
     bat << "y" << endl; -- is the system in a file?
     bat << targetfile << endl; -- name of input file
     bat << outfile << endl; -- name of output file
     -- set tolerance for error on root
     bat << "3" << endl << o.ErrorTolerance << endl;
     -- set tolerance for residual
     bat << "4" << endl << o.ResidualTolerance << endl;
     -- set #iterations
     niterations := o.Iterations;
     ndecimal := ceiling(o.Bits * log(2.) / log(10.));
     bat << "6" << endl << niterations << endl;
     bat << "7" << endl << ndecimal << endl;
     -- now exit menu
     bat << "0" << endl;
     close bat;
     compStartTime := currentTime();      
     run(PHCexe|" -v <"|batchfile|" >phc_session.log");
     if DBG>0 then << "PHCpack computation time: " << currentTime()-compStartTime << endl;
     run(PHCexe|" -z "|outfile|" "|solsTfile);
     -- parse and output the solutions                                                                                                                                                                                    
     parseSolutions(solsTfile, R, Bits => o.Bits)
     )

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

cascade = method()
cascade Ideal := (I) -> (
     -- returns a list of WitnessSet's -- or at it will...
     infile := temporaryFileName() | 
     "PHCmonodromy";
     targetfile := temporaryFileName() | 
     "PHCtarget";
     batchfile := temporaryFileName() | 
     "PHCbat";
     solsfile :=  temporaryFileName() | 
     "PHCsolfile";
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
     filnames := while (
	  fil := (targetfile|"_sw"|i);
     	  i >=0 and fileExists fil
	  ) list i => fil do i=i-1;
     for ff in filnames list (
	  (j,f) := toSequence ff;
     	  --  1. read file, and make sure that there are solutions.  If not, return null
     	  S := get f;
     	  if not match("THE SOLUTIONS", S) 
	  then null
	  else (
     	   --  2. now clean the equations
     	   if fileExists solsfile then removeFile solsfile;
     	   run(PHCexe|" -z "|f|" "|solsfile);
	   ff => parseSolutions(solsfile, ring I)
	  ))
     )


///
-- WitnessSet and monodromyBreakupPHC
restart
debug loadPackage "PHCpack"

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

----------------------------------------------------------------------
--**************************  DOCUMENTATION ************************--
----------------------------------------------------------------------

-----------------------------------
----- Documenting main package ----
-----------------------------------
beginDocumentation()

doc ///
     Key 
          PHCpack 
     Headline
     	  Interface for PHCpack
     Description
          Text
	       Interfaces the functionality of the software {\tt PHCpack} available at
	       @HREF"http://www.math.uic.edu/~jan/download.html"@.
	       (The user needs to have {\tt PHCpack} installed on his/her machine.)
	        
	       PHCpack uses homotopy continuation methods to numerically solve systems of polynomial equations.  
	       The most popular function, the blackbox solver, returns solution vectors for the isolated roots of a system 
	       and diagnostical information about the quality of each solution. 	      
     	       
	  Example     
	       R=QQ[x,y,z]      --R=CC[x,y,z]
	       system={y-x^2,z-x^3,x+y+z-1}
      	       solns =solveBlackBox(system);
	       numSolns = #solns
	       solns/print
	  Text
	       We see that there are three solutions to the above system. In addition to the values of the three variables, the hashtable for each solution contains the following keys for 
	       diagnostics: err, the magnitude of the last correction term used in Newton's method; mult, the multiplicity of the solution;
	       rco, estimate for the inverse condition of the root; residual, the maginitude of the system when evaluated at the given solution; and time,
	       the end value of the continuation parameter, if t=1 then the solver reached the end of the path properly.
	  Example
	       vol = mixedVolume(system)
	       vol == 3 --in this case the mixed volume bound is sharp     

          
     Caveat
	       {\bf 1.} If you are having trouble installing the package, check whether the path to your PHCpack executable was set correctly. 
	       You can check this by typing the following command:
	       
	       options PHCpack 
	       
	       If it is wrong, you can update it by putting the absolute path into the  init-PHCpack.m2 file,
	       which is located in Library/Application Support/Macaulay2 .    
     	       For example, if PHC executable is located in C:/cygwin/PHC, then the line 
	       inside the {\tt init-PHCpack.m2} file will look like this: 
	       
	        "path" => "C:/cygwin/PHC/" .
	       
	       Alternately, the path could be set when loading the package using the following command:
     	       
	       loadPackage ("PHCpack", Configuration=>{"path"=>"C:/cygwin/PHC/","PHCexe"=>"./phc"}) 

      	       {\bf 2.} If the package SimpleDoc is not found when installing {\tt PHCpack.m2}, 
	       see questions and answers 6, 7, and 8 on the Macaulay2 web site.
///;

-----------------------------------
---- Documenting solveBlackBox ----
-----------------------------------
doc ///
     Key
     	  solveBlackBox
          (solveBlackBox, List)
     Headline
     	  a black-box solver, which returns approximations to all complex isolated roots of a polynomial system; invokes "phc -b" from PHCpack
     Usage
     	  solveBlackBox(S)
     Inputs
      	  S:List
	       whose entries are the polynomials of a 0-dimensional system	       
     Outputs
     	  L:List 
	       whose entries are lists corresponding to the solutions of the polynomial system; 
	       for each list, the first entry is a list of the values of the variables and the second entry is a hashtable 
     Description
     	  Text
	       Suppose we would like to compute the numerical solutions to the following system which lives in the polynomical ring with 3 variables.
	  Example
	       R=QQ[x,y,z]
	       S={x+y+z-1,x^2+y^2,x+y-z-3}
	  Text
	       We call PHCpack's blackbox solver.
	  Example
	       printWidth = 300 -- want output to fit on one line
	       L=solveBlackBox(S)
	  Text
	       Notice that in addition to the values of the three variables, the hashtable for each solution contains the following keys for 
	       diagnostics: err, the magnitude of the last correction term used in Newton's method; mult, the multiplicity of the solution;
	       rco, estimate for the inverse condition of the root; residual, the maginitude of the system when evaluated at the given solution; and time,
	       the end value of the continuation parameter, if t=1 then the solver reached the end of the path properly.
	  Text
	       {\bf Ok, here comes some more text that is not completely done, and may be redundant:     }

	       {\bf DISCUSSION ON DIMENSION:  }
	       
	       THE FOLLOWING 2 I'M NOT EXECUTING SINCE THEY RETURN AN ERROR;
	       BUT I DO WANT THEM DOCUMENTED! :
	       --solveBlackBox(flatten entries mingens I)
	       --break
	       this is because our code does not check for dimension, it checks for number of equations instead. 
	       the dimension computation can take forever (as Liz already noted!)  and should not be used. 
	       
	       THE PROBLEM IS if the dimension check is not there, the code will *freak out* 
	       (gives prompt for phc inside m2!!!) ---try it, using the 2 commands above!
	       
	       SOLUTION: this will really only happen if some equations are redundant;
	       one way to solve this is tell the user to make the system and use only minimal generators
	       of the ideal :-)
	        QUESTION: do we do this inside the package, OR  do we let the user know how to do it
	        inside the documentaton??
	        I vote for the latter because I don't want the package 
	        to perform "mingens" or "trim" if it doesn't have to!!
	       
	       Here is a second system which is square, but has a free variable anyway (x) :
	  Example
	       I=ideal(y-x^2,z-x^3,x^2-y)
	       dim I 
	  Text
	       the system is not zero-dimensional (there is a free variable!!); but the code does not know that;
	       since we have the system is ``square''...
	  Example
	       system = flatten entries gens I
	       vol = mixedVolume(system) --this returns zero, but that's not informative!
	       solveBlackBox(system) 
	  Text
	       however, i get a different error message when i prune the generators:
	  Example
	       numgens I
	       numgens ring I
	       numcols mingens I --not all generators are minimal.
	  Text
	       Thus, if you are not sure if you have a *truly* square (or overdetermined system), that is, 
	       if you want to make sure the system is not positive dimensional (underdetermined),
	       you can check this by getting rid of the non-minimal generators of the ideal
	       (note: i use "mingens" which returns a matrix; I could have used "trim" which returns an ideal)
	       
	       In case we need slack variables: 
	  Example
	       dim trim I
	  Text
	       Also, if the system is overdetermined, then the code inserts slack variables, so it still works:
	  Example
	       system={y-x^2, z-x^3,x+y+z-1,x+y+ x^3 -1}
	       #system > numcols vars R --overdetermined system
	       solns =solveBlackBox(system); --but code still works (slack vars)
	       numSolns = #solns
///;

-----------------------------------
----- Documenting mixedVolume -----
-----------------------------------
doc ///
     Key
     	  mixedVolume
          (mixedVolume, List)
     Headline
     	  mixed volume of a polynomial system; invokes "phc -m" from PHCpack
     Usage
     	  mixedVolume(S)
     Inputs
      	  S:List
	       whose entries are the polynomials of a 0-dimensional system	       
     Outputs
     	  m:ZZ 
	       mixed volume of the system 
     Description
     	  Text
	       The mixed volume of a polynomial system provides an upper bound on the number of complex isolated roots 
	       without zero components.
	       
	       When the coefficients of the system are sufficiently generic, the bound is sharp. 
	  Example
	       R=QQ[x,y,z]
	       S={y-x^2,z-x^3,x+y+z-1}
	       m=mixedVolume(S)     
///;

-----------------------------------
----- Documenting refineSoln ------
-----------------------------------
doc ///
     Key
     	  refineSoln
          (refineSoln, List)
     Headline
     	  refines the solutions of a system; invokes "phc -????" from PHCpack
     Usage
     	  refineSoln(solutions)
     Inputs
      	  solutions:List
	       whose entries are the solutions .........
     Outputs
     	  s:List 
	       refined solutions of the system....... 
     Description
     	  Text
	       Describe what is going on; and insert an example below.......
	  Example
	       R=QQ[x,y,z]
	       S={y-x^2,z-x^3,x+y+z-1}--this is not a good example; insert the one we were playing with @office last month.     
///;

-----------------------------------
----- Documenting convertToPoly ------
-----------------------------------
doc ///
     Key
     	  convertToPoly
          (convertToPoly, List)
     Headline
     	  converts a rational system to a Laurent polynomial system
     Usage
     	  convertToPoly(system)
     Inputs
      	  system:List
	       of rational expressions
     Outputs
     	  s:List 
	       the same system converted to a Laurent polynomial system
     Description
     	  Text 
	       Here is an example when this function will be used:
	  Example
	       QQ[x,y,z];
	       sys={y-x^2, z-x^3, (x+y+z-1)/x};
     	       describe ring ideal sys --"there are denominators! i will call the conversion method."
	       convertedSys = convertToPoly(sys);
     	  Text
	       convertedSys is an equivalent system living in a Laurent polynomial ring. For each denominator, a new variable was created.
	  Example
	       printWidth = 300;
	       toString convertedSys
	       ring ideal convertedSys
     	       describe oo --that this is a ring with negative exponents allowed is evident from the option "Inverses=>true".
  	  Text
	       Note that if the system is already polynomial, or in Laurent polynomial form, the method doesn't change it.
	       Of course, sometimes it is possible that it is polynomial "in disguise" as in the following example:
	  Example
     	       P=QQ[x,y,z];
	       f=(x*y + z^2*y) / y 
	       liftable(f,P) 
	  Text
	       But, the method detects this and simplifies the system accordingly. Instead of creating Laurent polynomials, 
	       it simply updates the system using the following:
	  Example
	       lift(f,P)
     	  Text	       
	  
	       This method is called by @TO solveBlackBox @.
     	  
	  Text
	  
	       {\bf ERASE ME later: } this might be useful to know: <<<---- NOTE FOR THE DEVELOPERS
	  Example
	       QQ[x,y,z]
      	       denominator(x^2+y/z+z/x^3) --returns the common denominator, of course.

///;


---------------------------------------------------------------------------------------
--*********************************** TESTS *****************************************--
---------------------------------------------------------------------------------------

-----------------------------------
-- Testing mixedVolume
-----------------------------------
TEST/// 
     loadPackage "PHCpack"    
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
-- Testing blackBox
-----------------------------------
TEST/// 
     loadPackage "PHCpack"    
     R=QQ[x,y,z]
     S={x^2-y*z-3,y^2-x*z-4,z^2-x*y-5}
     L=solveBlackBox(S)
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
-- Testing nameOfFunction....INSERT meaningful tests for *each* function in the package!
-----------------------------------


end   	   --********************************************************
----------------------------------------------------------------------
----------------------------------------------------------------------



restart
uninstallPackage "PHCpack"
installPackage("PHCpack", RemakeAllDocumentation=>true, UserMode=>true,DebuggingMode => true)

viewHelp PHCpack


options PHCpack


---!!!!!!!!!!!!!!-------------------------------------------------
 -- please keep this line as it is:
loadPackage ("PHCpack", Configuration=>{"path"=>"/Users/petrovic/PHCpack/./phc"})
 -- make a new line if you need a different path. 
 -- thanks. ~~sonja
---!!!!!!!!!!!!!!-------------------------------------------------
loadPackage ("PHCpack", Configuration=>{"path"=>"/Users/petrovic/","PHCexe"=>"./phc"})

options PHCpack --to check the configuration


help PHCpack

viewHelp PHCpack


check PHCpack

debug PHCpack







----------------------------------------------------
----------------------------------------------------
QQ[x,y]
g=x+y
myRing = newRing( ring g, Inverses=>true,MonomialOrder=>RevLex) --<<<<<<--- THIS IS HOW TO CHANGE SOME OPTIONS ON SAME RING!!!
g=sub(g,myRing)
g+x^-1
toString(oo) --<<<< ---- THIS is what PHCpack will understand!
q = y+x^-1
ring q
denominator(q)
numerator(q)





