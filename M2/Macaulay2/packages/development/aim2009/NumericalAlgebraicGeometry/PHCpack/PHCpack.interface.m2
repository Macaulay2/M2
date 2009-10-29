-- PHCpack interface for NAG4M2
-- used by ../NumericalAlgebraicGeometry.m2

solvePHCpack = method(TypicalValue => List)
solvePHCpack (List,HashTable) := List => (F,o) -> (
     -- assume the ideal is 0-dim. 
     -- !!! problem with temporaryFileName: cygwin's /tmp is different from Windows' /tmp 
     tarsolfile := temporaryFileName() | 
     "PHCtasols";
     targetfile := temporaryFileName() | 
     "PHCtarget";
     outfile := temporaryFileName() | 
     "PHCoutput";
     {tarsolfile, targetfile, outfile} / (f->if fileExists f then removeFile f);
     -- writing data to the corresponding files                                                                                                                                                                           
     systemToFile(F,targetfile);
     -- launching blackbox solver; converting the solutions to the Maple format                                                                                                                                           
     run(PHCexe|" -b "|targetfile|" "|outfile);
     run(PHCexe|" -z "|targetfile|" "|tarsolfile);
     -- parse and output the solutions                                                                                                                                                                                    
     result = parseSolutions(tarsolfile, ring first F);
     -- clean up                                                                                                                                                                                                          
     if DBG<10 then (
	  removeFile targetfile;
	  removeFile tarsolfile; 
	  removeFile outfile;
	  );
     result
     )

trackPHCpack = method(TypicalValue => List)
trackPHCpack (List,List,List,HashTable) := List => (S,T,solsS,o) -> (
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

refinePHCpack = method(TypicalValue => List, Options => {
	  ResidualTolerance => 0, 
	  ErrorTolerance => 1e-10,
	  Iterations => null,
	  Bits => 300
	  }
	  )
refinePHCpack (List,List) := List => o -> (T,sols) -> (
     -- T is a list of polynomials over CC
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

-- service functions ------------------------------------------
systemToFile = method(TypicalValue => Nothing)
systemToFile (List,String) := (F,name) -> (
     file := openOut name;
     file << #F << endl;
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
///
restart
loadPackage "NumericalAlgebraicGeometry"; debug NumericalAlgebraicGeometry;
R = CC[x,y,z]
solutionsToFile( {(0,0,1),(0,0,-1)}, R, "PHCsols" )
///

parseSolutions = method(TypicalValue => Sequence,
     Options => {Bits => 53})
parseSolutions (String,Ring) := o -> (s,R) -> (
-- parses solutions in PHCpack format 
-- IN:  s = string of solutions in PHCmaple format 
--      V = list of variable names
-- OUT: {list of solutions, list of multiplicities}
     oldprec := defaultPrecision;
     defaultPrecision = o.Bits;
     L := get s;
     L = replace("=", "=>", L);
     L = replace("I", "ii", L);
     L = replace("E\\+","e",L);
     L = replace("E", "e", L);
     L = replace("time", "\"time\"", L);
     L = replace("multiplicity", "\"mult\"", L);
     
     use R; 	  
     sols := toList apply(value L, x->new HashTable from toList x);
     defaultPrecision = oldprec;
     sols/(x->{apply(gens R, v->x#v), x})
     )

///
restart
debug loadPackage "NumericalAlgebraicGeometry"

R = CC[x]
L = {x^2-2}
refinePHCpack(L, {{1.7}}, Iterations => 10, Bits => 400, ErrorTolerance => 1p400e-130)

R = CC_200[x,y,z]
L = {y-x^2,z-x^3,x+y+z-1}
B = solveSystem(L,Software=>PHCpack)
B = B/first
C = apply(B, b -> refinePHCpack(L, {b}, Iterations => 10, Bits => 400, ErrorTolerance => 1p400e-130))
C/first/first

-- Using higher precision
R = CC_53[x,y,z]
R200 = CC_200[x,y,z]
L = {y-x^2,z-x^3,x+y+z-.5p200}
B = solveSystem(L,Software=>PHCpack)
B = solveSystem(L)
pt = B_0_0

C = refinePHCpack(L, {pt}, Iterations => 10, Bits => 400, ErrorTolerance => 1p400e-130)
pt1 = C_0_0
pt_0
pt1_0
///
