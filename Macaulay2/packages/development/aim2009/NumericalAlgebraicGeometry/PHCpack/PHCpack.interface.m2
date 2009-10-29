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

generalEquations = method()
generalEquations WitnessSet := (W) -> (
     -- change the equations to be general change of vars, if not a CI
     -- the output is a new witness set, with the same points and slice.
     R := ring W;
     n := numgens R;
     d := dim W;
     ngens := numgens ideal W;
     if ngens === n-d then W
     else (
	  -- take random combinations of the equations
	  neweqns := (generators ideal W) * random(R^ngens, R^(n-d));
	  witnessSet(ideal neweqns, ideal W.Slice, W.Points))
     )

addSlackVariables = method()
addSlackVariables WitnessSet := (W) -> (
     -- creates a new system of polynomials, in variables:
     -- old set of variables, and zz1, ..., zzd, where
     -- d is the dimension of W.
     R := ring W;
     n := numgens R;
     d := dim W; -- this will be the number of slack variables to add
     W1 := generalEquations W;
     -- Add in new variables zz1, ..., zzd,
     --  this changes the equations, the slice, and the points
     slackvars := apply(d, i->getSymbol("zz"|toString (i+1)));
     newR := (coefficientRing R)[gens R, slackvars];
     newvars := (vars newR)_{n..n+d-1};
     -- new slice:
     newSlice := apply(d, i -> sub(W1.Slice#i,newR) + newR_(n + i));
     -- add a linear matrix 
     A := random(newR^(d),newR^(n-d));
     AZ := transpose newvars * A;
     newEqns := (sub(gens ideal W1, newR) + AZ) | newvars;
     -- new points
     zeros := toList apply(d, i -> 0_(coefficientRing R));
     newPoints := apply(W1.Points, pt -> join(pt,zeros));
     witnessSet(ideal newEqns, ideal newSlice, newPoints))

monodromyBreakupPHC = method(Options => {})
monodromyBreakupPHC WitnessSet := o -> (W) -> (
     -- Input: a witness set (i.e. numerical equidimensional set)
     -- Output: a list of witness sets, probably the irreducible
     --  decomposition of W.
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
     L = replace("res", "\"res\"", L);
     
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

///
-- WitnessSet and monodromyBreakupPHC
restart
debug loadPackage "NumericalAlgebraicGeometry"

R = QQ[x,y,z]
I = ideal"x+y-2,y-z+3"
J = ideal"x2+y2+z2-1,xy-z2"
L = trim intersect(I,J)
RC = CC[gens R]
L = sub(L,RC)
W = witnessSet L
W1 = generalEquations W
W2 = addSlackVariables W1
monodromyBreakupPHC W2
peek W2
see ideal W2
peek oo
///