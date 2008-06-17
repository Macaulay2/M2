newPackage(
	"NAG",
    	Version => "1.0", 
    	Date => "04/01/2008",
    	Authors => {
	     {Name => "Anton Leykin", Email => "leykin@ima.umn.edu"}
	     },
    	HomePage => "http://www.ima.umn.edu/~leykin",
    	Headline => "Numerical Algebraic Geometry",
	Configuration => { "PHCpack" => "phc",  "Bertini" => "bertini", "HOM4PS2" => "hom4ps2" },	
	-- DebuggingMode should be true while developing a package, 
	--   but false after it is done
    	DebuggingMode => true 
    	)

-- Any symbols or functions that the user is to have access to
-- must be placed in one of the following two lists
export {
     solveSystem, track
     }
exportMutable {
     }

-- GLOBAL VARIABLES ----------------------------------
PHCexe = NAG#Options#Configuration#"PHCpack";
BERTINIexe = NAG#Options#Configuration#"Bertini";
HOM4PS2exe = NAG#Options#Configuration#"HOM4PS2";

DBG = 0; -- debug level (10=keep temp files)

-- M2 tracker ----------------------------------------

track = method(TypicalValue => List, Options =>{
	  gamma=>1, 
	  tStep=>0.1, 
	  Predictor=>RungeKutta4, 
	  Projectivize=>true, 
	  AffinePatches=>{},
	  RandomSeed=>0})
track (List,List,List) := List => o -> (S,T,solsS) -> (
-- tracks solutions from start system to target system
-- IN:  S = list of polynomials in start system
--      T = list of polynomials in target system
--      solsS = list of solutions to S
-- 	gamma => nonzero complex number
-- OUT: solsT = list of target solutions corresponding to solsS
     n := #T; 
     if n > 0 then R := ring first T else error "expected nonempty target system";
     if #S != n then 
     error "expected same number of polynomials in start and target systems";
     if any(S, f->ring f != R) or any(T, f->ring f != R)
     then error "expected all polynomials in the same ring";
     if n != numgens R then error "expected a square system";
     if o.tStep <= 0 then "expected positive tStep";  

     solsS = solsS / (s->sub(transpose matrix {toList s}, CC)); -- convert to vectors
     
     if o.Projectivize then (
	  h := symbol h;
	  R = CC[gens R | {h}]; 
	  n = numgens R;
     	  
	  patches := { 	    
	       promote(matrix{{append((n-1):0, 1)}},CC), -- dehomogenizing patch
	       if #o.AffinePatches > 0 then first o.AffinePatches -- either provided patch...
	       else ( 
		    setRandomSeed o.RandomSeed; 
		    matrix{apply(n, i->exp(random(0.,2*pi)*ii))} ) -- ... or random patch
	       };
	  patches = patches | { o.gamma*patches#1 };
     	            
	  if DBG>1 then << "affine patch: " << toString patches#1 <<endl;
     	  
     	  -- affine patch functions 
     	  pointToPatch := (x0,p)-> (1/(p*x0)_(0,0))*x0; -- representative for point x0 in patch p
	  patchEquation := p -> p * transpose vars R - 1;
	  
	  T = apply(T, f->homogenize(sub(f,R), h)) | {patchEquation patches#1};
	  S = apply(S, f->homogenize(sub(f,R), h)) | {patchEquation patches#2};
	  solsS = solsS / (s->pointToPatch(s||matrix{{toCC 1}}, patches#2));
	  ); 
     
     -- create homotopy
     t := symbol t;
     Rt := CC[gens R, t]; -- how do you cleanly extend the generators list: e.g., what if "n" is a var name?
     H := matrix {apply(n, i->(1-t)*sub(S#i,Rt)+o.gamma*t*sub(T#i,Rt))};
     JH := transpose jacobian H; 
     Hx := JH_(toList(0..n-1));
     Ht := JH_{n};
     
     -- evaluation functions
     evalH := (x0,t0)-> lift(sub(transpose H, transpose x0 | matrix {{t0}}), CC);
     evalHx := (x0,t0)-> lift(sub(Hx, transpose x0 | matrix {{t0}}), CC);
     evalHt := (x0,t0)-> lift(sub(Ht, transpose x0 | matrix {{t0}}), CC);
     evalMinusInverseHxHt := (x0,t0)-> -(inverse evalHx(x0,t0))*evalHt(x0,t0);
          
     
     -- threshholds and other tuning parameters
     epsilon := 1e-5; -- tracking tolerance (universal)
     divThresh := 1e4; 
     condNumberThresh := 1e3;
     stepDecreaseFactor := 0.5;
     stepIncreaseFactor := 2.0;
     numberSuccessesBeforeIncrease := 4;
     tStepMin := 1e-2;
     theSmallestNumber := 1e-12;
     -- corrector tuning
     maxCorSteps := 4;
      
     rawSols := apply(solsS, s->(
	       if DBG > 1 then << "tracking solution " << toString s << endl;
     	       tStep := o.tStep;
	       predictorSuccesses := 0;
	       x0 := s; 
	       t0 := toCC 0.; 
     	       history := {t0=>x0};
	       while x0 =!= infinity and 1-t0 > theSmallestNumber do (
		    if DBG > 3 then 
		    << "t0=" << t0 << endl;
		    -- evaluate the jacobian Hx, change patch if the condition number is large
		    Hx0 := evalHx(x0,t0);
		    --svd := sort first SVD Hx0;
		    --if o.Projectivize and first svd / last svd > condNumberThresh then ( 
     	       	    --	 << "CONDITION NUMBER = " <<  first svd / last svd << endl;			 
		    --	 );
		    -- predictor step
		    local dx, dt, t1;
		    if o.Predictor == Tangent then (
		    	 Ht0 := evalHt(x0,t0);
	     	    	 invHx0 := inverse Hx0;
		    	 invHx0timesHt0 := invHx0*Ht0;
		    	 -- if norm invHx0timesHt0 > divThresh then (x0 = infinity; break);
		    	 tStepSafe := 1; -- 1/(norm(Hx0)*norm(invHx0)); -- what is a good heuristic?
		    	 dt = min(max(min(tStep,tStepSafe),tStepMin), 1-t0);
		         dx = -dt*invHx0timesHt0;
			 ) 
		    else if o.Predictor == Euler then (
			 H0 := evalH(x0,t0);
			 Ht0 = evalHt(x0,t0);
	     	    	 invHx0 = inverse Hx0;
			 dt = min(tStep, 1-t0);
			 dx = -invHx0*(H0+Ht0*dt);
			 )
		    else if o.Predictor == Secant then (
			 dt = min(tStep, 1-t0);
			 if #history > 1 then ( -- if there is a preceding point
			      u := x0 - history#(#history-2)#1;
			      dx = (dt/sqrt(sum(flatten entries u,x->x^2)))*u;
     			      )			      
			 else ( -- use tangential predictor
			      Ht0 = evalHt(x0,t0);
	     	    	      invHx0 = inverse Hx0;
			      dx = -dt*invHx0*Ht0;
			      );
			 )
		    else if o.Predictor == RungeKutta4 then (
			 dt = min(tStep, 1-t0);
			 Ht0 = evalHt(x0,t0);
	     	    	 invHx0 = inverse Hx0;
			 k1 := -dt*invHx0*Ht0;
			 k2 := dt*evalMinusInverseHxHt(x0+.5*k1,t0+.5*dt);
			 k3 := dt*evalMinusInverseHxHt(x0+.5*k2,t0+.5*dt);
			 k4 := dt*evalMinusInverseHxHt(x0+k3,t0+dt);
			 dx = (1/6)*(k1+2*k2+2*k3+k4);
			 )
		    else error "unknown Predictor";
		    		    
    	 	    t1 = t0 + dt;
		    x1 = x0 + dx;
		    
		    -- corrector step
		    dx = 1; -- dx = + infinity
		    nCorSteps := 0;
		    while norm dx > epsilon 
		          and nCorSteps < (if 1-t1 < theSmallestNumber and dt < tStepMin 
			                   then 11 -- infinity
			                   else maxCorSteps) 
		    do ( 
			 if norm x1 > divThresh then (x1 = infinity; break);
			 if DBG > 3 then << "x=" << toString x1 << " res=" <<  toString evalH(x1,t1) << " dx=" << dx << endl;
			 dx = - (inverse evalHx(x1,t1))*evalH(x1,t1);
			 x1 = x1 + dx;
			 nCorSteps = nCorSteps + 1;
			 );
		    if dt > tStepMin and nCorSteps == maxCorSteps then ( 
			 predictorSuccesses = 0;
	 	 	 tStep = stepDecreaseFactor*tStep;
			 if DBG > 2 then << "decreased tStep to "<< tStep << endl;	 
			 ) 
		    else (
			 predictorSuccesses = predictorSuccesses + 1;
			 if nCorSteps <= maxCorSteps - 1 -- over 2 / minus 2 ???
              		    and predictorSuccesses >= numberSuccessesBeforeIncrease 
			 then (			      
			      tStep = stepIncreaseFactor*tStep;	
			      if DBG > 2 then << "increased tStep to "<< tStep << endl;
			      );
		         x0 = x1;
		         t0 = t1;
		         history = append(history, t0=>x0);
			 );
		    );        	    
	       x0
	       ));
     
     if o.Projectivize then (
	  print rawSols;
	  return select(
	       apply(select(rawSols,s->s=!=infinity), s->(
		    	 s = flatten entries s;
		    	 --if norm(last s) < epsilon then infinity 
		    	 --else 
			 apply(drop(s,-1),u->(1/last s)*u)
			 )
	       	    ),
	       s->s=!=infinity);
	  )
     -- consolidate clusters (perhaps, should not be done)
     else (
	  solsT = new MutableHashTable;
     	  scan(select(rawSols,s->s=!=infinity), 
	       r->if (p:=position(keys solsT, s->norm(s-r)<epsilon))===null 
	       then solsT#r = 1  
	       else ( k:=(keys solsT)#p;  solsT#k = solsT#k + 1 )
	       );
          return { (keys solsT)/flatten@@entries, values solsT };
	  );
     )

refine = method(TypicalValue => List, Options =>{epsilon=>1e-8, maxCorSteps=>30})
refine (List,List) := List => o -> (T,solsT) -> (
-- tracks solutions from start system to target system
-- IN:  T = list of polynomials in target system
--      solsT = list of solutions to T
-- OUT: solsR = list of refined solutions 
     n := #T; 
     if n > 0 then R := ring first T else error "expected nonempty target system";
     if n != numgens R then error "expected a square system";
          
     T = matrix {T};
     J = transpose jacobian T; 
     evalT := x0 -> lift(sub(transpose T, transpose x0), CC);
     evalJ := x0 -> lift(sub(J, transpose x0), CC);
     
     solsT = solsT / (s->sub(transpose matrix {toList s}, CC)); -- convert to vectors
     solsR := apply(solsT, s->(
	       x1 := s; 
	       -- corrector step
	       dx = 1; -- dx = + infinity
	       nCorSteps := 0;
	       while norm dx > o.epsilon and nCorSteps < o.maxCorSteps do ( 
		    if DBG > 3 then << "x=" << toString x1 << " res=" <<  toString evalT(x1) << " dx=" << dx << endl;
		    dx = - (inverse evalJ(x1))*evalT(x1);
		    x1 = x1 + dx;
		    nCorSteps = nCorSteps + 1;
		    );
	       x1
	       ));
     solsR / (s -> flatten entries s)      
     )     

totalDegreeStartSystem = method(TypicalValue => Sequence)
totalDegreeStartSystem List := Sequence => T -> (
-- contructs a total degree start system and its solutions 
-- for the given target system T
-- IN:  T = list of polynomials 
-- OUT: (S,solsS}, where 
--      S     = list of polynomials, 
--      solsS = list of sequences
     R := ring first T;
     S := apply(numgens R, i->R_i^(first degree T_i)-1);
     s := apply(numgens R, i->( 
	  d = first degree T_i; 
	  set apply(d, j->exp(ii*2*pi*j/d))
	  ));
     solsS := first s;
     scan(drop(s,1), t->solsS=solsS**t);
     solsS = toList solsS/deepSplice; 
     (S, solsS)
     )     

-- INTERFACE part ------------------------------------
solveSystem = method(TypicalValue => List, Options =>{Software=>M2})
solveSystem List := List => o -> F -> (
-- solves a system of polynomial equations
-- IN:  F = list of polynomials
--      Software => {PHCpack, Bertini, hom4ps2}
-- OUT: {s,m}, where 
--             s = list of solutions 
--     	       m = list of corresponding multiplicities	 
     local result;
     R := ring F#0;
     v := flatten entries vars R;
     if o.Software == M2 then ( 
	  if all(F, f -> first degree f <= 1)
     	  then ( 
	       A := matrix apply(F, f->apply(v, x->coefficient(x,f)));
	       b := matrix apply(F, f->{coefficient(1_R,f)});
	       solve(A,b)
	       )
	  else (
	       (S,solsS) := totalDegreeStartSystem F;
	       track(S,F,solsS,gamma=>exp(random(0.,2*pi)*ii))
	       )
	  )
     else if o.Software == PHCpack then ( 
	  -- assume the ideal is 0-dim. 
	  -- !!! problem with temporaryFileName: cygwin's /tmp is different from Windows' /tmp 
	  tarsolfile := -- temporaryFileName() | 
	                "tasols";
	  targetfile := -- temporaryFileName() | 
	             "target";
	  outfile := -- temporaryFileName() | 
	             "output";
	  -- writing data to the corresponding files                                                                                                                                                                           
	  systemToFile(F,targetfile);
	  -- launching blackbox solver; converting the solutions to the Maple format                                                                                                                                           
	  run(PHCexe|" -b "|targetfile|" "|outfile);
	  run(PHCexe|" -z "|targetfile|" "|tarsolfile);
	  -- parse and output the solutions                                                                                                                                                                                    
	  result = parseSolutions(tarsolfile, gens R);
	  -- clean up                                                                                                                                                                                                          
	  if DBG<10 then (
	       removeFile targetfile;
	       removeFile tarsolfile; 
	       removeFile outfile;
	       )
	  )
     else if o.Software == Bertini then (
	  -- tempdir := temporaryFileName() | "NAG-bertini";
	  -- mkdir tempdir; 	  
  	  makeBertiniInput(gens R, F);
  	  run(BERTINIexe);
	  sols := readSolutionsBertini({"finite_solutions"});
	  result = {sols, toList(#sols:1)};
	  -- remove Bertini input/output files
    	  for f in {"failed_paths", "nonsingular_solutions",
               "raw_data", "start", "input", "output", "raw_solutions",
               "main_data", "real_finite_solutions", "finite_solutions",
               "midpath_data", "singular_solutions", "real_solutions",
               "singular_solutions", "midpath_data"} do
          if fileExists f then removeFile f;
	  )
     else if o.Software == hom4ps2 then (
	  -- newR := coefficientRing R[xx_1..xx_(numgens R)];
	  (name, p) := makeHom4psInput(R, F);
	  targetfile = name; --(map(newR, R, gens newR)\F);
	  tarsolfile = temporaryFileName() | 
	                "tasols";
 	  run(HOM4PS2exe|" "|targetfile|" "|tarsolfile);
	  sols = readSolutionsHom4ps(tarsolfile, p);
	  result = {sols, toList(#sols:1)};
	  if DBG<10 then (
	       removeFile targetfile;
	       removeFile tarsolfile; 
	       )
	  )
     else error "invalid Software option";  		
     result
     )

-- PHC part ------------------------------------------
systemToFile = method(TypicalValue => Nothing)
systemToFile (List,String) := (F,name) -> (
     file := openOut name;
     file << #F << endl;
     scan(F, f->( 
	       file << toString f << ";" << endl;
	       ));
     close file;
     ) 

parseSolutions = method(TypicalValue => Sequence)
parseSolutions (String,List) := (s,V) -> (
-- parses solutions in PHCpack format 
-- IN:  s = string of solutions in PHCmaple format 
--      V = list of variable names
-- OUT: {list of solutions, list of multiplicities}
     L := get s;
     L = replace("=", "=>", L);
     L = replace("I", "ii", L);
     L = replace("E", "e", L);
     L = replace("time", "\"time\"", L);
     L = replace("multiplicity", "\"mult\"", L);
	  
     sols := apply(value L, x->new HashTable from toList x);
     mults := apply(sols, x->x#"mult");
     coords := apply(sols, x->apply(V, v->x#v));
     {coords, mults}     
     )

-- HOM4PS2 part -----------------------------------------------------------

makeHom4psInput = method(TypicalValue=>Sequence)
makeHom4psInput (Ring, List) := (R, T) -> (
-- IN:  R = ring
--      T = polynomials of target system (in R)
-- OUT: (name, perm), where
--      name = input filename   
--      perm = hashtable of order of appearences of variables in the input
  filename := temporaryFileName() | "input"; 
  s := "{\n";
  scan(T, p -> s = s | toString p | ";\n");
  s = s | "}\n";
  f := openOut filename; 
  f << s;
  close f;
  -- assume names of vars are not substrings of each other
  p := sort apply(numgens R, i->(first first regex(toString R_i, s), i));
  print p;
  ( filename, new HashTable from apply(#p, i->p#i#1=>i) )
  )

readSolutionsHom4ps = method(TypicalValue=>List)
readSolutionsHom4ps (String, HashTable) := (f,p) -> (
-- IN:  f = output filename
--      p = permutation of coordinates to be applied (hashtable)
-- OUT: list of solutions
  s := {};
  l := lines get f;
  i := 0; -- line counter
  while #l#i > 2 do ( -- current line is non-empty	   
       coords := {};
       while #l#i > 2 do ( -- until an empty line
	    a := select(separate(" ",  cleanupOutput(l#i)), t->#t>0);
	    coords = coords | {(value a#0)+ii*(value a#1)};
	    i = i + 1;
       	    );
       if DBG>=10 then << coords << endl;
       s = s | { apply(#coords, i->coords#(p#i)) };
       i = i + 4; -- skip to the next solution
       );
  s
  )
  
makeBertiniInput = method(TypicalValue=>Nothing, Options=>{S=>{}})
makeBertiniInput (List, List) := o -> (v,T) -> (
-- IN:
--	v = variables
--	T = polynomials of target system
--      o.S = start system
  f := openOut "input"; -- THE name for Bertini's input file 
  f << "CONFIG" << endl;
  f << "MPTYPE: 2;" << endl; -- multiprecision
  if #o.S > 0 then
    f << "USERHOMOTOPY: 1;" << endl;
  f << endl << "END;" << endl << endl;
  f << "INPUT" << endl << endl;
  if #o.S > 0 then
    f << "variable "
  else f << "variable_group "; -- variable section
  scan(#v, i->
       if i<#v-1 
       then f << toString v#i << ", "
       else f << toString v#i << ";" << endl
       );
  f << "function "; -- "function" section
  scan(#T, i->
       if i<#T-1
       then f << "f" << i << ", "
       else f << "f" << i << ";" << endl << endl
      );
  if #o.S == 0 
  then scan(#T, i -> f << "f" << i << " = " << toString T#i << ";" << endl)
  else (
       if #o.S != #T then error "expected equal number of equations in start and target systems";
       f << "pathvariable t;" << endl 
         << "parameter s;" << endl
         << "s = t;" << endl;
       scan(#T, i -> f << "f" << i 
	    << " = (" << toString T#i << ")*(1-s)+s*(" << toString o.S#i << ");" << endl 
	   );
       );
  f << endl << "END" << endl << endl;
  close f;
  )

cleanupOutput = method(TypicalValue=>String)
cleanupOutput String := s -> (
-- cleanup output (Bertini and hom4ps2)
  t := replace("E", "e", s);
  t = replace("[(,)]","", t);
  t = replace("e\\+","e",t)
  )

readSolutionsBertini = method(TypicalValue=>List)
readSolutionsBertini List := ofiles -> (
  s := {};
  for f in ofiles do (
    l := lines get f;
    nsols := value first separate(" ", l#0);
    l = drop(l,2);
    while #s < nsols do (
	 if DBG>=10 then << "sol[<<" << temp << "] --------------------" << endl;
	 coords := {};
	 while #(first l) > 2 do ( 
	      coords = coords | {(
		   	a := separate(" ",  cleanupOutput(first l));	 
		   	(value a#0)+ii*(value a#1)
	      	   	)};
    	      l = drop(l,1);
	      );
	 l = drop(l,1);
         if DBG>=10 then << coords << endl;
	 s = s | {coords};
	 );     
    );
  s
  )

-- A function with an optional argument

{*
secondFunction = method(
     TypicalValue => ZZ,
     Options => {MyOption => 0}
     )
secondFunction(ZZ,ZZ) := o -> (m,n) -> (
     if not instance(o.MyOption,ZZ)
     then error "The optional MyOption argument must be an integer";
     m + n + o.MyOption
     )
secondFunction(ZZ,List) := o -> (m,n) -> (
     if not instance(o.MyOption,ZZ)
     then error "The optional MyOption argument must be an integer";
     m + #n + o.MyOption
     )

beginDocumentation()
document { 
	Key => NAG,
	Headline => "Numerical Algebraic Geometry",
	EM "PackageTemplate", " is an example package which can
	be used as a template for user packages."
	}
document {
	Key => {firstFunction, (firstFunction,ZZ)},
	Headline => "a silly first function",
	Usage => "firstFunction n",
	Inputs => {
		"n" => ZZ => {}
		},
	Outputs => {
		String => {}
		},
	"This function is provided by the package ", TO PackageTemplate, ".",
	EXAMPLE {
		"firstFunction 1",
		"firstFunction 0"
		}
	}
document {
	Key => secondFunction,
	Headline => "a silly second function",
	"This function is provided by the package ", TO PackageTemplate, "."
	}
document {
	Key => (secondFunction,ZZ,ZZ),
	Headline => "a silly second function",
	Usage => "secondFunction(m,n)",
	Inputs => {
	     "m" => {},
	     "n" => {}
	     },
	Outputs => {
	     {"The sum of ", TT "m", ", and ", TT "n", 
	     ", and "}
	},
	EXAMPLE {
		"secondFunction(1,3)",
		"secondFunction(23213123,445326264, MyOption=>213)"
		}
	}
document {
     Key => MyOption,
     Headline => "optional argument specifying a level",
     TT "MyOption", " -- an optional argument used to specify a level",
     PARA{},
     "This symbol is provided by the package ", TO PackageTemplate, "."
     }
document {
     Key => [secondFunction,MyOption],
     Headline => "add level to result",
     Usage => "secondFunction(...,MyOption=>n)",
     Inputs => {
	  "n" => ZZ => "the level to use"
	  },
     Consequences => {
	  {"The value ", TT "n", " is added to the result"}
	  },
     "Any more description can go ", BOLD "here", ".",
     EXAMPLE {
	  "secondFunction(4,6,MyOption=>3)"
	  },
     SeeAlso => {
	  "firstFunction"
	  }
     }

*}

TEST ///
  assert(firstFunction 1 === "Hello, World!")
  assert(secondFunction(1,3) === 4)
  assert(secondFunction(1,3,MyOption=>5) === 9)
///
  
       
end

-- Here place M2 code that you find useful while developing this
-- package.  None of it will be executed when the file is loaded,
-- because loading stops when the symbol "end" is encountered.

restart
loadPackage "NAG"
installPackage "NAG"
installPackage("PackageTemplate", RemakeAllDocumentation=>true)
check PackageTemplate

-- Local Variables:
-- compile-command: "make -C $M2BUILDDIR/Macaulay2/packages NAMEOFPACKAGE=PackageTemplate install-one"
-- End:
