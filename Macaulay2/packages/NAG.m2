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
     solveSystem
     --MyOption
     }
exportMutable {
     PHCexe
     }

-- GLOBAL VARIABLES ----------------------------------
PHCexe = NAG#Options#Configuration#"PHCpack";
BERTINIexe = NAG#Options#Configuration#"Bertini";
HOM4PS2exe = NAG#Options#Configuration#"HOM4PS2";

DBG = 0; -- debug level (10=keep temp files)

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

solveSystem = method(TypicalValue => List, Options =>{Software=>PHCpack})
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
     if all(F, f -> first degree f <= 1)
     then ( 
	  A := matrix apply(F, f->apply(v, x->coefficient(x,f)));
	  b := matrix apply(F, f->{coefficient(1_R,f)});
	  solve(A,b)
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
