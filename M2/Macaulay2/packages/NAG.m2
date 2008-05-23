
newPackage(
	"NAG",
    	Version => "1.0", 
    	Date => "04/01/2008",
    	Authors => {
	     {Name => "Anton Leykin", Email => "leykin@ima.umn.edu"}
	     },
    	HomePage => "http://www.google.com",
    	Headline => "Numerical Algebraic Geometry",
	-- DebuggingMode should be true while developing a package, 
	--   but false after it is done
    	DebuggingMode => true 
    	)

-- Any symbols or functions that the user is to have access to
-- must be placed in one of the following two lists
export {
     systemToFile,
     solveSystem
     --MyOption
     }
exportMutable {
     PHCpackPATH,
     PHCexe
     }

PHCpackPATH = "";
PHCexe = "phc"
DBG = 10;

systemToFile = method(TypicalValue => Nothing)
systemToFile (List,String) := (F,name) -> (
     file := openOut name;
     file << #F << endl;
     scan(F, f->( 
	       file << toString f << ";" << endl;
	       ));
     close file;
     ) 

parseSolutions = method(TypicalValue => List)
parseSolutions (String,List) := (name,V) -> (
     L := get name;
     time L = replace("=", "=>", L);
     time L = replace("I", "ii", L);
     time L = replace("E", "e", L);
     time L = replace("e\\+", "e", L);     
     time L = replace("time", "\"time\"", L);
     time L = replace("multiplicity", "\"mult\"", L);
	  
     sols := apply(value L, x->new HashTable from toList x);
     mults := apply(sols, x->x#"mult");
     coords := apply(sols, x->apply(V, v->x#v));
     (coords, mults)     
     )

solveSystem = method(TypicalValue => List)
solveSystem List := List => F -> (
     R := ring F#0;
     v := flatten entries vars R;
     if all(F, f -> first degree f <= 1)
     then ( 
	  A := matrix apply(F, f->apply(v, x->coefficient(x,f)));
	  b := matrix apply(F, f->{coefficient(1_R,f)});
	  solve(A,b)
	  )
     else ( -- assume the ideal is 0-dim. 
	  tarsolfile := --temporaryFileName() | 
	                "tasols";
	  targetfile := --temporaryFileName() | 
	             "target";
	  outfile := --temporaryFileName() | 
	             "output";
	  -- writing data to the corresponding files                                                                                                                                                                           
	  systemToFile(F,targetfile);
	  -- launching blackbox solver; converting the solutions to the Maple format                                                                                                                                           
	  run(PHCexe|" -b "|targetfile|" "|outfile);
	  run(PHCexe|" -z "|targetfile|" "|tarsolfile);
	  -- parse and output the solutions                                                                                                                                                                                    
	  result := parseSolutions(tarsolfile, gens R);
	  -- clean up                                                                                                                                                                                                          
	  if DBG<10 then (
	       removeFile targetfile;
	       removeFile tarsolfile; 
	       removeFile outfile;
	       )
	  );  		
     	  result
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

R = QQ[a,b,c]
I = ideal"a2-b-12, ab-3c2-10bc,abc-1"
solveSystem I_*
-- Local Variables:
-- compile-command: "make -C $M2BUILDDIR/Macaulay2/packages NAMEOFPACKAGE=PackageTemplate install-one"
-- End:
