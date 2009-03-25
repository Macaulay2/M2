document {
     Key => NAG,
     Headline => "Numerical Algebraic Geometry",
     "The package ", EM "NAG4M2 (Numerical Algebraic Geometry for Macaulay 2)", " implements methods of polynomial homotopy continuation                                                                                                  
     to solve systems of polynomial equations and deal with complex algebraic varieties.",
     HEADER3 "Under construction:",
     "The package will be included in the future releases of Macaulay 2.", BR{},
     "In order to run the current version of the package Macaulay 2 needs to be compiled from the source code. ",
     "Instructions are posted at ", TT "http://www.math.uiuc.edu/Macaulay2/Downloads/SourceCode", ".",
     BR{}, BR{},"( Another way is to download the packages subdirectory only:", BR{}, 
     TT "svn co svn://macaulay2.math.uiuc.edu/Macaulay2/trunk/M2/Macaulay2/packages", BR{},
     "and then use ", BR{}, TT "loadPackage(\"NAG\", FileName=>\"packages/NAG.m2\").", BR{},  
     "Note: in this case the functionality of the package is limited; in particular, SLP and M2engine options do not work. )", BR{},
     HEADER3 "Functions:",
     UL{
	  TO{"solveSystem"},
	  TO{"track"}
	  }
     }
					
document {
	Key => {(solveSystem, List),solveSystem},
	Headline => "solve a square system of polynomial equations",
	Usage => "s = solveSystem F",
	Inputs => { {"F", ", polynomials"} },
	Outputs => {{ TT "s", ", solutions to the system ", TT "F=0" }},
        -- SourceCode => {(solveSystem, List)},
	EXAMPLE lines ///
R = CC[x,y];
F = {x^2+y^2-1, x*y};
solveSystem F / first 			 	     
     	///
	}
document {
	Key => {(track, List, List, List),track},
	Headline => "track a user homotopy",
	Usage => "solsT = track(S,T,solsS)",
	Inputs => { 
	     {"S", ", polynomials in the start system"},
	     {"T", ", polynomials in the target system"},
	     {"solsS", ", start solutions"}
	     },
	Outputs => {{ TT "solsT", ", solutions of ", TT "T=0", " obtained by continuing ", TT "solsS" }},
	EXAMPLE lines ///
R = CC[x,y];
S = {x^2-1,y^2-1};
T = {x^2+y^2-1, x*y};
solsS = {(1,-1),(1,1),(-1,1),(-1,-1)};
track(S,T,solsS) / first 
     	///
	}

					
document {
     Key => {[solveSystem,Software],[track,Software],"Software","M2","M2engine"},
     Headline => "specify software for the solver",
     UL{
	  {"M2", " -- use top-level Macaulay2 homotopy continuation routines"},
	  {"M2engine", " -- use subroutines implemented in Macaulay2 engine"},
	  TO "PHCpack",
	  TO "Bertini",
	  TO "HOM4PS2"
	  }
     }
document {
     Key => Bertini,
     Headline => "use Bertini for homotopy continuation",
     "Available at ", TT "http://www.nd.edu/~sommese/bertini/"
     }
document {
     Key => PHCpack,
     Headline => "use PHCpack for homotopy continuation",
     "Available at ", TT "http://www.math.uic.edu/~jan/download.html"
     }
document {
     Key => HOM4PS2,
     Headline => "use HOM4PS for homotopy continuation",
     "Available at ", TT "http://hom4ps.math.msu.edu/HOM4PS_soft.htm"
     }
			      		    								 