document {
     Key => NAG,
     Headline => "Numerical Algebraic Geometry",
     "The package ", EM "NAG (Numerical Algebraic Geometry)", " implements methods of polynomial homotopy continuation                                                                                                  
     to solve systems of polynomial equations and deal with complex algebraic varieties.",
     HEADER3 "Under construction:",
     "In order to download the current versions of packages from the repository type ", TT "svn ci svn://macaulay2.math.uiuc.edu/Macaulay2/trunk/M2/Macaulay2/packages", 
     " (google \"svn\" for details). ", "This package is stored in ", TT "NAG.m2", " and the subdirectory ", TT "NAG", ".",
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
     Key => {[solveSystem,Software],[track,Software]},
     Headline => "specify software for the solver",
     UL{
	  TO "M2",
	  TO "PHCpack",
	  TO "Bertini",
	  TO "HOM4PS2"
	  }
     }
							
document {
     Key => M2,
     Headline => "use internal Macaulay 2 homotopy continuation routines"
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
			      		    								 