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
	Key => { (track, List, List, List), track, 
	     [track,gamma], [track,tDegree], [track,tStep], [track,tStepMin], 
	     gamma, tDegree, tStep, tStepMin, 
	     [track,stepIncreaseFactor], [track, numberSuccessesBeforeIncrease], 
	     stepIncreaseFactor, numberSuccessesBeforeIncrease, 
	     Predictor, [track,Predictor], RungeKutta4, Multistep, Tangent, Euler, Secant,
	     MultistepDegree, [track,MultistepDegree], ProjectiveNewton,
     	     [track,EndZoneFactor], [track,maxCorrSteps], [track,InfinityThreshold],
     	     EndZoneFactor, maxCorrSteps, InfinityThreshold,
     	     Projectivize, [track,Projectivize], AffinePatches, [track,AffinePatches], DynamicPatch, 
	     SLP, [track,SLP], HornerForm, CompiledHornerForm, 
	     CorrectorTolerance, [track,CorrectorTolerance], 
     	     [track,SLPcorrector], [track,SLPpredictor], [track,NoOutput], 
	     SLPcorrector, SLPpredictor, NoOutput
	     },
	Headline => "track a user homotopy",
	Usage => "solsT = track(S,T,solsS)",
	Inputs => { 
	     {"S", ", polynomials in the start system"},
	     {"T", ", polynomials in the target system"},
	     {"solsS", ", start solutions"}
	     },
	Outputs => {{ TT "solsT", ", solutions of ", TT "T=0", " obtained by continuing ", TT "solsS" }},
	"Polynomial homotopy continuation techniques are used to obtain solutions 
	of the target system given a start system.", BR{},
	"Main options:",
	UL {
	     {TT "gamma", " and ", TT "tDegree", " specify homotopy: H(t)=(1-t)^tDegree S + gamma t^tDegree T"}, 
	     {TT "tStep", " -- initial step size"}, 
	     {TT "tStepMin", " -- minimal step size"},
	     {TT "stepIncreaseFactor", " and ", TT "numberSuccessesBeforeIncrease", 
		  " determine how step size is adjusted"},
	     {TT "Predictor", " -- choose between ", TO "RungeKutta4", ", ", TO "Tangent", ", ", 
		  TO "Euler", ", ", TO "Secant", ", ", TO "ProjectiveNewton"},
	     {TT "maxCorrSteps", " -- max number of steps corrector takes before a failure is declared"}, 
	     {TT "CorrectorTolerance", " -- corrector succeeds if the relative error does not esceed this tolerance"},
     	     {TT "EndZoneFactor", "  -- size of `end zone'"},  
	     {TT "InfinityThreshold", 
		  " -- paths are truncated if the norm of the approximation exceeds the threshold"},
     	     {TT "Projectivize", " -- if true then the system is homogenized and projective tracker is executed"},
	     {TT "NoOutput", " -- if true, no output is produced (useful in combination with ", TO "getSolution", ")"} 	     
	     },
	EXAMPLE lines ///
R = CC[x,y];
S = {x^2-1,y^2-1};
T = {x^2+y^2-1, x*y};
solsS = {(1,-1),(1,1),(-1,1),(-1,-1)};
track(S,T,solsS) / first 
     	///
	}

document {
	Key => {(refine, List, List), refine, [refine, maxCorrSteps], [refine, Software]},
	Headline => "refine numerical solutions to a system of polynomial equations",
	Usage => "solsR = refine(T,sols)",
	Inputs => { 
	     {"T", ", polynomials of the system"},
	     {"sols", ", solutions (lists of coordinates)"}
	     },
	Outputs => {{ TT "solsR", ", refined solutions" }},
	"Uses Newton's method to correct the given solutions so that the resluting approximation 
	has its estimated relative error bound by ", TT "Tolerance", 
	"; the number of iterations is at most ", TT "maxCorrSteps", ".",
	Caveat => {"If option ", TT "Software=>M2engine", " is specified, 
	     then the refinement happens in the M2 engine and it is assumed that the last path tracking procedure 
	     took place with the same option and was given the same target system. 
	     Any other value of this option would launch an M2-language procedure."},
	EXAMPLE lines ///
R = CC[x,y];
S = {x^2-1,y^2-1};
T = {x^2+y^2-1, x*y};
sols = { {1.1_CC,0.1}, {-0.1,1.2} };
refine(T, sols, Software=>M2, Tolerance=>.001, maxCorrSteps=>10)
     	///
	}

document { Key => {Tolerance, [refine,Tolerance], [sortSolutions,Tolerance], [areEqual,Tolerance]},
     Headline => "specifies the tolerance of a numerical computation" 
     }

document {
	Key => {(totalDegreeStartSystem, List), totalDegreeStartSystem},
	Headline => "construct a start system for the total degree homotopy",
	Usage => "(S,solsS) = totalDegreeStartSystem T",
	Inputs => { 
	     {"T", ", polynomials of the target system"},
	     },
	Outputs => {{ TT "(S,solsS)", ", where ", TT "S", " is the list of polynomials in the start system and ", TT "solsS", " the list of start solutions"}},
     	"Given a square target system, constructs a start system 
	for a total degree homotopy together with the total degree many start solutions.",
	EXAMPLE lines ///
R = CC[x,y];
T = {x^2+y^2-1, x*y};
totalDegreeStartSystem T
     	///
	}

document {
     Key => {[solveSystem,Software],[track,Software], "Software","M2","M2engine","M2enginePrecookedSLPs"},
     Headline => "specify software for the solver",
     UL{
	  {"M2", " -- use top-level Macaulay2 homotopy continuation routines"},
	  {"M2engine", " -- use subroutines implemented in Macaulay2 engine"},
	  {"M2enginePrecookedSLPs", " -- (obsolete)"},
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
			      		    								 
document {
	Key => {(getSolution, ZZ), getSolution, SolutionAttributes, [getSolution,SolutionAttributes], 
	     Coordinates, SolutionStatus, LastT, RCondition, NumberOfSteps},
	Headline => "get various attributes of the specified solution",
	Usage => "s = getSolution i, s = getSolution(i,SolutionAttributes=>...)",
	Inputs => { 
	     {"i", ", the number of the solution"}
	     },
	Outputs => {{ TT "s", ", (an) attributes of the solution"}},
	"Returns attribute(s) of the ", TT "i", "-th solution specified in the option", TO "SolutionAttributes", 
	", which could be either a sequence or a single attribute.", BR{}, 
	"Attributes include:",
	UL{
	  {"Coordinates", " -- the list of coordinates"},
	  {"SolutionStatus", " -- REGULAR, SINGULAR, FAILED, etc."},
	  {"NumberOfSteps", " -- number of steps taken on the corresponding homotopy path"},
	  {"LastT", " -- the last value of the continuation parameter"},
	  {"RCondintion", "-- the reverse condition number at the last step of Newton's method"}
	  },
  	Caveat => {"Requires a preceding run of " , TO "track", " or ", TO "solveSystem", 
	     " with the (default) option ", TT "Software=>M2engine"},	
        EXAMPLE lines ///
R = CC[x,y];
S = {x^2-1,y^2-1};
T = {x^2+y^2-1, x*y};
track(S,T,{(1,1),(1,-1)})
getSolution 0
getSolution(0, SolutionAttributes=>LastT)
getSolution(1, SolutionAttributes=>(Coordinates, SolutionStatus, RCondition))
     	///
	}

document {
	Key => {(NAGtrace, ZZ), NAGtrace},
	Headline => "set the trace level in NAG package",
	Usage => "a = NAGtrace b",
	Inputs => { 
	     {TT "b", ", new level"}
	     },
	Outputs => {{ TT "a", ", old level"}},
	"Determines how talkative the procedures of NAG are. The most meaningful values are:", 
	UL{
	     {"0", " -- silent"},
	     {"1", " -- progress and timings"},
	     {"2", " -- more messages than 1"}
	     },
	"The progress is displayed as follows: ", 
	UL{
	     {"'.' = regular solution found"   },
	     {"'I' = a homotopy path (most probably) diverged to infinity"},
	     {"'M' = minimum step bound reached"}
	     },
     	     	
        EXAMPLE lines ///
R = CC[x,y];
S = {x^2-1,y^2-1};
T = {x^2+y^2-1, x+y};
NAGtrace 1
track(S,T,{(1,1),(1,-1),(-1,1),(-1,-1)})
     	///
	}

document {
	Key => {(sortSolutions,List), sortSolutions},
	Headline => "sort the list of solutions",
	Usage => "t = sortSolutions s",
	Inputs => { 
	     {TT "s", ", the list of solutions"}
	     },
	Outputs => {{ TT "t", ", sorted list"}},
	"The sorting is done lexicographically regarding each complex n-vector as real 2n-vector. ",
	"The output format of ", TO track, " and ", TO solveSystem, " is respected.", BR{}, 
	"The parts of coordinates are considered equal if within ", TO Tolerance, 
        EXAMPLE lines ///
R = CC[x,y];
s = solveSystem {x^2+y^2-1, x*y}
sortSolutions s
     	///
	}

document {
	Key => {areEqual, (areEqual,CC,CC), (areEqual,List,List), (areEqual,Matrix,Matrix)},
	Headline => "determine if solutions are equal",
	Usage => "b = areEqual(x,y)",
	Inputs => { 
	     {TT "x",TT "y", " solutions or lists of solutions"}
	     },
	Outputs => {{ TT "b", ", true = approximately equal"}},
	"Comparisons are done coordinatewise using ", TO Tolerance, " as the measure for closeness.",
        EXAMPLE lines ///
R = CC[x,y];
s = solveSystem {x^2+y^2-1, x*y}
areEqual(sortSolutions s, {{{-1, 0}}, {{0, -1}}, {{0, 1}}, {{1, 0}}})
     	///
	}
													 