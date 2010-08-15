document {
     Key => NumericalAlgebraicGeometry,
     Headline => "Numerical Algebraic Geometry",
     "The package ", EM "NAG4M2 (Numerical Algebraic Geometry for Macaulay 2)", 
     " implements methods of polynomial homotopy continuation                                                                                                  
     to solve systems of polynomial equations and describe positive-dimensional complex algebraic varieties.", BR{},
     "The current version focuses on solving square systems with finite number of solutions." 
     }
					
document {
	Key => {(solveSystem, List),solveSystem},
	Headline => "solve a square system of polynomial equations",
	Usage => "s = solveSystem F",
	Inputs => { {"F", ", polynomials with complex coefficients"} },
	Outputs => {{ TT "s", ", all complex solutions to the system ", TT "F=0" }},
	"Solve a system of polynomial equations using homotopy continuation methods.",
	EXAMPLE lines ///
R = CC[x,y];
F = {x^2+y^2-1, x*y};
solveSystem F / coordinates 			 	     
     	///,
	Caveat => {"The system is assumed to be square (#equations = #variables) and have finitely many solutions."}	
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
     	     [track,SLPcorrector], [track,SLPpredictor], [track,NoOutput], [track,Normalize], 
	     SLPcorrector, SLPpredictor, NoOutput, Normalize
	     },
	Headline => "track a user homotopy",
	Usage => "solsT = track(S,T,solsS)",
	Inputs => { 
	     "S" => {TO List, " of polynomials in the start system"},
	     "T" => {TO List, " of polynomials in the target system"},
	     "solsS" => {TO List, " of start solutions"},
	     gamma => {"a parameter in the homotopy: H(t)=(1-t)^tDegree S + gamma t^tDegree T"}, 
	     tDegree => {"a parameter in the homotopy: H(t)=(1-t)^tDegree S + gamma t^tDegree T"},
	     tStep => {"initial step size"}, 
	     tStepMin => {"minimal step size"},
	     stepIncreaseFactor => {"determine how step size is adjusted"},
	     numberSuccessesBeforeIncrease => {"determine how step size is adjusted"},
	     Predictor => {"a method to predict the next point on the homotopy path: 
		  choose between ", TO "RungeKutta4", ", ", TO "Tangent", ", ", 
		  TO "Euler", ", ", TO "Secant", ", ", TO "ProjectiveNewton", ". The latter provides certified tracking."},
	     maxCorrSteps => {"max number of steps corrector takes before a failure is declared"}, 
	     CorrectorTolerance => {"corrector succeeds if the relative error does not esceed this tolerance"},
     	     EndZoneFactor => {"size of `end zone'"},  
	     InfinityThreshold => {"paths are truncated if the norm of the approximation exceeds the threshold"},
     	     Projectivize => {"if true then the system is homogenized and projective tracker is executed"},
	     Normalize => {"normalize the start and target systems w.r.t. Bombieri-Weyl norm"},
	     NoOutput => {"if true, no output is produced (useful in combination with ", TO "getSolution", ")"} 	     
	     },
	Outputs => {{ TT "solsT", ", solutions of ", TT "T=0", " obtained by continuing ", TT "solsS" }},
	"Polynomial homotopy continuation techniques are used to obtain solutions 
	of the target system given a start system.", BR{},
	Caveat => {"Predictor=>ProjectiveNewton works only with Software=>M2 and Normalize=>true"},
	EXAMPLE lines ///
R = CC[x,y];
S = {x^2-1,y^2-1};
T = {x^2+y^2-1, x*y};
solsS = {(1,-1),(1,1),(-1,1),(-1,-1)};
track(S,T,solsS) / coordinates 
     	///
	}

document {
	Key => {
	     (refine, List, List), refine, 
	     [refine, Iterations], [refine, Bits], [refine,ErrorTolerance], 
	     Iterations, Bits, ErrorTolerance
	     },
	Headline => "refine numerical solutions to a system of polynomial equations",
	Usage => "solsR = refine(T,sols)",
	Inputs => { 
	     "T" => {"polynomials of the system"},
	     "sols" => {"solutions (lists of coordinates)"},
	     Iterations => {"number of refining iterations of Newton's methods"}, 
	     Bits => {"number of bits of precision"}, 
	     ErrorTolerance => {"a bound on the desired estimated error"},
	     ResidualTolerance => {"a bound on desired residual"}
	     },
	Outputs => {"solsR" => {"refined solutions" }},
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
refine(T, sols, Software=>M2, ErrorTolerance=>.001, Iterations=>10)
     	///
	}

document { Key => {Tolerance, [sortSolutions,Tolerance], [areEqual,Tolerance]},
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
     Key => {[solveSystem,Software],[track,Software],[refine, Software], "Software",
	  "M2","M2engine","M2enginePrecookedSLPs"},
     Headline => "specify software for the solver",
     UL{
	  {"M2", " -- use top-level Macaulay2 homotopy continuation routines"},
	  {"M2engine", " -- use subroutines implemented in Macaulay2 engine"},
	  {"M2enginePrecookedSLPs", " -- (obsolete)"},
	  TO "PHCPACK",
	  TO "BERTINI",
	  TO "HOM4PS2"
	  }
     }
document {
     Key => BERTINI,
     Headline => "use Bertini for homotopy continuation",
     "Available at ", TT "http://www.nd.edu/~sommese/bertini/"
     }
document {
     Key => HOM4PS2,
     Headline => "use HOM4PS for homotopy continuation",
     "Available at ", TT "http://hom4ps.math.msu.edu/HOM4PS_soft.htm"
     }
			      		    								 
document {
	Key => {(getSolution, ZZ), getSolution, SolutionAttributes, [getSolution,SolutionAttributes]},
	Headline => "get various attributes of the specified solution",
	Usage => "s = getSolution i, s = getSolution(i,SolutionAttributes=>...)",
	Inputs => { 
	     {"i", ", the number of the solution"}
	     },
	Outputs => {{ TT "s", ", (an) attributes of the solution"}},
	"Returns attribute(s) of the ", TT "i", "-th solution specified in the option", TO "SolutionAttributes", 
	", which could be either a sequence or a single attribute.", BR{}, 
	"SolutionAttributes include:",
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
	Headline => "set the trace level in NumericalAlgebraicGeometry package",
	Usage => "a = NAGtrace b",
	Inputs => { 
	     {TT "b", ", new level"}
	     },
	Outputs => {{ TT "a", ", old level"}},
	"Determines how talkative the procedures of NumericalAlgebraicGeometry are. The most meaningful values are:", 
	UL{
	     {"0", " -- silent"},
	     {"1", " -- progress and timings"},
	     {"2", " -- more messages than 1"}
	     },
	"The progress is displayed as follows: ", 
	UL{
	     {"'.' = regular solution found"   },
   	     {"'S' = singular solution (or encountered a singular point on the path)"   },
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
	Key => {areEqual, (areEqual,CC,CC), (areEqual,List,List), (areEqual,Matrix,Matrix), [areEqual,Projective]},
	Headline => "determine if solutions are equal",
	Usage => "b = areEqual(x,y)",
	Inputs => {
	     "x" => "a solution or list of solutions",
	     "y" => "a solution or list of solutions",
	     Projective=>{"if true, then solutions are considered as representatives of points in the projective space"}
	     },
	Outputs => {"b"=>{"true, if approximately equal"}},
	"The function returns false if Riemannian distance exceeds ", TO Tolerance, " and true, otherwise.",
        EXAMPLE lines ///
R = CC[x,y];
s = solveSystem {x^2+y^2-1, x*y}
areEqual(sortSolutions s / coordinates, {{-1, 0}, {0, -1}, {0, 1}, {1, 0}})
     	///
	}

document {
	Key => {randomSd, (randomSd, List)},
	Headline => "a random homogeneous system of polynomial equations",
	Usage => "T = randomSd d",
	Inputs => { 
	     {TT "d", ", list of degrees"}
	     },
	Outputs => {{ TT "T", ", list of polynomials"}},
	"Generates a system of homogeneous polynomials T_i such that deg T_i = d_i. 
	The system is normalized, so that it is on a unit sphere in the Bombieri-Weyl norm.",
        EXAMPLE lines ///
T = randomSd {2,3}
(S,solsS) = goodInitialPair T;
M = track(S,T,solsS,gamma=>0.6+0.8*ii,Software=>M2)
     	///
	}

document {
	Key => {goodInitialPair, (goodInitialPair, List), [goodInitialPair,GeneralPosition], GeneralPosition},
	Headline => "a good (conjectured by Shub and Smale) initial pair",
	Usage => "(S,sol) = goodInitialPair T",
	Inputs => { 
	     "T" => {"a list of polynomials"},
	     GeneralPosition => {"make a random unitary change of coordinates"} 
	     },
	Outputs => {{ TT "S", ", list of polynomials"},
	     { TT "sol", ", a list containing (one) solution of S"}},
	"Generates a start system S that is conjectured to have good complexity when used in linear homotopy 
       	with target system T leading to one solution. ",
        EXAMPLE lines ///
T = randomSd {2,3};
(S,solsS) = goodInitialPair T
M = track(S,T,solsS,gamma=>0.6+0.8*ii,Software=>M2)
     	///
	}

document {
	Key => {randomInitialPair, (randomInitialPair, List)},
	Headline => "a random initial pair",
	Usage => "(S,sol) =randomInitialPair T",
	Inputs => { 
	     {TT "T", ", list of polynomials"}
	     },
	Outputs => {{ TT "S", ", list of polynomials"},
	     { TT "sol", ", a list containing (one) solution of S"}},
	"Generates a start system S that has and equal chance of reaching any of the solutions of 
       	the target system T. ",
        EXAMPLE lines ///
T = randomSd {2,3};
(S,solsS) = randomInitialPair T
M = track(S,T,solsS,gamma=>0.6+0.8*ii,Software=>M2)
     	///
	}
								
					 
													 
							   