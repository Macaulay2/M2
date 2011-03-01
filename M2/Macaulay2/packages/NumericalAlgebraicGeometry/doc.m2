refBeltranLeykin := "C. Beltran and A. Leykin, \"Certified numerical homotopy tracking\", arXiv:0911.1783" 
refIntroToNAG := "A.J. Sommese, J. Verschelde, and C.W. Wampler, \"Introduction to numerical algebraic geometry\", 
                  in \"Solving polynomial equations\"(2005), 301--338" 
refSWbook := "A.J. Sommese and C.W. Wampler, \"The numerical solution of systems of polynomials\",
              World Scientific Publishing (2005)"
certifiedTrackingFunctions := UL{
	     TO randomInitialPair,
	     TO goodInitialPair,
	     TO randomSd 
	     }
document {
     Key => NumericalAlgebraicGeometry,
     Headline => "Numerical Algebraic Geometry",
     "The package ", TO "NumericalAlgebraicGeometry", ", also known as ", 
     EM "NAG4M2 (Numerical Algebraic Geometry for Macaulay2)", 
     " implements methods of polynomial homotopy continuation                                                                                                  
     to solve systems of polynomial equations and describe positive-dimensional complex algebraic varieties. ", 
     "The current version focuses on solving square systems with finite number of solutions. ",
     
     HEADER3 "Basic functions:",
     UL{
	  TO track,
	  TO solveSystem,
	  TO refine,
	  TO totalDegreeStartSystem
	  },
     HEADER3 "Service functions:",
     UL{
	  TO setDefault,
	  TO getDefault,
	  TO areEqual,
     	  TO sortSolutions,
	  TO NAGtrace
	  },
     HEADER3 {"Functions related to ", TO "Certified", " tracking:"},
     certifiedTrackingFunctions,
     HEADER3 {"References:"},
     UL{
       refIntroToNAG,
       refSWbook,
       refBeltranLeykin
       }
     }

document {
	Key => {setDefault, 1:(setDefault), Attempts, [setDefault, Attempts], 
	     SingularConditionNumber, [setDefault, SingularConditionNumber], [refine, SingularConditionNumber],
	     getDefault, (getDefault,Symbol)},
	Headline => "set/get the default parameters for continuation algorithms",
	Usage => "setDefault(p1=>v1, p2=>v2, ...); \\n v = getDefault p",
	Inputs => { {TT "p, p1, p2", ", ", TO "Symbol", "(s), the name(s) of parameter(s)"},
	     	  Attempts => {" -- the maximal number of attempts (e.g., to make a random regular homotopy); 
		  so far used only in the functions under development."},
		  SingularConditionNumber => {" -- a matrix is considered to be singular 
		       if its condition number is greater than this value"}
		  },
	Outputs => { {TT "v, v1, v2", ", value(s) of the parameter(s)"} },
	"Set/get value(s) of (a) parameter(s) in the functions ", 
	TO "track", ", ", TO "solveSystem", ", ", TO "refine", " as well as higher-level functions (that are under construction).", 
     	PARA {},
	EXAMPLE lines ///
	getDefault Predictor
     	setDefault(Predictor=>Euler, CorrectorTolerance=>1e-10)
	getDefault Predictor  
     	///,
     	PARA {},
	SeeAlso => {track, solveSystem, refine, areEqual}
	}
					
document { Key => {AffinePatches, [track,AffinePatches], [setDefault,AffinePatches], DynamicPatch, 
	     SLP, [track,SLP], [setDefault,SLP], HornerForm, CompiledHornerForm, 
	     SLPcorrector, SLPpredictor, [track,SLPcorrector], [setDefault,SLPcorrector], [track,SLPpredictor], [setDefault,SLPpredictor]},
     Headline => "reserved for developers"
     } 

document {
	Key => {(solveSystem, List),solveSystem},
	Headline => "solve a square system of polynomial equations",
	Usage => "s = solveSystem F",
	Inputs => { "F"=>"polynomials with complex coefficients" },
	Outputs => { "s"=>{"all complex solutions to the system ", TT "F=0" }},
	"Solve a system of polynomial equations using homotopy continuation methods.",
     	PARA {},
	EXAMPLE lines ///
R = CC[x,y];
F = {x^2+y^2-1, x*y};
solveSystem F 
     	///,
     	PARA {},
	"The output contains all ", TO Point, "s obtained at the end of homotopy paths when tracking a total-degree homotopy. ",
	"In particular, this means that solving the system that has fewer than Bezout bound many solutions will produce 
	points that are not marked as regular. See ", TO track, " for detailed examples. ", 
	Caveat => {"The system is assumed to be square (#equations = #variables) 
	     and to have finitely many solutions."}	
	}
document {
	Key => { (track, List, List, List), track, 
	     [track,gamma], [setDefault,gamma], [track,tDegree], [setDefault,tDegree], 
	     [track,tStep], [setDefault,tStep], [track,tStepMin], [setDefault,tStepMin],
	     gamma, tDegree, tStep, tStepMin, 
	     [track,stepIncreaseFactor], [setDefault,stepIncreaseFactor], 
	     [track, numberSuccessesBeforeIncrease], [setDefault,numberSuccessesBeforeIncrease],
	     stepIncreaseFactor, numberSuccessesBeforeIncrease, 
	     Predictor, [track,Predictor], [setDefault,Predictor], RungeKutta4, Multistep, Tangent, Euler, Secant,
	     MultistepDegree, [track,MultistepDegree], [setDefault,MultistepDegree], 
     	     [track,EndZoneFactor], [setDefault,EndZoneFactor], [track,maxCorrSteps], [setDefault,maxCorrSteps],
	     [track,InfinityThreshold], [setDefault,InfinityThreshold],
     	     EndZoneFactor, maxCorrSteps, InfinityThreshold,
     	     Projectivize, [track,Projectivize], [setDefault,Projectivize], 
	     CorrectorTolerance, [track,CorrectorTolerance], [setDefault,CorrectorTolerance],
	     [track,NoOutput], [setDefault,NoOutput], 
	     [track,Normalize], [setDefault,Normalize],
	     NoOutput, Normalize
	     },
	Headline => "track a user homotopy",
	Usage => "solsT = track(S,T,solsS)",
	Inputs => { 
	     "S" => {" polynomials in the start system"},
	     "T" => {" polynomials in the target system"},
	     "solsS" => {" start solutions"},
	     gamma => {"a parameter in the homotopy: ", TEX "H(t)=(1-t)^{tDegree} S + \\gamma t^{tDegree} T"}, 
	     tDegree => {"a parameter in the homotopy: ", TEX "H(t)=(1-t)^{tDegree} S + \\gamma t^{tDegree} T"},
	     tStep => {"initial step size"}, 
	     tStepMin => {"minimal step size"},
	     stepIncreaseFactor => {"determine how step size is adjusted"},
	     numberSuccessesBeforeIncrease => {"determine how step size is adjusted"},
	     Predictor => {"a method to predict the next point on the homotopy path: 
		  choose between ", TO "RungeKutta4", ", ", TO "Tangent", ", ", 
		  TO "Euler", ", ", TO "Secant", ", ", TO "Multistep", ", ", TO "Certified", 
		  ". The option ", TO "Certified", " provides certified tracking."},
	     MultistepDegree => {"degree of the Multistep predictor"},
	     maxCorrSteps => {"max number of steps corrector takes before a failure is declared"}, 
	     CorrectorTolerance => {"corrector succeeds if the relative error does not exceed this tolerance"},
     	     EndZoneFactor => {"size of `end zone'"},  
	     InfinityThreshold => {"paths are truncated if the norm of the approximation exceeds the threshold"},
     	     Projectivize => {"if true then the system is homogenized and projective tracker is executed"},
	     Normalize => {"normalize the start and target systems w.r.t. Bombieri-Weyl norm"},
	     NoOutput => {"if true, no output is produced (useful in combination with ", TO "getSolution", ")"} 	     
	     },
	Outputs => {{ TT "solsT", " is a list of ", TO2{Point,"points"}, " that are solutions of ", TT "T=0", " obtained by continuing ", TT "solsS", " of ", TT "S=0" }},
	"Polynomial homotopy continuation techniques are used to obtain solutions 
	of the target system given a start system. ",
	"For an introduction to the subject see ", UL{
	     {refIntroToNAG}, {refSWbook}
	     }, 
	"Most commonly the following homotopy is considered:", 
	PARA{ 
	     TEX "H(t) = \\gamma t^d T + (1-t)^d S" 
	     }, 
	"where ", TEX "t", " is in the interval ", TEX "[0,1]", " and ",
	TEX "d = ", TO "tDegree",   
	". ", PARA {"Here is an example with regular solutions at the ends of all homotopy paths:"},   
        EXAMPLE lines ///
	R = CC[x,y];
	S = {x^2-1,y^2-1};
	T = {x^2+y^2-1, x*y};
	solsS = {(1,-1),(1,1),(-1,1),(-1,-1)};
	track(S,T,solsS)  
     	///,
	PARA {
	     "Another outcome of tracking a path is divergence (established heuristically). 
	     In that case the divergent paths are marked with ", TT "I", 
	     " (", TO status, " is set to ", TO Infinity, "). "
	     },
        EXAMPLE lines ///
     	R = CC[x,y];
     	S = {x^2-1,y^2-1};
     	T = {x^2+y^2-1, x-y};
     	solsS = {(1,-1),(1,1),(-1,1),(-1,-1)};
     	track(S,T,solsS,gamma=>0.6+0.8*ii) 
     	///,
	PARA {
	     "Some divergent paths as well as most of the paths ending in singular (multiplicity>1) 
	     or near-singular (clustered) solutions are marked with ", TT "M", 
	     " (", TO status, " is set to ", TO MinStepFailure, "). "
	     },
	EXAMPLE lines ///
     	R = CC[x,y];
     	S = {x^2-1,y^2-1};
     	T = {x^2+y^2-1, (x-y)^2};
     	solsS = {(1,-1),(1,1),(-1,1),(-1,-1)};
     	track(S,T,solsS)
	///,
	PARA {
       	     "Tracking in the projective space uses the homotopy corresponding to an arc of a great circle 
	     on  a unit sphere in the space of homogeneous polynomial systems of a fixed degree. 
	     In particular, this is done for certified homotopy tracking (see "|refBeltranLeykin|"):"
	     },
	EXAMPLE lines ///
	R = CC[x,y,z];
	S = {x^2-z^2,y^2-z^2};
	T = {x^2+y^2-z^2, x*y};
	solsS = {(1,-1,1),(1,1,1),(-1,1,1),(-1,-1,1)};
	track(S,T,solsS,Predictor=>Certified,Normalize=>true)
	///,
	PARA {
	     "Note that projective tracker is invoked either if the target system is a homogenous system or if ", TO "Projectivize", TT"=>true",
	     " is specified. "
	     },
	SeeAlso => {solveSystem, setDefault, Point},
	Caveat => {"Predictor=>Certified works only with Software=>[M2,M2engine] and Normalize=>true"}
	}

document {
	Key => {
	     (refine, List, List), refine, 
	     [refine, Iterations], [setDefault,Iterations], [refine, Bits], [setDefault,Bits], 
	     [refine,ErrorTolerance], [setDefault,ErrorTolerance], 
	     [refine, ResidualTolerance], [setDefault,ResidualTolerance],
	     Iterations, Bits, ErrorTolerance, ResidualTolerance
	     },
	Headline => "refine numerical solutions to a system of polynomial equations",
	Usage => "solsR = refine(T,sols)",
	Inputs => { 
	     "T" => {"polynomials of the system"},
	     "sols" => {"solutions (lists of coordinates or ", TO2{Point,"points"}},
	     Iterations => {"number of refining iterations of Newton's method"}, 
	     Bits => {"number of bits of precision"}, 
	     ErrorTolerance => {"a bound on the desired estimated error"},
	     ResidualTolerance => {"a bound on desired residual"}
	     },
	Outputs => {"solsR" => {"refined solutions" }},
	"Uses Newton's method to correct the given solutions so that the resulting approximation 
	has its estimated relative error bound by ", TO "ErrorTolerance", 
	"; the number of iterations is at most ", TO "Iterations", ".",
-- 	Caveat => {"If option ", TT "Software=>M2engine", " is specified, 
-- 	     then the refinement happens in the M2 engine and it is assumed that the last path tracking procedure 
-- 	     took place with the same option and was given the same target system. 
-- 	     Any other value of this option would launch an M2-language procedure."},
        PARA {},
	EXAMPLE lines ///
R = CC[x,y];
T = {x^2+y^2-1, x*y};
sols = { {1.1_CC,0.1}, {-0.1,1.2} };
refine(T, sols, Software=>M2, ErrorTolerance=>.001, Iterations=>10)
     	///,
	PARA {},
	"In case of a singular (multiplicity>1) solution, while ", TO solveSystem, " and ", TO track, 
	" return the end of the homotopy paths marked as a 'failure', it is possible to improve the quality of approximation with ", 
	TO refine, ". The resulting point will be marked as singular:", 
	PARA {},
	EXAMPLE lines ///
     	R = CC[x,y];
     	S = {x^2-1,y^2-1};
     	T = {x^2+y^2-1, (x-y)^2};
     	solsS = {(1,1),(-1,-1)};
     	solsT = track(S,T,solsS)
	solsT / coordinates
	refSols = refine(T, solsT)
	refSols / status
     	///
	}

document { Key => {Tolerance, [sortSolutions,Tolerance], [areEqual,Tolerance], [setDefault,Tolerance]},
     Headline => "specifies the tolerance of a numerical computation" 
     }

document {
	Key => {(totalDegreeStartSystem, List), totalDegreeStartSystem},
	Headline => "construct a start system for the total degree homotopy",
	Usage => "(S,solsS) = totalDegreeStartSystem T",
	Inputs => { 
	     "T"=>{"polynomials of the target system"}
	     },
	Outputs => { {"where ", TT "S", " is the list of polynomials in the start system and ", 
		  TT "solsS", " is the list of start solutions"} },
     	"Given a square target system, constructs a start system 
	for a total degree homotopy together with the total degree many start solutions.",
     	PARA {},
	EXAMPLE lines ///
R = CC[x,y];
T = {x^2+y^2-1, x*y};
totalDegreeStartSystem T
     	///
	}

document {
     Key => {[solveSystem,Software],[track,Software],[refine, Software],[setDefault,Software],Software,
	  "M2","M2engine","M2enginePrecookedSLPs"},
     Headline => "specify internal or external software",
     "One may specify which software is used in homotopy continuation. 
     Possible values for internal software are:",  
     UL{
	  {"M2", " -- use top-level Macaulay2 homotopy continuation routines"},
	  {"M2engine", " -- use subroutines implemented in Macaulay2 engine"},
	  {"M2enginePrecookedSLPs", " -- (obsolete)"},
	  },
     "An external program may be used to replace a part of functionality of the package
     provide the corresponding software is installed:",
     UL{
	  TO "PHCPACK",
	  TO "BERTINI",
	  TO "HOM4PS2"
	  }
     }
document {
     Key => BERTINI,
     Headline => "use Bertini for homotopy continuation",
     "Available at ", TT "http://www.nd.edu/~sommese/bertini/",
     SeeAlso => Software
     }
document {
     Key => HOM4PS2,
     Headline => "use HOM4PS for homotopy continuation",
     "Available at ", TT "http://hom4ps.math.msu.edu/HOM4PS_soft.htm",
     SeeAlso => Software
     }
document {
     Key => PHCPACK,
     Headline => "use PHCpack for homotopy continuation",
     "Available at ", TT "http://www.math.uic.edu/~jan/download.html",
     SeeAlso => Software
     }
///--getSolution and SolutionAttributes are not exported anymore
document {
	Key => {(getSolution, ZZ), getSolution, SolutionAttributes, [getSolution,SolutionAttributes]},
	Headline => "get various attributes of the specified solution",
	Usage => "s = getSolution i, s = getSolution(i,SolutionAttributes=>...)",
	Inputs => { 
	     {"i", ", the number of the solution"}
	     },
	Outputs => {{ TT "s", ", (an) attributes of the solution"}},
	"Returns attribute(s) of the ", TT "i", "-th solution specified in the option", 
	TO "SolutionAttributes", 
	", which could be either a sequence or a single attribute. ", 
	"SolutionAttributes include:",
	UL{
	  {"Coordinates", " -- the list of coordinates"},
	  {"SolutionStatus", " -- Regular, Singular, Infinity, MinStepFailure"},
	  {"NumberOfSteps", " -- number of steps taken on the corresponding homotopy path"},
	  {"LastT", " -- the last value of the continuation parameter"},
	  {"ConditionNumber", "-- the condition number at the last step of Newton's method"}
	  },
  	Caveat => {"Requires a preceding run of " , TO "track", " or ", TO "solveSystem", 
	     " with the (default) option ", TT "Software=>M2engine"},	
        EXAMPLE lines "
R = CC[x,y];
S = {x^2-1,y^2-1};
T = {x^2+y^2-1, x*y};
track(S,T,{(1,1),(1,-1)})
getSolution 0
getSolution(0, SolutionAttributes=>LastT)
getSolution(1, SolutionAttributes=>(Coordinates, SolutionStatus, ConditionNumber))
     	"
	}
///
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
	     "s"=>{"solutions (represented either by lists of coordinates or ", TO2{Point,"points"}}
	     },
	Outputs => {"t"=> "sorted solutions"},
	"The sorting is done lexicographically regarding each complex n-vector as real 2n-vector. ",
	"The output format of ", TO track, " and ", TO solveSystem, " is respected.", BR{}, 
	"For the corresponding coordinates a and b (of two real 2n-vectors) a < b if b-a is larger than ", 
	TO Tolerance, ". ", 
     	PARA {},
        EXAMPLE lines ///
R = CC[x,y];
s = solveSystem {x^2+y^2-1, x*y}
sortSolutions s
     	///
	}

document {
	Key => {areEqual, (areEqual,CC,CC), (areEqual,List,List), (areEqual,Matrix,Matrix), (areEqual,Point,Point), 
	     [areEqual,Projective]},
	Headline => "determine if solutions are equal",
	Usage => "b = areEqual(x,y)",
	Inputs => {
	     "x" => "a solution or list of solutions",
	     "y" => "a solution or list of solutions",
	     Projective=>{"if ", TO true, " then solutions are considered as representatives of points 
		  in the projective space and the Riemannian distance in the projective space is measured"}
	     },
	Outputs => {"b"=>{"tells if ", TT "x", " and ", TT "y", " are approximately equal"}},
	"The function returns false if distance exceeds ", TO Tolerance, " and true, otherwise.",
	PARA {},
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
	The system is normalized, so that it is on the unit sphere in the Bombieri-Weyl norm.",
        PARA {},
	EXAMPLE lines ///
T = randomSd {2,3}
(S,solsS) = goodInitialPair T;
M = track(S,T,solsS,gamma=>0.6+0.8*ii,Software=>M2)
     	///
	}

document {
	Key => {goodInitialPair, (goodInitialPair, List), [goodInitialPair,GeneralPosition], GeneralPosition},
	Headline => "make an intial pair conjectured to be good by Shub and Smale",
	Usage => "(S,sol) = goodInitialPair T",
	Inputs => { 
	     "T" => {"a list of polynomials"},
	     GeneralPosition => {"make a random unitary change of coordinates"} 
	     },
	Outputs => {{ TT "S", ", list of polynomials"},
	     { TT "sol", ", a list containing (one) solution of S"}},
	"Generates a start system S that is conjectured to have good complexity when used in linear homotopy 
       	with target system T leading to one solution. ",
        PARA {},
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
	"Generates a start system S that has an equal chance of reaching any of the solutions of 
       	the target system T. ",
        PARA {},
	EXAMPLE lines ///
T = randomSd {2,3};
(S,solsS) = randomInitialPair T
M = track(S,T,solsS,gamma=>0.6+0.8*ii,Software=>M2)
     	///
	}
								
document {
	Key => {numericalRank, (numericalRank, Matrix), [numericalRank, Threshold]},
	Headline => "numerical rank of a matrix",
	Usage => "r = numericalRank M",
	Inputs => { 
	     {TT "M", ", a matrix with real or complex entries"}
	     },
	Outputs => {{ TT "r"}},
	"Find an approximate rank of the matrix ", TT "M", " by finding the first large 'gap' between two consecutive 
	singular values. The gap between sigma_i and sigma_{i+1} is large if ", TT "sigma_i/sigma_{i+1} > Threshold",
	".",
	Caveat => {"sigma_0=1 is assumed"},
        EXAMPLE lines ///
numericalRank matrix {{2,1},{0,0.001}}
     	///,
     	SeeAlso => {SVD}	
	}

document {
	Key => {Certified},
	Headline => "a value for the option Predictor that triggers certified tracking",
	PARA {
       	     "Tells basic functions, e.g., ", TO track, ", to use soft certification described in"
	     },
	refBeltranLeykin,
	PARA{"The functions related to this paper are:"},
	certifiedTrackingFunctions,
	EXAMPLE lines ///
	R = CC[x,y,z];
	S = {x^2-z^2,y^2-z^2};
	T = {x^2+y^2-z^2, x*y};
	solsS = {(1,-1,1),(1,1,1)};
	track(S,T,solsS,Predictor=>Certified,Normalize=>true)
	///
	}
