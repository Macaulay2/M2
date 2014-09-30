refKroneLeykin := "R. Krone and A. Leykin, \"Numerical algorithms for detecting embedded components.\", arXiv:1405.7871"
refBeltranLeykin := "C. Beltran and A. Leykin, \"Certified numerical homotopy tracking\", Experimental Mathematics 21(1): 69-83 (2012)" 
refBeltranLeykinRobust := "C. Beltran and A. Leykin, \"Robust certified numerical homotopy tracking\", Foundations of Computational Mathematics 13(2): 253-295 (2013)" 
refIntroToNAG := "A.J. Sommese, J. Verschelde, and C.W. Wampler, \"Introduction to numerical algebraic geometry\", 
                  in \"Solving polynomial equations\" (2005), 301--338" 
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
     ", implements methods of polynomial homotopy continuation                                                                                                  
     to solve systems of polynomial equations and describe positive-dimensional complex algebraic varieties. ", 
     
     PARA {"Basic types (such as ", TO Point, " and ", TO "WitnessSet", ") are defined in the package ", TO NAGtypes, "."},
     
     HEADER3 "Basic functions:",
     UL{
	 TO track,
	 TO solveSystem,
	 TO refine,
	 TO totalDegreeStartSystem,
	 TO numericalIrreducibleDecomposition,
     	 TO sample,
     	 TO (isSubset,NumericalVariety,NumericalVariety),
	 },
     "Some of the basic computations can be outsourced to ", TO "Bertini", " and ", TO "PHCpack", 
     " (look for ", TO Software, " option).",
     
     HEADER3 "Service functions:",
     UL{
	 {  
	     "Many service functions (such as ", 
	     TO areEqual," and ",TO sortSolutions,") are defined in the package ", TO NAGtypes, "."
	 },
     TO setDefault,
     TO getDefault,
     TO NAGtrace,
     --     TO toAffineChart,     
     TO newton,
     TO numericalRank,
     TO isOn,
     TO union,
     TO removeRedundantComponents,
--     TO ("==",NumericalVariety,NumericalVariety)
     },

     HEADER3 {"Functions related to scheme analysis:"},
     UL{
	 TO isPointEmbedded,
	 TO isPointEmbeddedInCurve,
	 TO colon,
	 },

     HEADER3 {"Functions related to ", TO "Certified", " tracking:"},
     certifiedTrackingFunctions,

     HEADER3 {"References:"},
     UL{
       refIntroToNAG,
       refSWbook,
       refBeltranLeykin,
       refKroneLeykin
       }
     }

document {
	Key => {setDefault, 1:(setDefault), Attempts, [setDefault, Attempts], 
	     SingularConditionNumber, [setDefault, SingularConditionNumber], 
	     [refine, SingularConditionNumber],  [track,SingularConditionNumber],
	     getDefault, (getDefault,Symbol)},
	Headline => "set/get the default parameters for continuation algorithms",
	Usage => "setDefault(p1=>v1, p2=>v2, ...), v = getDefault p",
	Inputs => { {TT "p, p1, p2", ", ", TO "Symbol", "(s), the name(s) of parameter(s)"},
	     	  Attempts => {" (meaning Attempts = ", toString DEFAULT.Attempts, "). The maximal number of attempts (e.g., to make a random regular homotopy)."},
		  SingularConditionNumber => {" (meaning SingularConditionNumber = ", toString DEFAULT.SingularConditionNumber, "). Matrix is considered to be singular 
		       if its condition number is greater than this value."}
		  },
	Outputs => {
	     {TT "setDefault", " returns ", TO null, "."}, 
	     {TT "getDefault", " returns ", TT "v", ", the value of the specified parameter ", TT "p", "."} },
	PARA {"To see a detailed description of an option click on its name."},
	PARA { "These functions set/get values of optional parameters used in the functions ", 
	     TO "track", ", ", TO "solveSystem", ", ", TO "refine", 
	     " as well as higher-level functions (that are under construction)." }, 
	EXAMPLE lines ///
	getDefault Predictor
     	setDefault(Predictor=>Euler, CorrectorTolerance=>1e-10)
	getDefault Predictor  
     	///,
	SeeAlso => {track, solveSystem, refine, areEqual}
	}
					
document { Key => {AffinePatches, [track,AffinePatches], [setDefault,AffinePatches], DynamicPatch, 
	     SLP, [track,SLP], [setDefault,SLP], HornerForm, CompiledHornerForm, 
	     SLPcorrector, SLPpredictor, [track,SLPcorrector], [setDefault,SLPcorrector], 
	     [track,SLPpredictor], [setDefault,SLPpredictor]},
     	Headline => "reserved for developers"
     	} 

document {
	Key => {(solveSystem, List),solveSystem,(solveSystem,PolySystem)},
	Headline => "solve a square system of polynomial equations",
	Usage => "s = solveSystem F",
	Inputs => { "F"=>"contains polynomials with complex coefficients" },
	Outputs => { "s"=>{"contains all complex solutions to the system ", TT "F=0" }},
	"Solve a system of polynomial equations using homotopy continuation methods. (See ", TO track, " for more optional arguments.)",
     	PARA {"The system is assumed to be square (number of equations = number of variables) 
	     and to have finitely many solutions."},
	EXAMPLE lines ///
R = CC[x,y];
F = {x^2+y^2-1, x*y};
solveSystem F 
     	///,
     	PARA {},
	"The output (produced by ", TO track, " with default options) contains all ", TO2{Point,"points"}, 
	" obtained at the end of homotopy paths when tracking starting at the ", TO totalDegreeStartSystem, ". ",
	"In particular, this means that solving a system that 
	has fewer than Bezout bound many solutions will produce 
	points that are not marked as regular. See ", TO track, " for detailed examples. "
	}


document {
	Key => { (track, List, List, List), track, (track,PolySystem,PolySystem,List),
	     [track,NumericalAlgebraicGeometry$gamma], [setDefault,NumericalAlgebraicGeometry$gamma], [track,NumericalAlgebraicGeometry$tDegree], [setDefault,NumericalAlgebraicGeometry$tDegree], 
	     [track,tStep], [setDefault,tStep], [track,tStepMin], [setDefault,tStepMin],
	     NumericalAlgebraicGeometry$gamma, NumericalAlgebraicGeometry$tDegree, tStep, tStepMin, 
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
	     "S" => {" contains the polynomials in the start system"},
	     "T" => {" contains the polynomials in the target system"},
	     "solsS" => {" contains start solutions"},
	     NumericalAlgebraicGeometry$gamma => {" (meaning gamma = ",  toString DEFAULT.NumericalAlgebraicGeometry$gamma, "). A parameter in the homotopy: ", TEX "H(t)=(1-t)^{tDegree} S + \\gamma t^{tDegree} T."}, 
	     NumericalAlgebraicGeometry$tDegree =>{" (meaning tDegree = ", toString DEFAULT.NumericalAlgebraicGeometry$tDegree, "). A parameter in the homotopy: ", TEX "H(t)=(1-t)^{tDegree} S + \\gamma t^{tDegree} T."},
	     tStep => {" (meaning tStep = ", toString DEFAULT.tStep, "). Initial step size."}, 
	     tStepMin => {" (meaning tStepMin = ", toString DEFAULT.tStepMin, "). Minimal step size."},
	     stepIncreaseFactor => {" (meaning stepIncreaseFactor = ", toString DEFAULT.stepIncreaseFactor, "). Determines how the step size is adjusted."},
	     numberSuccessesBeforeIncrease => {
		  " (meaning numberSuccessesBeforeIncrease = ", toString DEFAULT.numberSuccessesBeforeIncrease, 
		  "). Determines how the step size is adjusted."},
	     Predictor => {" (meaning Predictor = ", toString DEFAULT.Predictor, 
		  "). A method to predict the next point on the homotopy path: choose between ", 
		  TO "RungeKutta4", ", ", TO "Tangent", ", ", 
		  TO "Euler", ", ", TO "Secant", ", ", TO "Multistep", ", ", TO "Certified", 
		  ". The option ", TO "Certified", " provides certified tracking."},
	     MultistepDegree => {" (meaning MultistepDegree = ", toString DEFAULT.MultistepDegree, 
		  "). Degree of the Multistep predictor."},
	     maxCorrSteps => {" (meaning maxCorrSteps = ", toString DEFAULT.maxCorrSteps, 
		  "). Max number of steps corrector takes before a failure is declared."}, 
	     CorrectorTolerance => {" (meaning CorrectorTolerance = ", toString DEFAULT.CorrectorTolerance, "). Corrector succeeds if the relative error does not exceed this tolerance."},
     	     EndZoneFactor => {" (meaning EndZoneFactor = ", toString DEFAULT.EndZoneFactor, "). Determines the size of the \"end zone\", the interval at the end of the path where ", TO CorrectorTolerance, " is tighter." },  
	     InfinityThreshold => {" (meaning InfinityThreshold = ", toString DEFAULT.InfinityThreshold, "). Paths are truncated if the norm of the approximation exceeds the threshold."},
     	     Projectivize => {" (meaning Projectivize = ", toString DEFAULT.Projectivize, "). If true then the system is homogenized and the projective tracker is executed."},
	     Normalize => {" (meaning Normalize = ", toString DEFAULT.Normalize, "). Normalize the start and target systems w.r.t. the Bombieri-Weyl norm."},
	     NoOutput => {" (meaning NoOutput = ", toString DEFAULT.NoOutput, "). If true, no output is produced (used by developers)."} 	     
	     },
	Outputs => {{ TT "solsT", " is a list of ", TO2{Point,"points"}, " that are solutions of ", TT "T=0", " obtained by continuing ", TT "solsS", " of ", TT "S=0" }},
	"Polynomial homotopy continuation techniques are used to obtain solutions 
	of the target system given a start system. ",
	"For an introduction to the subject see ", UL{
	     {refIntroToNAG}, {refSWbook}
	     }, 
	"The package implements a most commonly used homotopy:", 
	PARA{ 
	     TEX "H(t) = \\gamma t^d T + (1-t)^d S" 
	     }, 
	"where ", TEX "S", " and ", TEX "T", " are square systems (number of equations = number of variables) of polynomials over ", TO CC, ", ", 
	TEX "t", " is in the interval ", TEX "[0,1]", " and ",
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
	     In that case the divergent paths are marked with an ", TT "I", 
	     " (", TO2{Point, "status"}, " is set to ", TO Infinity, "). "
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
	     or near-singular (clustered) solutions are marked with an ", TT "M", 
	     " (", TO2{Point, "status"}, " is set to ", TO MinStepFailure, "). "
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
	     "Note that the projective tracker is invoked either if the target system is a homogenous system or if ", TO "Projectivize", TT"=>true",
	     " is specified. "
	     },
	SeeAlso => {solveSystem, setDefault, Point},
	Caveat => {"Predictor=>Certified works only with (Software=>M2 or Software=>M2engine) and Normalize=>true. ", 
	     PARA{"Unspecified optional arguments (with default values ", TO null, 
	     	  ") have their actual values taken from a local hashtable of defaults controlled by the functions ", 
	     	  TO getDefault, " and ", TO setDefault, "."}
	     }
	}

document {
     Key => {[solveSystem,PostProcess],PostProcess},
     Headline => "specifies whether to postprocess the solutions",
     "Postprocessing includes refinement and clustering the solutions.",
     Caveat=>{"Postprocessing is coded in top-level M2 language 
	  and can be much slower than the homotopy contination done without postprocessing."},
     SeeAlso=>{refine}
     }

document {
	Key => {
	     (refine, List, List), refine, 
	     (refine,Point), (refine,PolySystem,List), (refine,PolySystem,Point),
	     [refine, Iterations], [setDefault,Iterations], [refine, Bits], [setDefault,Bits], 
	     [refine,ErrorTolerance], [setDefault,ErrorTolerance], 
	     [refine, ResidualTolerance], [setDefault,ResidualTolerance],
	     Iterations, Bits, ErrorTolerance, ResidualTolerance
	     },
	Headline => "refine numerical solutions to a system of polynomial equations",
	Usage => "solsR = refine(T,sols)",
	Inputs => { 
	     "T" => {"contains the polynomials of the system (may be of type ", TO PolySystem, ")"},
	     "sols" => {"contains (a) solution(s) (", TO2{Point,"points"}," or lists of coordinates or ", TO2{Point,"points"}, ")"},
	     Iterations => {" (meaning Iterations = ", toString DEFAULT.Iterations, "). Number of refining iterations of Newton's method."}, 
	     Bits => {" (meaning Bits = ", toString DEFAULT.Bits, "). Number of bits of precision."}, 
	     ErrorTolerance => {" (meaning ErrorTolerance = ", toString DEFAULT.ErrorTolerance, "). A bound on the desired estimated error."},
	     ResidualTolerance => {" (meaning ResidualTolerance = ", toString DEFAULT.ResidualTolerance, "). A bound on desired residual."}
	     },
	Outputs => {"solsR" => {"contains refined solutions (as ", TO2{Point, "points"}, ")" }},
	"Uses Newton's method to correct the given solutions so that the resulting approximation 
	has its estimated relative error bounded by ", TO "ErrorTolerance", 
	". The number of iterations made is at most ", TO "Iterations", ".",
-- 	Caveat => {"If option ", TT "Software=>M2engine", " is specified, 
-- 	     then the refinement happens in the M2 engine and it is assumed that the last path tracking procedure 
-- 	     took place with the same option and was given the same target system. 
-- 	     Any other value of this option would launch an M2-language procedure."},
        PARA {},
	EXAMPLE lines ///
R = CC[x,y];
T = {x^2+y^2-1, x*y};
sols = { {1.1_CC,0.1}, {-0.1_CC,1.2} };
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
     	///,
	SeeAlso => {solveSystem, track}
	}

document { Key => {[setDefault,Tolerance]},
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
	for a total degree homotopy together with the total degree (Bezout bound) many start solutions.",
     	PARA {"For details see: ", refIntroToNAG},
	EXAMPLE lines ///
R = CC[x,y,z];
T = {x^2+y^2-1, x*y^2, x^5+y*z+3};
totalDegreeStartSystem T
     	///,
	SeeAlso => { track, solveSystem }
	}

document {
     Key => {Software,
	 [solveSystem,Software],[track,Software],[refine, Software],[setDefault,Software],
	 [regeneration,Software],[parameterHomotopy,Software],[isOn,Software],
	 [numericalIrreducibleDecomposition,Software], [hypersurfaceSection,Software],
	 M2,M2engine,M2enginePrecookedSLPs},
     Headline => "specify internal or external software",
     "One may specify which software is used in homotopy continuation. 
     Possible values for internal software are:",  
     UL{
	  {"M2", " -- use top-level Macaulay2 homotopy continuation routines"},
	  {"M2engine", " -- use subroutines implemented in Macaulay2 engine (DEFAULT)"},
	  {"M2enginePrecookedSLPs", " -- (reserved for developers)"},
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
     PARA {"PHCpack interface provided via the ", TO "PHCpack::PHCpack", " package."},
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
	     {TT "b", ", the new level"}
	     },
	Outputs => {{ TT "a", ", the old level"}},
	"Determines how talkative the procedures of NumericalAlgebraicGeometry are. The most meaningful values are:", 
	UL{
	     {"0", " -- silent"},
	     {"1", " -- progress and timings"},
	     {"2", " -- more messages than 1"}
	     },
	"The progress is displayed as follows: ", 
	UL{
	     {TT ".", " = regular solution found"   },
   	     {TT "S", " = singular solution (or encountered a singular point on the path)"   },
	     {TT "I", " = a homotopy path diverged to infinity"},
	     {TT "M", " = minimum step bound reached"}
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
	Key => {(randomSd, List), randomSd},
	Headline => "a random homogeneous system of polynomial equations",
	Usage => "T = randomSd d",
	Inputs => { 
	     "d"=>"contains the degrees"
	     },
	Outputs => {"T"=>"contains polynomials"},
	"Generates a system of homogeneous polynomials ", TEX "T_i", " such that ", TEX "deg(T_i) = d_i", ". 
	The system is normalized, so that it is on the unit sphere in the Bombieri-Weyl norm.",
        PARA {},
	EXAMPLE lines ///
T = randomSd {2,3}
(S,solsS) = goodInitialPair T;
M = track(S,T,solsS,gamma=>0.6+0.8*ii,Software=>M2)
     	///,
	SeeAlso => {Certified,track}
	}

document {
	Key => {(goodInitialPair, List), goodInitialPair, [goodInitialPair,GeneralPosition], GeneralPosition},
	Headline => "make an intial pair conjectured to be good by Shub and Smale",
	Usage => "(S,sol) = goodInitialPair T",
	Inputs => { 
	     "T" => {"contains homogeneous polynomials"},
	     GeneralPosition => {"make a random unitary change of coordinates"} 
	     },
	Outputs => {"S"=>"contains homogeneous polynomials",
	     "sol"=>"contains one solution of S"},
	"Generates a start system ", TT "S", " that is conjectured to have good complexity when used in linear homotopy 
       	with target system ", TT "T", " leading to one solution. ", "For more details see: ", refBeltranLeykin,
        PARA {},
	EXAMPLE lines ///
T = randomSd {2,3};
(S,solsS) = goodInitialPair T
M = track(S,T,solsS,gamma=>0.6+0.8*ii,Software=>M2)
     	///,
	SeeAlso => {Certified, track}
	}

document {
	Key => {randomInitialPair, (randomInitialPair, List)},
	Headline => "a random initial pair",
	Usage => "(S,sol) = randomInitialPair T",
	Inputs => { 
	     "T"=>"contains homogeneous polynomials"
	     },
	Outputs => {
	     "S"=>"contains homogeneous polynomials",
	     "sol"=>"contains one solution of S"},
	"Generates a start system ", TT "S", " that has an equal chance of reaching any of the solutions of 
       	the target system ", TT "T", ". ", 
	"For more details see: ", refBeltranLeykin,  
        PARA {},
	EXAMPLE lines ///
T = randomSd {2,3};
(S,solsS) = randomInitialPair T
M = track(S,T,solsS,gamma=>0.6+0.8*ii,Software=>M2)
     	///,
	SeeAlso => {Certified}
	}
								
document {
	Key => {numericalRank, (numericalRank, Matrix), [numericalRank, Threshold],
	    isFullNumericalRank, (isFullNumericalRank,Matrix)},
	Headline => "numerical rank of a matrix",
	Usage => "r = numericalRank M\nB = isFullNumericalRank M",
	Inputs => { 
	    "M"=>Matrix=>"a matrix with real or complex entries"
	     },
	Outputs => {
	    "r"=>ZZ, 
	    "B"=>Boolean
	    },
	PARA {
	    TO numericalRank, " finds an approximate rank of the matrix ", TT "M", "."
	    },
	PARA {
	    TO isFullNumericalRank, " = ", TT "M", " is _not_ rank-deficient."
	    },
	PARA {
	    "Let ", TEX "\\sigma_1,...,\\sigma_n", " be the singular values of ", TT "M", ". "
	    },
	PARA {
	    "If ", TO Threshold, " is >1, then to establish numerical rank we look 
	    for the first large gap between two consecutive singular values. ",
	    "The gap between ", TEX "\\sigma_i", " and ", TEX "\\sigma_{i+1}", 
	    " is large if ", TEX "\\sigma_i/\\sigma_{i+1} > ", TO Threshold,
	    "."
	    },
	PARA {
	    "If ", TO Threshold, " is <=1, then the rank equals 
	    the number of singular values larger then ", TO Threshold, "." 
	    },
	Caveat => {"We assume ", TEX "\\sigma_0=1", " above."},
        EXAMPLE lines ///
numericalRank matrix {{2,1},{0,0.001}}
     	///,
     	SeeAlso => {SVD}
	}

document {
	Key => {Certified},
	Headline => "a value for the option Predictor that triggers certified tracking",
	PARA {
       	     "Tells basic functions, e.g., ", TO track, ", to use ", EM "soft", " certification described in"
	     },
	refBeltranLeykin,
        PARA {
	    "The code for ", EM "robust", " certification is not incorporated in this package at the moment; the location of this stand-alone code is in the references of " 
	    },
	refBeltranLeykinRobust, 
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

document {
	Key => {(regeneration, List),regeneration,[regeneration,Output],Output},
	Headline => "solve a system of polynomial equations with regeneration method",
	Usage => "Ws = regeneration F",
	Inputs => { "F"=>"contains polynomials with complex coefficients" },
	Outputs => { "Ws"=>{"contains ", TO2{WitnessSet,"witness sets"}, " for equidimensional components of the variety ", TT "{x|F(x)=0}" }},
     	"Regeneration is a blackbox method that obtains a numerical describtion of an algebraic variety. ",
	"Note that ", TT "Ws", " are not necessarily irreducible witness sets; use ", 
	TO (decompose, WitnessSet), " to decompose into irreducibles. ",
	EXAMPLE lines ///
R = CC[x,y]
F = {x^2+y^2-1, x*y};
regeneration F 
R = CC[x,y,z]
sph = (x^2+y^2+z^2-1); 
regeneration {sph*(x-1)*(y-x^2), sph*(y-2)*(z-x^3)}
     	///,
-- 	EXAMPLE lines /// -- nonreduced scheme
-- setRandomSeed 7
-- R = CC[x,y]
-- F = {x^2+y^2-1, x*y};
-- regeneration F 
-- R = CC[x,y,z]
-- sph = (x^2+y^2+z^2-1); 
-- I = ideal {sph*(x-1)*(y-x^2), sph*(y-1)*(z-x^3)};
-- cs = regeneration I_*
--      	///,
	Caveat => {"This function is under development. It may not work well if the input represents a nonreduced scheme.",
	     "The (temporary) option ", TO Output, " can take two values: ", TO Regular, " (default) and ", TO Singular, ". 
	     It specifies whether the algorithm attempts to keep singular points." },
        SeeAlso=>{(decompose, WitnessSet)}
	}
document {
	Key => {(decompose,WitnessSet)},
	Headline => "decompose a witness set into irreducibles",
	Usage => "Ws = decompose W",
	Inputs => { "W"=>"represents an equidimensional component of a variety" },
	Outputs => { "Ws"=>{"contains irreducible witness sets ", TO2{WitnessSet,"witness sets"}, ", the union of which is ", TT "W"}},
     	"Monodromy driven decomposition is followed by the linear trace test. ",
	EXAMPLE lines ///
R = CC[x,y]
F = {x^2+y^2-1, x*y};
W = first components regeneration F 
decompose W
R = CC[x,y,z]
sph = (x^2+y^2+z^2-1); 
decompose \ components regeneration {sph*(x-1)*(y-x^2), sph*(y-2)*(z-x^3)}
     	///,
	Caveat => {"This function is under development. It can not decompose nonreduced components at the moment. 
	     If monodromy breakup algorithm fails to classify some points, the unnclassified points appear 
	     as one witness set (that is not marked as irreducible)." },
        SeeAlso=>{regeneration}
	}

document {
	Key => {(numericalIrreducibleDecomposition, Ideal), numericalIrreducibleDecomposition},
	Headline => "constructs a numerical variety defined by the given ideal",
	Usage => "V = numericalIrreducibleDecomposition I",
	Inputs => { "I"=>"contained in the ring of polynomials with complex coefficients" },
	Outputs => { "V" },
     	"The ", TO2{WitnessSet,"witness sets"}, " of the ", TO2{NumericalVariety,"numerical variety"}, TT "V",
	" are in one-to-one correspondence with irreducible components of the variety defined by ", TT "I", ". ", 
	EXAMPLE lines ///
setRandomSeed 1
R = CC[x,y,z]
sph = (x^2+y^2+z^2-1); 
I = ideal {sph*(x-1)*(y-x^2), sph*(y-2)*(z-x^3)};
V = numericalIrreducibleDecomposition I 
peek V
    	///,
	Caveat => {"This function is under development. It may not work well if the input represents a nonreduced scheme." },
        SeeAlso=>{(decompose, WitnessSet)}
	}


document {
    Key => {isOn, (isOn,Point,Ideal), (isOn,Point,NumericalVariety), 
	(isOn,Point,RingElement), (isOn,Point,WitnessSet), (isOn,Point,WitnessSet,ZZ),
	[isOn,Tolerance]
	},
    Headline => "determines if a point belongs to a variety",
    Usage => "B = isOn(P,V)",
    Inputs => { 
	"P"=>Point,  
	"V"=>{ofClass NumericalVariety, ofClass WitnessSet, ofClass Ideal, ofClass RingElement}
	},
    Outputs => { "B"=>Boolean },
    "Determines whether the given point is (approximately) on the given variety, 
    which is either represented numerically or defines by polynomials.", 
    EXAMPLE lines ///
R = CC[x,y]
I = ideal((x^2+y^2+2)*x,(x^2+y^2+2)*y);
e = 0.0000001
W = witnessSet(ideal I_0 , ideal(x-y), {point {{ (1-e)*ii,(1-e)*ii}},point {{ -(1+e)*ii,-(1+e)*ii}}})	
isOn(point {{sqrt 5*ii,sqrt 3}},W)
///,
    SeeAlso=>{Point,NumericalVariety}
    }

document {
    Key => {newton, (newton,PolySystem,Matrix), (newton,PolySystem,Point)},
    Headline => "Newton-Raphson method",
    "Performs one step of the Newton-Raphson method.",
    Caveat=>{"Works for a regular square or overdetermined system."}
    }

document {
    Key => {(union,NumericalVariety,NumericalVariety), union},
    Headline => "union of numerical varieties",
    Usage => "VW=union(V,W)",
    Inputs => { "V","W" },
    Outputs => { "VW"=>NumericalVariety },
    "Constructs the union of numerical varieties", 
    Caveat => {"The rusulting numerical variety may have redundant components."},
    SeeAlso=>{removeRedundantComponents}
    }

document {
    Key => {(removeRedundantComponents,NumericalVariety), removeRedundantComponents, [removeRedundantComponents,Tolerance]},
    Headline => "remove redundant components",
    Usage => "removeRedundantComponents V",
    Inputs => { "V"},
--    Outputs => { "" },
    "Removes components contained in other components of the variety. (This is done \"in place\".)", 
    SeeAlso=>{(isSubset,WitnessSet,WitnessSet)}
    }
document {
    Key => {(sample,WitnessSet), sample, [sample,Tolerance]},
    Headline => "sample a point on a component",
    Usage => "P = sample W",
    Inputs => { "W" },
    Outputs => { "P"=>Point },
    "Gets a random point on a component represented numerically.", 
    EXAMPLE lines ///
R = CC[x,y,z]
W = new WitnessSet from { Equations => ideal {x^2+y^2+z^2-1, z^2}, Slice => matrix "1,0,0,0", Points => {{{0,1,0_CC}},{{0,-1,0_CC}}}/point } 
P := sample(W, Tolerance=>1e-15)   
isOn(P,W)
    ///,
    Caveat => {"not yet working for singular components"},
    SeeAlso=>{WitnessSet, isOn}
    }

document {
    Key => {deflate,(deflate,Ideal),(deflate,PolySystem,List),(deflate,PolySystem,Matrix),
	(deflate,PolySystem,Point),(deflate,PolySystem,Sequence),(deflate,PolySystem,ZZ),
	Deflation, DeflationSequence, DeflationRandomMatrix, -- attached to a PolySystem
	liftPointToDeflation,(liftPointToDeflation,Point,PolySystem,ZZ),
	LiftedSystem, LiftedPoint, SolutionSystem, DeflationSequenceMatrices, -- attached to a Point
	deflateInPlace, (deflateInPlace,Point,PolySystem), 
	SquareUp, [deflateInPlace,SquareUp], -- whether to square up at each step
	},
    Headline => "first-order deflation",
    "Deflate a polynomial system to restore quadratic convergence of Newton's method", 
    Caveat => {"Needs more documentation!!!"},
    SeeAlso=>{PolySystem,newton}
    }

document {
    Key => {(isSubset,NumericalVariety,NumericalVariety), (isSubset,WitnessSet,WitnessSet)},
    Headline => "check containment",
    Usage => "B = isSubset(V,W)",
    Inputs => { 
	"V"=>{" or ", ofClass WitnessSet}, 
	"W"=>{" or ", ofClass WitnessSet} 
	},
    Outputs => { "B"=>Boolean },
    "Checks containment of one variety represented numerically in the other.", 
    Caveat => {"Does not work for singular components."},
    SeeAlso=>{WitnessSet,isOn}
    }

document {
    Key => {
	(isPointEmbedded,Point,Ideal,List), isPointEmbedded,
	AllVisible, [isPointEmbedded,AllVisible],
	},
    Headline => "determine if the point is an embedded component of the scheme",
    Usage => "B = isPointEmbedded(P,I,C)",
    Inputs => { 
	"P", 
	"I",
	"C"=>{" witness sets representing components of ", TT "Spec(I)", " containing ", TT "P"} 
	},
    Outputs => { "B"=>Boolean },
    PARA {"Runs an embedded component test described in "},
    refKroneLeykin,
    SeeAlso=>{isPointEmbeddedInCurve}
    }

document {
    Key => {
	(isPointEmbeddedInCurve,Point,Ideal), isPointEmbeddedInCurve
	},
    Headline => "determine if the point is an embedded component of a 1-dimensional scheme",
    Usage => "B = isPointEmbeddedInCurve(P,I)",
    Inputs => { 
	"P", 
	"I"
	},
    Outputs => { "B"=>Boolean },
    PARA {"Runs an embedded component test described in "},
    refKroneLeykin,
    SeeAlso=>{isPointEmbeddedInCurve}
    }

document {
    Key => {colon, (colon,DualSpace,RingElement), (colon,DualSpace,Ideal)},
    Headline => "colon of a (truncated) dual space",
    Usage => "Dg = colon(D,g)\nDJ = colon(D,J)",
    Inputs => { "D"=>DualSpace, "g"=>RingElement, "J"=>Ideal },
    Outputs => { "Dg, DJ"=>DualSpace },
    "Computes (a part of) the dual space of the dual. See",
    PARA { refKroneLeykin },
    "for a description."
    }

document {
    Key => {squareUp, (squareUp,PolySystem), (squareUp,PolySystem, Matrix), 
	SquaredUpSystem, SquareUpMatrix
	},
    Headline => "square up a polynomial system",
    Usage => "G = squareUp F\nG = squareUp(F,M)",
    Inputs => { 
	"F"=>PolySystem,
	"M"=>Matrix=>{" the matrix used to square up the system (by default a random matrix is picked)"}  
	},
    Outputs => { "G"=>PolySystem },
    "Squares up an overdetermined polynomial system. Attaches keys ", 
    TO SquareUpMatrix, " and ", TO SquaredUpSystem,
    " to ", TT "F", ".", 
    EXAMPLE lines ///
    CC[x,y]; F = polySystem {x^2+y^2,x^3+y^3,x^4+y^4}
    G := squareUp F
    peek F
    ///,
    SeeAlso=>{PolySystem}
    }

document {
    Key => {
	numericalIntersection, (numericalIntersection,NumericalVariety,Ideal), 
	(numericalIntersection,NumericalVariety,NumericalVariety), (numericalIntersection,WitnessSet,WitnessSet),
	hypersurfaceSection, (hypersurfaceSection,NumericalVariety,RingElement)
	},
    Headline => "intersection of numerical varieties",
    Caveat => {"Under construction!!!"}
    }

document {
    Key => {(isSolution,Point,PolySystem), isSolution},
    Headline => "check if a point satisfies a polynomial system approximately",
    Caveat => {"Either rewrite or phase out!!!"}
    }

document {
    Key => {(parameterHomotopy,List,List,List),parameterHomotopy},
    Headline => "solve a parametric system of equations",
    "Solves a parameteric polynomial system for several values of parameters.", 
    Caveat => {"Avalaible only with Software=>BERTINI at the moment..."}
    }

document {
    Key => {(trackSegment,PolySystem,Number,Number,List), trackSegment},
    Headline => "track the one-parametric homotopy",
    "Tracks a homotopy on a linear segment in complex plane..",
    Caveat => {"Experimental: implemented only with SLPs at the moment!!!"}
    }

{*-------- TEMPLATE ------------------
document {
    Key => {,},
    Headline => "",
    Usage => "",
    Inputs => { ""=>"" },
    Outputs => { "" },
    "", 
    EXAMPLE lines ///
    ///,
    Caveat => {"" },
    SeeAlso=>{()}
    }
*}
