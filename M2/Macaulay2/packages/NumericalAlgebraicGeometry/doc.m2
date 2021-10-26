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
     to solve systems of polynomial equations, ",
     EXAMPLE lines ///
R = CC[x,y,z];
F = {x^2+y^2+z^2-1, y-x^2, z-x^3};
s = solveSystem F 
realPoints s
///,
     "and describe positive-dimensional complex algebraic varieties, ",
     EXAMPLE lines ///
R = CC[x,y,z];
sph = x^2+y^2+z^2-1; 
I = ideal {x*sph*(y-x^2), sph*(z-x^3)};
numericalIrreducibleDecomposition I 
///,      
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
     "Optionally, the user may outsource some basic routines to ", TO "Bertini", " and ", TO "PHCpack", 
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
     TO isOn,
     TO union,
     TO removeRedundantComponents,
--     TO ("==",NumericalVariety,NumericalVariety)
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
	     [setDefault,Precision],
	     getDefault, (getDefault,Symbol)},
	Headline => "set/get the default parameters for continuation algorithms",
	Usage => "setDefault(p1=>v1, p2=>v2, ...), v = getDefault p",
	Inputs => { {TT "p, p1, p2", ", ", TO "Symbol", "(s), the name(s) of parameter(s)"},
	     	  Attempts => {" (meaning Attempts = ", toString DEFAULT.Attempts, "). The maximal number of attempts (e.g., to make a random regular homotopy)."},
		  SingularConditionNumber => {" (meaning SingularConditionNumber = ", toString DEFAULT.SingularConditionNumber, "). Matrix is considered to be singular 
		      if its condition number is greater than this value."},
		  Precision =>{" (meaning bits of precision)"}		      	   
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
	     [track,SLPpredictor], [setDefault,SLPpredictor],
	     --[trackSegment,AffinePatches], [trackSegment,SLP], [trackSegment,SLPcorrector], [trackSegment,SLPpredictor]
	     },
     	Headline => "reserved for developers"
     	} 

document {
	Key => {(solveSystem, List),solveSystem,(solveSystem,PolySystem)},
	Headline => "solve a system of polynomial equations",
	Usage => "s = solveSystem F",
	Inputs => { "F"=>"contains polynomials with complex coefficients" },
	Outputs => { "s"=>{"contains all complex solutions to the system ", TT "F=0" }},
	"Solve a system of polynomial equations using homotopy continuation methods.",
     	PARA {},
	EXAMPLE lines ///
R = CC[x,y];
F = {x^2+y^2-1, x*y};
solveSystem F 
     	///,
	EXAMPLE lines ///
R = CC[x,y];
F = {x^2+y^2-1, x*y, x*(y+1)};
solveSystem F 
	///,
     	PARA {"The system is assumed to have finitely many solutions. If it is not square (number of equations = number of variables), ", 
	    TO squareUp, " is applied and solutions to the original system are then picked out from the resulting (larger) set of solutions."},
	PARA {"The output (produced by ", TO track, " with default options) contains all ", TO2{Point,"points"}, 
	    " obtained at the end of homotopy paths when tracking starting at the ", TO totalDegreeStartSystem, ". ",
	    "In particular, this means that solving a system that 
	    has fewer than Bezout bound many solutions will produce 
	    points that are not marked as regular. See ", TO track, " for detailed examples. "
	    }
	}


document { Key => {"numerical homotopy tracking options",
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
	NoOutput, 
	[refine, Iterations], [setDefault,Iterations], [refine, Bits], [setDefault,Bits], 
	[refine,ErrorTolerance], [setDefault,ErrorTolerance], 
	[refine, ResidualTolerance], [setDefault,ResidualTolerance],
	Iterations, Bits, ErrorTolerance, ResidualTolerance,
	-- solveSystem
	[solveSystem,CorrectorTolerance], [solveSystem,EndZoneFactor], [solveSystem,gamma], [solveSystem,InfinityThreshold], 
	[solveSystem,maxCorrSteps], [solveSystem,Normalize], [solveSystem,numberSuccessesBeforeIncrease],
	[solveSystem,Predictor], [solveSystem,Projectivize], [solveSystem,SingularConditionNumber],
	[solveSystem,stepIncreaseFactor], [solveSystem,tDegree], [solveSystem,tStep], [solveSystem,tStepMin],
	[solveSystem,Precision],[solveSystem,ResidualTolerance],
    	-- endGameCauchy
	[endGameCauchy,InfinityThreshold], [endGameCauchy,EndZoneFactor], [endGameCauchy,maxCorrSteps], [endGameCauchy,numberSuccessesBeforeIncrease], [endGameCauchy,stepIncreaseFactor], [endGameCauchy,CorrectorTolerance], [endGameCauchy,tStep], [endGameCauchy,tStepMin],
    	-- trackHomotopy
	[trackHomotopy,EndZoneFactor], [trackHomotopy,maxCorrSteps], [trackHomotopy,Predictor], [trackHomotopy,stepIncreaseFactor], [trackHomotopy,Precision], [trackHomotopy,tStep], [trackHomotopy,CorrectorTolerance], [trackHomotopy,NoOutput], [trackHomotopy,InfinityThreshold], [trackHomotopy,Software], [trackHomotopy,numberSuccessesBeforeIncrease], [trackHomotopy, tStepMin],
	-- segmentHomotopy
	[segmentHomotopy,gamma]
	},
    Headline => "options for core functions of Numerical Algebraic Geometry",
    UL apply({
	NumericalAlgebraicGeometry$gamma => {" (default gamma = ",  toString DEFAULT.NumericalAlgebraicGeometry$gamma, "). A parameter in the homotopy: ", TEX "H(t)=(1-t)^{tDegree} S + \\gamma t^{tDegree} T."}, 
	NumericalAlgebraicGeometry$tDegree =>{" (default tDegree = ", toString DEFAULT.NumericalAlgebraicGeometry$tDegree, "). A parameter in the homotopy: ", TEX "H(t)=(1-t)^{tDegree} S + \\gamma t^{tDegree} T."},
	tStep => {" (default tStep = ", toString DEFAULT.tStep, "). Initial step size."}, 
	tStepMin => {" (default tStepMin = ", toString DEFAULT.tStepMin, "). Minimal step size."},
	stepIncreaseFactor => {" (default stepIncreaseFactor = ", toString DEFAULT.stepIncreaseFactor, "). Determines how the step size is adjusted."},
	numberSuccessesBeforeIncrease => {
	    " (default numberSuccessesBeforeIncrease = ", toString DEFAULT.numberSuccessesBeforeIncrease, 
	    "). Determines how the step size is adjusted."},
	Predictor => {" (default Predictor = ", toString DEFAULT.Predictor, 
	    "). A method to predict the next point on the homotopy path: choose between ", 
	    TO "RungeKutta4", ", ", TO "Tangent", ", ", 
	    TO "Euler", ", ", TO "Secant", ", ", TO "Multistep", ", ", TO "Certified", 
	    ". The option ", TO "Certified", " provides certified tracking."},
	MultistepDegree => {" (default MultistepDegree = ", toString DEFAULT.MultistepDegree, 
	    "). Degree of the Multistep predictor."},
	maxCorrSteps => {" (default maxCorrSteps = ", toString DEFAULT.maxCorrSteps, 
	    "). Max number of steps corrector takes before a failure is declared."}, 
	CorrectorTolerance => {" (default CorrectorTolerance = ", toString DEFAULT.CorrectorTolerance, "). Corrector succeeds if the relative error does not exceed this tolerance."},
	EndZoneFactor => {" (default EndZoneFactor = ", toString DEFAULT.EndZoneFactor, "). Determines the size of the \"end zone\", the interval at the end of the path where ", TO CorrectorTolerance, " is tighter." },  
	InfinityThreshold => {" (default InfinityThreshold = ", toString DEFAULT.InfinityThreshold, "). Paths are truncated if the norm of the approximation exceeds the threshold."},
	Projectivize => {" (default Projectivize = ", toString DEFAULT.Projectivize, "). If true then the system is homogenized and the projective tracker is executed."},
	Normalize => {" (default Normalize = ", toString DEFAULT.Normalize, "). Normalize the start and target systems w.r.t. the Bombieri-Weyl norm."},
	NoOutput => {" (default NoOutput = ", toString DEFAULT.NoOutput, "). If true, no output is produced (used by developers)."},
	Iterations => {" (default Iterations = ", toString DEFAULT.Iterations, "). Number of refining iterations of Newton's method."}, 
	Bits => {" (default Bits = ", toString DEFAULT.Bits, "). Number of bits of precision."}, 
	ErrorTolerance => {" (default ErrorTolerance = ", toString DEFAULT.ErrorTolerance, "). A bound on the desired estimated error."},
	ResidualTolerance => {" (default ResidualTolerance = ", toString DEFAULT.ResidualTolerance, "). A bound on desired residual."},
	Precision => {" (default Precision = ", toString DEFAULT.Precision, "). Precision of the floating-point numbers used in computation. If set to ", 
	    TO "infinity", "(EXPERIMENTAL!) the precision in homotopy continuation adapts according to numerical conditioning. ", 
	    "The default setting, ", TT "DoublePrecision", ", forces fast arithmetic and linear algebra in standard precision. (Other settings invoke MPFR library.)"}
    	}, 
        item -> {TT "[", TT toString item#0, TT "]: "} | item#1 
	)
    }
document {Key => { (track, List, List, List), track, (track,PolySystem,PolySystem,List) },
	Headline => "track a linear segment homotopy given start and target system",
	Usage => "solsT = track(S,T,solsS)",
	Inputs => { 
	     "S" => {" contains the polynomials in the start system"},
	     "T" => {" contains the polynomials in the target system"},
	     "solsS" => {" contains start solutions"},
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
	     "Note that the projective tracker is invoked either if the target system is a homogeneous system or if ", TO "Projectivize", TT"=>true",
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
	  and can be much slower than the homotopy continuation done without postprocessing."},
     SeeAlso=>{refine}
     }

document {
	Key => {
	     (refine, List, List), refine, 
	     (refine,Point), (refine,PolySystem,List), (refine,PolySystem,Point),
	     },
	Headline => "refine numerical solutions to a system of polynomial equations",
	Usage => "solsR = refine(T,sols)",
	Inputs => { 
	     "T" => {"contains the polynomials of the system (may be of type ", TO PolySystem, ")"},
	     "sols" => {"contains (a) solution(s) (", TO2{Point,"points"}," or lists of coordinates or ", TO2{Point,"points"}, ")"},
	     },
	Outputs => {"solsR" => {"contains refined solutions (as ", TO2{Point, "points"}, ")" }},
	"Uses Newton's method to correct the given solutions so that the resulting approximation 
	has its estimated relative error bounded by min(", TO "ErrorTolerance", ",2^(-", TO "Bits", ")). ",
	"The number of iterations made is at most ", TO "Iterations", ".",
-- 	Caveat => {"If option ", TT "Software=>M2engine", " is specified, 
-- 	     then the refinement happens in the M2 engine and it is assumed that the last path tracking procedure 
-- 	     took place with the same option and was given the same target system. 
-- 	     Any other value of this option would launch an M2-language procedure."},
        PARA {},
	EXAMPLE lines ///
     	R = CC[x];
     	F = polySystem {x^2-2};
	P := refine(F, point{{1.5+0.001*ii}}, Bits=>1000)
	first coordinates P
	R = CC[x,y];
	T = {x^2+y^2-1, x*y};
	sols = { {1.1,-0.1}, {0.1,1.2} };
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
	PARA {},
    	"The failure to complete the refinement procedure is indicated 
	by warning messages and the resulting point is displayed as ", TT "[R]", ".",
	PARA {},
	EXAMPLE lines ///
     	R = CC[x];
     	F = polySystem {x^2-2};
	Q := refine(F, point{{1.5+0.001*ii}}, Bits=>1000, Iterations=>2)
	peek Q
     	///,
	PARA {},	
	Caveat => {"There are 2 'safety' bits in the computation. 
	    If the condition of the system at the refined point is poor 
	    the number of correct bits may be much smaller than requested."},
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
	 --[trackSegment,Software],
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
	Headline => "make an initial pair conjectured to be good by Shub and Smale",
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
     	"Regeneration is a blackbox method that obtains a numerical description of an algebraic variety. ",
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
R = CC[x,y,z]
sph = (x^2+y^2+z^2-1); 
I = ideal {sph*(y-x^2), sph*(z-x^3)};
numericalIrreducibleDecomposition I 
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
	"V"=>{ofClass NumericalVariety, ", ", ofClass WitnessSet, ", ", ofClass Ideal, ", or ", ofClass RingElement}
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
W = witnessSet(ideal {x^2+y^2+z^2-1, z^2}, matrix "1,0,0,0", {{{0,1,0_CC}},{{0,-1,0_CC}}}/point ) 
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
	[deflate,Variable]
	},
    Headline => "first-order deflation",
    Usage => "r = deflate(F,P); r = deflate(F,r); r = deflate(F,B), ...",
    Inputs => { "P"=>Point, "F"=>PolySystem, "r"=>ZZ, "B"=>Matrix },
    Outputs => { "r"=>ZZ=>"the rank used in the (last) deflation"},
    PARA{
	"The purpose of deflation is to restore quadratic convergence of Newton's method in a neighborhood of a singular 
    isolated solution P. This is done by constructing an augemented polynomial system with a solution of strictly lower multiplicity projecting to P."},
    Consequences => {{"Attaches the keys ", TO Deflation, " and ", TO DeflationRandomMatrix, 
	" which are MutableHashTables that (for rank r, a potential rank of the jacobian J of F) store ",
	" the deflated system DF and a matrix B used to obtain it. ", 
	" Here B is a random matrix of size n x (r+1), where n is the number of variables 
	and DF is obtained by appending to F the matrix equation J*B*[L_1,...,L_r,1]^T = 0.
	The polynomials of DF use the original variables and augmented variables L_1,...,L_r."}},
    PARA{
	"Apart from ", TT "P", ", ", ofClass Point,", one can pass various things as the second argument."  
	},
    UL {
	{ofClass ZZ, " ", TT "r", " specifies the rank of the Jacobian dF (that may be known to the user)"},
	{ofClass Matrix, " ", TT "B", " specifies a fixed (r+1)-by-n matrix to use in the deflation construction."},
	{"a pair of matrices ", TT "(B,M)", " specifies additionally a matrix that is used to ", TO squareUp, "."},
	{"a list", TT "{(B1,M1),(B2,M2),...}", 
	    " prompts a chain of successive delations using the provided pairs of matrices."},
	},
    "The option ", TT "Variable", " specifies the base name for the augmented variables.",
    EXAMPLE lines ///
CC[x,y,z]
F = polySystem {x^3,y^3,x^2*y,z^2}
P0 = point matrix{{0.000001, 0.000001*ii,0.000001-0.000001*ii}}
isFullNumericalRank evaluate(jacobian F,P0)
r1 = deflate (F,P0)
P1' = liftPointToDeflation(P0,F,r1) 
F1 = F.Deflation#r1
P1 = newton(F1,P1')
isFullNumericalRank evaluate(jacobian F1,P1)
r2 = deflate (F1,P1)
P2' = liftPointToDeflation(P1,F1,r2) 
F2 = F1.Deflation#r2
P2 = newton(F2,P2')
isFullNumericalRank evaluate(jacobian F2,P2)
P = point {take(coordinates P2, F.NumberOfVariables)}
assert(residual(F,P) < 1e-50)	
    ///,
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
    Key => {squareUp, (squareUp,PolySystem), (squareUp,PolySystem,ZZ), (squareUp,PolySystem, Matrix), 
	SquaredUpSystem, SquareUpMatrix
	},
    Headline => "square up a polynomial system",
    Usage => "G = squareUp F\\nG = squareUp(F,M)\\nsquareUp(F,n)",
    Inputs => { 
	"F"=>PolySystem,
	"M"=>Matrix=>{" the matrix used to square up the system (by default a random matrix is picked)"},
	"n"=>ZZ=>{" the number of polynomials to be formed (by default, this equals the number of variables)"}  
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
    Key => {(isSolution,Point,PolySystem), isSolution, [isSolution,Tolerance] },
    Headline => "check if a point satisfies a polynomial system approximately",
    Caveat => {"Either rewrite or phase out!!!"}
    }

document {
    Key => {(parameterHomotopy,List,List,List),parameterHomotopy},
    Headline => "solve a parametric system of equations",
       Usage => "sols = parameterHomotopy(F,varsP,valuesP)",
    Inputs => { 
	"F" => {" contains the polynomials in the system"},
	"varsP" => {" names of the parameters"},
	"valuesP" => {" contains (possibly several sets of) values of the parameters"}  
	},
    Outputs => { "sols"=>" lists of lists of solutions for each set of the parameters" },
    "Solves a parameteric polynomial system for several values of parameters.", 
    EXAMPLE lines ///
    R = CC[u1,u2,u3,x,y]
    f1 = u1*(y-1)+u2*(y-2)+u3*(y-3)
    f2 = (x-11)*(x-12)*(x-13)
    try parameterHomotopy({f1,f2},{u1,u2,u3},{{1,0,0},{0,1+2*ii,0}}, Software=>BERTINI) else "need to install Bertini to run these lines"
///,
    Caveat => {"Available only with Software=>BERTINI at the moment..."}
    }

-*
document {
    Key => {(trackSegment,PolySystem,Number,Number,List), trackSegment},
    Headline => "track the one-parametric homotopy",
    "Tracks a homotopy on a linear segment in complex plane..",
    Caveat => {"Experimental: implemented only with SLPs at the moment!!!"}
    }
*-

document {
    Key => {(solveGenericSystemInTorus,List), solveGenericSystemInTorus, (solveGenericSystemInTorus,PolySystem)},
    Headline => "solve a generic system of sparse polynomial equations in the torus",
    Usage => "s = solveGenericSystemInTorus F",
    Inputs => { "F"=>"contains polynomials with complex coefficients" },
    Outputs => { "s"=>{"contains all complex solutions in the torus 
	    (i.e., with no zero coordinates) to ", TT "G=0", 
	    ", where ", TT "G", 
	    " is a generic system with the same monomial support as ", TT "F" } 
	},
    "Polyhedral homotopy approach is used to compute the solutions in the torus. 
    The number of the solutions equals the ", EM "mixed volume", 
    " of the Newton polytopes of polynomials in ", TT "F", ".", 
    Caveat => {"PHCpack needs to be installed."},
    SeeAlso=>{PHCPACK, PHCpack, solveSystem}
    }

-*-------- TEMPLATE ------------------
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
*-

document {
    Key => {(gateHomotopy, GateMatrix, GateMatrix, InputGate),
	gateHomotopy,
	GateHomotopy,
	(numVariables,GateHomotopy) 
	},
    Headline => "homotopy implemented via straight line programs",
    Usage => "HS = gateHomotopy(H,X,T)",
    Inputs => { 
	"H"=>"a family of systems (given by a column vector)",
	"X"=>"(a row vector of) variables",
	"T"=>"homotopy (continuation) parameter" 
	 },
    Outputs => { "HS", {
	    ofClass {GateHomotopy, GateParameterHomotopy},
	    ", a homotopy that can be used with some routines of ", TO "NumericalAlgebraicGeometry" }},
    "Optional arguments:",
    UL{
	{TO "Software", "-- specifies how the homotopy is evaluated: ", TT "(M2,M2engine)"}
	},  
    EXAMPLE lines ///
X = inputGate symbol X
Y = inputGate symbol Y
T = inputGate symbol T
F = {X*X-1, Y*Y*Y-1}
G = {X*X+Y*Y-1, X*X*X+Y*Y*Y-1}
H = (1 - T) * F + T * G
HS = gateHomotopy(transpose matrix {H},matrix{{X,Y}},T)
    ///,
    Caveat => {"The order of inputs for unexported internal evaluation functions (evaluateH, etc.) is fixed as follows: ",
	TT "Parameters, X, T", "."},
    SeeAlso=>{GateHomotopy,GateParameterHomotopy,specialize}
    }
doc ///
    Key 
        [gateHomotopy,Software]
    Headline
    	specifies where evaluation should be done (M2=top level, M2engine=core)     	
///
doc ///
    Key 
	[gateHomotopy,Parameters]
    Headline
    	specifies parameter names
///
doc ///
    Key 
	[gateHomotopy,Strategy]
    Headline
    	strategy is either to "compress" or not (any other value)
///    

document {
    Key => "DoublePrecision",
    Headline => "a constant equal to 53 (the number of bits of precision)"
    }

doc ///
Key
  (evaluateH,GateHomotopy,Matrix,Number)
  (evaluateHt,GateHomotopy,Matrix,Number)
  (evaluateHx,GateHomotopy,Matrix,Number)
Headline
  evaluate gate homotopy and its derivatives 
///  

doc ///
    Key
	gateSystem
	(gateSystem,GateMatrix,GateMatrix)
	(gateSystem,GateMatrix,GateMatrix,GateMatrix)
	(gateSystem,Matrix)
	(gateSystem,PolySystem)
	(gateSystem,PolySystem,List)
    Headline
        a constructor for GateSystem
    Usage
    	gateSystem(params,variables,M)
	gateSystem(variables,M)
    Inputs
    	M:GateMatrix
	parameters:GateMatrix
	variables:GateMatrix	  
    Description
    	Text 
            @TO GateMatrix@ {\tt M} is expected to have 1 column.
    	    Matrices {\tt params} and {\tt variables} are expected to have 1 row.
        Example
            variables = declareVariable \ {x,y}
    	    F = gateSystem(matrix{variables}, matrix{{x*y-1},{x^3+y^2-2}})
	    evaluate(F,point{{0.1,0.2+ii}}) 
	    evaluate(F,point{{1/2,1/3}})
	    evaluate(F,point{{2_(ZZ/101),3}})
	Text
	    Systems with parameters are allowed. 
	Example    
	    params = declareVariable \ {a,b}  
	    Fab = gateSystem(matrix{params}, matrix{variables}, matrix{{a*x*y-1},{x^3+y^2-b}})
	    evaluate(Fab,point{{1,2}},point{{0.1,0.2+ii}})
    Caveat
        Note for developers: there is a version of the constructor that builds @TO GateSystem@ from @TO PolySystem@. 
	Its variant that takes the list of variables to treat as parameters is likely to disappear. 
    SeeAlso
    	System
        PolySystem
        GateSystem	
///

undocumented {
    (toExternalString,GateSystem),
    (evaluateJacobian,GateSystem,Matrix),
    (evaluateJacobian,GateSystem,Matrix,Matrix),    
    (evaluateJacobian,GateSystem,Point),
    (evaluateJacobian,GateSystem,Point,Point)
    }

doc ///
Key
  (jacobian,GateSystem)
  (jacobian,List,GateSystem)
Headline
  jacobian of a (gate) system
///

doc ///
    Key
    	endGameCauchy
	(endGameCauchy,GateHomotopy,Number,Point)
	(endGameCauchy,GateHomotopy,Number,MutableMatrix)
    Headline
        Cauchy end game for getting a better approximation of a singular solution 
    Usage
    	endGameCauchy(H,t'end,p0)
    	endGameCauchy(H,t'end,points)
    Inputs
	H:GateHomotopy
	t'end:Number
	p0:Point
	points:MutableMatrix
    Description
    	Text 
            Refines an approximation of a (singular) solution to a polynomial system which was obtained via homotopy continuation.
	    This method is used for posprocessing in the blackbox solver implemented in @TO solveSystem@.  
        Example
            CC[x,y]
	    T = {(x-2)^3,y-x+x^2-x^3}
	    sols = solveSystem(T,PostProcess=>false);
	    p0 = first sols;
	    peek p0
	    t'end = 1
    	    p = endGameCauchy(p0#"H",t'end,p0)
    SeeAlso
    	refine
///

doc ///
    Key
	(trackHomotopy,Homotopy,List)
    	trackHomotopy
	(trackHomotopy,Matrix,List)
	(trackHomotopy,Sequence,List)
	Field
	[trackHomotopy, Field]
    Usage
    	trackHomotopy(H,S)
    Inputs
    	H:Homotopy
	S:List
	    start solutions
    Headline
        follow points along a homotopy
    Description
        Text
    	    This method implements homotopy continuation: it follows a given list {\tt S} of start solutions along a @TO Homotopy@ {\tt H}.
	  
            Option @TO Field@ is unique to this method (the default is @TO CC@, but one can imagine using @TO RR@). 
	    It specifies which @TO InexactFieldFamily@ to use when adaptive precision is requested via {\tt Precision=>infinity}.
	    The rest are a subset of @TO "numerical homotopy tracking options"@. 
    Caveat	
            Note for developers: the old implementation @TO track@ eventually will be replaced by a call to @TO trackHomotopy@.
	    Alternative ways for calling ({\tt H} could be a Sequence (a preSLP), Matrix, etc.) are deprecated and are for debugging purposes only.	
    SeeAlso
    	GateHomotopy
	segmentHomotopy
    	Point	    
///

doc ///
  Key
    GateSystem
    (net, GateSystem)
    (numVariables,GateSystem)
    (numFunctions,GateSystem)
    (numParameters,GateSystem)
    (evaluate,GateSystem,Matrix,Matrix)
  Headline
    a system of functions evaluated via a straightline program
  Description
    Text
      An object of this type is a system of functions evaluated via an SLP 
      that is constructed using the tools of package @TO SLPexpressions@.
      
      An object of this type (constructed with @TO gateSystem@) 
      is a @TO System@ of functions represented via a @TO GateMatrix@.
      In particular, polynomial systems and systems of rational functions can be represented this way.
	        
      Unlike @TO PolySystem@, the functions of a @TO GateSystem@ do not belong to a ring and can be evaluated on @TO Number@s and @TO RingElement@s 
      as long as the constants in the evaluation circuits can be promoted to the corresponding @TO Ring@s. 
  SeeAlso
    gateSystem
    (gateMatrix,GateSystem)
    (vars,GateSystem)
    (parameters,GateSystem)
    System
    PolySystem
///

doc ///
    Key
        (gateMatrix,GateSystem)
    Headline
        evaluation circuit used for the system 
    Description
    	Text 
    	   This method returns the @TO GateMatrix@ used to evaluate the system. 
///	 

doc ///
    Key
        (vars,GateSystem)
    Headline
        the variable gates in the evaluation circuit used for the system 
    Description
        Text 
	  This method returns the 1-row @TO GateMatrix@ that contains @TO InputGate@s 
	  that are considered variables for the evaluation circuit.  
///	 

doc ///
    Key
        (parameters,GateSystem)    		
    Headline
        the parameter gates in the evaluation circuit used for the system 
    Description
        Text 
	  This method returns the 1-row @TO GateMatrix@ that contains @TO InputGate@s 
	  that are considered parameters for the evaluation circuit.  
///	 

doc ///
    Key 
      GateParameterHomotopy
    Headline 
      a homotopy that involves parameters and is implemented via straight line programs
    Description
      Text
    	An object of this type specializes to a @TO Homotopy@ given values of the parameters. 
	It is related to @TO GateHomotopy@. 
///

doc ///
Key 
  (specialize,GateParameterHomotopy,MutableMatrix)
Headline
  specialize parameters in a (gate) parameter homotopy 
///

doc ///
Key
  (evaluateH,GateParameterHomotopy,Matrix,Matrix,Number)
  (evaluateHt,GateParameterHomotopy,Matrix,Matrix,Number)
  (evaluateHx,GateParameterHomotopy,Matrix,Matrix,Number)
Headline
  evaluate (gate) parameter homotopy and its derivatives 
///  

doc ///
    Key
        segmentHomotopy
	(segmentHomotopy,GateSystem,GateSystem)
	(segmentHomotopy,PolySystem,PolySystem)
	(segmentHomotopy,List,List)    		
    Headline
        a segment homotopy
    Usage 
    	H = segmentHomotopy(S,T)
    Inputs
        S:System
	  start system (could be GateSystem, PolySystem, or list of polynomials)
	T:System
	  target system  (could be GateSystem, PolySystem, or list of polynomials)
    Outputs 
    	H:GateHomotopy
    Description
        Text 
	  This method produces a @TO Homotopy@ @TEX "(1-t) S+ t \\gamma T, t\\in[0,1]"@.
	Example
	  R = QQ[x,y]
	  T = {random(3,R)-1, random(2,R)-2}
	  (S,solsS) = totalDegreeStartSystem T
	  H = segmentHomotopy(S,T,gamma=>1+ii)
	  evaluateH(H,transpose matrix first solsS,0)
///	 

doc ///
    Key
        parametricSegmentHomotopy
	(parametricSegmentHomotopy,GateSystem)
	(parametricSegmentHomotopy,PolySystem)    		
    Headline
        creates an ansatz for a segment homotopy
    Usage 
    	PH = parametricSegmentHomotopy F
    Inputs
        F:System
	  either a @TO GateSystem@ or a @TO PolySystem@
    Outputs 
    	PH:GateParameterHomotopy	
    Description
        Text 
	  This method returns a homotopy that after specialization of parameters is akin 
	  to the output of @TO segmentHomotopy@. There are {\bf 2 m} parameters in{\tt PH} 
	  where {\bf m} is the number of parameters in {\tt F}. 
	  The first {\bf m} parameters correspond to the starting point A in the parameter space.
	  The last {\bf m} parameters correspond to the end point B in the parameter space.        
///	 

