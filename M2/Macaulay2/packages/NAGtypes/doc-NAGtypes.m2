document {
     Key => NAGtypes,
     Headline => "Common types used in Numerical Algebraic Geometry",
     PARA{
     	  "The package defines types used by the package ", TO "NumericalAlgebraicGeometry::NumericalAlgebraicGeometry", 
     	  " as well as other numerical algebraic geometry packages: e.g., interface packages ", 
     	  TO "PHCpack::PHCpack", " and ", TT "Bertini::Bertini", "."
	  },  
     PARA{"Datatypes: "},
     UL{    
	 {TO Point, " -- a numerical approximation of a point in a complex space"},
	 {TO PolySystem, " -- a polynomial system (usually with complex coefficients)"},
	 {TO WitnessSet, " -- a witness set representing (possibly positive-dimensional) solution components"},
	 {TO NumericalVariety, " -- a numerical description of a variety"},
	 TO PolySpace,
	 TO DualSpace
	 },
     PARA{"See the corresponding documentation nodes for description of provided service functions."},
     PARA {
     	 "We display the objects of all new types showing only partial data. 
     	 Moreover, if an object is assigned to a global variable, only the name of the variable is shown. Use ", TO peek, 
     	 " for more information."
     	 },
     EXAMPLE lines ///
R = CC[x,y]	
I = ideal((x^2+y^2+2)*x,(x^2+y^2+2)*y);
w1 = witnessSet(I , ideal(x-y), {point {{0.999999*ii,0.999999*ii}}, point {{-1.000001*ii,-1.000001*ii}}} )
O = point {{0.,0.}}
numericalVariety {witnessSet(I, ideal R, {O}),w1}
V = oo
peek V
peek w1
peek O
///
     }

-- Point ---------------------------------------------------------------------------
document {
     Key => {Point, coordinates, (coordinates,Point), (status,Point), (matrix,Point), (net, Point),
	  Regular, Singular, Infinity, MinStepFailure, NumericalRankFailure, RefinementFailure,
	  Origin, IncreasePrecision, DecreasePrecision,
	  Multiplicity,
	  Coordinates, SolutionStatus, LastT, ConditionNumber, NumberOfSteps, ErrorBoundEstimate,
	  MaxPrecision, WindingNumber, DeflationNumber
	  },
     Headline => "a type used to store a point in complex space",
     "This type is used to store a solution to a polynomial system obtained by such fuctions as ", 
     TO "NumericalAlgebraicGeometry::solveSystem", ", ", TO "NumericalAlgebraicGeometry::track",
     ". The following methods can be used to access a ", 
     TO "Point", ":",
     UL{
	  {"coordinates", " -- get the coordinates (returns a list)"},
	  {"matrix", " -- get the coordinates (returns a matrix)"},
	  {"status", " -- get the type of solution (e.g., Regular)"}
	  },
     "Possible types of Points (accessed by ", TO "status", "): ",
     UL { {"Regular", " -- the jacobian of the polynomial system is regular at the point"}, 
	  {"Singular", " -- the jacobian of the polynomial system is (near)singular at the point"}, 
	  {"Infinity", " -- the solution path is deemed divergent"},
	  {"MinStepFailure", " -- the tracker failed to stay above the minimal step increment threshold"},
	  {"NumericalRankFailure", " -- it is likely that in a sequence of deflations numerical rank did not give the correct rank"},
	  {"RefinementFailure", " -- a solution refinement function failed"},
	  {"Origin", " -- the solution path approaches the origin (impossible to give a relative error estimate)"},
	  {"IncreasePrecision", " -- the current precision is deemed inadequate for robust computation"},
	  {"DecreasePrecision", " -- the current precision is deemed excessive (more than the double of sufficient precision)"},
	  {"null", " -- the point has not been classified"}
	  },
     "Only coordinates are displayed (by ", TO "net", "); to see the rest use ", 
     TO "peek", ".  Different algorithms attach different information describing the point. For example, ", TO "NumericalAlgebraicGeometry::solveSystem", " produces the following.",
     PARA{},
     EXAMPLE lines ///
       loadPackage "NumericalAlgebraicGeometry";
       R = CC[x,y];
       sols = solveSystem{x^2+y^2-3, x^3-y^3-7}
       pt = first sols
       peek pt
       coordinates pt
       status pt
     ///,
     -* condition number is not computed by default anymore !!!
     PARA{"For example, one may see the condition number of the Jacobian of the polynomial system, evaluated at this point
      (the smaller the value, the better) as follows."},
     EXAMPLE lines ///
       pt.ConditionNumber
     ///,
     *-
     PARA{"The other keys that may be attached include "}, 
     UL{
	  {TO NumberOfSteps, " -- the number of steps in made by the continuation procedure"}, 
     	  {TO LastT, " -- the last value of the continuation parameter produced during tracking (equals 1 for a regular solution)"},
	  {TO ErrorBoundEstimate, " -- an estimate of the distance from the approximation to the actual solution"},
	  {TO MaxPrecision, " -- max precision used during the homotopy tracking"}, 
	  {TO Multiplicity, " -- the multiplicity of an isolated solution"}, 
	  {TO WindingNumber, " -- the winding numeber of a singular solution determined in the end-games"}, 
	  {TO DeflationNumber, " -- number of first-order deflations in the regularization of a singular solution"},
	  {TT "Tracker", " -- reserved for developers"}
	  },
     PARA {"Basic service functions: "},
     UL{
     	 TO areEqual,
	 TO sortSolutions,
	 TO isRealPoint,
	 TO realPoints,
	 TO solutionsWithMultiplicity,
	 TO (norm,Thing,Point),
	 TO toAffineChart
	 }
     }

document {
	Key => {point, (point,List), (point,Matrix), (point,Point)},
	Headline => "construct a Point",
	Usage => "p = point c",
	Inputs => { 
	     "c"=> {ofClass List, "containing  elements in the form {{list of complex coordinates}, other data} or ",
		 ofClass Matrix, " (only coordinates) or ", ofClass Point}
	     },
	Outputs => {"p"=>Point},
	PARA{"Used to construct a ", TO2{Point, "point"}, " from the old format of output."},
        EXAMPLE lines ///
p := point {{1+0.2*ii, 0.5}, SolutionStatus=>Regular, LastT=>1., NumberOfSteps=>10, ConditionNumber=>2.3}
peek p 
q := point p
     	///
	}

document {
	Key => {(sortSolutions,List), sortSolutions, [sortSolutions,Weights]},
	Headline => "sort the list of solutions",
	Usage => "t = sortSolutions s",
	Inputs => { 
	     "s"=>{"contains solutions (represented either by lists of coordinates or ", TO2{Point,"points"}, ")"}
	     },
	Outputs => {"t"=> "contains solutions sorted as described below"},
	"The sorting is done lexicographically regarding each complex n-vector as real 2n-vector. ",
	"The output format of ", TO "NumericalAlgebraicGeometry::track", " and ", TO "NumericalAlgebraicGeometry::solveSystem", " is respected.", BR{}, 
	"For the corresponding coordinates a and b (of two real 2n-vectors) a < b if b-a is larger than ", 
	TO Tolerance, ". ", 
     	PARA {},
        EXAMPLE lines ///
needsPackage "NumericalAlgebraicGeometry"
R = CC[x,y];
s = solveSystem {x^2+y^2-1, x*y}
sortSolutions s
     	///,
	Caveat => {"The sorting described above does not possess good properties, since there may be near ties in specific coordinate values between several points. ",
	    "A better way is to specify a random weight (of length 2n where n=#points) as an optional parameter ", TO [sortSolutions,Weights], 
	    ", which provides a linear functional that evaluates to distinct (and sufficiently) real numbers on the given points. " }, 
	SeeAlso => {"NumericalAlgebraicGeometry::solveSystem", "NumericalAlgebraicGeometry::track", "areEqual"}
	}

document { Key => {Tolerance, 
	[sortSolutions,Tolerance], 
	[areEqual,Tolerance], 
	[isGEQ,Tolerance], 
	[isRealPoint,Tolerance], 
	[realPoints,Tolerance], 
	[solutionsWithMultiplicity,Tolerance],
	[differencePointSet,Tolerance], [unionPointSet,Tolerance]
	},
     Headline => "the tolerance of a numerical computation" 
     }

document { Key => {Norm, 
    	[residual,Norm]
	},
     Headline => "p in the p-norm",
     "Specifies p-norm, where p is either ", ofClass ZZ, " or ", TO infinity
     }

document {
	Key => {isGEQ, (isGEQ,List,List),(isGEQ,Point,Point)},
	Headline => "compare two points",
	Usage => "b = isGEQ(x,y)",
	Inputs => {
	     "x" => {ofClass Point, "or a list of complex (floating point) numbers"},
	     "y" => {ofClass Point, "or a list of complex (floating point) numbers"}
	     },
	Outputs => {"b"=>{"tells if ", TT "x", " is (approximately) greater or equal than ", TT "y"}},
	PARA {"The inputs are lists of complex numbers, the order is (approximately) lexicographic: regard each complex n-vector as real 2n-vector, 
	      for the corresponding coordinates a and b (of two real 2n-vectors) a < b if b-a is larger than ", TO Tolerance, ". "},
	EXAMPLE lines ///
isGEQ({1,1,1},{1,0,2})
isGEQ({1,1e-7},{1, 0})
     	///,
	SeeAlso => {"areEqual"}
	}
document {
	Key => {areEqual, (areEqual,CC,CC), (areEqual,Number,Number), 
	    (areEqual,List,List), (areEqual,BasicList,BasicList),
	    (areEqual,Matrix,Matrix), (areEqual,MutableMatrix,MutableMatrix), (areEqual,Point,Point), 
	    (areEqual,BasicList,Point), (areEqual,Point,BasicList),
	    (symbol ==,Point,Point),
	    [areEqual,Projective]},
	Headline => "determine if solutions are equal",
	Usage => "b = areEqual(x,y)",
	Inputs => {
	     "x" => "a solution or a list of solutions",
	     "y" => "a solution or a list of solutions",
	     Projective=>{"if ", TO true, " then solutions are considered as representatives of points 
		  in the projective space"}
	     },
	Outputs => {"b"=>{"tells if ", TT "x", " and ", TT "y", " are approximately equal"}},
	PARA {
	    "The inputs can be complex numbers, ", TO2{Point, "points"}, ", ", 
	    " or lists of points (presented as ", TO2{Point, "points"}, " or lists of coordinates). ",
	    "The function returns false if the distance between ", TT "x", " and ", TT "y", 
	    " exceeds ", TO Tolerance, " and true, otherwise."
	    },
	PARA {
	    "If ", TT "Projective=>true", " then ", 
	    TEX "1-\\cos\\alpha", " is compared with the ", TO Tolerance, ", where ",
	    TEX "\\alpha", " is the angle between ", TT "x", " and ", TT "y", "." 
	    },
	EXAMPLE lines ///
areEqual({{-1,1e-7},{1e-7*ii,-1}}, {{-1, 0}, {0, -1}})
areEqual({3*ii,2*ii,1+ii}, {-6,-4,-2+2*ii}, Projective=>true)  
     	///,
	PARA {
	    "For two ", TO2(Point, "points"), " ", TT "A", " and ", TT "B", 
	    "calling ", TT "A == B", "is equivalent to ", TT "areEqual(A,B)", 
	    ", however, there is no way to specify the optional parameter."
	    },
	EXAMPLE lines ///
A = point {{-1,1e-7}, {1e-7*ii,-1}}
B = point {{-1,0}, {0, -1}}
A == B
        ///,
	SeeAlso => {"NumericalAlgebraicGeometry::solveSystem", "NumericalAlgebraicGeometry::track", sortSolutions}
	}


document {
     Key => {isRealPoint, (isRealPoint,Point)},
     Headline => "determine whether a point is real",
     Usage => "b = isRealPoint p",
     Inputs => {
	     "p"
	     },
     Outputs => {"b"=>{"tells if ", TT "p", " is within ", TO Tolerance, " to a real point"}},
     PARA{},
     EXAMPLE lines ///
     needsPackage "NumericalAlgebraicGeometry"
     R = CC[x,y];
     sols = solveSystem{x^3-y^2, x-y-2}
     sols / isRealPoint
     ///
     }
document {
     Key => {realPoints, (realPoints,List)},
     Headline => "select real points",
     Usage => "R = realPoints L",
     Inputs => {
	     "L" => {TO2{Point,"points"}}
	     },
     Outputs => {"R"=>{TO2{Point,"points"}, " that are real (up to ", TO Tolerance, ")"}},
     PARA{"Selects real points from a list of points using the function ", TO isRealPoint, "."},
     EXAMPLE lines ///
     needsPackage "NumericalAlgebraicGeometry"
     R = CC[x,y];
     sols = solveSystem{x^6-y^4, x-y-2}
     realPoints sols
     ///,
     SeeAlso => {realPoints}
     }
document {
     Key => {(norm,Thing,Point)},
     Headline => "p-norm of the point",
     Usage => "a = norm(p,pt)",
     Inputs => {
	     "p"=>{"a positive real number or ", TO infinity},
	     "pt"
	     },
     Outputs => {"a"=>{"the ", TT "p", "-norm of the point ", TT "pt"}},
     PARA{},
     EXAMPLE lines ///
     needsPackage "NumericalAlgebraicGeometry"
     R = CC[x,y];
     sols = solveSystem{x^2+y^2-3, x^3-y^3-7}
     norm(infinity, first sols)
     norm(2.5, last sols) 
     ///
     }

document {
     Key => {solutionsWithMultiplicity, (solutionsWithMultiplicity,List)},
     Headline => "replaces clusters of approximately equal points by single points with multiplicity",
     Usage => "M = solutionsWithMultiplicity S",
     Inputs => {
	     "S" => {TO2{Point,"points"}}
	     },
     Outputs => {"M"=>{TO2{Point,"points"}, " with a multiplicity field"}},  
     PARA{"Clusters the points and outputs a list with one point ", TT "p", " per cluster with ", TT "p.", TO Multiplicity, 
	 " equal to the size of the cluster. If the multiplicity is not 1, then ", TT "p.", TO SolutionStatus, " is set to ", TO Singular, 
	 "; otherwise, it is inherited from one of the points in the cluster."},
     PARA{"Whether two points are approximately equal is decided by the function ", TO areEqual, " that depends on ", TO Tolerance, "."},
     EXAMPLE lines ///
     a = point {{0,1}}
     b = point {{0.000000001,1+0.00000000001*ii}}
     c = point {{0.001*ii,1}}
     M = solutionsWithMultiplicity {a,b,c}
     peek M
     ///,
     Caveat => {"A point in a cluster may be farther than ", TO Tolerance, 
	 " from another point in the cluster. (In that case there has to be another point in the cluster that is within the ", 
	 TO Tolerance, ".)"}
     }


document {
	Key => {(project,Point,ZZ), project},
	Headline => "project a point",
	Usage => "q = project(p,n)",
	Inputs => {
	     "p",
	     "n"
	     },
	Outputs => {"q"=>{"projection of ", TT "p", " to the first ", TT "n", " coordinates"}},
	PARA {
	    "Projects a point to the subspace corresponding to the first ", TT "n", " coordinates. "
	    },
	EXAMPLE lines ///
p = point({{1+ii,2.3,2*ii}, ConditionNumber=>1000, ErrorBoundEstimate =>0.01});
project(p,2)
     	///,
	SeeAlso => {WitnessSet,ProjectionDimension}
	}

document {
	Key => {(toAffineChart, ZZ, List), toAffineChart},
	Headline => "coordinates of a point in the projective space in an affine chart",
	Usage => "y = toAffineChart(i,x)",
	Inputs => {
	     "i" => "the number of the standard chart",
	     "x" => "projective coordinates of a point"
	     },
	Outputs => {"y"=>{"coordinates of ", TT "x", " in the ", TT "i", "-th affine chart"}},
	Caveat => {"Returns ", TT "infinity", " if the ", TT "i", "-th coordinate of ", TT "x", " is zero."},
	EXAMPLE lines ///
toAffineChart(2,{1,2,3,4,5,6}) 
toAffineChart(2,{1,2,0,4,5,6}) 
     	///,
	SeeAlso => {areEqual}
	}

-- PolySystem ------------------------------------------------------------------------------
document {
    Key => {PolySystem, 
	(ideal,PolySystem), (isHomogeneous,PolySystem), (jacobian,PolySystem), (net,PolySystem),
	(ring,PolySystem), (equations,PolySystem), (parameters,PolySystem),
	(numVariables,PolySystem), (numFunctions,PolySystem),(numParameters,PolySystem),
	NumberOfPolys, NumberOfVariables, PolyMap, ContinuationParameter, 
	SpecializationRing
	},
    Headline => "a polynomial system",
    "This type stores a polynomial system, ",
    "the following methods can be used to access a ", 
    TT "PolySystem", ":",
    UL{
	{"ideal", " -- the ideal generated by the system"},
	{"equations", " -- the list of polynomials in the system"},	
	{"ring", " -- the ring containing the polynomials"},
	{"jacobian", " -- the jacobian of the polynomial map"},
	{"parameters", " -- the list of parameter variables (if any)"}
	},
    "Only polynomials are displayed (by ", TO "net", "); ",
    "to see the data stored in a witness set use ", TO "peek", ".",
    SUBSECTION "For developers:",
    "Required entries in a ", TO PolySystem, " are",
     UL {
	 {TT "NumberOfVariables", " of type ", TO ZZ},
	 {TT "NumberOfPolys", " of type ", TO ZZ},
	 {TT "PolyMap", " of type ", TO Matrix, ", a column matrix over a polynomial ring"},
    	 {TT "Jacobian", " of type ", TO Matrix, ", the jacobian of ", TT "PolyMap"},
	 },
-*     "Basic methods for ", TO "polynomial homotopy", " use additional keys: ",
     UL {
	 {TT "ContinuationParameter", " -- stores one variable of the ring" },
	 {TT "SpecializationRing", 
	     " -- stores the subring generated my all variables except the additional parameter",
	     " (e.g., used by ", TO specializeContinuationParameter, ")"}
	 },
     *-
     EXAMPLE lines ///
CC[x,y]
S = polySystem {x^2+y^2-6, 2*x^2-y}
p = point {{1.0+3*ii,2.3+ii}};
evaluate(S,p)
evaluate(jacobian S, p)
     ///,
     PARA {"Basic service functions: "},
     UL{
    	TO polySystem,
	TO evaluate,
	--TO segmentHomotopy,
	--TO specializeContinuationParameter,
	},     
     SeeAlso => {WitnessSet}
     }

document {
    Key => {evaluate, (evaluate,Matrix,Matrix), (evaluate,Matrix,Point), (evaluate,PolySystem,Matrix), (evaluateJacobian,PolySystem,Point)},
    Headline => "evaluate a polynomial system or matrix at a point",
    Usage => "y = evaluate(f,x)",
    Inputs => { 
	"f" => {ofClass PolySystem, " or ", ofClass Matrix},
	"x" => {ofClass Point, " or ", ofClass Matrix},
	},
    Outputs => {"y"=> {"the value ", TT "f(x)"}},
    PARA {"Evaluates a ", TO PolySystem, " or a matrix with polynomial entries at a point."},
    EXAMPLE lines ///
R = CC[x,y]; S = polySystem {x^2+y^2-6, 2*x^2-y};
p = point {{1.0+3*ii,2.3+ii}};
evaluate(S,p)
evaluate(jacobian S, p)
    ///,
    SeeAlso => {PolySystem}
    }

document {
    Key => {residual, 
	(residual,List,Point),
	(residual,Matrix,Matrix),
	(residual,PolySystem,Point)
	},
    Headline => "residual of a polynomial function at a point",
    Usage => "y = residual(f,x)",
    Inputs => { 
	"f" => {ofClass PolySystem, " or ", ofClass Matrix},
	"x" => {ofClass Point, " or ", ofClass Matrix},
	},
    Outputs => {"y"=> {"the norm of ", TT "f(x)"}},
    PARA {
	"Evaluates a ", TO PolySystem, 
	" or a matrix with polynomial entries at a point and returns the norm of the result."
	},
    EXAMPLE lines ///
R = CC[x,y]; S = polySystem {x^2+y^2-5, 2*x^2-y};
p = point {{1.001-0.0001*ii,2.+0.0001*ii}};
evaluate(S,p)
residual(S,p)
residual(S,p,Norm=>3)
residual(S,p,Norm=>infinity)
    ///,
    SeeAlso => {PolySystem}
    }

document {
    Key => {polySystem, (polySystem,List), (polySystem,Matrix), (polySystem,PolySystem), (polySystem,Ideal)},
    Headline => "construct a polynomial system",
    Usage => "P = polysystem F",
    Inputs => { 
	"F" => {ofClass List, " or ", ofClass Ideal, " or ", ofClass Matrix, 
	    " (column matrix) with polynomial entries or ", ofClass PolySystem},
	},
    Outputs => {"P"=> PolySystem},
    PARA {"Constructs a ", TO PolySystem, " from the given polynomials."},
    EXAMPLE lines ///
R = CC[x,y]; S := polySystem {x^2+y^2-6, 2*x^2-y}
S = polySystem transpose matrix {{x^2+y^2-6, 2*x^2-y}}
T = polySystem S
    ///,
    SeeAlso => {PolySystem}
    }

document {
    Key => {(substitute,PolySystem,Ring)},
    Headline => "substitute a ring in a polynomial system",
    Usage => "G = sub(F,R)",
    Inputs => { 
	"F" => PolySystem,
	"R" => Ring
	},
    Outputs => {"G"=> PolySystem},
    PARA {
	"Constructs ", ofClass PolySystem, " by attempting to map polynomials of a given system to a given ring."
	},
    EXAMPLE lines ///
R = QQ[x,y]; S := polySystem {x^2+y^2-6, 2*x^2-y}
T := sub(S,CC[x,y])
ring T
    ///,
    SeeAlso => {polySystem,PolySystem}
    }

document {
    Key => {(homogenize,PolySystem,Ring,RingElement)},
    Headline => "homogenize a polynomial system",
    Usage => "G = sub(F,R,t)",
    Inputs => { 
	"F" => PolySystem,
	"R" => Ring,
	"t" => {ofClass RingElement, ", a variable in ", TT "R"} 
	},
    Outputs => {"G"=> PolySystem},
    PARA {
	"Constructs ", ofClass PolySystem, " that is a homogenization of the given system. "
	},
    EXAMPLE lines ///
R = CC[x,y]; S := polySystem {x^2+y^2-6, 2*x^4-y}
T := homogenize(S,CC[x,y,t],t)
ring T
    ///,
    SeeAlso => {homogenize,PolySystem}
    }

-*
document {
    Key => {"polynomial homotopy", 
	segmentHomotopy, (segmentHomotopy,PolySystem,PolySystem), 
	substituteContinuationParameter, (substituteContinuationParameter,PolySystem,RingElement),
	specializeContinuationParameter, (specializeContinuationParameter,PolySystem,Number)
	},
    Headline => "basic methods for manipulating polynomial homotopies",
    Usage => "H = segmentHomotopy(S,T)\nH2=substituteContinuationParameter(H1,s)\nspecializeContinuationParameter(H,t0)",
    Inputs => { 
	"S" => PolySystem,
	"T" => PolySystem,
	"H1" => PolySystem
	},
    Outputs => {"H"=> PolySystem},
    PARA {
	"Construct ", ofClass PolySystem, " representing a segment homotopy ", 
	TEX "H = (1-t) F + t G", " for ", TEX "t \\in [0,1]", "."
	},
    EXAMPLE lines ///
R = CC[x,y]; 
S = polySystem {x^2-1, y^2-1};
T = polySystem {x^2+y^2-6, 2*x^2-y};
H := segmentHomotopy(S,T)
    ///,    
    PARA {
	"Specialize the continuation parameter:"
	},
    EXAMPLE lines ///
specializeContinuationParameter(H,0)
specializeContinuationParameter(H,1)
specializeContinuationParameter(H,2+3*ii)
    ///,    
    PARA {
	"Substitute ", TEX "1-t", " for the continuation parameter to swap the ends of the homotopy."
	},
    EXAMPLE lines ///
t := H.ContinuationParameter
H' := substituteContinuationParameter(H,1-t)
    ///,    
    SeeAlso => {ContinuationParameter,SpecializationRing}
    }
*-
-- WitnessSet ------------------------------------------------------------------------------
document {
     Key => {WitnessSet,equations,(equations,WitnessSet),slice,(slice,WitnessSet),
	  points,(points,WitnessSet),(ideal,WitnessSet),Equations,Slice,Points,IsIrreducible,ProjectionDimension,
     	  (codim,WitnessSet),(degree,WitnessSet),(dim,WitnessSet),(ring,WitnessSet),(net,WitnessSet) 
     	  },
     Headline => "a witness set",
     "This type stores a witness set of an equidimensional solution component. ", 
     "The following methods can be used to access a ", 
     TO WitnessSet, ":",
     UL{
     	  {"ideal", " -- get the defining ideal of the algebraic superset"},
	  {"equations", " -- get the list of defining polynomials of the algebraic superset"},
	  {"slice", " -- get linear functions defining the slicing plane"},
	  {"points", " -- get the list of witness points (which are zeroes of all above)"}
	  },
     "Also one may determine",
     UL {
	  {"dim", " -- the dimension"},
	  {"codim", " -- the codimension"},
	  {"deg", " -- the degree (the number of witness points)"},
	  {"ring", " -- the ring of the defining polynomials"}
	  }, 
     "Only dimension and degree are displayed (by ", TO "net", "); to see the data stored in a witness set use ", 
     TO "peek", ".",
     SUBSECTION "For developers:",
     "Required keys in a ", TO WitnessSet, " are",
     UL {
	  {TT "Equations", " -- ", ofClass Ideal},
	  {TT "Slice", " -- ", ofClass List, " or ", ofClass Matrix},
	  {TT "Points", "--  a list of ", TO2(Point, "points")},
	  {TT "IsIrreducible", " -- takes values ", TO "null", "(not determined), ", TO "true", ", or ", TO "false"}
	  },
     "Optional keys:",
     UL {
	  {TT "ProjectionDimension", " -- ", ofClass ZZ, 
	      ", the witness set describes a lifted variety (its projection on the first ", 
	      TT "ProjectionDimension", " coordinates is the variety the witness set represents)"},
	  },     
     SeeAlso => {witnessSet, ProjectiveWitnessSet, NumericalVariety}
     }

document {
	Key => {witnessSet,
	    (witnessSet,Ideal,Ideal,List),(witnessSet,Ideal,Matrix,List),
	    (witnessSet,PolySystem,Matrix,List),(witnessSet,PolySystem,PolySystem,List)
	    },
	Headline => "construct a WitnessSet",
	Usage => "w = witnessSet(E,S,P)",
	Inputs => { 
	     "E" => {ofClass Ideal, " or ", ofClass PolySystem},
	     "S" => {ofClass Ideal, " generated by linear polynomials (or ", 
		 ofClass PolySystem, " of the generators or ", 
		 ofClass Matrix, " of their coefficients)"},
	     "P" => List => {"contains witness points (of type ", TO "Point", ")"}
	     },
	Outputs => {"w"=> WitnessSet},
	PARA {"Used to construct a witness set of a component of the variety ", TT "V(E)", ". It is expected that ", TT "codim E == dim S", 
	     " and that ", TT "P", " is a subset of the intersection of ", TT "V(E)", " and ", TT "V(S)", "."},
        EXAMPLE lines ///
R = CC[x,y]	
w = witnessSet( ideal(x^2+y^2+2), ideal(x-y), {point {{0.999999*ii,0.999999*ii}}, point {{-1.000001*ii,-1.000001*ii}}} )
peek w
///
	}

document {
     Key => {ProjectiveWitnessSet, AffineChart},
     Headline => "a projective witness set",
     "This type stores a witness set of an equidimensional projective solution component. ", 
     SeeAlso => {WitnessSet, projectiveWitnessSet}
     }

document {
	Key => {projectiveWitnessSet,(projectiveWitnessSet,Ideal,Matrix,Matrix,List)},
	Headline => "construct a ProjectiveWitnessSet",
	Usage => "w = projectiveWitnessSet(E,C,S,P)",
	Inputs => { 
	     "E" => Ideal => {"in a polynomial ring over ", TO CC },
	     "C" => Matrix => {"in a polynomial ring over ", TO CC },
	     "S" => Matrix => {" complex coefficients of a linear system"},
	     "P" => List => {"contains witness points (of type ", TO "Point", ")"}
	     },
	Outputs => {"w"=> ProjectiveWitnessSet},
	PARA {"Used to construct a witness set for a component of the variety ", TT "V(E)", 
	    ". ", " An affine chart is specified by the matrix of the coefficients of the (normalized) linear equation defining the chart: e.g., ",
	    TT "ax+by+cz=1", " is encoded as ", TT "[a,b,c]", "." }, 
	PARA {"It is expected that the, ", TT "V(E)", " and the plane ", TT "V(S)", " defined by ", TT "S", 
	    " are of complementary dimensions and that ", TT "P", " is contained in the intersection of ", TT "V(E+C)", " and ", TT "V(S)", "."}
	,
	EXAMPLE lines ///
R = CC[x,y,z]	
w = projectiveWitnessSet( ideal(x^2+y^2+2*z^2), matrix{{0,0,1}}, matrix{{1,-1,0}}, {point {{0.999999*ii,0.999999*ii,1.}}, point {{ -1.000001*ii,-1.000001*ii,1.}}} )
peek w///
-- 	,
--         EXAMPLE lines ///
-- R = CC[x,y,z]
-- w = projectiveWitnessSet(
--     ideal(x^2+y^2+2*z^2),
--     matrix{{0,0,1}}, -- chart: Z=1
--     matrix{{1,-1,0}},
--     {point {{1.000001*ii,0.999999*ii,1}}, point {{ -1.000001*ii,-1.000001*ii,1}}} 
--     )
-- peek w
-- ///
}

document {
	Key => {(sliceEquations,Matrix,Ring),sliceEquations,
	    (projectiveSliceEquations,Matrix,Ring),projectiveSliceEquations},
	Headline => "slicing linear functions",
	Usage => "S = sliceEquations(M,R)\nS = projectiveSliceEquations(M,R)",
	Inputs => { 
	     "M"=> Matrix => " contains the coefficients of the slicing linear polynomials",
	     "R"=> Ring => " where the output polynomials belong"
	     },
	Outputs => {"S"=>List=>"contains linear polynomials"},
        PARA {"A service function used  in ", TO "NumericalAlgebraicGeometry::NumericalAlgebraicGeometry", "."},
	EXAMPLE lines ///
R = CC[x,y]	
sliceEquations(matrix{{1,2,3},{4,5,6*ii}}, R)
projectiveSliceEquations(matrix{{1,2,3},{4,5,6*ii}}, CC[x,y,z])
     	///
	}

-- NumericalVariety --------------------------------------------------------------------
document {
     Key => {NumericalVariety, 
	 (dim,NumericalVariety), (degree,NumericalVariety), 
	 (net,NumericalVariety), (check,NumericalVariety)
	 },
     Headline => "a numerical variety",
     PARA {"This type stores a collection of witness sets representing a complex affine variety. "},
     "Note that",
     UL {     	  
	  {"The ambient space is expected to be the same, i.e., ", 
	      TO2((dim,WitnessSet),"dimension"), " (or ", TO ProjectionDimension, ") of ", TO2(WitnessSet, "witness sets"),
	      " should be the same."},
	  -- "However, the witness sets need not come from the decomposition of the same variety.",
	  {"The constructor ", TO (numericalVariety,List), " does not check the sensibility of the input; run ", 
	  TO (check, NumericalVariety), " to verify the validity of a numerical variety."} 
	  },
     "Basic service routines:",
     UL {
	 {"dim", " -- the dimension"},
	 {"codim", " -- the codimension"},
	 {"deg", " -- the degree"},
	 {TO (components,NumericalVariety)}
	 },
     EXAMPLE lines ///
R = CC[x,y]; I = ideal((x^2+y^2+2)*x,(x^2+y^2+2)*y*(y-1));
w1 := witnessSet(I , ideal(x-y), {point {{0.999*ii,0.999*ii}}, point {{-1.001*ii,-1.001*ii}}} )
w0 := witnessSet(I, ideal R, {point {{0.,0.}}})
w0' := witnessSet(I, ideal R, {point {{0.,1.}}})
V := numericalVariety {w0,w1,w0'}
dim V
degree V
     ///,
     SeeAlso => {WitnessSet}
     }
document {
	Key => {(numericalVariety,List), numericalVariety, (projectiveNumericalVariety,List), projectiveNumericalVariety},
	Headline => "construct a numerical variety",
	Usage => "V = numericalVariety Ws; V = projectiveNumericalVariety Ws; ",
	Inputs => { 
	     "Ws" => {"contains (projective) witness sets representing components of a variety"}
	     },
	Outputs => {"V"=> NumericalVariety},
	PARA {"Constructs a numerical (affine or projective) variety. It is NOT expected that every witness set ", TT "W", 
	     " in the list ", TT "Ws", " has the same ", TT "W.Equations", "."},
        EXAMPLE lines ///
R = CC[x,y]; I = ideal((x^2+y^2+2)*x,(x^2+y^2+2)*y);
w1 = witnessSet(I , ideal(x-y), {point {{0.999999*ii,0.999999*ii}}, point {{-1.000001*ii,-1.000001*ii}}} )
w0 = witnessSet(I, ideal R, {point {{0.,0.}}})
V = numericalVariety {w0,w1}
     	///,
	SeeAlso => {WitnessSet, ProjectiveWitnessSet, numericalAffineSpace}
	}

document {
    Key => {
	(components,NumericalVariety),
	(components,NumericalVariety,ZZ),
	(components,NumericalVariety,ZZ,InfiniteNumber),
	(components,NumericalVariety,ZZ,ZZ)
	},
    Headline => "list components of a numerical variety",
    Usage => "components(V)\ncomponents(V,a)\ncomponents(V,a,b)",
    Inputs => { 
	"V" => NumericalVariety,
	},
    Outputs => {{ofClass List, " of ", TO2(WitnessSet,"witness sets")}},
    PARA {
	"Returns a list of components of a numerical variety. ",
	"If ", TT "a", " (", ofClass ZZ, ") and/or ", 
	TT "b", " (", ofClass ZZ, " or ", TO infinity, 
	") are specified, then components of dimension ", TT "a", 
	" (respectively, components of dimension at least ", TT "a", 
	" and at most ", TT "b", ") are returned."
	},
    EXAMPLE lines ///
R = CC[x,y]; I = ideal((x^2+y^2+2)*x,(x^2+y^2+2)*y);
w1 := witnessSet(I , ideal(x-y), {point {{0.999999*ii,0.999999*ii}}, point {{-1.000001*ii,-1.000001*ii}}} )
w0 := witnessSet(I, ideal R, {point {{0.,0.}}})
V := numericalVariety {w0,w1}
components V    
    ///,
    SeeAlso => {NumericalVariety}
    }

document {
	Key => {numericalAffineSpace, (numericalAffineSpace,PolynomialRing)},
	Headline => "affine space as a numerical variety",
	Usage => "numericalAffineSpace R",
	Inputs => { 
	     "R"=> PolynomialRing
	     },
	Outputs => {NumericalVariety},
        PARA {"Constructs a numerical variety representing the complex affine space corresponding to the given coordinate ring."},
	EXAMPLE lines ///
R = CC[x,y]	
V := numericalAffineSpace R
C := first components V
equations C
slice C
points C
     	///,
	SeeAlso => {NumericalVariety}
	}

document {
     Key => {ProjectiveNumericalVariety},
     Headline => "a projective numerical variety",
     "An object of this type stores a collection of ", TO2(projectiveWitnessSet, "projective witness sets"),
     SeeAlso => {projectiveWitnessSet}
     }

-- legacy stuff ------------------------------------------------------------------------------
doc ///
  Key
    generalEquations
    (generalEquations,ZZ,Ideal)
    (generalEquations,ZZ,List)
    (generalEquations,WitnessSet)
  Headline
    random linear combinations of equations/generators 
  Usage
    L = generalEquations(k,F)
    L = generalEquations(k,I)
  Inputs
    k:ZZ
    F:List
      a list of polynomials
    I:Ideal
  Outputs
    L:List
      {\tt k} linear combinations of polynomials in {\tt F} (of generators of {\tt I})
  Description
    Text
      A variety {\em V} (that is not necessarily a complete intersection) of codimension {\tt k} 
      is a component of a complete intersection of codimension {\tt k} defined by 
      {\tt k} general linear combinations of any generating set of the defining ideal of {\em V}.
      
      This function automates the above construction.   
    
    Example
      R = CC[x,y,z]; 
      F = {x*y, x^2 - y, x*z};
      L = generalEquations(2,F)      
///

-- PolySpace ------------------------------------------------------------------------------
doc ///
  Key
    PolySpace
    (gens,PolySpace)
    (dim,PolySpace)
    (ring,PolySpace)
    (net,PolySpace)
  Headline
    a polynomial vector subspace
  Description
    Text
      This type stores a finite dimensional vector subspace of a polynomial ring, given by a spanning set. 
      The spanning set is generally assumed to be a basis although this is not enforced.  
      The following methods can be used to access a {\tt PolySpace}: 

      @UL {
	  {"gens -- a one-row matrix of the generators"},
	  {"dim -- the number of generators"},
	  {"ring -- the ring of the generators"}
	  }@

  SeeAlso
    polySpace
    DualSpace
///

doc ///
  Key
    polySpace
    (polySpace,Matrix)
    (polySpace,PolySpace)
    Reduced
    [polySpace,Reduced]
  Headline
    construct a PolySpace
  Usage
    S = polySpace M
  Inputs
    M:Matrix
      with one row of generators
  Outputs
    S:PolySpace
  Description
    Text
      Used to construct a finite dimensional vector subspace of a polynomial ring.
    Example
      R = CC[x,y];	
      M = matrix{{y^2,x^2+x*y}};
      S = polySpace M
///

doc ///
  Key
    DualSpace
    (gens,DualSpace)
    (dim,DualSpace)
    (ring,DualSpace)
    (net,DualSpace)
    (point,DualSpace) 
  Headline
    a dual functional vector subspace
  Description
    Text
      This type stores a finite dimensional subspace of the local dual of a polynomial ring at a point.
      In practice, the subspace is stored as a @TO PolySpace@ with functionals represented by the
      corresponding polynomial, along with a @TO Point@. 
      The following methods can be used to access a {\tt DualSpace}: 

      @UL {
	  {"gens -- a one-row matrix of the generators"},
	  {"dim -- the number of generators"},
	  {"ring -- the ring of the generators"},
	  {"point -- the base point of the dual space"}
	  }@
  SeeAlso
    dualSpace
    PolySpace
///

doc ///
  Key
    dualSpace
    (dualSpace,Matrix,Point)
    (dualSpace,PolySpace,Point)
    (dualSpace,DualSpace)
  Headline
    construct a DualSpace
  Usage
    D = dualSpace(M,p)
    D = dualSpace(S,p)
  Inputs 
    M:Matrix
      with one row of generators
    S:PolySpace
    p:Point
  Outputs
    D:DualSpace
  Description
    Text
      Used to construct a finite dimensional subspace of the local dual space of polynomial ring at a point.
    Example
      R = CC[x,y];
      M = matrix{{1,x,x^2-y}}
      p = point matrix{{1,0}};
      D = dualSpace(M,p)
///

doc ///
  Key
    System
    numVariables
    (numVariables,System)
    numFunctions
    (numFunctions,System)
    numParameters
    (numParameters,System)
    (evaluate,System,Matrix)
    (evaluate,System,Matrix,Matrix)
    (evaluate,System,Point)
    (evaluate,System,Point,Point)
    evaluateJacobian
    (evaluateJacobian,System,Matrix)
    (evaluateJacobian,System,Matrix,Matrix)
    (evaluateJacobian,System,Point)
    (evaluateJacobian,System,Point,Point)
  Headline
    a system of functions
  Description
    Text
      A type that inherits from this {\bf abstract} type should supply methods for 
      evaluating a map that takes @TO numVariables@ (+ @TO numParameters@ if the sstem is parametric) inputs and 
      produces @TO numFunctions@ outputs and its jacobian.
      
      Note for developers: it suffices to override the versions of @TO evaluate@ and @TO evaluateJacobian@ 
      that take {\tt (System,Matrix,Matrix)} as arguments. 
  SeeAlso
    PolySystem
///

doc ///
  Key
    Homotopy
  Headline
    a homotopy asbtract type
  Description
    Text
      A type that inherits from this {\bf abstract} type should supply methods for 
      evaluating a homotopy.
///

doc ///
  Key 
    ParameterHomotopy
  Headline
    a homotopy that involves parameters
  Description
    Text
      An abstract type that of homotopy that involces parameters.
      Can be specialized to produce @TO SpecializedParameterHomotopy@.
///	    

doc ///
  Key 
     SpecializedParameterHomotopy
  Headline
    a homotopy obtained from a parameter homotopy by specializing parameters
///	    

doc ///
  Key 
    Parameters
  Headline
    a collection of parameters
///

doc ///
  Key
      WSet
      (points,WSet)
      (ambient,WSet)
      (degree,WSet)
      (codim,WSet)
      (dim,WSet)
      (net,WSet)
      SlicingVariety
      (ambient,SlicingVariety)
      (codim,SlicingVariety)
      (dim,SlicingVariety)
      (map,SlicingVariety)
      (net,SlicingVariety)
      Ambient
      (dim,Ambient)
      (net,Ambient)      
  Headline
    (under construction!) new types and methods needed to generalize WitnessSet
///


-* doc template
doc ///
  Key
  Headline
    *
  Usage
    D = function(M,p)
    D = ...
  Inputs 
    M:[Type]
      [optional decription]
    p:[Type]
  Outputs
    D:[Type]
  Description
    Text
      *
    Example
      *
///
*-
