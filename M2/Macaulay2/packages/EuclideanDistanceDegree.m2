newPackage(
  "EuclideanDistanceDegree",
  Version => "1.1", 
  Date => "April 2026",
  Authors => {
    {Name => "Jose Israel Rodriguez",
    Email => "Jose@Math.wisc.edu",
    HomePage => "http://www.math.wisc.edu/~jose/"},
    {Name => "Will Huang",
    Email => "whuang259@wisc.edu",
    HomePage => ""}
  },
  Headline => "produce critical equations and compute ED degrees",
  DebuggingMode => false,
  AuxiliaryFiles => true,
  PackageImports => {"Elimination","MonodromySolver"},
  PackageExports => {"Bertini","NumericalAlgebraicGeometry"},
  Configuration => { "Continuation"=>"Bertini" },
  OptionalComponentsPresent => (readPackage "Bertini").OptionalComponentsPresent,
  CacheExampleOutput => true,
  Keywords => {"Applied Algebraic Geometry"}
)
 
randomCC=()->random CC
randCC=()->random CC
randomRR=()->((-1)^(random(1,2)) *random RR)
randomZZ=()->random(1,30103)
randomValue=(kk)-> if kk===CC then randomCC() else if kk===RR then randomRR() else randomZZ() 
randomVector=method(Options=>{		})
randomVector(ZZ,Thing):= o->(n,R) ->apply(n,i->randomValue(R))--list of length n of randomValue

load "./EuclideanDistanceDegree/Determinantal.m2"
load "./EuclideanDistanceDegree/LeftKernel.m2"
load "./EuclideanDistanceDegree/Numerical.m2"
load "./EuclideanDistanceDegree/Parameterization.m2"

export {
  "TempDirectory",
  "ReturnCriticalIdeal",
  "UseMonodromy",
  "Submodel",
  "SampleGenerator",
  "symbolicWeightEDDegree",
  "determinantalUnitEDDegree",
  "determinantalGenericEDDegree",
  "leftKernelWeightEDDegree",
  "leftKernelUnitEDDegree",
  "leftKernelGenericEDDegree",
  "newNumericalComputationOptions",
  "NumericalComputationOptions",
  "homotopyEDDegree",
  "numericWeightEDDegree",
  "numericGenericEDDegree",
  "numericUnitEDDegree",
  "parameterizedWeightEDDegree",
  "parameterizedGenericEDDegree",
  "parameterizedUnitEDDegree",
  "averageNumericEDDegree"
}

--##########################################################################--
-- INTERNAL METHODS
--##########################################################################--
parString = (aString) -> ("("|toString(aString)|")");
addSlash = (aString) -> (
  if #aString =!= 0 then (
    if aString_-1 === " " then error(aString | " cannot end with whitespace.");
    if aString_-1 =!= "/" then aString = aString | "/";
  );
  aString
);
checkZero = (aSol, eps) -> if aSol/abs//min < eps then false else true
sortPointFunction = (aSol) -> (if not (apply(aSol,i->{realPart i,imaginaryPart i}/abs//max)//min<1e-8) then true else false);

--##########################################################################--
-- DOCUMENTATION
--##########################################################################--

beginDocumentation()

doc /// -- Package 
  Key
    EuclideanDistanceDegree 
  Headline
    produce critical equations and compute ED degrees
  Description
    Text
      The Euclidean distance (ED) degree of a varieties arises from the following problem: given a (generic) data point, find the point on a
      given variety which minimizes the (possibly weighted) Euclidean distance function. The number of nonsingular critical points of the 
      distance function is the ED degree of the variety.
    Text
      This package provides several routines for determining the (generic or unit) Euclidean distance degree of an algebraic variety. These 
      routines include symbolic methods and numerical methods for determining these degrees; they can be grouped into four main types:
    Code
      UL {
        TO symbolicWeightEDDegree,
        TO leftKernelWeightEDDegree,
        TO numericWeightEDDegree,
        TO parameterizedWeightEDDegree
      }
    Text
      As an example, this code computes the (unit) ED degree of a circle both symbolically and numerically
    Example
      R = QQ[x,y];
      F = {x^2 + y^2 - 1};
      determinantalUnitEDDegree(F)
      leftKernelUnitEDDegree(F)
    Text
      When the variety is an affine cone, one is able to compute ED degrees using ED degree homotopies, i.e., a structured parameter 
      homotopy. The easiest case is when the variety is a hypersurface (or more generally, a complete intersection)  
    Example
      R = QQ[x1,x2,x3,x4];
      F = G = {det genericMatrix(R,2,2)};
      numericGenericEDDegree(F, G)
    Text
      When a $V(F)$ is not a complete intersection we incorporate a membership test to filter out residual critical points. Here $V(F)$
      is an irreducible component of $V(G)$ (a reducible variety) and `#G===codim ideal F`.  These methods employ an equation by equation
      method called regeneration. 
    Example
      R = QQ[x1,x2,x3,x4,x5,x6];
      F = (minors(2, genericMatrix(R,3,2)))_*;
      G = drop(F, -1);
      #G == codim ideal F;
      numericGenericEDDegree(F, G)
    Text
      One may also determine ED degrees using a parameter homotopy called a Weight-ED Degree Homotopy. This code computes a unit ED degree:
    Example
      R = QQ[x1,x2,x3,x4,x5,x6];
      F = (minors(2, genericMatrix(R,3,2)))_*;
      G = drop(F, -1);
      NCO = newNumericalComputationOptions(F, G);
      NCO#"TargetWeight" = apply(#gens R, i->1);
      homotopyEDDegree(NCO, "Weight", true, true)
  Subnodes
    symbolicWeightEDDegree
    leftKernelWeightEDDegree
    numericWeightEDDegree
    parameterizedWeightEDDegree
    averageNumericEDDegree
///

doc /// -- symbolic
  Key
    ReturnCriticalIdeal
    [symbolicWeightEDDegree, ReturnCriticalIdeal]
    [determinantalUnitEDDegree, ReturnCriticalIdeal]
    [determinantalGenericEDDegree, ReturnCriticalIdeal]
  Headline
    whether to return the ideal of critical points
  Usage
    ICP = symbolicWeightEDDegree(F, U, W, ReturnCriticalIdeal => true)
  Description
    Text
      Symbolic methods compute the Euclidean Distance degree of a variety by computing the degree of its critical ideal, which defines the
      critical points of the distance function on the variety. Setting this option will cause symbolic methods to return this ideal rather
      than its degree.
    Example
      R = QQ[x,y];
      F = {x^2 + y^2 - 1};
      (U,W) = ({12, 23}, {15, 331});
      ICP = symbolicWeightEDDegree(F, U, W, ReturnCriticalIdeal => true)
///

doc ///
  Key
    symbolicWeightEDDegree
    (symbolicWeightEDDegree, List, List, List)
    determinantalUnitEDDegree
    (determinantalUnitEDDegree, List)
    determinantalGenericEDDegree
    (determinantalGenericEDDegree, List)
  Headline
    symbolically compute ED degrees of affine varieties
  Usage
    UED = determinantalUnitEDDegree(F)
    GED = determinantalGenericEDDegree(F)
    GED = symbolicWeightEDDegree(F,U,W)
  Inputs
    F:List
      a system of polynomials (system need not be square)
    U:List
      a (generic) data vector 
    W:List
      a (generic) weight vector
  Outputs
    ED:ZZ
      a generic/unit Euclidean distance degree 
    ICP:Ideal
      ideal of critical points (if ReturnCriticalIdeal is set)
  Description
    Text
      This method computes Euclidean distance (ED) degrees for the variety defined by the system $F$ via symbolic computation. The ideal of
      critical points is formed by saturating the defining ideal of the variety with minors of the Jacobian and Augmented Jacobian. The
      degree of this ideal is the ED degree of the variety. The unit variant of this method computes an ED degree using random (integer) 
      data and unit weights, whereas the generic variant will use random data and random weights.
    Example
      R = QQ[x,y];
      F = {x^2 + y^2 - 1};
      (U,W) = ({12, 23}, {15, 331});
      UED = determinantalUnitEDDegree F
      GED = determinantalGenericEDDegree F
      ICP = symbolicWeightEDDegree(F, U, W, ReturnCriticalIdeal => true)
  Subnodes
    ReturnCriticalIdeal
///

doc /// -- parameterized
  Key
    UseMonodromy
    [parameterizedWeightEDDegree, UseMonodromy]
    [parameterizedUnitEDDegree, UseMonodromy]
    [parameterizedGenericEDDegree, UseMonodromy]
  Headline
    whether to use monodromy methods to compute degrees
  Usage
    GED = parameterizedWeightEDDegree(F, U, W, UseMonodromy => true)
  Description
    Text
      If this option is set, the parameterized ED degree methods will use the MonodromySolver to numerically solve the critical equations 
      and report the number of regular solutions as the ED degree of the parameterized variety.
    Example
      R = QQ[x,y];
      F = {x^2 + 1, x * y, y - 1};
      GED = parameterizedGenericEDDegree(F, UseMonodromy => true)
  Caveat
    Using monodromy may result in an ED degree that is less than expected. In particular, if the parameterization map is $n$-to-one, 
    using monodromy to solve for the degree may result in a degree than is off by a factor of $n$.
///

doc ///
  Key
    parameterizedWeightEDDegree
    (parameterizedWeightEDDegree, List, List, List)
    parameterizedUnitEDDegree
    (parameterizedUnitEDDegree, List)
    parameterizedGenericEDDegree
    (parameterizedGenericEDDegree, List)
  Headline
    compute ED degrees of parameterized varieties
  Usage
    UED = parameterizedUnitEDDegree(F)
    GED = parameterizedGenericEDDegree(F)
    GED = parameterizedWeightEDDegree(F,U,W)
  Inputs
    F:List
      a system of polynomials in $d$ variables parameterizing a $d$-dimensional variety 
    U:List
      a (generic) data vector 
    W:List
      a (generic) weight vector
  Outputs
    ED:ZZ
      a generic/unit Euclidean distance degree
  Description
    Text
      This method computes Euclidean distance (ED) degrees for the variety parameterized by the a set of polynomials $F$ in $d$ variables. If
      the resulting variety is $d$-dimensional, then by finding a global description for the kernel of the transpose of the Jacobian map, the
      critical equations of the variety can be computed. The unit variant of this method computes an ED degree using random (integer) data 
      and unit weights, whereas the generic variant will use random data and random weights.
    Example
      R = QQ[x,y];
      F = {x^2 + 1, x * y, y - 1};
      (U,W) = ({12, 23, 25}, {15, 331, 1});
      UED = parameterizedUnitEDDegree F
      GED = parameterizedGenericEDDegree F
      GED = parameterizedWeightEDDegree(F, U, W)
  Subnodes
    UseMonodromy
///

doc /// -- TempDirectory
  Key
    TempDirectory
    [leftKernelWeightEDDegree, TempDirectory]
    [leftKernelUnitEDDegree, TempDirectory]
    [leftKernelGenericEDDegree, TempDirectory]
    [numericWeightEDDegree, TempDirectory]
    [numericUnitEDDegree, TempDirectory]
    [numericGenericEDDegree, TempDirectory]
    [averageNumericEDDegree, TempDirectory]
  Headline
    set directory for Bertini runs
  Usage
    GED = leftKernelGenericEDDegree(F, U, W, TempDirectory => dir)
  Description
    Text
      This option is used to change the directory from which Bertini files are written to and run from for the numerical methods. If unset,
      a temp directory will be created and used.
    Example
      R = QQ[x,y];
      F = {x^2 + y^2 - 1};
      dir = temporaryFileName();
      GED = leftKernelGenericEDDegree(F, TempDirectory => dir)
///

doc /// -- leftKernel
  Key
    leftKernelWeightEDDegree
    (leftKernelWeightEDDegree, List, List, List)
    leftKernelUnitEDDegree
    (leftKernelUnitEDDegree, List)
    leftKernelGenericEDDegree
    (leftKernelGenericEDDegree, List)
  Headline
    numerically compute Euclidean distance degrees of complete intersections
  Usage
    UED = leftKernelUnitEDDegree(F)
    GED = leftKernelGenericEDDegree(F)
    GED = leftKernelWeightEDDegree(F,U,W)
  Inputs
    F:List
      a system of polynomials (need not be square) defining an affine variety that is a complete intersection
    U:List
      a (generic) data vector
    W:List
      a (generic) weight vector
  Outputs
    ED:ZZ
      a generic/unit Euclidean distance degree
  Description
    Text
      This method computes Euclidean distance (ED) degrees for affine varieties defined by the system $F$ numerically using Lagrange
      multipliers. If the resulting variety is a complete intersection, the left kernel of the augmented Jacobian is used to derive a set of 
      critical erquations which are passed into Bertini. The resulting number of critical points is returned as the ED degree. The unit 
      variant of this method computes an ED degree using random (complex) data and unit weights, whereas `leftKernelGenericEDDegree` will 
      use random data and random weights.
    Example
      R = QQ[x,y];
      F = {x^2 + y^2 - 1};
      (U,W) = ({.12, .23}, {.15, .331});
      UED = leftKernelUnitEDDegree F
      GED = leftKernelGenericEDDegree F
      GED = leftKernelWeightEDDegree(F, U, W)
  Caveat
    The computed ED degree may be lower than expected due to path tracking.
  Subnodes
    TempDirectory
///

doc ///  -- NumericalComputationOptions
  Key
    NumericalComputationOptions
  Headline
    define homotopy options and configurations
  Description
    Text
      This object stores all the options needed compute an ED degree using homotopy continuation with Bertini. It is created using the
      @TO newNumericalComputationOptions@ method.
    Text
      Keys available to customize the homotopy include: 
      "Directory" to change the directory Bertini is run from, the directory need not exist initially. 
      "StartData" and "TargetData" if executing a Data homotopy, defaults to random complex data. 
      "StartWeight" and "TargetWeight" if executing a Weight homotopy, defaults to random complex and unit weights respectively. 
      "Infinity" to add a hyperplane at infinity for homogenization, one is created by default in @TO homotopyEDDegree@ if not present. 
      "PrimalCoordinates" to add additional variables to the ambient space. By default this field will already contain the variables of 
      `ring F`, thus it is safer to append extra variables rather than overwriting the less, otherwise subsequent methods may fail. 
    Text
      Keys are also available to customize the Bertini run: 
      "FinerRestriction" a list of polynomials to filter down critical points. Critical points will only be kept if they vanish
      for every polynomial in this list. 
      "BertiniStartFiberSolveConfiguration" a list of options (e.g. `FinalTol -> 1e-8`) that will be passed into the Berni inputs file. By
      default, the options `{"TrackType"=>0, "PrintPathProgress"=>1000}` are included.
///

doc ///
  Key
    newNumericalComputationOptions
    (newNumericalComputationOptions, List, List)
    [newNumericalComputationOptions, TempDirectory]
    [newNumericalComputationOptions, Submodel]
  Headline
    define homotopy options and configurations
  Usage
    NCO = newNumericalComputationOptions(F, G)
  Inputs
    F:List
      a system of polynomials (need not be square) defining the variety
    G:List
      a system of polynomials (complete intersection) such that V(F) is an irreducible component of V(G), i.e. a witness model
    Submodel => List
      a system of linear polynomials to slice the variety
  Outputs
    NCO:NumericalComputationOptions
      a MutableHashTable to keep track of options and configurations for the homotopy methods
  Description
    Text
      Creates a @TO NumericalComputationOptions@ object. At mimumum it requires the model $F$ for which the ED Degree will be computed and
      a witness model $G$. A submodel $L$ may be passed in to "slice" the variety, by default it will be the empty set. A String indicating
      the temporary directory from which Bertini will read/write files during the run may also be passed in as an optional argument, by 
      default it will be a random temporary file name. The temporary directory will be created if it does not exist.
    Example
      R = QQ[x,y];
      F = G = {x^2 + y^2 - 1};
      dir = temporaryFileName();
      NCO = newNumericalComputationOptions(F, G, TempDirectory => dir);
      NCO#"TargetWeight" = apply(#gens R, i -> 1_R);
      UED = homotopyEDDegree(NCO, "Weight", true, true)
///

doc /// -- homotopy
  Key
    homotopyEDDegree   
    (homotopyEDDegree, NumericalComputationOptions, String, Boolean, Boolean)
  Headline
    numerically compute ED degrees of affine cones using homotopy continuation
  Usage
    GED = homotopyEDDegree(NCO, "Weight", true, false)
  Inputs
    NCO:NumericalComputationOptions
      a MutableHashTable to keeps track of the options and configurations for the homotopy methods
    ht:String
      one of "Weight" or "Data" which defines the type of homotopy to execute
    isStageOne:Boolean
      if set, runs stage 1 to solve the start system
    isStageTwo:Boolean
      if set, runs stage 2 to solve the target system via path tracking
  Outputs
    ED:ZZ
      a generic/unit Euclidean distance degree
  Description
    Text
      Executes the homotopy defined by the passed in @TO NumericalComputationOptions@ object. A weight homotopy will vary the weights of the
      distance function whereas a data homotopy will vary the data point. A stage1 homotopy solves the start system as defined in NCO whereas
      stage2 will perform the weight/data homotopy to solve the defined target system. It is possible to perform stage2 multiple times for
      different target systems without rerunning stage1, but at least one stage1 homotopy must be executed in the directory specified in NCO
      for stage2 to executed properly.
    Example
      R = QQ[x,y];
      F = G = {x^2+y^2-1};
      NCO = newNumericalComputationOptions(F, G);
      NCO#"TargetWeight" = apply(#gens R, i -> random RR);
      GED = homotopyEDDegree(NCO, "Weight", true, true)

      NCO#"TargetWeight" = apply(#gens R, i -> 1_R);
      UED = homotopyEDDegree(NCO, "Weight", false, true)
  Caveat
    Inaccurate results may be returned if $V(F)$ is contained in $V(L)$. The computed ED degree may be lower than expected due to path tracking.
  Subnodes
    NumericalComputationOptions
    newNumericalComputationOptions
///

doc /// -- numeric
  Key
    numericWeightEDDegree
    (numericWeightEDDegree, List, List, List, List)
    numericUnitEDDegree
    (numericUnitEDDegree, List, List)
    numericGenericEDDegree
    (numericGenericEDDegree, List, List)
    [numericWeightEDDegree, Submodel]
    [numericUnitEDDegree, Submodel]
    [numericGenericEDDegree, Submodel]
  Headline
    numerically compute ED degrees of affine cones using homotopy continuation
  Usage
    UED = numericUnitEDDegree(F,G)
    GED = numericGenericEDDegree(F,G)
    GED = homotopyEDDegree(F,G,U,V)
  Inputs
    F:List
      a system of polynomials (need not be square) defining the variety
    G:List
      a system of polynomials (complete intersection) such that $V(F)$ is an irreducible component of $V(G)$, i.e. a witness model
    U:List
      a (generic) data vector
    W:List
      a (generic) weight vector
    Submodel => List
      list of linear polynomials to slice the variety
  Outputs
    ED:ZZ
      a generic/unit Euclidean distance degree
  Description
    Text
      Special instances of the @TO homotopyEDDegree@ method that uses a homotopy on weights. The unit variant of this method computes an ED
      degree using random (complex) data and unit weights, whereas `numericGenericEDDegree` will use random data and random weights.
    Example
      R = QQ[x,y];
      F = G = {x^2+y^2-1};
      (U,W) = ({.12, .23}, {.15, .331});
      UED = numericUnitEDDegree(F, G)
      GED = numericGenericEDDegree(F, G)
      GED = numericWeightEDDegree(F, G, U, W)
  Caveat
    Inaccurate results may be returned if $V(F)$ is contained in $V(L)$. The computed ED degree may be lower than expected due to path tracking.
  Subnodes
    homotopyEDDegree
    Submodel
///

doc /// -- average
  Key
    averageNumericEDDegree
    (averageNumericEDDegree, List, List, ZZ)
    [averageNumericEDDegree, Submodel]
    [averageNumericEDDegree, SampleGenerator]
    [averageNumericEDDegree, Tolerance]
  Headline
    compute average ED degrees using sampled data
  Usage
    aED = averageNumericEDDegree(F, G, 10)
  Inputs
    F: List
      a system of polynomials (need not be square) defining the variety
    G: List
      list of polynomials (complete intersection) such that $V(F)$ is an irreducible component of $V(G)$, i.e. a witness model
    n: ZZ
      number of samples to take
    Submodel => List
      list of linear polynomials to slice the variety
    SampleGenerator => FunctionClosure
      a function which generates data samples
    Tolerance => RR
      tolerance to use when checking if a critical point is real
  Outputs
    aED: RR
      average ED degree after n trials
  Description
    Text
      Generate data samples using the given function and uses homotopy continuation to find critical points of the distance function. The
      average number of real critical points after $n$ trials is returned. This method creates a @TO NumericalComputationOptions@ object and 
      computes critical points using the @TO homotopyEDDegree@ method. By default, `random(RR)` is used to generate data samples. Points are 
      tested using the @TO realPoints@ function from the @TO NumericalAlgebraicGeometry@ package, a tolerance can be passed along using the
      `Tolerance` option, by default it is 1e-6.
    Example
      R = QQ[x,y];
      F = G = {x^2 + y^2 - 1};
      sampleGen = () -> apply(#gens R, i -> random(RR));
      aED = averageNumericEDDegree(F, G, 10, SampleGenerator => sampleGen)
  Subnodes
    SampleGenerator
///

--##########################################################################--
-- END DOCUMENTATION
--##########################################################################--

TEST /// -- basic examples
  setRandomSeed(123);
  R = QQ[x0, x1, x2];
  F = {x0^2*x2 - x1^2*(x1 + x2)};
  assert(determinantalGenericEDDegree(F) === 7);
  assert(determinantalUnitEDDegree(F) === 7);

  R = QQ[jj]/ideal(jj^2+1)[x0,x1,x2];
  F = {x0^2*x1 -(x1 - jj*x2)^2*x2};
  assert(determinantalGenericEDDegree(F) === 2*7);
  assert(determinantalUnitEDDegree(F) === 2*7);

  R = QQ[x0,x1,x2,x3];
  F = {x0^2*x1-x2*x3^2};
  assert(determinantalGenericEDDegree(F) === 10);
  assert(determinantalUnitEDDegree(F) === 10);

  R = QQ[x,y];
  F = {x^2 + y^2 - 1};
  (U, W) = ({12, 23}, {15, 331});
  assert(symbolicWeightEDDegree(F, U, W) === 4);
  assert(determinantalGenericEDDegree F === 4);
  assert(determinantalUnitEDDegree F === 2);
  
  R = QQ[x,y];
  F = G = {x^2+y^2-1};
  (U, W) = ({.12, .23}, {.15, .331})
  assert(leftKernelWeightEDDegree(F, U, W) === 4);
  assert(leftKernelGenericEDDegree F === 4);
  assert(leftKernelUnitEDDegree F === 2);

  assert(numericWeightEDDegree(F, G, U, W) === 4);
  assert(numericGenericEDDegree(F, G) === 4);
  assert(numericUnitEDDegree(F, G) === 2);
///

TEST ///  -- parameterization: basic test
  setRandomSeed(123456);
  R = QQ[x,y];
  F = {x^2 + 1, x * y, y - 1};
  GED_param = parameterizedGenericEDDegree F;

  n = #F;
  numX = #gens R;
  S = QQ[gens R] ** QQ[y_1..y_(n-numX), u_1..u_n];
  M = sub(matrix{F}, S);
  imageModel = eliminate(support M, ideal(M - matrix{for i from 1 to n list u_i}));
  GED_implicit = determinantalUnitEDDegree((sub(imageModel, QQ[support imageModel]))_*);
  assert(GED_param === GED_implicit);
///

TEST ///  -- parameterization: spurious critical points
  setRandomSeed(123456);
  R = QQ[x];
  F = {x^2, x^3};
  GED_param = parameterizedGenericEDDegree F;

  n = #F;
  numX = #gens R;
  S = QQ[gens R] ** QQ[y_1..y_(n-numX), u_1..u_n];
  M = sub(matrix{F}, S);
  imageModel = eliminate(support M, ideal(M - matrix{for i from 1 to n list u_i}));
  GED_implicit = determinantalUnitEDDegree((sub(imageModel, QQ[support imageModel]))_*);
  assert(GED_param === GED_implicit);
///

TEST ///  -- parameterization: monodromy vs symbolic
  setRandomSeed(123);
  R = QQ[x,y,z];
  F = {x*y*z, x^2 + y^2 + z^2, x + y + z};
  U = {1,2,3};
  W = {1,1,1};
  GED1 = parameterizedWeightEDDegree(F,U,W);
  GED2 = parameterizedWeightEDDegree(F,U,W, UseMonodromy => true);
  assert(GED1 === GED2);
///

end



uninstallPackage "EuclideanDistanceDegree"
restart
loadPackage "EuclideanDistanceDegree"
installPackage "EuclideanDistanceDegree"
check "EuclideanDistanceDegree"

viewHelp

-- Debugging surface test seed issue
loadPackage "EuclideanDistanceDegree"
setRandomSeed(123);
R = QQ[x,y,z];
  surfaces = {
    3*x^2 + 3*y^2 + z^2 - 1,
    x^2 + y^2 + z^3 - z^2,
    x^2 + y^3 + z^5,
    x^2 - y^2*z,
    x^2 + y^2*z^3,
    x^2 + y^2 + z^2 - 1,
    x^2 + z^2 - y^3*(y-1)^3,
    x^2 - y^3*z^3,
    x^2 + y^2 + z,
    x^2*y*z + x*y^2 + y^3 + y^3*z - x^2*z^2
  };

GED1 = {};
GED2 = {};
GED3 = {};

for surface in surfaces do (
  F = {surface};
  print(toString surface);

  GED1 = GED1 | {determinantalGenericEDDegree F};
  GED2 = GED2 | {leftKernelGenericEDDegree F};
  GED3 = GED3 | {numericGenericEDDegree(F, F)};
)

print("checking degrees");
scan(#GED1, i -> (
  if GED1_i =!= GED2_i then print("symb-left F: " | toString surfaces_i | "; " | GED1_i | ", " | GED2_i);
  if GED2_i =!= GED3_i then print("left-num F: " | toString surfaces_i | "; " | GED2_i | ", " | GED3_i);
  if GED1_i =!= GED3_i then print("sym-num F: " | toString surfaces_i | "; " | GED1_i | ", " | GED3_i);
))