-- -*- coding: utf-8 -*-
-- licensed under GPL v2 or any later version

newPackage select((
     "NumericalAlgebraicGeometry",
     Version => "1.17",
     Date => "Mar 2021",
     Headline => "numerical algebraic geometry",
     HomePage => "http://people.math.gatech.edu/~aleykin3/NAG4M2",
     AuxiliaryFiles => true,
     Authors => {
	  {Name => "Anton Leykin", Email => "leykin@math.gatech.edu", HomePage => "https://people.math.gatech.edu/~aleykin3"},
	  {Name => "Robert Krone", Email => "krone@math.gatech.edu"}
	  },
     Keywords => {"Numerical Algebraic Geometry"},
     Configuration => { "PHCPACK" => "phc",  "BERTINI" => "bertini", "HOM4PS2" => "hom4ps2" },	
     PackageExports => {"NAGtypes",
	 "NumericalLinearAlgebra",
	 "SLPexpressions"},
     PackageImports => {"PHCpack","Bertini"},
     -- DebuggingMode should be true while developing a package, 
     --   but false after it is done
     --DebuggingMode => true,
     DebuggingMode => false,
     Certification => {
	  "journal name" => "The Journal of Software for Algebra and Geometry: Macaulay2",
	  "journal URI" => "http://j-sag.org/",
	  "article title" => "Numerical Algebraic Geometry",
	  "acceptance date" => "2011-05-20",
	  "published article URI" => "http://j-sag.org/Volume3/jsag-2-2011.pdf",
	  "published code URI" => "http://j-sag.org/Volume3/NumericalAlgebraicGeometry.tar",
	  "repository code URI" => "https://github.com/Macaulay2/M2/blob/master/M2/Macaulay2/packages/NumericalAlgebraicGeometry.m2",
	  "release at publication" => "c3a7ec33ee30195c2a8a15eef3456b2f27d73bf3",
	  "version at publication" => "1.4",
	  "volume number" => "3",
	  "volume URI" => "http://j-sag.org/Volume3/"
	  }
     ), x -> x =!= null)

-- Any symbols or functions that the user is to have access to
-- must be placed in one of the following two lists
export {
     "DoublePrecision",
     "setDefault", "getDefault",
     "solveSystem", 
     "solveGenericSystemInTorus", -- works with PHCpack only
     "totalDegreeStartSystem",
     "parameterHomotopy", "numericalIrreducibleDecomposition",
     "Field",
     "Software", "PostProcess", "PHCPACK", "BERTINI","HOM4PS2","M2","M2engine","M2enginePrecookedSLPs",
     "gamma","tDegree","tStep","tStepMin","stepIncreaseFactor","numberSuccessesBeforeIncrease",
     "Predictor","RungeKutta4","Multistep","Tangent","Euler","Secant","MultistepDegree","Certified",
     "EndZoneFactor", "maxCorrSteps", "InfinityThreshold", 
     -- "Normalize", -- exported by NumericalLA 
     "Projectivize",
     "AffinePatches", "DynamicPatch",
     "SLP", "HornerForm", "CompiledHornerForm", "CorrectorTolerance", "SLPcorrector", "SLPpredictor",
     "NoOutput",
     "randomSd", "goodInitialPair", "randomInitialPair", "GeneralPosition",
     "Bits", "Iterations", "ErrorTolerance", "ResidualTolerance",
     "Attempts", "SingularConditionNumber", 
     "regeneration", "isSolution", "SquaredUpSystem", "SquareUpMatrix", "squareUp",
     "Output", -- may rename/remove later
     "NAGtrace"
     }
exportMutable {
     }


-- local functions/symbols:
protect Processing; protect Undetermined -- possible values of SolutionStatus
protect SolutionAttributes -- option of getSolution 
protect Tracker -- an internal key in Point 
protect LastIncrement;

-- possible solution statuses returned by engine
solutionStatusLIST = {
    Undetermined, 
    Processing, 
    Regular, 
    Singular, 
    Infinity, 
    MinStepFailure, 
    Origin, 
    IncreasePrecision, 
    DecreasePrecision, 
    RefinementFailure
    }

-- experimental:
protect LanguageCPP, protect MacOsX, -- protect System, 
protect LanguageC, protect Linux, protect Language
protect maxNumberOfVariables
protect maxPrecision

-- DEBUG Core and NAGtypes ----------------------------------------
debug Core -- to enable engine routines
debug NAGtypes -- to enable private routines

-- ./NumericalAlgebraicGeometry/ FILES -------------------------------------
load "./NumericalAlgebraicGeometry/PHCpack/PHCpack.interface.m2" 
load "./NumericalAlgebraicGeometry/Bertini/Bertini.interface.m2" 

-- GLOBAL VARIABLES ----------------------------------
NAG = NumericalAlgebraicGeometry
protect symbol NAG
DoublePrecision = 53
protect symbol DoublePrecision

--PHCexe = NumericalAlgebraicGeometry#Options#Configuration#"PHCPACK";
BERTINIexe = NumericalAlgebraicGeometry#Options#Configuration#"BERTINI";
HOM4PS2exe = NumericalAlgebraicGeometry#Options#Configuration#"HOM4PS2";

DBG = 0; -- debug level (10=keep temp files)
SLPcounter = 0; -- the number of compiled SLPs (used in naming dynamic libraries)

Package % Symbol := (p,s) -> value (toString p | "$" | toString s) -- get an option name from the correct dictionary

DEFAULT = new MutableHashTable from {
     Field => CC,
     Software=>M2engine, NoOutput=>false, 
     NumericalAlgebraicGeometry$gamma=>1, 
     NumericalAlgebraicGeometry$tDegree=>1,
     -- step control
     tStep => 0.05, -- initial
     tStepMin => 1e-6,
     stepIncreaseFactor => 2_QQ,
     numberSuccessesBeforeIncrease => 4,
     -- predictor 
     Predictor=>RungeKutta4, 
     SLPpredictor=>false, --temp!!!
     MultistepDegree => 3, -- used only for Predictor=>Multistep
     -- corrector 
     SLPcorrector=>false, --temp!!!
     maxCorrSteps => 3,
     CorrectorTolerance => 1e-6, -- tracking tolerance
     -- end of path
     EndZoneFactor => 0.05, -- EndZoneCorrectorTolerance = CorrectorTolerance*EndZoneFactor when 1-t<EndZoneFactor 
     InfinityThreshold => 1e9, -- used to tell if the path is diverging
     -- projectivize and normalize
     -- Normalize => true, -- normalize in the Bombieri-Weyl norm -- turning this on fails something in NSC!!!
     Normalize => false, -- normalize in the Bombieri-Weyl norm
     Projectivize => false, 
     AffinePatches => DynamicPatch,
     SLP => false, -- possible values: false, HornerForm, CompiledHornerForm 	  
     -- refine options 
     ErrorTolerance => 1e-8,
     ResidualTolerance => 1e-4,
     Iterations => 30, 
     Bits => infinity,
     -- general
     Attempts => 5, -- max number of attempts (e.g., to find a regular path)
     Tolerance => 1e-6,
     SingularConditionNumber => 1e5, -- this may need to go away!!!
     Precision => DoublePrecision, -- infinity,
     maxNumberOfVariables => 50
     }

setDefault = method(Options => {
     Software=>null, 
     NoOutput=>null, 
     NumericalAlgebraicGeometry$gamma=>null, 
     NumericalAlgebraicGeometry$tDegree=>null,
     -- step control
     tStep=>null, -- initial
     tStepMin=>null,
     stepIncreaseFactor=>null,
     numberSuccessesBeforeIncrease=>null,
     -- predictor 
     Predictor=>null, 
     SLPpredictor=>null, --temp!!!
     MultistepDegree => null, -- used only for Predictor=>Multistep
     -- corrector 
     SLPcorrector=>null, --temp!!!
     maxCorrSteps => null,
     CorrectorTolerance => null, -- tracking tolerance
     -- end of path
     EndZoneFactor => null, -- EndZoneCorrectorTolerance = CorrectorTolerance*EndZoneFactor when 1-t<EndZoneFactor 
     InfinityThreshold => null, -- used to tell if the path is diverging
     -- projectivize and normalize
     Normalize => null, -- normalize in the Bombieri-Weyl norm
     Projectivize => null, 
     AffinePatches => null,
     -- slp's 
     SLP => null, -- possible values: null, HornerForm, CompiledHornerForm 	  
     -- refine options 
     ErrorTolerance => null,
     ResidualTolerance => null,
     Iterations => null,
     Bits => null,
     -- general
     Attempts => null, -- max number of attempts (e.g., to find a regular path)
     Tolerance => null,
     SingularConditionNumber => null,
     Precision => null
     })
installMethod(setDefault, o -> () -> scan(keys o, k->if o#k=!=null then DEFAULT#k=o#k))
getDefault = method()
getDefault Symbol := (s)->DEFAULT#s

-- CONVENTIONS ---------------------------------------

-- OLD FORMAT:
-- Polynomial systems are represented as lists of polynomials.
-- NEW FORMAT:
-- PolySystem (defined in NAGtypes).

-- OLD FORMAT:
-- Solutions are lists {s, a, b, c, ...} where s is list of coordinates (in CC)
-- and a,b,c,... contain extra information, e.g, SolutionStatus=>Regular indicates the solution is regular.
-- NEW FORMAT:
-- Solutions are of a type derived from AbstractPoint (defined in NAGtypes), e.g. Point.
 
-- M2 tracker ----------------------------------------
integratePoly = method(TypicalValue => RingElement)
integratePoly (RingElement,Number,Number) := RingElement => (f,a,b) -> (
     -- integral of a polynomial f from a to b
     R := ring f; 
     if numgens R != 1 then error "expected a univariate polynomial";
     CM := coefficients f;
     M := apply(flatten entries first CM, m->(
	       e := first flatten exponents m;
	       (1/(e+1))*R_0^(e+1)     
	       ));
     g := (matrix{M}*CM#1)_(0,0); -- antiderivative
     T := first gens R; 
     sub(g,T=>b)-sub(g,T=>a)
     )
		       

multistepPredictor = method(TypicalValue => List)
multistepPredictor (QQ,List) := List => memoize((c,s) -> (
-- coefficients for a multistep predictor
-- IN:  c = step adjustment coefficient (in QQ)
--      s = list of step adjustments (from the stepsize h = t_1-t_0)
--          s#i =  1 => t_(i+2)-t_(i+1) = c^(s#j)*(t_(i+1)-t_i)
-- OUT: b = coefficients in Adams-Bashforth-like method (list of rational numbers)
     t := symbol t;
     n := #s + 1; -- t_n is the one for which prediction is being made
     R := QQ; -- frac(QQ[h]);        
     stp := 1_R; -- h; WLOG, assume h=1 
     t_0 = 0_R;
     scan(n, j->(
	       t_(j+1) = t_j+stp;
	       if DBG>3 then << "t_("<< j+1 <<") = " << t_(j+1) << endl;
	       if j<n-1 then stp = c^(s#j)*stp;
	       ));
     apply(n, i->(
	       -- lagrange poly
	       T := symbol T;
	       RT := R[T];
	       allBUTi := toList(0..i-1) | toList(i+1..n-1);
	       L := sub((1/product(allBUTi, j->t_i-t_j))*product(allBUTi, j->T-t_j),RT);
	       -- << "i = " << i << "  L = " << L << endl;
	       integratePoly(L,t_(n-1),t_n)
	       ))
     ))

multistepPredictorLooseEnd = method(TypicalValue => List)
multistepPredictorLooseEnd (QQ,List) := List => memoize((c,s) -> (
-- coefficients for a multistep predictor with indeterminate last step
-- IN:  c = step adjustment coefficient (in QQ)
--      s = list of step adjustments (from the initial stepsize h = t_1-t_0)
-- OUT: b = list of polynomials in QQ[a], where a=(last step size)/(next to last stepsize)   
     t := symbol t;
     n := #s + 2; -- t_n is the one for which prediction is being made
     a := symbol a;
     R := QQ[a];         
     stp := 1_R; -- h; WLOG, assume h=1 
     t_0 = 0_R;
     scan(n, j->(
	       t_(j+1) = t_j+stp;
	       if DBG>3 then << "t_("<< j+1 <<") = " << t_(j+1) << endl;
	       stp = if j<n-2 then c^(s#j)*stp 
	       else if j==n-2 then a*stp;
	       ));
     apply(n, i->(
	       -- lagrange poly
	       T := symbol T;
	       RT := R[T];
	       allBUTi := toList(0..i-1) | toList(i+1..n-1);
	       L := sub((1/product(allBUTi, j->lift(t_i-t_j,QQ)))*product(allBUTi, j->T-t_j),RT);
	       -- << "i = " << i << "  L = " << L << endl;
	       integratePoly(L,t_(n-1),t_n)
	       ))
     ))

inverseMatrix = method() -- since inverse(Matrix) does not work for matrices with CC entries. 
inverseMatrix Matrix := M -> (
     n := numRows M;
     if n!=numColumns M then error "square matrix expected";
     solve(M, map CC^n) 
     )

norm2 = method(TypicalValue=>RR) -- 2-norm of a vector with CC entries
norm2 List := v -> sqrt(sum(v, x->x*conjugate x));
norm2 Matrix := v -> norm2 flatten entries v

normF = method(TypicalValue=>RR) -- Frobenius norm of a matrix
normF Matrix := M -> max first SVD M;
     

normalize = method(TypicalValue => Matrix) -- normalizes a column vector with CC entries
normalize Matrix := v -> (1/norm2 v)*v

BombieriWeylScalarProduct = method(TypicalValue=>CC)
BombieriWeylScalarProduct (RingElement,RingElement) := CC => (f,g) -> sum(listForm f, a->(
	  imc := product(a#0, d->d!) / (sum(a#0))!; -- inverse of multinomial coeff
	  bc := coefficient((ring f)_(first a),g); -- coeff of corresponding monomial in g
	  imc*a#1*conjugate bc -- ring=CC[...]
	  ))
BombieriWeylNormSquared = method(TypicalValue=>RR)
BombieriWeylNormSquared RingElement := RR => f -> realPart sum(listForm f, a->(
	  imc := product(a#0, d->d!) / (sum(a#0))!; -- inverse of multinomial coeff
	  imc*a#1*conjugate a#1 -- ring=CC[...]
	  ))

normalize RingElement := f -> (
    a := 1/sqrt(numgens ring f * BombieriWeylNormSquared f);
    promote(a,coefficientRing ring f) * f
    )
------------------------------------------------------
load "./NumericalAlgebraicGeometry/extraNAGtypes.m2"
load "./NumericalAlgebraicGeometry/track.m2"
load "./NumericalAlgebraicGeometry/refine.m2"

parameterHomotopy = method(TypicalValue => List, Options =>{
	Software=>null
	})
parameterHomotopy (List, List, List) := o -> (F, P, T) -> (
    o = fillInDefaultOptions o;
    if o.Software === BERTINI then bertiniParameterHomotopy(F,P,T)
    else error "not implemented"
    )

-* not used???
homogenizeSystem = method(TypicalValue => List)
homogenizeSystem List := List => T -> (
     R := commonRing T;
     h := symbol h;
     Rh := (coefficientRing R)[gens R | {h}]; 
     apply(T, f->homogenize(sub(f,Rh), h))
     )
dehomogenizeSystem = method(TypicalValue => List)
dehomogenizeSystem List := List => T -> (
     Rh := commonRing T;
     R := (coefficientRing Rh)[drop(gens Rh,-1)]; 
     apply(T, f -> (map(R,Rh,vars R | matrix{{1_R}})) f)
     )
*-

randomGaussian = method()
randomGaussian := () -> sum(12, i->random 1.0) - 6;

randomInComplexUnitSphere = method()
randomInComplexUnitSphere ZZ := Matrix => n->(
     x := transpose matrix {apply(n, i->randomGaussian()+ii*randomGaussian())};
     (1/norm2 x)*x
     )

randomInComplexUnitBall = method()
randomInComplexUnitBall ZZ := Matrix => n->(
     x := randomInComplexUnitSphere n; 
     r := (random 1.)^(1/(2*n));
     r*x
     );  
     
--dimension of \cal H_{(d)}, where d is a degree vector
dimHd = method()
dimHd List := ZZ => d->sum(#d, i->binomial(#d+d#i,d#i));

randomDiagonalUnitaryMatrix = method()
randomDiagonalUnitaryMatrix ZZ := n -> diagonalMatrix apply(n, i->exp(ii*random(2*pi)))

--random unitary n-by-n matrix (w.r.t. Haar measure): what is the ref?
randomUnitaryMatrix = method()
randomUnitaryMatrix ZZ := n -> (
     Ml := flatten entries randomInComplexUnitBall(n^2);
     M := map(CC^n, n, (i,j)->Ml#(n*i+j)); -- n by n matrix                         
     randomDiagonalUnitaryMatrix n * (last SVD M) 
     )

randomOrthonormalRows = method() -- return a random m-by-n matrix with orthonormal rows (m<=n)
randomOrthonormalRows(ZZ,ZZ) := (m,n) -> 
if n<m or m<1 then error "wrong input" else (randomUnitaryMatrix n)^(toList(0..m-1))

randomOrthonormalCols = method() -- return a random m-by-n matrix with orthonormal columns (m>=n)
randomOrthonormalCols(ZZ,ZZ) := (m,n) -> 
if m<n or n<1 then error "wrong input" else (randomUnitaryMatrix m)_(toList(0..n-1))

squareUp = method() -- squares up a polynomial system (presented as a one-column matrix)
squareUp PolySystem := P -> if P.?SquaredUpSystem then P.SquaredUpSystem else squareUp(P, P.NumberOfVariables)
squareUp (PolySystem,ZZ) := (P,n) -> (
    m := P.NumberOfPolys;
    if m<=n then "overdetermined system expected";
    C := coefficientRing ring P;
    M := if class C === ComplexField then sub(randomOrthonormalRows(n,m), C) else random(C^n,C^m);
    squareUp(P,M)
    )
squareUp(PolySystem,Matrix) := (P,M) -> (
    P.SquareUpMatrix = M;
    P.SquaredUpSystem = polySystem (sub(M,ring P)*P.PolyMap) -- should work without sub!!!
    )

squareUpMatrix = method()
squareUpMatrix PolySystem := P -> if P.?SquareUpMatrix then P.SquareUpMatrix else (
    n := P.NumberOfVariables;
    C := coefficientRing ring P;
    map(C^n)
    ) 

load "./NumericalAlgebraicGeometry/BSS-certified.m2"
load "./NumericalAlgebraicGeometry/0-dim-methods.m2"
load "./NumericalAlgebraicGeometry/witness-set.m2"
load "./NumericalAlgebraicGeometry/WSet-NumericalAlgebraicGeometry.m2"
load "./NumericalAlgebraicGeometry/intersection.m2"
load "./NumericalAlgebraicGeometry/decomposition.m2"
load "./NumericalAlgebraicGeometry/positive-dim-methods.m2"
load "./NumericalAlgebraicGeometry/deflation.m2"
load "./NumericalAlgebraicGeometry/SLP.m2"

load "./NumericalAlgebraicGeometry/WSet-deflation.m2"

-- HOM4PS2 part -----------------------------------------------------------

makeHom4psInput = method(TypicalValue=>Sequence)
makeHom4psInput (Ring, List) := (R, T) -> (
-- IN:  R = ring
--      T = polynomials of target system (in R)
-- OUT: (name, perm), where
--      name = input filename   
--      perm = hashtable of order of appearances of variables in the input
  filename := temporaryFileName() | "input"; 
  s := "{\n";
  scan(T, p -> s = s | replace("ii", "I", toString p) | ";\n");
  s = s | "}\n";
  f := openOut filename; 
  f << s;
  close f;
  -- assume names of vars are not substrings of each other
  p := sort apply(numgens R, i->(first first regex(toString R_i, s), i));
  ( filename, new HashTable from apply(#p, i->p#i#1=>i) )
  )

cleanupOutput = method(TypicalValue=>String)
cleanupOutput String := s -> (
-- cleanup output (Bertini and hom4ps2)
  t := replace("E", "e", s);
  t = replace("[(,)]","", t);
  t = replace("e\\+","e",t)
  )

readSolutionsHom4ps = method(TypicalValue=>List)
readSolutionsHom4ps (String, HashTable) := (f,p) -> (
-- IN:  f = output filename
--      p = permutation of coordinates to be applied (hashtable)
-- OUT: list of solutions
  s := {};
  l := lines get f;
  i := 0; -- line counter
  while substring(2,9,l#i) != "The order" do ( -- current line is non-empty	   
       coords := {};
       while #l#i > 2 do ( -- until an empty line
	    a := select(separate(" ",  cleanupOutput(l#i)), t->#t>0);
	    coords = coords | {(value a#0)+ii*(value a#1)};
	    i = i + 1;
       	    );
       if DBG>=10 then << coords << endl;
       s = s | { {apply(#coords, i->coords#(p#i))} };
       i = i + 4; -- skip to the next solution
       );
  s
  )


-----------------------------------------------------------------------
-- AUXILIARY FUNCTIONS

-- fills in options from DEFAULT option table
fillInDefaultOptions = method()
fillInDefaultOptions OptionTable := o -> (
     o = new MutableHashTable from o;
     scan(keys o, k->if o#k===null then o#k=DEFAULT#k); 
     new OptionTable from o
     )
 
selectUnique = method(TypicalValue=>Boolean, Options=>{Tolerance=>1e-6, Projective=>false})
selectUnique List := o -> sols ->(
     u := {};
     scan(sols, s->if all(u, t->not areEqual(s,t,o)) then u = u|{s});
     u
     )
 
NAGtrace = method()
NAGtrace ZZ := l -> (numericalAlgebraicGeometryTrace=l; oldDBG:=DBG; DBG=l; oldDBG);

-- normalized condition number of F at x
conditionNumber = method()
conditionNumber Matrix := M -> (s := first SVD M; if min s == 0 then infinity else max s / min s)
conditionNumber (List,List) := (F,x) -> (
     nF := apply(F, f->f/sqrt(#F * BombieriWeylNormSquared f)); -- normalize F
     x0 := normalize transpose matrix{x}; -- column unit vector
     DMforPN := diagonalMatrix(nF/(f->1/sqrt sum degree f) | {1});
     J := sub(transpose jacobian matrix{nF}, transpose sub(x0,CC)); -- Jacobian of F at x
     J = J || matrix{ flatten entries x0 / conjugate};
     conditionNumber(DMforPN*J) --  norm( Moore-Penrose pseudoinverse(J) * diagonalMatrix(sqrts of degrees) )     
     )

isSolution = method(Options=>{Tolerance=>null})
isSolution(AbstractPoint,PolySystem) := o -> (P,F) -> (
    o = fillInDefaultOptions o;
    -- P = newton(F,P); -- !!! change for non regular
    -- P.ErrorBoundEstimate < o.Tolerance
    residual(F,P) < o.Tolerance
    )

beginDocumentation()

load "./NumericalAlgebraicGeometry/doc.m2";

-*
undocumented {
    Field, 
    GateParameterHomotopy, 
    GateHomotopy, trackHomotopy, (trackHomotopy,Thing,List), endGameCauchy, (endGameCauchy,GateHomotopy,Number,MutableMatrix), 
    (endGameCauchy,GateHomotopy,Number,AbstractPoint),
    (evaluateH,GateHomotopy,Matrix,Number),
(evaluateH,GateParameterHomotopy,Matrix,Matrix,Number),
(evaluateHt,GateHomotopy,Matrix,Number),
(evaluateHt,GateParameterHomotopy,Matrix,Matrix,Number),
(evaluateHx,GateHomotopy,Matrix,Number),
(evaluateHx,GateParameterHomotopy,Matrix,Matrix,Number),
(specialize,GateParameterHomotopy,MutableMatrix),
[trackHomotopy,Software],
    }
*-

TEST ///
load concatenate(NumericalAlgebraicGeometry#"source directory","./NumericalAlgebraicGeometry/TST/SoftwareM2.tst.m2")
///
TEST ///
load concatenate(NumericalAlgebraicGeometry#"source directory","./NumericalAlgebraicGeometry/TST/SoftwareM2engine.tst.m2")
///
TEST ///
load concatenate(NumericalAlgebraicGeometry#"source directory","./NumericalAlgebraicGeometry/TST/SoftwareM2enginePrecookedSLPs.tst.m2")
///

load concatenate(NumericalAlgebraicGeometry#"source directory","./NumericalAlgebraicGeometry/TST/simple-tests.tst.m2")
load concatenate(NumericalAlgebraicGeometry#"source directory","./NumericalAlgebraicGeometry/TST/border-case-errors.m2")


TEST ///
if Bertini.Options.OptionalComponentsPresent then
load concatenate(NumericalAlgebraicGeometry#"source directory","./NumericalAlgebraicGeometry/Bertini/Bertini.test.m2")
///

TEST ///
if PHCpack.Options.OptionalComponentsPresent then
load concatenate(NumericalAlgebraicGeometry#"source directory","./NumericalAlgebraicGeometry/PHCpack/PHCpack.test.m2")
///
end

-- Here place M2 code that you find useful while developing this
-- package.  None of it will be executed when the file is loaded,
-- because loading stops when the symbol "end" is encountered.

restart
uninstallPackage "NumericalAlgebraicGeometry"
installPackage "NumericalAlgebraicGeometry"
installPackage ("NumericalAlgebraicGeometry",RerunExamples=>true, RemakeAllDocumentation=>true)
installPackage ("NumericalAlgebraicGeometry",RerunExamples=>false, RemakeAllDocumentation=>true)

-- (old way) installPackage("NumericalAlgebraicGeometry", SeparateExec=>true)

-- install docs with no absolute links
uninstallPackage "Style"
installPackage("Style")
installPackage("NumericalAlgebraicGeometry")

installPackage ("NumericalAlgebraicGeometry", MakeDocumentation=>false)
check "NumericalAlgebraicGeometry"

-- Local Variables:
-- compile-command: "make -C $M2BUILDDIR/Macaulay2/packages PACKAGES=NumericalAlgebraicGeometry "
-- End:
