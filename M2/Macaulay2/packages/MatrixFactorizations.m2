newPackage(
    "MatrixFactorizations",
    AuxiliaryFiles => true -*may need to change*-,
    Version => "0.1", 
    Date => "August 13, 2025",
    Authors => {
        {Name => "David Favero", 
            Email => "favero@umn.edu", 
            HomePage => ""},
	{Name=> "Sasha Pevzner",
	    Email => "pevzn002@umn.edu",
	    HomePage => ""},
	{Name => "Timothy Tribone",
	    Email => "tim.tribone@utah.edu",
	    HomePage => ""},
        {Name => "Keller VandeBogert", 
            Email => "kvandebo@nd.edu", 
            HomePage => "https://sites.google.com/view/kellervandebogert/home"}
    },
    Headline => "computing with matrix factorizations of different lengths",
    Keywords => {"Commutative Algebra", "Homological Algebra"},
    PackageExports => {
        "Complexes",
	--"CompleteIntersectionResolutions",
	--"TensorComplexes"
    },
    PackageImports => {
        "Complexes",
	"CompleteIntersectionResolutions",
	"PushForward",
	"TensorComplexes"
    }
)

export{"ZZdFactorization",
    "ZZdFactorizationMap",
    "period",
    "ZZdfactorization",
    "Fold",
    "isFactorizationMorphism",
    --"dHom",
    "randomFactorizationMap",
    "rootOfUnity",
    "RootOfUnity",
    "isZZdComplex",
    --MF_Functions
    "WellDefined",
    "collapseMF",
    "fullCollapse",
    "isdFactorization",
    "tailMF",
    "randomTailMF",
    "randomLinearMF",
    "monomialMF",
    "koszulMF",
    "eulerMF",
    "adjoinRoot",
    --"dTensor",
    --"eulerChi",
    "unfold",
    --"dDual",
    --"dShift",
    "linearMF",
    --"trivialFactorization",
    --"linearFactorization",
    --"randomFactorization",
    "potential",
    "projectiveCover",
    "injectiveCover",
    "suspension",
    "trivialMF",
    --CIResCompatibility.m2
    "higherHomotopyFactorization",
    "toBranchedCover",
    "branchedToMF",
    "mooreMF",
    "rk1MCM2gen",
    "adjointFactorization",
    "zeroOutDegrees"
    }
    
------------------------------------------------------------------------------
------------------------------------------------------------------------------
-- **CODE** --
------------------------------------------------------------------------------
------------------------------------------------------------------------------
--debug needsPackage "Complexes"
load "./MatrixFactorizations/ZZdFactorizations.m2"
load "./MatrixFactorizations/MF_functions.m2"
load "./MatrixFactorizations/CIResCompatibility.m2"

--load "./MatrixFactorizations/ZZdFactorizations.m2"
-*load "./Matrix-Factorizations/chernCharacter.m2"
load "./Matrix-Factorizations/CIResCompatibility.m2"
load "./Matrix-Factorizations/Compositions.m2"
load "./Matrix-Factorizations/eisenbud-schreyer-examples.m2"
load "./Matrix-Factorizations/functionsMF_new.m2"*-
------------------------------------------------------------------------------
------------------------------------------------------------------------------
-- **DOCUMENTATION** --
------------------------------------------------------------------------------
------------------------------------------------------------------------------
beginDocumentation ()    


load "./MatrixFactorizations/MatrixFactorizationsDOC.m2"
------------------------------------------------------------------------------
------------------------------------------------------------------------------
-- **TESTS** --
------------------------------------------------------------------------------
------------------------------------------------------------------------------

load "./MatrixFactorizations/KellerTests.m2"


end---------------------------------------------------------------------------     

------------------------------------------------------------------------------
------------------------------------------------------------------------------
-- **SCRATCH SPACE** --
------------------------------------------------------------------------------
------------------------------------------------------------------------------


------------------------------------
--Development Section
------------------------------------

restart
uninstallPackage "MatrixFactorizations"
restart
debug installPackage "MatrixFactorizations"
debug loadPackage("MatrixFactorizations",Reload => true, LoadDocumentation => true)
restart
needsPackage "MatrixFactorizations"
elapsedTime check "MatrixFactorizations"
viewHelp "MatrixFactorizations"
