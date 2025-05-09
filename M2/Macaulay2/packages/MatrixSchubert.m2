newPackage(
    "MatrixSchubert",
    AuxiliaryFiles => true,
    Version => "1.2",
    Date => "May 8, 2025",
    Keywords => {"Combinatorics", "Commutative Algebra"},
    Authors => {
        {Name => "Ayah Almousa", 
            Email => "aalmousa@sc.edu",
            HomePage => "http://sites.google.com/view/ayah-almousa"},
	{Name=> "Sean Grate",
	    Email => "sean.grate@auburn.edu",
	    HomePage => "https://seangrate.com/"},
	{Name => "Daoji Huang",
	    Email => "dhuang@ias.edu",
	    HomePage => "https://daojihuang.me"},
        {Name => "Patricia Klein", 
            Email => "pjklein@tamu.edu", 
            HomePage => "https://patriciajklein.github.io/"},
	{Name => "Adam LaClair",
	    Email => "alaclair@purdue.edu",
	    HomePage=> "https://sites.google.com/view/adamlaclair/home"},
        {Name => "Yuyuan Luo",
            Email => "yuyuan.luo@princeton.edu",
            HomePage=> "https://yuyuan-luo.github.io/"},
	{Name => "Joseph McDonough",
	    Email => "mcdo1248@umn.edu",
	    HomePage=> "https://jmcdonough98.github.io/"}
    },
    Headline => "functions for investigating ASM and matrix Schubert varieties",
    PackageExports => {
        "SimplicialComplexes",
        "SimplicialDecomposability",
        "Posets",
        "MinimalPrimes"
    }
)

export{
    --MatrixSchubertConstructions.m2
    "padASM",	     	     	    -- ++
    "isPartialASM",    	       	    --documented ++
    "partialASMToASM",	      	    --documented ++
    "antiDiagInit",    	       	    --documented ++
    "antidiagInit" => "antiDiagInit",
    "antidiagonalInitialIdeal" => "antiDiagInit",
    "rankTable",    	     	    --documented ++  
    "rotheDiagram",    	       	    --documented ++  
    "augmentedRotheDiagram",	    --documented ++
    "essentialSet",	     	        --documented ++
    "augmentedEssentialSet",        --documented ++
    "schubertDeterminantalIdeal",     --documented ++
    "schubDetIdeal" => "schubertDeterminantalIdeal",
    "fultonGens",    	     	    --documented ++
    "diagLexInitSE",   	      	    --documented ++
    "diagLexInitNW",	    	    --documented ++
    "diagRevLexInit",	     	    --documented ++
    "subwordComplex",	     	    --documented ++
    "entrywiseMinRankTable",	    --documented ++
    "entrywiseMaxRankTable",	    --documented ++
    "schubertDecompose",	        --documented ++
    "schubDecompose" => "schubertDecompose",
    "permSetOfASM",                  --documented ++
    "permutationSetofASM" => "permSetOfASM",
    "isIntersectionOfSchubertDeterminantalIdeals",    --documented ++
    "isIntersectionSchubIdeals" => "isIntersectionOfSchubertDeterminantalIdeals",
    "isASMIdeal",    	     	    --documented ++	 
    "isASM",    	            --documented ++	 
    "isASMUnion",    	     	    --documented ++
    "getASM",	     	     	    --documented ++
    "isMinRankTable",	     	    --documented ++
    "rankTableToASM",	     	    --documented ++
    "rankTableFromMatrix",    	    --documented ++
    "schubertIntersect",	     	    --documented ++
    "schubIntersect" => "schubertIntersect",
    "schubertAdd",	       	       	    --documented ++
     "schubAdd" => "schubertAdd",
   
 --permutationMethods.m2   
    "isPerm",	     	     	    --documented ++
     "isPermutation" => "isPerm",
    "permToMatrix",    	       	    --documented ++
     "permutationToMatrix" => "permToMatrix",
    "lastDescent",    	      	    --documented ++
    "firstDescent",    	       	    --documented ++
    "descentSet",    	     	    --documented ++
    "permLength",    	     	    --documented ++
     "permutationLength" => "permLength",
    "inverseOf",             	    --documented ++
    "longestPerm",    	      	    --documented ++
     "longestPermutation" => "longestPerm",
    "toOneLineNotation",    	    --documented ++
    "composePerms",    	       	    --documented ++
     "composePermutations" => "composePerms",
    "isPatternAvoiding",    	    --documented ++
    "isVexillary",    	      	    --documented ++
    "avoidsAllPatterns",	        --documented ++
    "isCartwrightSturmfels",	    --documented ++
    "isCDG",	    	    	    --documented ++
    "rajcode",	      	      	    --documented ++
    "rajchgotCode" => "rajcode",
    "rajCode" => "rajcode",
    "rajIndex",        	       	    --documented ++
    "rajchgotIndex" => "rajIndex",
    "grothendieckPolynomial",	       	    -- documented ++
    "grothendieckPoly" => "grothendieckPolynomial",
    "schubertPolynomial",    	       	    -- documented ++
    "schubertPoly" => "schubertPolynomial",
    "schubPoly" => "schubertPolynomial",
    "doubleSchubertPolynomial",           -- documented ++
    "doubleSchubertPoly" => "doubleSchubertPolynomial",
    "doubleSchubPoly" => "doubleSchubertPolynomial",
    "PipeDream",
    "pipeDreams",    	     	    -- documented ++
    "pipeDreamsNonReduced",    	    -- documented ++
    "ASMToMonotoneTriangle",        --documented ++
    "monotoneTriangleToASM",        --documented ++

--MatrixSchubertInvariants.m2    
    "schubertRegularity",    	                --documented ++
    "schubReg" => "schubertRegularity",
    "schubertCodim",           	    --documented ++
    "schubCodim" => "schubertCodim",
    "KPolynomialASM",	     	    --documented ++
    "isSchubertCM",    	       	    --documented ++
    "isSchubCM" => "isSchubertCM",

--ASM_Lists.m2
    "ASMFullList",    	      	    --documented ++
    "ASMRandomList",	    	    --documented ++
    "cohenMacaulayASMsList",	    --documented ++
    "nonCohenMacaulayASMsList",	    --documented ++
    "initialIdealsList"    	    --documented ++
}

--keys used for Schubert/Grothendieck polynomials
protect PolyType
protect Double
protect Operator
protect ASM
------------------------------------------------------------------------------
------------------------------------------------------------------------------
-- **CODE** --
------------------------------------------------------------------------------
------------------------------------------------------------------------------
load "./MatrixSchubert/permutationMethods.m2"
load "./MatrixSchubert/MatrixSchubertConstructions.m2"
load "./MatrixSchubert/MatrixSchubertInvariants.m2"
load "./MatrixSchubert/ASM_Lists.m2"
------------------------------------------------------------------------------
------------------------------------------------------------------------------
-- **DOCUMENTATION** --
------------------------------------------------------------------------------
------------------------------------------------------------------------------
beginDocumentation ()    
load "./MatrixSchubert/permutationMethodsDOC.m2"
load "./MatrixSchubert/MatrixSchubertConstructionsDOC.m2"
load "./MatrixSchubert/MatrixSchubertInvariantsDOC.m2"
load "./MatrixSchubert/ASM_ListsDOC.m2"
------------------------------------------------------------------------------
------------------------------------------------------------------------------
-- **TESTS** --
------------------------------------------------------------------------------
------------------------------------------------------------------------------
load "./MatrixSchubert/MatrixSchubertTests.m2"

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
uninstallPackage "MatrixSchubert"
restart
installPackage "MatrixSchubert"
restart
needsPackage "MatrixSchubert"
elapsedTime check "MatrixSchubert"
viewHelp "MatrixSchubert"
