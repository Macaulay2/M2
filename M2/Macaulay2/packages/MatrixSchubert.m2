newPackage(
    "MatrixSchubert",
    AuxiliaryFiles => true,
    Version => "0.1", 
    Date => "",
    Authors => {
        {Name => "Ayah Almousa", 
            Email => "almou007@umn.edu", 
            HomePage => "http://sites.google.com/view/ayah-almousa"},
	{Name=> "Sean Grate",
	    Email => "sean.grate@auburn.edu",
	    HomePage => "https://seangrate.com/"},
	{Name => "Daoji Huang",
	    Email => "huan0664@umn.edu",
	    HomePage => "https://daojihuang.me"},
        {Name => "Patricia Klein", 
            Email => "pjklein@tamu.edu", 
            HomePage => "https://patriciajklein.github.io/"},
	{Name => "Adam LaClair",
	    Email => "alaclair@purdue.edu",
	    HomePage=> "https://sites.google.com/view/adamlaclair/home"},
        {Name => "Yuyuan Luo",
            Email => "lyuyuan@mit.edu",
            HomePage=> "https://www.mit.edu/~lyuyuan/"}, 
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
    },
    DebuggingMode => true
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
    "inverseOf",             	    --documented (check)
    "longestPerm",    	      	    --documented (check)
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
    "grothendieckPolynomial",	       	    -- CHECK DOC
    "grothendieckPoly" => "grothendieckPolynomial",
    "schubertPolynomial",    	       	    -- CHECK DOC
    "schubertPoly" => "schubertPolynomial",
    "schubPoly" => "schubertPolynomial",
    "doubleSchubertPolynomial",           -- CHECK DOC
    "doubleSchubertPoly" => "doubleSchubertPolynomial",
    "doubleSchubPoly" => "doubleSchubertPolynomial",
    "pipeDreams",    	     	    -- CHECK DOC
    "pipeDreamsNonReduced",    	    -- CHECK DOC
    "netPD",	    	    	    -- CHECK DOC
    "netPipeDream" => "netPD",
    "ASMToMonotoneTriangle",        --documented ++
    "MonotoneTriangleToASM",        --documented ++

--MatrixSchubertInvariants.m2    
    "schubertRegularity",    	                --documented ++
    "schubReg" => "schubertRegularity",
    "schubertCodim",           	    --documented ++
    "schubCodim" => "schubertCodim",
    "KPolynomialASM",	     	    -- CHECK DOC
    "isSchubertCM",    	       	    --documented ++
    "isSchubCM" => "isSchubertCM",

--ASM_Lists.m2
    "ASMFullList",    	      	    --ADD EX TO DOC
    "ASMRandomList",	    	    --ADD EX TO DOC
    "cohenMacaulayASMsList",	    --ADD EX TO DOC
    "nonCohenMacaulayASMsList",	    --ADD EX TO DOC
    "initialIdealsList"    	        --ADD EX TO DOC
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

M = matrix{{0,3,4},{1,1,1}}
rankTableFromMatrix M

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
