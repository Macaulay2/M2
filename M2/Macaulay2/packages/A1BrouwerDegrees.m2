--A1BrouwerDegrees.m2
newPackage (
    "A1BrouwerDegrees",
    Version => "2.0",
    Date => "October 13, 2025",
    Authors => {
        {Name=> "Stephanie Atherton",
    Email => "satherton@student.otis.edu"},
        {Name => "Nikita Borisov",
	 Email => "nborisov@sas.upenn.edu",
	 HomePage => "https://www.math.upenn.edu/people/nikita-borisov"},
        {Name => "Thomas Brazelton",
	 Email => "brazelton@math.harvard.edu",
	 HomePage => "https://tbrazel.github.io/"},
	 {Name => "Somak Dutta",
	 Email => "somakdutta@tamu.edu"},
        {Name => "Frenly Espino",
	 Email => "frenly@sas.upenn.edu",
	 HomePage => "https://www.math.upenn.edu/people/frenly-espino"},
         {Name => "Tom Hagedorn",
	 Email => "hagedorn@tcnj.edu",
	 HomePage => "https://hagedorn.pages.tcnj.edu/"},
        {Name => "Zhaobo Han",
	 Email => "zbtomhan@sas.upenn.edu",
	 HomePage => "https://www.linkedin.com/in/zhaobo-han-77b1301a2/"},
     	{Name => "Jordy Lopez Garcia",
	 Email => "jordy.lopez@tamu.edu",
	 HomePage => "https://jordylopez27.github.io/"},
        {Name => "Joel Louwsma",
	 Email => "jlouwsma@niagara.edu",
	 HomePage => "https://www.joellouwsma.com/"},
        {Name => "Yuyuan Luo",
	 Email => "luo.yuyuan@princeton.edu",
	 HomePage => "https://web.math.princeton.edu/~yl4428/"},
        {Name => "Wern Juin Gabriel Ong",
	 Email => "wgabrielong@uni-bonn.de",
	 HomePage => "https://wgabrielong.github.io/"},
        {Name => "Ruzho Sagayaraj",
	 Email => "ruzhomath@tamu.edu",
	 HomePage => "https://github.com/Ruzho-S"},
        {Name => "Andrew Tawfeek",
	 Email => "atawfeek@uw.edu",
	 HomePage => "https://www.atawfeek.com/"}
	},
    Headline => "for working with A1-Brouwer degree computations and quadratic forms",
	PackageImports => {"MinimalPrimes", "Elimination"},
    PackageExports => {},
    AuxiliaryFiles => true,
	DebuggingMode => false,
	Keywords => {"Homotopy Theory","Commutative Algebra"},
    Certification => {
	"journal name" => "Journal of Software for Algebra and Geometry",
	"journal URI" => "https://msp.org/jsag/",
	"article title" => "$\\mathbb{A}^1$-Brouwer degrees in Macaulay2",
	"acceptance date" => "2024-08-07",
	"published article URI" => "https://msp.org/jsag/2024/14-1/p15.xhtml",
	"published article DOI" => "10.2140/jsag.2024.14.175",
	"published code URI" => "https://msp.org/jsag/2024/14-1/jsag-v14-n1-x15-A1BrouwerDegrees.zip",
	"release at publication" => "98f142fe23304b808c1e931b723b3addff77a643",
	"version at publication" => "1.1",
	"volume number" => "14",
	"volume URI" => "https://msp.org/jsag/2024/14-1/"
	}
    )

export{
    
    --AnisotropicDimension.m2
    "getAnisotropicDimensionQQp",
    "getAnisotropicDimension",
    "getWittIndex",
    
    -- ArithmeticMethods.m2
    "getPadicValuation",
    "getLocalAlgebraBasis",
    
    --BuildingForms.m2
    "makeDiagonalForm",
    "makeHyperbolicForm",
    "makePfisterForm",
    "makeDiagonalUnstableForm",
    "makeHyperbolicUnstableForm",   
    
    --Decomposition.m2
    "getAnisotropicPart",
    "getSumDecomposition",
    "getSumDecompositionString",
    
    --GrothendieckWittClasses.m2    
    "makeGWClass",
    "GrothendieckWittClass",
    "getAlgebra",
    "getBaseField",
    "getMatrix",
    "addGW",
    "multiplyGW",

    --GWInvariants.m2
    "getRank",
    "getSignature",
    "getIntegralDiscriminant",
    "getRelevantPrimes",
    "getHasseWittInvariant",
    
    --GWTransfer.m2
    "transferGW",
    
    --HilbertSymbols.m2
    "getHilbertSymbolReal",
    "getHilbertSymbol",
    
    --IsomorphismOfForms.m2
    "isIsomorphicForm",
    
    --Isotropy.m2
    "isAnisotropic",
    "isIsotropic",
    
    --LocalGlobalDegrees.m2
    "getGlobalA1Degree",
    "getLocalA1Degree",
    
    --MatrixMethods.m2
    "diagonalizeViaCongruence",
    
    --SimplifiedRepresentatives.m2
    "getDiagonalClass",
    "getDiagonalEntries",

    --TraceAndNorm.m2
    "getTrace",
    "getNorm",
    "getMultiplicationMatrix",

    --UnstableGrothendieckWittClasses.m2
    "makeGWuClass",
    "getScalar",
    "addGWu",
    "addGWuDivisorial",
    "getGWClass",
    "UnstableGrothendieckWittClass",

    --UnstableLocalGlobalDegrees.m2
    "getGlobalUnstableA1Degree",
    "getLocalUnstableA1Degree",

    --Options
    "linearTolerance",
    }

-- Basic arithmetic, p-adic, and commutative algebra operations we will use
load "./A1BrouwerDegrees/Code/ArithmeticMethods.m2"

-- Basic manipulations of matrices we will use
load "./A1BrouwerDegrees/Code/MatrixMethods.m2"

-- Establishing the GrothendieckWittClass type and some basic operations
load "./A1BrouwerDegrees/Code/GrothendieckWittClasses.m2"

-- Building new Grothendieck-Witt classes
load "./A1BrouwerDegrees/Code/BuildingForms.m2"

-- Providing simplified representatives of Grothendieck-Witt classes
load "./A1BrouwerDegrees/Code/SimplifiedRepresentatives.m2"

-- Hilbert symbols over the p-adic rational numbers
load "./A1BrouwerDegrees/Code/HilbertSymbols.m2"

-- Invariants of Grothendieck-Witt classes and symmetric bilinear forms
load "./A1BrouwerDegrees/Code/GWInvariants.m2"
    
-- Computing local and global A1-Brouwer degrees
load "./A1BrouwerDegrees/Code/LocalGlobalDegrees.m2"

-- Checking if forms are isomorphic
load "./A1BrouwerDegrees/Code/IsomorphismOfForms.m2"

-- Verifying (an)isotropy
load "./A1BrouwerDegrees/Code/Isotropy.m2"

-- Computing anisotropic dimension
load "./A1BrouwerDegrees/Code/AnisotropicDimension.m2"

-- Unstable Grothendieck-Witt classes 
load "./A1BrouwerDegrees/Code/UnstableGrothendieckWittClasses.m2"

-- Decomposing Grothendieck-Witt classes and symmetric bilinear forms
load "./A1BrouwerDegrees/Code/Decomposition.m2"

-- Unstable local and global A1-Brouwer degrees 
load "./A1BrouwerDegrees/Code/UnstableLocalGlobalDegrees.m2"

-- Compute traces and norms
load "./A1BrouwerDegrees/Code/TraceAndNorm.m2"

-- Compute transfer
load "./A1BrouwerDegrees/Code/GWTransfer.m2"


----------------------------
----------------------------
-- DOCUMENTATION
----------------------------
----------------------------

beginDocumentation()

doc ///
Node
    Key
        A1BrouwerDegrees
    Headline
        a package for working with A1-Brouwer degree computations and quadratic forms
    Description
        Text
            This package is intended to allow the computation of local and global A1-Brouer degrees in both the stable and unstable settings, and for manipulations of unstable Grothendieck-Witt classes and symmetric bilinear forms including their invariants and decompositions. 

            Version history: 
            
            @UL{
				(BOLD("V 1.1: "), "this version was developed by N. Borisov, T. Brazelton, F. Espino, T. Hagedorn, Z. Han, J. Lopez Garcia, J. Louwsma, G. Ong, and A. Tawfeek. This version implements computations of local and global A1-Brouwer degrees, as well as Grothendieck-Witt classes and their invariants. "),
				(BOLD("V 2.0: "), "this version was developed by S. Atherton, S. Dutta, J. Lopez Garcia, J. Louwsma, Y. Luo, G. Ong, and R. Sagayaraj. This version implements the computation of unstable local and global A1-Brouwer degrees, manipulations of the unstable Grothendieck-Witt group, and generalizes several methods in V 1.1 for Grothendieck-Witt class manipulations over fields to the setting of finite étale algebras over fields.")
			}@

            The $\mathbb{A}^{1}$-Brouwer degree and its unstable counterpart are valued in the Grothendieck-Witt ring and unstable Grothendieck-Group of a field $\text{GW}(k)$ and $\text{GW}^{u}(k)$, respectively. These can be computed as follows: 

        Example
            R = QQ[x];
            f = {x^4 - 6*x^2 - 7*x - 6};
            alpha = getGlobalA1Degree f
        
        Text


        Example
            K = frac R;
            q = (x^2 + x - 2)/(3*x + 5);
            beta = getGlobalUnstableA1Degree(q)

        Text
            Furthermore, we can compute a number of invariants associated to symmetric bilinear forms such as their @TO2(getWittIndex, "Witt indices")@, @TO2(getIntegralDiscriminant, "integral discriminants")@, and @TO2(getHasseWittInvariant, "Hasse-Witt invariants")@ at a fixed prime:
        
        Example
            getWittIndex alpha
            getIntegralDiscriminant alpha
            getHasseWittInvariant(alpha, 3)

        Text
            Finally, we provide methods for verifying if two symmetric bilinear forms or unstable Grothendieck-Witt classes are @TO2(isIsomorphicForm,"isomorphic")@, and for computing @TO2(getSumDecomposition,"simplified representatives")@ of these objects. 

        Example
            getSumDecompositionString alpha
            twoH = makeDiagonalForm(QQ, (1,-1,1,-1))
            isIsomorphicForm(alpha, twoH)
        Example
            getSumDecomposition beta
            gamma = makeGWuClass(matrix(QQ, {{11, 0},{0,22}}), 8)
            isIsomorphicForm(beta, gamma)
///

load "./A1BrouwerDegrees/Documentation/ArithmeticMethodsDoc.m2"

load "./A1BrouwerDegrees/Documentation/MatrixMethodsDoc.m2"

load "./A1BrouwerDegrees/Documentation/GrothendieckWittClassesDoc.m2"

load "./A1BrouwerDegrees/Documentation/BuildingFormsDoc.m2"

load "./A1BrouwerDegrees/Documentation/SimplifiedRepresentativesDoc.m2"

load "./A1BrouwerDegrees/Documentation/HilbertSymbolsDoc.m2"

load "./A1BrouwerDegrees/Documentation/GWInvariantsDoc.m2"

load "./A1BrouwerDegrees/Documentation/LocalGlobalDegreesDoc.m2"

load "./A1BrouwerDegrees/Documentation/IsomorphismOfFormsDoc.m2"

load "./A1BrouwerDegrees/Documentation/IsotropyDoc.m2"

load "./A1BrouwerDegrees/Documentation/AnisotropicDimensionDoc.m2"

load "./A1BrouwerDegrees/Documentation/DecompositionDoc.m2"

load "./A1BrouwerDegrees/Documentation/UnstableGrothendieckWittClassesDoc.m2"

load "./A1BrouwerDegrees/Documentation/TraceAndNormDoc.m2"

load "./A1BrouwerDegrees/Documentation/GWTransferDoc.m2"

load "./A1BrouwerDegrees/Documentation/UnstableLocalGlobalDegreesDoc.m2"

----------------------------
----------------------------
-- Testing
----------------------------
----------------------------

-- Tests for getDiagonalClass
-- Test 0
TEST ///
M1 = matrix(RR, {{0,1},{1,0}});
G1 = makeGWClass M1;
G2 = getDiagonalClass G1;
assert(getMatrix(G2) === matrix(RR, {{1,0},{0,-1}}));
///

-- Test 1
TEST ///
M3 = matrix(CC, {{1,2,3},{2,4,5},{3,5,7}});
G3 = makeGWClass M3;
G4 = getDiagonalClass G3;
assert(getMatrix(G4) === matrix(CC, {{1,0,0},{0,1,0},{0,0,1}}));
///

--Test 2
TEST ///
M5 = matrix(QQ, {{1,2,3},{2,4,5},{3,5,7}});
G5 = makeGWClass M5;
G6 = getDiagonalClass G5;
assert(getMatrix(G6) === matrix(QQ, {{1,0,0},{0,-2,0},{0,0,2}}));
///

-- Test 3
TEST ///
-- Tests for GrothendieckWittClass type
M1 = matrix(QQ, {{1,0},{0,1}});
M2 = matrix(QQ, {{1,2},{2,5}});
G1 = makeGWClass M1;
G2 = makeGWClass M2;
assert(getBaseField(G1) === QQ);
assert(getMatrix(G1) === M1);
-- Tests for addGW and multiplyGW
G3 = addGW(G1, G2);
G4 = multiplyGW(G1, G2);
assert(getMatrix(G3) === matrix(QQ, {{1,0,0,0},{0,1,0,0},{0,0,1,2},{0,0,2,5}}));
assert(getMatrix(G4) === matrix(QQ, {{1,2,0,0},{2,5,0,0},{0,0,1,2},{0,0,2,5}}));
///

-- Tests for getGlobalA1Degree and getLocalA1Degree
-- Test 4
TEST ///
T1 = QQ[x];
f = {x^2};
alpha = getGlobalA1Degree f;
beta = makeGWClass matrix(QQ, {{0,1},{1,0}});
assert(isIsomorphicForm(alpha, beta));
///

-- Test 5
TEST ///
QQ[z_1,z_2];
f1 = {(z_1 - 1)*z_1*z_2, (3/5)*z_1^2 - (17/3)*z_2^2};
f1GD = getGlobalA1Degree f1;
assert(getWittIndex(f1GD) == 3);
I1 = ideal(z_1, z_2);
I2 = ideal(z_1 - 1, z_2^2 - 9/85);
f1LD1 = getLocalA1Degree(f1, I1);
f1LD2 = getLocalA1Degree(f1, I2);
f1LDsum = addGW(f1LD1, f1LD2);
assert(isIsomorphicForm(f1LDsum, f1GD));
///

-- Test 6
TEST ///
GF(17)[w];
f2 = {w^4 + w^3 - w^2 - w};
f2GD = getGlobalA1Degree f2;
assert(getWittIndex(f2GD) == 2);
J1 = ideal(w + 1);
f2LD1 = getLocalA1Degree(f2, J1);
assert(getWittIndex(f2LD1) == 1);
J2 = ideal(w - 1);
f2LD2 = getLocalA1Degree(f2, J2);
J3 = ideal(w);
f2LD3 = getLocalA1Degree(f2, J3);
f2LDsum = addGW(addGW(f2LD1, f2LD2), f2LD3);
assert(isIsomorphicForm(f2LDsum, f2GD));
///

-- Tests for building forms
-- Test 7
TEST ///
P = makePfisterForm(GF(17), (2,3));
twoH = makeHyperbolicForm(GF(17), 4);
assert(isIsomorphicForm(P, twoH));
///

-- Test 8
TEST ///
alpha = makeDiagonalForm(RR, (1,-1));
beta = makeGWClass matrix(RR, {{0,1},{1,0}});
H = makeHyperbolicForm RR;
assert(isIsomorphicForm(alpha, H));
assert(isIsomorphicForm(beta, H));
///

-- Test for getLocalAlgebraBasis
-- Test 9
TEST ///
QQ[x,y];
f = {x^2 + 1 - y, y};
p = ideal(x^2 + 1, y);
assert(getLocalAlgebraBasis(f, p) == {1,x}); 
///

-- Tests for getDiagonalClass and getDiagonalEntries
-- Test 10
TEST ///
M1 = matrix(CC, {{1,0,0},{0,2,0},{0,0,-3}});
M2 = matrix(CC, {{1,0,0},{0,1,0},{0,0,1}});
G = makeGWClass M1;
assert(getMatrix(getDiagonalClass G) == M2);
assert(getDiagonalEntries(G) == {1,2,-3});
///

-- Test 11
TEST ///
M1 = matrix(RR, {{1,0,0},{0,2,0},{0,0,-3}});
M2 = matrix(RR, {{1,0,0},{0,1,0},{0,0,-1}});
G = makeGWClass M1;
assert(getMatrix(getDiagonalClass G) == M2);
assert(getDiagonalEntries(G) == {1,2,-3});
///

-- Test 12
TEST ///
M = matrix(QQ, {{1,0,0},{0,2,0},{0,0,-3}});
G = makeGWClass M;
assert(getMatrix(getDiagonalClass G) == M);
assert(getDiagonalEntries(G) == {1,2,-3});
///
    
-- Test 13
TEST ///
M = matrix(GF(5), {{1,0,0},{0,2,0},{0,0,-3}});
G = makeGWClass M;
assert(getMatrix(getDiagonalClass G) == M);
assert(getDiagonalEntries(G) == {1,2,-3});
///

-- Test 14
TEST ///
kk = GF(7);
M1 = matrix(kk, {{1,0,0},{0,2,0},{0,0,-3}});
M2 = matrix(kk, {{1,0,0},{0,1,0},{0,0,1}});
G = makeGWClass M1;
assert(getMatrix(getDiagonalClass G) == M2);
assert(getDiagonalEntries(G) == {1,2,-3});
///

-- Test 15
TEST ///
M1 = matrix(QQ, {{18,0,0},{0,125/9,0},{0,0,-8/75}});
M2 = matrix(QQ, {{2,0,0},{0,5,0},{0,0,-6}});
G1 = makeGWClass M1;
assert(getMatrix(getDiagonalClass G1) == M2);
///

-- Test for getPadicValuation
-- Test 16
TEST ///
assert(getPadicValuation(27,3) == 3);
///

-- Test 17
TEST ///
-- Test for getWittIndex and diagonalizeViaCongruence
B = matrix(QQ, {{0/1,1},{1,0}});
beta = makeGWClass B;
assert(getWittIndex(beta) == 1);
P = matrix(QQ, {{0/1, 5,1},{2,2,1},{0,0,1}});
A = matrix(QQ, {{1/1,0,0},{0,-1,0},{0,0,1}});
assert(getWittIndex(makeGWClass(diagonalizeViaCongruence(P*A*transpose(P)))) == 1);
///

-- Test for makeGWClass
-- Test 18
TEST ///
M1 = matrix(QQ, {{1/1,0,0},{0,1,0},{0,0,1}});
M2 = matrix(QQ, {{1/1,24/10,0},{24/10,-5,0},{0,0,69}});
M3 = matrix(GF(7), {{1,0,0},{0,2,0},{0,0,-3}});
assert(instance(makeGWClass M1, GrothendieckWittClass));
assert(instance(makeGWClass M2, GrothendieckWittClass));
assert(instance(makeGWClass M3, GrothendieckWittClass));
///

-- Test for getBaseField
-- Test 19
TEST ///
M1 = makeGWClass matrix(QQ, {{1/1,0,0},{0,2,3},{0,3,1}});
M2 = makeGWClass matrix(RR, {{1.0,24/10,-2.41},{24/10,-5,0},{-2.41,0,69}});
M3 = makeGWClass matrix(CC, {{1*ii,24/10,-2.41},{24/10,-5,0},{-2.41,0,69+ii}});
M4 = makeGWClass matrix(GF(7), {{1,0,0},{0,2,0},{0,0,-3}});

assert(getBaseField(M1) === QQ);
assert(instance(getBaseField(M2),RealField));
assert(instance(getBaseField(M3),ComplexField));
assert((getBaseField M4).order == 7);
///

-- Test for addGW
-- Test 20
TEST ///
A1 = makeGWClass matrix(QQ, {{1/1,0,-3},{0,23,0},{-3,0,-2/5}});
A2 = makeGWClass matrix(QQ, {{0,1/2,0},{1/2,5/9,0},{0,0,1}});
A3 = makeGWClass matrix(QQ, {{1/1,0,-3,0,0,0},{0,23,0,0,0,0},{-3,0,-2/5,0,0,0},{0,0,0,0,1/2,0},{0,0,0,1/2,5/9,0},{0,0,0,0,0,1}})

B1 = makeGWClass matrix(RR, {{sqrt(2),0,-3},{0,sqrt(5),0},{-3,0,-1/5}});
B2 = makeGWClass matrix(RR, {{1/3}});
B3 = makeGWClass matrix(RR, {{sqrt(2),0,-3,0},{0,sqrt(5),0,0},{-3,0,-1/5,0},{0,0,0,1/3}});

C1 = makeGWClass matrix(CC, {{2*ii,0,0},{0,-2,0},{0,0,-3}});
C2 = makeGWClass matrix(CC, {{1,0,-3+ii,0},{0,-2,0,0},{-3+ii,0,-3,0},{0,0,0,5}});
C3 = makeGWClass matrix(CC, {{2*ii,0,0,0,0,0,0},{0,-2,0,0,0,0,0},{0,0,-3,0,0,0,0},{0,0,0,1,0,-3+ii,0},{0,0,0,0,-2,0,0},{0,0,0,-3+ii,0,-3,0},{0,0,0,0,0,0,5}});

assert(addGW(A1, A2) === A3);
assert(addGW(B1, B2) === B3);
assert(addGW(C1, C2) === C3);
///

-- Test for isIsotropic/isAnisotropic
-- Test 21
TEST ///
A1 = matrix(QQ, {{0,1/1},{1/1,0}});
assert(isIsotropic A1);
assert(not isAnisotropic makeGWClass(A1));

A2 = matrix(RR, {{1,-2,4},{-2,2,0},{4,0,-7}});
assert(not isAnisotropic A2);
assert(isIsotropic makeGWClass A2);

k = GF(13^4);
A3=matrix(k, {{7,81,63},{81,7,55},{63,55,109}});
assert(isIsotropic makeGWClass A3);
-- Isotropic by the Chevalley-Warning Theorem

A4 = matrix(QQ, {{5,0},{0,5}});
assert(isAnisotropic A4);

A5 = matrix(CC, {{3+ii,0},{0,5-ii}});
assert(not isAnisotropic A5);
///

-- Tests for isIsomorphicForm
-- Test 22
TEST ///
B1 = matrix(QQ, {{1/1,-2/1,4/1},{-2/1,2/1,0},{4/1,0,-7/1}});
B2 = matrix(QQ, {{-17198/4225,-166126/975,-71771/1560},{-166126/975,-27758641/4050,-251077/135},{-71771/1560,-251077/135,-290407/576}});
assert(isIsomorphicForm(makeGWClass B1, makeGWClass B2));
B3 = matrix(QQ, {{-38/1,-50/1,23/1},{-50/1,-62/1,41/1},{23/1,41/1,29/1}});
assert(isIsomorphicForm(makeGWClass B1, makeGWClass B3));
///

--Test 23

TEST ///
A1 = matrix(RR, {{1/1,-2/1,4/1},{-2/1,2/1,0},{4/1,0,-7/1}});
A2 = matrix(RR, {{-38/1,-50/1,23/1},{-50/1,-62/1,41/1},{23/1,41/1,29/1}});
assert(isIsomorphicForm(makeGWClass A1, makeGWClass A2));

B1 = matrix(QQ, {{1/1,-2/1,4/1},{-2/1,2/1,0},{4/1,0,-7/1}});
B2 = matrix(QQ, {{-38/1,-50/1,23/1},{-50/1,-62/1,41/1},{23/1,41/1,29/1}});
assert(isIsomorphicForm(makeGWClass B1, makeGWClass B2));

k = GF(13)
C1 = matrix(k, {{1,11,4},{11,2,0},{4,0,6}});
C2 = matrix(k, {{1,2,10},{2,3,2},{10,2,3}});
assert(isIsomorphicForm(makeGWClass C1, makeGWClass C2));
///

-- Test for GWinvariants
-- Test 24
TEST ///
M1 = makeGWClass matrix(QQ, {{1/1,0,-3},{0,23,0},{-3,0,-2/5}});
M2 = makeGWClass matrix(QQ, {{1/1,0,0},{0, 23,0},{0,0,-2/5}});
M3 = makeGWClass matrix(QQ, {{1/1,0,0},{0,-23,0},{0,0,-2/5}});
M4 = makeGWClass matrix(QQ, {{-1/1,0,0},{0,-23,0},{0,0,-2/5}});

assert(getSignature(M1) == 1);
assert(getSignature(M2) == 1);
assert(getSignature(M3) == -1);
assert(getSignature(M4) == -3);

assert(getIntegralDiscriminant(M1) == -5405);
assert(getRelevantPrimes(M1) == {23, 5, 47});
assert(getHasseWittInvariant(M1, 5) == -1);
assert(getHasseWittInvariant(M1, 23) == 1);
assert(getHasseWittInvariant(M1, 47) == -1);
///

-- Test for getHilbertSymbols
-- Test 25
TEST ///
assert(getHilbertSymbol(100, 7, 3) == 1);
assert(getHilbertSymbol(100/121, 7/169, 3) == 1);

assert(getHilbertSymbol(5, 1/9, 7) == 1);
assert(getHilbertSymbol(1/9, 5, 7) == 1);

assert(getHilbertSymbol(3, 11, 3) == -1);
assert(getHilbertSymbol(3, 11, 2) == -1);
assert(getHilbertSymbol(-3, -11, 2) == 1);
assert(getHilbertSymbol(-5, 11, 2) == -1);

assert(getHilbertSymbolReal(-3/1, 5) == 1);
assert(getHilbertSymbolReal(-3, -5/1) == -1);
assert(getHilbertSymbolReal(-3/1, -5) == -1);
assert(getHilbertSymbolReal(3, 5) == 1);
///

-- New tests for v2.0 of package
-- Test 26
TEST ///
M1 = matrix(RR, {{0,1},{1,0}});
G1 = makeGWuClass M1;
G2 = getDiagonalClass G1;
assert(getMatrix(G2) === matrix(RR, {{1,0},{0,-1}}));
assert(getScalar(G2) === det M1);
assert(getGWClass(G1) === makeGWClass(M1));
///

-- Test 27
TEST ///
M3 = matrix(CC, {{1,2,3},{2,4,5},{3,5,7}});
G3 = makeGWuClass M3;
G4 = getDiagonalClass G3;
assert(getMatrix(G4) === matrix(CC, {{1,0,0},{0,1,0},{0,0,1}}));
assert(getScalar(G4) === det M3);
assert(getGWClass(G3) === makeGWClass(M3));
///

--Test 28
TEST ///
M5 = matrix(QQ, {{1,2,3},{2,4,5},{3,5,7}});
G5 = makeGWuClass M5;
G6 = getDiagonalClass G5;
assert(getMatrix(G6) === matrix(QQ, {{1,0,0},{0,-2,0},{0,0,2}}));
assert(getScalar(G6) === det M5);
assert(getGWClass(G5) === makeGWClass(M5));
///

-- Test 29
TEST ///
-- Tests for UnstableGrothendieckWittClass type
M1 = matrix(QQ, {{1,0},{0,1}});
M2 = matrix(QQ, {{1,2},{2,5}});
G1 = makeGWuClass M1;
G2 = makeGWuClass M2;
assert(getBaseField(G1) === QQ);
assert(getMatrix(G1) === M1);
-- Test for addGWu
G3 = addGWu(G1, G2);
assert(getMatrix(G3) === matrix(QQ, {{1,0,0,0},{0,1,0,0},{0,0,1,2},{0,0,2,5}}));
assert(getScalar(G3) === (det M1) * (det M2));
///

-- Tests for UnstableGrothendieckWittClass constructors
-- Test 30
TEST ///
M1 = matrix(QQ, {{1/1,0,0},{0,1,0},{0,0,1}});
M2 = matrix(QQ, {{1/1,24/10,0},{24/10,-5,0},{0,0,69}});
M3 = matrix(GF(7), {{1,0,0},{0,2,0},{0,0,-3}});
assert(instance(makeGWuClass M1, UnstableGrothendieckWittClass));
assert(instance(makeGWuClass M2, UnstableGrothendieckWittClass));
assert(instance(makeGWuClass M3, UnstableGrothendieckWittClass));
assert(try(makeGWuClass(M1,-1)) then false else true);
assert(try(makeGWuClass(M2,sub(3,GF 17))) then false else true);
assert(try(makeGWuClass(M3,sub(-6,GF 5))) then false else true);
///


-- Test for getBaseField and getAlgebra
-- Test 31
TEST ///
M1 = makeGWuClass matrix(QQ, {{1/1,0,0},{0,2,3},{0,3,1}});
M2 = makeGWuClass matrix(RR, {{1.0,24/10,-2.41},{24/10,-5,0},{-2.41,0,69}})
M3 = makeGWuClass matrix(CC, {{1*ii,24/10,-2.41},{24/10,-5,0},{-2.41,0,69+ii}})
M4 = makeGWuClass matrix(GF(7), {{1,0,0},{0,2,0},{0,0,-3}});

assert(getBaseField(M1) === QQ);
assert(getBaseField(M2) === RR_53);
assert(getBaseField(M3) === CC_53);
assert((getBaseField M4).order == 7);
assert(getAlgebra(M1) === QQ);
assert(getAlgebra(M2) === RR_53);
assert(getAlgebra(M3) === CC_53);
assert((getAlgebra M4).order == 7);
///

-- Test for addGWu
-- Test 32
TEST ///
A1 = makeGWuClass matrix(QQ, {{1/1,0,-3},{0,23,0},{-3,0,-2/5}});
A2 = makeGWuClass matrix(QQ, {{0,1/2,0},{1/2,5/9,0},{0,0,1}});
A3 = makeGWuClass matrix(QQ, {{1/1,0,-3,0,0,0},{0,23,0,0,0,0},{-3,0,-2/5,0,0,0},{0,0,0,0,1/2,0},{0,0,0,1/2,5/9,0},{0,0,0,0,0,1}})

B1 = makeGWuClass matrix(RR, {{sqrt(2),0,-3},{0,sqrt(5),0},{-3,0,-1/5}});
B2 = makeGWuClass matrix(RR, {{1/3}});
B3 = makeGWuClass matrix(RR, {{sqrt(2),0,-3,0},{0,sqrt(5),0,0},{-3,0,-1/5,0},{0,0,0,1/3}});

C1 = makeGWuClass matrix(CC, {{2*ii,0,0},{0,-2,0},{0,0,-3}});
C2 = makeGWuClass matrix(CC, {{1,0,-3+ii,0},{0,-2,0,0},{-3+ii,0,-3,0},{0,0,0,5}});
C3 = makeGWuClass matrix(CC, {{2*ii,0,0,0,0,0,0},{0,-2,0,0,0,0,0},{0,0,-3,0,0,0,0},{0,0,0,1,0,-3+ii,0},{0,0,0,0,-2,0,0},{0,0,0,-3+ii,0,-3,0},{0,0,0,0,0,0,5}});

assert(addGWu(A1, A2) === A3);
assert(getMatrix addGWu(B1, B2) === getMatrix B3);
assert(abs(getScalar addGWu(B1, B2) - getScalar B3) < 1e-15);
assert(addGWu(C1, C2) === C3);
///

-- Test for constructors over etale algebras
-- Test 33
TEST ///
R = QQ[x]/(x^2 + 1);
S = QQ[y]/(y^2 - 1);
M1 = matrix(R, {{1,2},{2,x}});
M2 = matrix(S, {{1,2},{2,y}});
assert(try(makeGWuClass(M1)) then true else false);
assert(try(makeGWClass(M1)) then true else false);
assert(try(makeGWuClass(M2)) then true else false);
assert(try(makeGWClass(M2)) then true else false);
assert(getAlgebra(makeGWuClass(M1)) === R);
assert(getAlgebra(makeGWClass(M2)) === S);
///

-- Test for diagonalization over etale algebras
-- Test 34
TEST ///
R = QQ[x]/(x^2 + 1);
S = QQ[y]/(y^2 - 1);
G1 = makeGWClass matrix(R, {{1,2},{2,x}});
G2 = makeGWClass matrix(S, {{1,2},{2,y}});
G3 = makeGWuClass matrix(R, {{1,2},{2,x}});
G4 = makeGWuClass matrix(S, {{1,2},{2,y}});
assert(getDiagonalClass(G1) === makeGWClass matrix(R, {{1,0},{0,x-4}}));
assert(getDiagonalClass(G2) === makeGWClass matrix(S, {{1,0},{0,y-4}}));
assert(getDiagonalClass(G3) === makeGWuClass(matrix(R, {{1,0},{0,x-4}}), getScalar G3));
assert(getDiagonalClass(G4) === makeGWuClass(matrix(S, {{1,0},{0,y-4}}), getScalar G4));
assert(getGWClass(G3) === G1);
assert(getGWClass(G4) === G2);
///

-- Tests for isIsomorphicForm for unstable classes
-- Test 35
TEST ///
B1 = matrix(QQ, {{1/1,-2/1,4/1},{-2/1,2/1,0},{4/1,0,-7/1}});
B2 = matrix(QQ, {{-17198/4225,-166126/975,-71771/1560},{-166126/975,-27758641/4050,-251077/135},{-71771/1560,-251077/135,-290407/576}});
assert(isIsomorphicForm(makeGWuClass B1, makeGWuClass(B2,-18)));
B3 = matrix(QQ, {{-38/1,-50/1,23/1},{-50/1,-62/1,41/1},{23/1,41/1,29/1}});
assert(isIsomorphicForm(makeGWuClass B1, makeGWuClass(B3, -18)));
///

-- Test for local degree and global degree functions and that the local-global principle is satisfied
-- Test 36
TEST ///
frac QQ[x];
q = (x^5 - 6*x^4 + 11*x^3 - 2*x^2 - 12*x + 8)/(x^4 - 5*x^2 + 7*x + 1);
Gdeg = getGlobalUnstableA1Degree q;
M = matrix(QQ, {{-68, 38, 11, -14, 1}, {38, -63, 63, -29, 7}, {11, 63, -84, 39, -5}, {-14, -29, 39, -16, 0}, {1, 7, -5, 0, 1}});
assert(isIsomorphicForm(Gdeg,makeGWuClass(M, -53240)))
deg1 = getLocalUnstableA1Degree(q, -1)
deg2 = getLocalUnstableA1Degree(q, 1)
deg3 = getLocalUnstableA1Degree(q, 2)
assert(isIsomorphicForm(deg1, makeGWuClass(matrix(QQ, {{-5/27}}))));
assert(isIsomorphicForm(deg2, makeGWuClass(matrix(QQ, {{-2}}))));
assert(isIsomorphicForm(deg3, makeGWuClass(matrix(QQ, {{0, 0, 11/3}, {0, 11/3, 0}, {11/3, 0, 0}}))));
degSum = addGWuDivisorial({deg1, deg2, deg3}, {-1, 1, 2});
assert(isIsomorphicForm(degSum, Gdeg));
///

-- Test for general local and global degree functions
-- Test 37
TEST ///
F = GF(32003)
R = frac F[x];
q = (x^2 + x - 2)/(3*x + 5);
Gdeg = getGlobalUnstableA1Degree(q);
deg1 = getLocalUnstableA1Degree(q, -2);
deg2 = getLocalUnstableA1Degree(q, 1);
assert(isIsomorphicForm(Gdeg, makeGWuClass(matrix(F, {{11, 5}, {5, 3}}))));
assert(isIsomorphicForm(deg1, makeGWuClass(matrix(F, {{1/3}}))));
assert(isIsomorphicForm(deg2, makeGWuClass(matrix(F, {{8/3}}))));
degSum = addGWuDivisorial({deg1, deg2}, {-2, 1});
assert(isIsomorphicForm(degSum, Gdeg));
///


-- Test for trace and norm
-- Test 38
TEST ///
R=GF(2)[x]/(x^2+x+1)
F=frac R
K=F[y]/(y^2+x*y+1)
N=getMultiplicationMatrix(K,1+x*y)
assert(N-matrix{{1, x},{x, 1+x^2}}==map(F^2,F^2,0))
///

-- Test 39
TEST ///
F = QQ[x]/(x^5+2*x+3) 
F=frac F
N=getMultiplicationMatrix(F[y]/(y^3+3*y+2),1+x*y)
assert(N-matrix{{1_F, 0_F, -2*x },{x, 1, -3*x},{0, x, 1 }}==map(F^3,F^3,0))
assert(getTrace(F[y]/(y^3+3*y+2),1+x*y)==3_F)
assert(getNorm(F[y]/(y^3+3*y+2),1+x*y)==det N)
///

-- Test for transferGW
-- Test 40
TEST ///
R = QQ[x]/(x^5 - x - 1)
M = matrix(R, {{1, 3*x^2 + 4*x^4, 8*x^3 + 4}, {3*x^2 + 4*x^4, 5, 1}, {8*x^3 + 4, 1, 7*x^2 + 3*x}});
G = makeGWClass M;
GQ = transferGW G;
assert(GQ === makeDiagonalForm(QQ, (5, -75, 17059280/279299)));
///

-- Test 41
TEST ///
R = GF(7)[x]/(x^3 + 6*x^2 + 4);
M = matrix(R, {{1, 2, x}, {2, x^2 + 5, 3*x + 2}, {x, 3*x + 2, 5}});
G = makeGWClass M;
G7 = transferGW G;
assert(isIsomorphicForm(G7, makeDiagonalForm(GF(7), (3, 4, 4))));
///

-- Test for diagonal and hyperbolic unstable constructors
-- Test 42
TEST ///
alpha = makeDiagonalUnstableForm(RR, (1,-1));
beta = makeGWuClass matrix(RR, {{0,1},{1,0}});
H = makeHyperbolicUnstableForm RR;
assert(isIsomorphicForm(alpha, H));
assert(isIsomorphicForm(beta, H));
///

-- Test 43
TEST ///
alpha = makeDiagonalUnstableForm(GF(27), (1,-1));
beta = makeGWuClass matrix(GF(27), {{0,1},{1,0}});
H = makeHyperbolicUnstableForm GF(27);
assert(isIsomorphicForm(alpha, H));
assert(isIsomorphicForm(beta, H));
///
