--A1BrouwerDegrees.m2
newPackage (
    "A1BrouwerDegrees",
    Version => "1.1",
    Date => "July 23, 2024",
    Authors => {
        {Name => "Nikita Borisov",
	 Email => "nborisov@sas.upenn.edu",
	 HomePage => "https://www.math.upenn.edu/people/nikita-borisov"},
        {Name => "Thomas Brazelton",
	 Email => "brazelton@math.harvard.edu",
	 HomePage => "https://tbrazel.github.io/"},
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
        {Name => "Wern Juin Gabriel Ong",
	 Email => "gong@bowdoin.edu",
	 HomePage => "https://wgabrielong.github.io/"},
        {Name => "Andrew Tawfeek",
	 Email => "atawfeek@uw.edu",
	 HomePage => "https://www.atawfeek.com/"}
	},
    Headline => "A1-Brouwer degree computations",
    PackageImports => {},
    PackageExports => {},
    AuxiliaryFiles => true,
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
    
    -- ArithmeticMethods.m2
    "getPadicValuation",
    "getLocalAlgebraBasis",
    
    --MatrixMethods.m2
    "diagonalizeViaCongruence",
    
    --GrothendieckWittClasses.m2    
    "makeGWClass",
    "GrothendieckWittClass",
    "getBaseField",
    "getMatrix",
    "addGW",
    "multiplyGW",
    
    --BuildingForms.m2
    "makeDiagonalForm",
    "makeHyperbolicForm",
    "makePfisterForm",
    
    --SimplifiedRepresentatives.m2
    "getDiagonalClass",
    "getDiagonalEntries",
    
    --getHilbertSymbols.m2
    "getHilbertSymbolReal",
    "getHilbertSymbol",
    
    --GWInvariants.m2
    "getRank",
    "getSignature",
    "getIntegralDiscriminant",
    "getRelevantPrimes",
    "getHasseWittInvariant",

    --LocalGlobalDegrees.m2
    "getGlobalA1Degree",
    "getLocalA1Degree",
    
    --IsomorphismOfForms.m2
    "isIsomorphicForm",
    
    --Isotropy.m2
    "isAnisotropic",
    "isIsotropic",

    --AnisotropicDimension.m2
    "getAnisotropicDimensionQQp",
    "getAnisotropicDimension",
    "getWittIndex",
    
    --Decomposition.m2
    "getAnisotropicPart",
    "getSumDecomposition",
    "getSumDecompositionString"
    
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

-- Decomposing Grothendieck-Witt classes and symmetric bilinear forms
load "./A1BrouwerDegrees/Code/Decomposition.m2"

----------------------------
----------------------------
-- DOCUMENTATION
----------------------------
----------------------------

beginDocumentation()

document{
    Key => A1BrouwerDegrees,
    Headline => "package for working with A1-Brouwer degree computations",
    PARA{"This package is intended to allow the computation and manipulation of ", TO2(getLocalA1Degree,"local"), 
	" and ", TO2(getGlobalA1Degree,"global"), " ", TEX///$\mathbb{A}^1$///, EM "-Brouwer degrees.",
	" Global Brouwer degrees are non-degenerate symmetric bilinear forms valued in the Grothendieck-Witt ring of a field ", TEX///$\text{GW}(k)$///, "."},
    PARA{"In order to simplify the forms produced, this package produces invariants of symmetric bilinear forms, including their ", 
	TO2(getWittIndex,"Witt indices"), ", their ", 
	TO2(getIntegralDiscriminant,"discriminants"), ", and their ", 
	TO2(getHasseWittInvariant, "Hasse-Witt invariants"), ". Quadratic forms can be ", 
	TO2(getSumDecomposition,"decomposed"), " into their isotropic and ", TO2(getAnisotropicPart,"anisotropic parts"), 
	". Finally, and perhaps most crucially, we can certify whether two symmetric bilinear forms are ", TO2(isIsomorphicForm,"isomorphic") , " in the Grothendieck-Witt ring."},
PARA{"Below is an example using the methods provided by this package to compute the ", TO2(getLocalA1Degree,"local"),
        " and ", TO2(getGlobalA1Degree,"global"), " ", TEX///$\mathbb{A}^1$///,  "-Brouwer degrees for an endomorphism ",
	TEX///$\mathbb{A}_{\mathbb{Q}}^1\rightarrow \mathbb{A}_{\mathbb{Q}}^1.$///, " defined by ",TEX///$$ f(x)=(x^2+x+1)(x-3)(x+2).$$///,
	" We first compute the global degree."},
    EXAMPLE {
    "R = QQ[x]",
    "f = {x^4 - 6*x^2 - 7*x - 6}",
    "alpha = getGlobalA1Degree f",
    "beta = getSumDecomposition alpha",
    },
    PARA{"We can also compute the local degrees at the respective ideals."},   
    EXAMPLE {
    "I1 = ideal(x^2 + x + 1);",
    "alpha1 = getLocalA1Degree(f, I1)",
    "I2 = ideal(x - 3)",
    "alpha2 = getLocalA1Degree(f, I2)",
    "I3 = ideal(x + 2);",
    "alpha3 = getLocalA1Degree(f, I3)", 
    },

    PARA{"We can then use the ", TO isIsomorphicForm, "  method to verify that the ", 
    TO2(getLocalA1Degree, "local"), " degrees sum to the ", TO2(getGlobalA1Degree, "global"), 
    " degree."},
    EXAMPLE{
	"alpha' = addGW(alpha1, addGW(alpha2, alpha3))",
	"isIsomorphicForm(alpha,alpha')",
	"beta' = getSumDecomposition alpha'",
	},
    },
undocumented{
    }

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
assert(class(makeGWClass M1) === GrothendieckWittClass);
assert(class(makeGWClass M2) === GrothendieckWittClass);
assert(class(makeGWClass M3) === GrothendieckWittClass);
///

-- Test for getBaseField
-- Test 19
TEST ///
M1 = makeGWClass matrix(QQ, {{1/1,0,0},{0,2,3},{0,3,1}});
M2 = makeGWClass matrix(RR, {{1.0,24/10,-2.41},{24/10,-5,0},{-2.41,0,69}});
M3 = makeGWClass matrix(CC, {{1*ii,24/10,-2.41},{24/10,-5,0},{-2.41,0,69+ii}});
M4 = makeGWClass matrix(GF(7), {{1,0,0},{0,2,0},{0,0,-3}});

assert(getBaseField(M1) === QQ);
assert(getBaseField(M2) === RR_53);
assert(getBaseField(M3) === CC_53);
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
