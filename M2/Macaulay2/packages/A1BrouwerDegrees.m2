--A1BrouwerDegrees.m2
newPackage(
    "A1BrouwerDegrees",
    Version=>"1.0",
    Date=>"June 5, 2023",
    Authors=>{
        {Name=>"Nikita Borisov",
	 Email=>"nborisov@sas.upenn.edu",
	 HomePage=>"https://www.math.upenn.edu/people/nikita-borisov"},
        {Name=>"Thomas Brazelton",
	 Email=>"brazelton@math.harvard.edu",
	 HomePage=>"https://tbrazel.github.io/"},
        {Name=>"Frenly Espino",
	 Email=>"frenly@sas.upenn.edu",
	 HomePage=>"https://www.math.upenn.edu/people/frenly-espino"},
         {Name=>"Tom Hagedorn",
	 Email=>"hagedorn@tcnj.edu",
	 HomePage=>"https://hagedorn.pages.tcnj.edu/"},
        {Name=>"Zhaobo Han",
	 Email=>"zbtomhan@sas.upenn.edu",
	 HomePage=>"https://www.linkedin.com/in/zhaobo-han-77b1301a2/"},
     	{Name=>"Jordy Lopez Garcia",
	 Email=>"jordy.lopez@tamu.edu",
	 HomePage=>"https://jordylopez27.github.io/"},
        {Name=>"Joel Louwsma",
	 Email=>"jlouwsma@niagara.edu",
	 HomePage=>"https://www.joellouwsma.com/"},
        {Name=>"Andrew Tawfeek",
	 Email=>"atawfeek@uw.edu",
	 HomePage=>"https://www.atawfeek.com/"},
        {Name=>"Wern Juin Gabriel Ong",
	 Email=>"gong@bowdoin.edu",
	 HomePage=>"https://wgabrielong.github.io/"}
	},
    Headline=>"for working with A1-Brouwer degree computations",
    PackageImports=>{
	"Parametrization",
	"RealRoots",
	"RationalPoints2"
	},
    PackageExports=>{},
    AuxiliaryFiles=>true,
    DebuggingMode=>true,
    Keywords => {"Commutative Algebra"}
    )


export{
    
    -- ArithmeticMethods.m2
    "localAlgebraBasis",
    "padicValuation",

    --MatrixMethods.m2
    "congruenceDiagonalize",
    
    --GrothendieckWittClasses.m2    
    "GrothendieckWittClass",
    "baseField",
    "gwClass",
    "gwAdd",
    "gwMultiply",
    
    --BuildingForms.m2
    "diagonalForm",
    "hyperbolicForm",
    "PfisterForm",
    
    --SimplifiedRepresentatives.m2
    "diagonalClass",
    "diagonalEntries",
    
    --HilbertSymbols.m2
    "HilbertSymbol",
    "HilbertSymbolReal",
    
    --GWInvariants.m2
    "signature",
    "integralDiscriminant",
    "relevantPrimes",
    "HasseWittInvariant",

    --LocalGlobalDegrees.m2
    "globalA1Degree",
    "localA1Degree",
    
    --IsomorphismOfForms.m2
    "gwIsomorphic",
    
    --Isotropy.m2
    "isIsotropic",
    "isAnisotropic",

    --AnisotropicDimension.m2
    "anisotropicDimensionQp",
    "anisotropicDimension",
    "WittIndex",
    
    --Decomposition.m2
    "anisotropicPart",
    "sumDecomposition",
    "sumDecompositionString"
    
    }

-- Basic arithmetic, p-adic, and commutative algebra operations we will use
load "./A1BrouwerDegrees/Code/ArithmeticMethods.m2"

-- Basic manipulations of matrices we will use
load "./A1BrouwerDegrees/Code/MatrixMethods.m2"

-- Establishing the GrothendieckWittClass type and some basic manipulations
load "./A1BrouwerDegrees/Code/GrothendieckWittClasses.m2"

-- For building new symmetric bilinear forms
load "./A1BrouwerDegrees/Code/BuildingForms.m2"

-- For providing simplified representatives of symmetric bilinear forms
load "./A1BrouwerDegrees/Code/SimplifiedRepresentatives.m2"

-- For Hilbert symbols over p-adic numbers
load "./A1BrouwerDegrees/Code/HilbertSymbols.m2"

-- Invariants of symmetric bilinear forms
load "./A1BrouwerDegrees/Code/GWInvariants.m2"
    
-- Local and global A1-brouwer degrees
load "./A1BrouwerDegrees/Code/LocalGlobalDegrees.m2"

-- Checking if forms are isomorphic
load "./A1BrouwerDegrees/Code/IsomorphismOfForms.m2"

-- For verifying (an)isotropy
load "./A1BrouwerDegrees/Code/Isotropy.m2"

-- Anisotropic dimension
load "./A1BrouwerDegrees/Code/AnisotropicDimension.m2"

-- Finally, decomposing forms
load "./A1BrouwerDegrees/Code/Decomposition.m2"

----------------------------
----------------------------
-- DOCUMENTATION
----------------------------
----------------------------

beginDocumentation()

document{
    Key => A1BrouwerDegrees,
    Headline => "for working with A1-Brouwer degree computations",
    PARA{"This package is intended computing and manipulating ", TO2(localA1Degree,"local"), " and ", TO2(globalA1Degree,"global"), " ", TEX///$\mathbb{A}^1$///, EM "-Brouwer degrees."," Global Brouwer degrees are non-degenerate symmetric bilinear forms valued in the Grothendieck-Witt ring of a field ", TEX///$\text{GW}(k)$///, "."},
    PARA{"In order to simplify the forms produced, this package produces invariants of symmetric bilinear forms, including their ", TO2(WittIndex,"Witt indices"), ", their ", TO2(integralDiscriminant,"discriminants"), ", and their ", TO2(HasseWittInvariant, "Hasse Witt invariants"), ". Quadratic forms can furthermore be ", TO2(sumDecomposition,"decomposed"), " into their isotropic and ", TO2(anisotropicPart,"anisotropic parts"), ". Finally, and perhaps most crucially, we can certify whether two symmetric bilinear forms are ", TO2(gwIsomorphic,"isomorphic") , " in the Grothendieck-Witt ring."},
    }

undocumented {
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


-- Diagonal form testing
-- Test 0
TEST ///
print("diagonal form testing");
M1=matrix(RR, {{0, 1}, {1, 0}});
G1=gwClass(M1);
M2=diagonalClass(G1);
assert(M2.matrix===matrix(RR, {{1, 0}, {0, -1}}));
///

-- Test 1
TEST ///
M3=matrix(CC, {{1, 2, 3}, {2, 4, 5}, {3, 5, 7}});
G2=gwClass(M3);
M4=diagonalClass(G2);
assert(M4.matrix===matrix(CC, {{1, 0, 0}, {0, 1, 0}, {0, 0, 1}}));
///

--Test 2
TEST ///
M3=matrix(QQ, {{1, 2, 3}, {2, 4, 5}, {3, 5, 7}});
G2=gwClass(M3);
M4=diagonalClass(G2);
assert(M4.matrix===matrix(QQ,{{1, 0, 0}, {0, -2, 0}, {0, 0, 2}}));
///

-- gwTypeTest.m2
-- Test 3
TEST ///
M = matrix(QQ,{{1,0},{0,1}});
N = matrix(QQ, {{1, 2}, {2, 5}});
beta = gwClass(M);
gamma = gwClass(N);
assert(baseField(beta) === QQ)
assert(beta.matrix === M)
--Operations within GW-classes
A = gwAdd(beta, gamma);
B = gwMultiply(beta, gamma);
assert(A.matrix === matrix(QQ, {{1, 0, 0, 0}, {0, 1, 0, 0}, {0, 0, 1, 2}, {0, 0, 2, 5}}));
assert(B.matrix === matrix(QQ, {{1, 2, 0, 0}, {2, 5, 0, 0}, {0, 0, 1, 2}, {0, 0, 2, 5}}));
///


-- Testing for global and local A1 degrees
-- Test 4
TEST ///
T1 = QQ[x]
f = {x^2}
beta = globalA1Degree(f)
gamma = gwClass(matrix(QQ,{{0,1},{1,0}}))
assert(gwIsomorphic(beta,gamma))
///

-- Test 5
TEST ///
T1 = QQ[z_1..z_2];
f1 = {(z_1-1)*z_1*z_2, (3/5)*z_1^2 - (17/3)*z_2^2};
f1GD = globalA1Degree(f1);
assert(WittIndex(f1GD) == 3);
q=ideal {z_1,z_2};
r=ideal {z_1-1,z_2^2-(9/85)};
f1LDq= localA1Degree(f1,q);
f1LDr= localA1Degree(f1,r);
f1LDsum = gwAdd(f1LDq, f1LDr);
assert(gwIsomorphic(f1LDsum, f1GD));
///

-- Test 6
TEST ///
T2 = GF(17)[w];
f2 = {w^4 + w^3 - w^2 - w};
f2GD= globalA1Degree(f2);
assert(WittIndex(f2GD) == 2);
p=ideal {w+1};
f2LDp = localA1Degree(f2, p);
assert(WittIndex(f2LDp) == 1);
s=ideal{w-1};
f2LDs = localA1Degree(f2, s);
t=ideal{w};
f2LDt = localA1Degree(f2, t);
f2LDsum = gwAdd(gwAdd(f2LDp, f2LDs),f2LDt);
assert(gwIsomorphic(f2LDsum, f2GD));
///

-- Testing for building forms
-- Test 7
TEST ///
twoH = hyperbolicForm(GF(17),4);
P = PfisterForm(GF(17),(2,3));
assert(gwIsomorphic(P,twoH));
///

-- Test 8
TEST ///
H = hyperbolicForm(RR);
A = diagonalForm(RR,(1,-1));
B = gwClass(matrix(RR,{{0,1},{1,0}}));
assert(gwIsomorphic(H,A));
assert(gwIsomorphic(H,B));
///

-- Test for local algebra basis
-- Test 9
TEST ///
QQ[x,y]
f = {x^2+1-y,y};
p = ideal(x^2+1,y);
assert(localAlgebraBasis(f,p) == {1,x}); 
///

-- Tests for diagonalClass and diagonalEntries
-- Test 10
TEST ///
M1 = matrix(CC,{{1,0,0},{0,2,0},{0,0,-3}});
M2 = matrix(CC,{{1,0,0},{0,1,0},{0,0,1}});
G = gwClass(M1);
assert((diagonalClass(G)).matrix == M2);
assert(diagonalEntries(G) == {1,2,-3});

///

-- Test 11
TEST ///
M1 = matrix(RR,{{1,0,0},{0,2,0},{0,0,-3}});
M2 = matrix(RR,{{1,0,0},{0,1,0},{0,0,-1}});
G = gwClass(M1);
assert((diagonalClass(G)).matrix == M2);
assert(diagonalEntries(G) == {1,2,-3});
///

-- Test 12
TEST ///
M = matrix(QQ,{{1,0,0},{0,2,0},{0,0,-3}});
G = gwClass(M);
assert((diagonalClass(G)).matrix == M);
assert(diagonalEntries(G) == {1,2,-3})
///
    
-- Test 13
TEST ///
M = matrix(GF(5),{{1,0,0},{0,2,0},{0,0,-3}});
G = gwClass(M);
assert((diagonalClass(G)).matrix == M);
assert(diagonalEntries(G) == {1,2,-3});
///

-- Test 14
TEST ///
kk = GF(7);
M1 = matrix(kk,{{1,0,0},{0,2,0},{0,0,-3}});
M2 = matrix(kk,{{1,0,0},{0,1,0},{0,0,1}});
G = gwClass(M1);
assert((diagonalClass(G)).matrix == M2);
assert(diagonalEntries(G) == {1,2,-3});
///

-- Test 15
TEST ///
M1 = matrix(QQ,{{18,0,0},{0,125/9,0},{0,0,-8/75}});
M2 = matrix(QQ,{{2,0,0},{0,5,0},{0,0,-6}});
G1 = gwClass(M1);
assert((diagonalClass(G1)).matrix == M2);
///


-- Test for p-adic valuation
-- Test 16
TEST ///
assert(padicValuation(27,3) == 3);
///

-- Test for congruenceDiagonalize
-- Test 17
TEST ///
B=matrix(QQ,{{0/1,1},{1,0}});
eta = gwClass(B)
assert(WittIndex(eta) == 1);

P=matrix(QQ,{{0/1, 5,1},{2,2,1},{0,0,1}});
A=matrix(QQ,{{1/1,0,0},{0,-1,0},{0,0,1}});
assert(WittIndex(gwClass(congruenceDiagonalize(P*A*transpose(P)))) == 1);
///


-- Test for gwClass
-- Test 18
TEST ///
M1 = matrix(QQ,{{1/1,0,0},{0,1,0},{0,0,1}});
M2 = matrix(QQ,{{1/1,24/10,0},{24/10,-5,0},{0,0,69}});
M3 = matrix(GF(7),{{1,0,0},{0,2,0},{0,0,-3}});
assert(class(gwClass(M1)) === GrothendieckWittClass);
assert(class(gwClass(M2)) === GrothendieckWittClass);
assert(class(gwClass(M3)) === GrothendieckWittClass);
///

-- Test for baseField
-- Test 19
TEST ///
M = gwClass(matrix(QQ,{{1/1,0,0},{0,2,3},{0,3,1}}));
M1 = gwClass(matrix(RR,{{1.0,24/10,-2.41},{24/10,-5,0},{-2.41,0,69}}));
M2 = gwClass(matrix(CC,{{1*ii,24/10,-2.41},{24/10,-5,0},{-2.41,0,69+ii}}));
M3 = gwClass(matrix(GF(7),{{1,0,0},{0,2,0},{0,0,-3}}));

assert(baseField(M) === QQ);
assert(baseField(M1) === RR_53);
assert(baseField(M2) === CC_53);
assert(toString(baseField(M3)) === toString(GF(7)));
///

-- Test for gwAdd
-- Test 20
TEST ///
M1 = gwClass(matrix(QQ, {{1/1,0,-3},{0,23,0},{-3,0,-2/5}}));
M2 = gwClass(matrix(QQ, {{0,1/2,0},{1/2,5/9,0},{0,0,1}}));
M3 = gwClass(matrix(QQ, {{1/1,0,-3,0,0,0},{0,23,0,0,0,0},{-3,0,-2/5,0,0,0},{0,0,0,0,1/2,0},{0,0,0,1/2,5/9,0},{0,0,0,0,0,1}}))

G1 = gwClass(matrix(RR, {{sqrt(2),0,-3},{0,sqrt(5),0},{-3,0,-1/5}}));
G2 = gwClass(matrix(RR, {{1/3}}));
G3 = gwClass(matrix(RR, {{sqrt(2),0,-3,0},{0,sqrt(5),0,0},{-3,0,-1/5,0},{0,0,0,1/3}}));

H1 = gwClass(matrix(CC, {{2*ii,0,0},{0,-2,0},{0,0,-3}}));
H2 = gwClass(matrix(CC, {{1,0,-3+ii,0},{0,-2,0,0},{-3+ii,0,-3,0},{0,0,0,5}}));
H3 = gwClass(matrix(CC, {{2*ii,0,0,0,0,0,0},{0,-2,0,0,0,0,0},{0,0,-3,0,0,0,0},{0,0,0,1,0,-3+ii,0},{0,0,0,0,-2,0,0},{0,0,0,-3+ii,0,-3,0},{0,0,0,0,0,0,5}}));

assert(gwAdd(M1,M2) === M3);
assert(gwAdd(G1,G2) === G3);
assert(gwAdd(H1,H2) === H3);
///

-- Test for isIsotropic/isAnisotropic
-- Test 21
TEST ///
A1=matrix(QQ, {{0, 1/1},{1/1, 0}});
assert(isIsotropic(A1)===true);
assert(isAnisotropic(gwClass(A1))===false);

A2=matrix(RR, {{1, -2, 4}, {-2, 2, 0}, {4, 0, -7}});
assert(isAnisotropic(A2)===false);
assert(isIsotropic(gwClass(A2))===true);

K=GF(13^4);
A3=matrix(K, {{7, 81, 63}, {81, 7, 55}, {63, 55, 109}});
assert(isIsotropic(gwClass(A3))===true);
--Isotropic by the Chevalley-Warning Theorem.

A4=matrix(QQ, {{5, 0}, {0, 5}});
assert(isAnisotropic(A4)===true);

A5=matrix(CC, {{3+ii, 0}, {0, 5-ii}});
assert(isAnisotropic(A5)===false);
///

--Test for isIsomorphicFormQ
-- Test 22
TEST ///
B1=matrix(QQ, {{1/1, -2/1, 4/1}, {-2/1, 2/1, 0}, {4/1, 0, -7/1}});
B2=matrix(QQ, {{-17198/4225, -166126/975, -71771/1560}, {-166126/975, -27758641/4050, -251077/135}, {-71771/1560, -251077/135, -290407/576}});
assert(gwIsomorphic(gwClass(B1), gwClass(B2))===true);
B3=matrix(QQ, {{-38/1, -50/1, 23/1}, {-50/1, -62/1, 41/1}, {23/1, 41/1, 29/1}});
assert(gwIsomorphic(gwClass(B1), gwClass(B3))===true);
///


--Test for gwIsomorphic
--Test 23

TEST ///
D1=matrix(QQ, {{1/1, -2/1, 4/1}, {-2/1, 2/1, 0}, {4/1, 0, -7/1}});
D2=matrix(QQ, {{-38/1, -50/1, 23/1}, {-50/1, -62/1, 41/1}, {23/1, 41/1, 29/1}});
assert(gwIsomorphic(gwClass(D1), gwClass(D2))===true);

C1=matrix(RR, {{1/1, -2/1, 4/1}, {-2/1, 2/1, 0}, {4/1, 0, -7/1}});
C2=matrix(RR, {{-38/1, -50/1, 23/1}, {-50/1, -62/1, 41/1}, {23/1, 41/1, 29/1}});
assert(gwIsomorphic(gwClass(C1), gwClass(C2))===true);

M=GF(13^1)
C3=matrix(M, {{1, 11, 4}, {11, 2, 0}, {4, 0, 6}});
C4=matrix(M, {{1, 2, 10}, {2, 3, 2}, {10, 2, 3}});
assert(gwIsomorphic(gwClass(C3), gwClass(C4))===true);
///

-- Test for GWinvariants
-- Test 24
TEST ///
M1 = gwClass(matrix(QQ, {{1/1,0,-3},{0,23,0},{-3,0,-2/5}}));
M2 = gwClass(matrix(QQ, {{1/1,0,0},{0, 23,0},{0,0,-2/5}}));
M3 = gwClass(matrix(QQ, {{1/1,0,0},{0,-23,0},{0,0,-2/5}}));
M4 = gwClass(matrix(QQ, {{-1/1,0,0},{0,-23,0},{0,0,-2/5}}));

assert(signature(M1) == 1);
assert(signature(M2) == 1);
assert(signature(M3) == -1);
assert(signature(M4) == -3);

assert(integralDiscriminant(M1)==-5405);
assert(relevantPrimes(M1) == {23, 5, 47} );
assert(HasseWittInvariant(M1, 5) == -1);
assert(HasseWittInvariant(M1, 23) == 1);
assert(HasseWittInvariant(M1, 47) == -1);
///

-- Test for hilbertSymbols
-- Test 25
TEST ///
a = HilbertSymbol(100, 7, 3);
assert(a==1);

b = HilbertSymbol(100/121, 7/169, 3);
assert(b==1);

assert(HilbertSymbol(5, 1/9, 7)==1);
assert(HilbertSymbol(1/9, 5, 7)==1);

assert(HilbertSymbol(3, 11, 3)==-1);
assert(HilbertSymbol(3, 11, 2)==-1);
assert(HilbertSymbol(-3, -11, 2)==1);
assert(HilbertSymbol(-5, 11, 2) == -1);


assert(HilbertSymbolReal(-3/1, 5)==1);
assert(HilbertSymbolReal(-3, -5/1)==-1);
assert(HilbertSymbolReal(-3/1, -5)==-1);
assert(HilbertSymbolReal(3, 5)==1);
///


