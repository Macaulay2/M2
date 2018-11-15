newPackage(
        "CohomCalg",
        Version => "0.1", 
        Date => "",
        Authors => {{Name => "Mike Stillman", 
                  Email => "", 
                  HomePage => ""}},
        Headline => "interface to CohomCalg software for computing cohomology of torus invariant divisors on a toric variety",
        PackageExports => {"NormalToricVarieties"},
        Configuration => {
            "executable" => "",
            "silent" => "false"
            }
        )

export {
    "cohomCalg",
    "cohomCalgVector",
    "Silent"
    }

exportMutable {
    "cohomCalgExecutable"
    }

cohomCalgExecutable = CohomCalg#Options#Configuration#"executable"
if cohomCalgExecutable == "" then cohomCalgExecutable = prefixDirectory | currentLayout#"programs" | "cohomcalg"
silent = (
    if CohomCalg#Options#Configuration#"silent" == "false" then false 
    else if CohomCalg#Options#Configuration#"silent" == "true"  then true 
    else error "expected 'silent' configuration value to be \"true\" or \"false\""
    )

-- The following two defs are used only to help parse output from cohomcalg.
Symbol * Symbol := (a,b) -> Product{a,b}
ZZ * Symbol := (a,b) -> Product{a,b}

-- The following is a local function, which something similar should be in NormalToricVarieties
degree ToricDivisor := List => (D) -> (
    X := variety D; 
    entries((fromWDivToCl X) * (vector D))
    )

---------------------------------------------------------------------
-- Routines for translating to form of input expected by cohomcalg --
---------------------------------------------------------------------
displayAsSequence = (L) -> if #L > 1 then toString toSequence L else ("("|toString L#0|")");

toCohomCalg = method()
toCohomCalg NormalToricVariety := (X) -> (
    SR := dual monomialIdeal X;
    GLSM := entries transpose fromWDivToCl X;
    vertices := for i from 0 to #rays X - 1 list (
        -- the string for each variable
        "vertex x"|i|" | GLSM:"|displayAsSequence GLSM_i|";"
        );
    str1 := concatenate between("\n", vertices);
    stanleyreisner := for f in SR_* list (
        concatenate between("*", (support f)/(v -> "x"|toString index v))
        );
    str2 := "\nsrideal [" | concatenate between(",", stanleyreisner) | "];\n";
    str1 | str2
    )
toCohomCalg List := (L) -> (
    requests := for p in L list (
        "ambientcohom O"|(displayAsSequence p)|";\n"
        );
    (concatenate requests) | "monomialfile off;\n"
    )

-- The actual call to cohomcalg
-- It is required that the user has placed the
-- executable at cohomCalgExecutable, which is usually set to "cohomcalg"
-- Also, cohomcalg has a limit of 1024 computations at a time
cohomCalg0 = (X,pneeded,issilent) -> (
    -- X: NormalToricVariety
    -- pneeded: list of multi-degrees, of size <= 1024
    -- issilent: Boolean, whether to quiet the output of cohomcalg.
    H := X.cache.CohomCalg;
    filename := temporaryFileName();
    filename << (toCohomCalg X) | (toCohomCalg pneeded) << close;
    executable := "!"|cohomCalgExecutable|" --integrated " | filename;
    if issilent then executable = executable | " 2>/dev/null";
    if debugLevel >= 1 then << "-- CohomCalg: using temporary file " << filename << endl;
    if debugLevel >= 2 then << "-- CohomCalg: going to call: " << executable << endl;
    valstr := get executable;
    if debugLevel >= 5 then << "-- CohomCalg: output = " << net valstr << endl;
    --val := replace("\\*", "**", valstr);
    val := replace("False", "false", valstr);
    val = replace("True", "true", val);
    result := value val;        
    if not instance(result, List) or not instance(first result, Boolean) then 
        error("result of cohomCalg not in expected form: received: "|valstr);
    if not first result then
        error("cohomCalg returned false, meaning it could not complete the computation");
    -- remove the 'true': start at i==1.
    for i from 1 to #result-1 do H#(pneeded#(i-1)) = result#i;
    H
    )
cohomCalg = method(Options=>{Silent=>null})
cohomCalg(NormalToricVariety, List) := opts -> (X, p) -> (
    issilent := if opts.Silent === null then silent else opts.Silent;
    if not X.cache.?CohomCalg then X.cache.CohomCalg = new MutableHashTable;
    H := X.cache.CohomCalg;
    p = for a in p list (
        if instance(a,ToricDivisor) 
        then degree a
        else if instance(a,List) then a
        else error "expected lists of degrees and/or toric divisors"
        );
    pneeded := toList(set p - set keys X.cache.CohomCalg); 
    if #pneeded == 0 then return H;
    p1 := pack(pneeded, 1024);
    for p0 in p1 do cohomCalg0(X,p0,issilent);
    X.cache.CohomCalg
    )

cohomCalgVector = method(Options=>options cohomCalg)
cohomCalgVector(NormalToricVariety, List) := opts -> (X, p) -> (
    H := cohomCalg(X, {p}, opts);
    H#p#0
    )
cohomCalgVector ToricDivisor := opts -> (D) -> (
    X := variety D;
    deg := degree D;
    cohomCalgVector(X,deg,opts)
    )

beginDocumentation()

TEST ///
  needsPackage "CohomCalg"
  X = smoothFanoToricVariety(3,2)
  ans = for i from 0 to # rays X - 1 list cohomCalgVector X_i
  assert(ans === {{3, 0, 0, 0}, {3, 0, 0, 0}, {3, 0, 0, 0}, {4, 0, 0, 0}, {1, 0, 0, 0}})
  
  degrees ring X
  ans1 = {0,0,1,0}
  D = X_3-3*X_0  
  assert(degree D == {1,-3})
  assert(cohomCalgVector(X, {1,-3}) == ans1)
  assert(cohomCalgVector D == ans1)
  assert(ans1 == for i from 0 to dim X list rank HH^i(X, OO D))

  assert(cohomCalgVector(D-D) == {1,0,0,0})
///

TEST ///
  needsPackage "CohomCalg"
  X = smoothFanoToricVariety(3,2)
  Ds = for i from 0 to # rays X - 1 list X_i
  allDs = (drop(subsets Ds,1))/sum/(d -> -d);
  H = cohomCalg(X,allDs)
  peek H
  peek cohomCalg(X, {{0,-6}})
///

doc ///
Key
  CohomCalg
Headline
  an interface to CohomCalg software for computing cohomology of torus invariant divisors on a toric variety
Description
  Text
     This package is experimental.
///


end--



doc ///
Key
Headline
Usage
Inputs
Outputs
Consequences
Description
  Text
  Example
  Code
  Pre
Caveat
SeeAlso
///

TEST ///
-- test code and assertions here
-- may have as many TEST sections as needed
///

end
restart
needsPackage "CohomCalg"
X = hirzebruchSurface 6
transpose syz transpose matrix rays X
fromWDivToCl X
GLSM = fromWDivToCl X
X1 = normalToricVarietyFromGLSM(GLSM, {{0, 1}, {0, 3}, {1, 2}, {2, 3}})
rays X1
rays X
max X1
rays X
for i from 0 to 3 list for j from 0 to dim X list rank HH^j(X, OO(X_i))
cohomCalg(X, {{1, 3}, {1, 0}, {0, 1}, {-2,4}})
cohomCalgVector(X, {1,3})
cohomCalgVector(X, {1,-3})
cohomCalgVector (X_0+X_1-3*X_2)

GLSM = transpose matrix {{1, 3}, {0, 1}, {0, 1}, {0, 1}, {0, 1}, {1, 0}, {0, 1}}
R = QQ[x_0..x_6]
P111113 = normalToricVarietyFromGLSM(GLSM, monomialIdeal(x_0*x_5, x_1*x_2*x_3*x_4*x_6))
isWellDefined P111113
isProjective P111113
dim P111113

Y = P111113
X = completeIntersection(P111113, {2*Y_0, 3*Y_1})
dim X
cohomologyVector(Y, degree(2*Y_0, 3*Y_1))
basicCohomologies X
hodgeDiamond X
rays P111113
X = P111113
fromWDivToCl X
use ring X
max X
degrees oo
for i from 0 to 6 list for j from 0 to dim X list rank HH^j(X, OO(X_i))
HH^1(X, OO(X_1))
HH^0(X, OO_X(2,8))
R = ring X
dual monomialIdeal apply(I_*, f -> (L := f//support/index; product(7, i -> if member(i,L) then 1_R else R_i)))

X = (example0912'3524())()
dim X
rays X
isSmooth X
isProjective X
isSimplicial X
time for i from 0 to dim X list rank HH^i(X, OO(X_0))
time for i from 0 to dim X list rank HH^i(X, OO(X_1))
matrix time for i from 0 to # rays X - 1 list for j from 0 to dim X list rank HH^j(X, OO(X_i))
R = ring X
use R
H1 = (0,0,2,1,1)
H2 = (6,6,6,6,0)
OO_X(H1)
time for i from 0 to dim X list rank HH^i(X, OO_X(H1))
time for i from 0 to dim X list rank HH^i(X, OO_X(H2))
time for i from 0 to dim X list rank HH^i(X, OO_X(H1) ** OO_X(H2)) -- takes a while

restart
needsPackage "CohomCalg"
Y = (example0912'3524())()
toCohomCalg Y
toCohomCalg {{0,0,0,0,0},{6,6,6,6,0}}
peek cohomCalg(Y, {{0,0,0,0,0},{6,6,6,6,0}})


X = (exampleP11222'8())()
rays X
H = X_0
L = X_5
D = -toricDivisor X -- 8*H + 4*L
OO(D)
max X
dim X
assert isSmooth X
for i from 0 to 4 list HH^i(X, OO(-toricDivisor X))
for i from 0 to 4 list HH^i(X, OO(D))
for i from 0 to 4 list HH^i(X, OO_X(-4,7))
cohomCalg(X, {{8,4},{-7,4}})
cohomCalg(X, {{8,-6},{-7,4}})

restart
needsPackage "CohomCalg"
kk = QQ[a_0..a_4]        
M = transpose matrix{{0,0,2,180,0},{0,420,0,0,0},{a_0,a_1,a_2,a_3,a_4}}
cohomologyFromLES M
M = transpose matrix{{0,0,2,180,0},{0,420,2,0,0},{a_0,a_1,a_2,a_3,a_4}}
cohomologyFromLES M
removeUnusedVariables {oo}

-- example: get cohomology of X \subset Y, X CY
Y = (example0912'3524())()
OO toricDivisor Y
H = cohomCalg(Y, {{0,0,0,0,0},{-6,-6,-8,-7,-1}})
kk = QQ[a_0..a_6]        
M = transpose matrix{
    {matrix {first H#{-6,-6,-8,-7,-1}}},
    {matrix {first H#{0,0,0,0,0}}},
    {matrix {{a_0,a_1,a_2,a_3,a_4,a_5,a_6}}}
    }
cohomologyFromLES M

-- XXXXXXX
restart
debug needsPackage "CohomCalg"
Y = (exampleP111122'44())()
dim Y
H = {4*Y_0, 4*Y_0}
D = -2*Y_0
cohomologyCIOne(Y, {4*Y_0, 4*Y_0}, -2*Y_0)
cohomologyCI(Y, {4*Y_0, 4*Y_0}, {-2*Y_0, -3*Y_0}/degree)
cohomologyCI(Y, {4*Y_0, 4*Y_0}, {-15*Y_0}/degree)
A = makeCohomVectorRing(Y, {4*Y_0, 4*Y_0}, 4)
debugLevel = 1
cohomologyCI(Y, {4*Y_0, 4*Y_0}, -2*Y_0, a_0)
debugLevel = 1
cohomologyCI(Y, {4*Y_0, 4*Y_0}, -12*Y_0, a_0)
cohomologyCI(Y, {4*Y_0, 4*Y_0}, -11*Y_0, a_0)
cohomologyCI(Y, {4*Y_0, 4*Y_0}, -10*Y_0, a_0)
cohomologyCI(Y, {4*Y_0, 4*Y_0}, 10*Y_0, a_0)

cohomologyCI(Y, {4*Y_0}, -10*Y_0, a_0)
cohomologyCI(Y, {4*Y_0}, -10*Y_0-4*Y_0, a_0)

cohomologyCI(Y, {4*Y_0, 4*Y_0}, 12*Y_0, a_0)
cohomologyCI(Y, {4*Y_0, 4*Y_0}, -15*Y_0, a_0)
cohomologyCI(Y, {4*Y_0, 4*Y_0}, 15*Y_0, a_0)

cohomCalg(Y, {{-4},{-8},{-12},{-16},{-20}})

Ds := for i from 0 to #rays Y - 1 list (-Y_i)
(prepend({0}, Ds/degree))
linebundles = collectLineBundles(Y, H, prepend({0}, Ds)) -- prepares (uses CI info)
cohomCalg(Y, linebundles)
transpose matrix for d in linebundles list cohomologyVector(Y,d) -- this is the workhorse.
-- CI cohomologies:
--  a. compute needed linebundles from above
--  b0. create new ring
--  b1. for each needed OO_X(D), create a list of L_i's
--  b2. get a set of ses's
--  b3. splitSES: from a list of ses's, get ideal, reduce all matrices, return ideal and the matrices.
--  b4. grab the answers one wants, return that
cohomologyOmega1(Y,{4*Y_0, 4*Y_0})
hodgeDiamondCI(Y, {4*Y_0, 4*Y_0})

cohomologyCI(Y,{4*Y_0, 4*Y_0}, {-0*Y_0}/degree)

X = completeIntersection(Y, {4*Y_0, 4*Y_0})
dim X
ambient X
basicCohomologies X
peek oo
--nextLineBundle X
debugLevel = 1
cohomologyVector(X, degree(-12*Y_0))
peek X.cache.cohom
cohomologyOmega1 X
cohomologyVector(X, degree(-12*Y_0))
hodgeDiamond X
peek X.cache

X = completeIntersection(Y, {})
dim X
ambient X
basicCohomologies X
hodgeDiamond X

-- XXXXXXX
restart
debug needsPackage "CohomCalg"
Y = (exampleP111122'44())()
B = base(a,b,c)
aY = abstractVariety(Y, B)
X = completeIntersection(Y, {4*Y_0, 4*Y_0})
aX = abstractVariety(X,B)
intersectionRing aX
F = OO (Y_0) ++ OO (Y_3)
dim aY
A = intersectionRing aY
describe A
chi(OO_aY(b*t_5))
isSmooth Y
liftPicToCDiv Y
fromCDivToPic Y
fromWDivToCl Y
liftClToWDiv Y
use A
OO t_1 === OO t_2

B = base(a,b,c)

abstractSheaf(Y,B,F)
oo_0 ++ oo_1
chern oo

-- example: 
restart
debug needsPackage "CohomCalg"
Y = (exampleP11222'8())()
isSmooth Y
dim Y
pic Y
B = base(a,b)
aY = abstractVariety(Y,B)
A = intersectionRing aY
cohomologyVector(Y, Y_2)
F = abstractSheaf(Y, B, OO Y_2)
chi F

X = completeIntersection(Y, {-toricDivisor Y})
debugLevel = 1
aX = abstractVariety(X,B)
G = aX.StructureMap^* F
chern G
chi G
basicCohomologies X
cohomologyVector(X, {2,1})
cohomologyVector(X, {2,6})
cohomologyVector(X, {-2,-6})
integral(chern (tangentBundle aX))
basicCohomologies X
hodgeDiamond X
integral chern tangentBundle aX -- topological Euler characteristic

OO Y_2
chi OO(a*t_4+b*t_5)
chi OO(a*t_0+b*t_1)
chi abstractSheaf(Y, B, OO(a*Y_0)) -- TODO: have an easy way to go from toric divisor to an element of intersection ring.
chi abstractSheaf(Y, B, OO Y_0)
chi abstractSheaf(Y, B, OO (-Y_0))

chi abstractSheaf(Y, B, OO (toricDivisor Y))
rays = NormalToricVarieties$rays

abstractSheaf(Y,B, OO Y_0)
chi aX.StructureMap^* F
chi F
for i from 0 to 4 list HH^i(Y, OO Y_2)
OO Y_2
cohomologyVector(Y, {2,1})
hodgeDiamond(Y)
exteriorPower(1,Y)
methods exteriorPower
HH^1(Y, cotangentSheaf Y)
