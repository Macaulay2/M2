newPackage(
        "Oscillators",
        Version => "1.0", 
        Date => "1 May 2025",
        Authors => {
            {
                Name => "John Cobb", 
                Email => "jdc0173@auburn.edu", 
                HomePage => "https://johndcobb.github.io"
                },
            {
                Name => "Hal Schenck", 
                Email => "hks0015@auburn.edu",
                HomePage => "http://webhome.auburn.edu/~hks0015"
                },
            {
                Name => "Michael E. Stillman", 
                Email => "mike@math.cornell.edu", 
                HomePage => "http://www.math.cornell.edu/People/Faculty/stillman.html"
                }
            },
        Headline => "code to analyze graph oscillators",
        Keywords => {"Applied Algebraic Geometry"},
        DebuggingMode => false,
        AuxiliaryFiles => true,
        PackageExports => {
            "Graphs", 
            "NumericalAlgebraicGeometry"
            }
        )

export {
    "oscRing",
    "oscQuadrics",
    "oscJacobian",
    "identifyStability",
    "findRealSolutions",
    "isStableSolution",
    "getAngles",
    "getLinearlyStableSolutions",
    "getExoticSolutions",
    "showExoticSolutions",
    "allUniquePrincipalMinors",
    "vertexSpanningPolynomial",
    "standardSols",
    -- symbols
    "Symbols",
    "numOscillators",
    "Radians",
    "Stable",
    "Unstable",
    "Semistable",
    "Modulo",
    "oscSystem",
    "trig",
    }


baseDirectory = Oscillators#"source directory"
--------------------------------------------------------------------
----- CODE
--------------------------------------------------------------------
load(baseDirectory | "Oscillators/Code.m2")
--- THINGS TO IMPLEMENT? -- 

-*

*-

--------------------------------------------------------------------
----- DOCUMENTATION
--------------------------------------------------------------------
beginDocumentation()
load(baseDirectory | "Oscillators/Documentation.m2")

--------------------------------------------------------------------
----- TESTS
--------------------------------------------------------------------
load(baseDirectory | "Oscillators/Tests.m2")

end------------------------------------

restart
uninstallPackage "Oscillators";
restart
installPackage "Oscillators"
check Oscillators
debug needsPackage "Oscillators";


--------------------------------------------------------------------
----- SCRATCH SPACE
--------------------------------------------------------------------

restart
needsPackage "Oscillators"
R = oscRing(5,{})
oscRing(5,{k,w})
oscRing(5,{k,w},CoefficientRing=>CC)
oscRing(5,{k,w},CoefficientRing=>RR)
oscRing(5,{k,w},CoefficientRing=>ZZ/32003)
oscRing(5,{k,w},CoefficientRing=>ZZ/32003, Start=>0)

trig R

R = oscRing(4,{})
G = ringOscillatorGraph(5,1)
P = oscSystemReduced(G,R)
det oscJacobian P
I = P + trig ring P
decompose I



R0 = oscRing(5,{},Start=>0)
see oscSystem(G,R0,hashTable {})
ringOscillatorGraph(5,2)
ringOscillatorGraph(10,3)

ringOscillators0(5,1,CoefficientRing=>QQ,Symbols=>(x,y))
ringOscillators0(5,1,CoefficientRing=>QQ,Symbols=>(s,c))
ringOscillators0(5,1)

-- n=5, k=1
{0,1,2,3,4}, {{{0,1},1}, {{1,2},1}, ...
    
-- n=5, k=2
{0,1,2,3,4}, above, plus: {0,2}, {1,3}, {2,4}, {4,1}    

R = oscRing(6,{a,b},CoefficientRing=>QQ)
I = trig(6,R)
G = ringOscillatorGraph(6,2)
wts = hashTable {{2,4}=>a}
oscSystem(G,R,wts)
see oo

-- Numerically solve simple ring of oscillators
(J,I) = egRingOsc(5,1,CoefficientRing=>CC)
S = findRealSolutions I
aS = sort getAngles((ring I).numOscillators,S,Radians=>false)
matrix aS
jacs = for p in S list sub(J, matrix{p})
matrix for j in jacs list for e in eigenvalues j list realPart e
isStable = (p,jac) -> (
    j0 := sub(jac, matrix{p});
    all((eigenvalues j0)/realPart, a -> a < 0)
    )
for p in S list isStable(p,J)


-- Numerically solve simple ring of oscillators
(J,I) = egRingOsc(6,1,CoefficientRing=>CC)
(J,I) = egRingOsc(6,1,CoefficientRing=>QQ)
I
dim I
degree I
decompose I
S = findRealSolutions I
aS = sort getAngles((ring I).numOscillators,S,Radians=>false)
matrix aS
jacs = for p in S list sub(J, matrix{p})
matrix for j in jacs list for e in eigenvalues j list realPart e
isStable = (p,jac) -> (
    j0 := sub(jac, matrix{p});
    all((eigenvalues j0)/realPart, a -> a < 0)
    )
for p in S list isStable(p,J)

-- Weighted ring oscillators
restart
needsPackage "Oscillators"
(J,I) = egRingOscWeighted(5,2,symbol a,CoefficientRing=>QQ)
(J,I) = egRingOscWeighted(5,2,symbol a,CoefficientRing=>ZZ/32003)
gbTrace=3
gens gb I;


S = findRealSolutions I
aS = sort getAngles((ring I).numOscillators,S,Radians=>false)
matrix aS
jacs = for p in S list sub(J, matrix{p})
matrix for j in jacs list for e in eigenvalues j list realPart e
isStable = (p,jac) -> (
    j0 := sub(jac, matrix{p});
    all((eigenvalues j0)/realPart, a -> a < 0)
    )
for p in S list isStable(p,J)
