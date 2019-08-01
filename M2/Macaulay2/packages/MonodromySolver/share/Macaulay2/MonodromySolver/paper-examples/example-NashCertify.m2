needsPackage "AlphaTest"
load "example-Nash.m2"

certifySystem = method()
certifySystem HomotopyNode := x -> (
    FF := QQ[i]/ideal(i^2+1);
    SP := complexToRational(polySystem x.SpecializedSystem,FF);
    sols := complexToRational(points x.PartialSols,FF);
    all(sols, s -> certifySolution(SP,s))
    )

end

restart
load "example-NashCertify.m2"

G = getNashSystem(3,3)
mixedVolume = bkkBound(3,3)

s = 0
setRandomSeed s
(c0,pre0) = createSeedPair G
assert(norm sub(matrix{specializeSystem(c0,G)},matrix pre0) < 0.001)
(node,n') = monodromySolve(transpose G.PolyMap,c0,{pre0},
    NumberOfEdges => 3,
    NumberOfNodes => 3,
    TargetSolutionCount => mixedVolume,
    NumberOfRepeats => 50
    )
certifySystem node
