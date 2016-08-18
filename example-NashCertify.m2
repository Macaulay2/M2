-- everything working except certify

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

setRandomSeed 0
(c0,pre0) = createSeedPair G
(node,n') = monodromySolve(transpose G.PolyMap,c0,{pre0},
    NumberOfEdges => 5,
    NumberOfNodes => 5,
    TargetSolutionCount => mixedVolume
    )
assert(length node.PartialSols == mixedVolume)   
certifySystem node
