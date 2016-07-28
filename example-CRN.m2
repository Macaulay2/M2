restart
load "code/solveViaMonodromy.m2"
needsPackage "ReactionNetworks"
needsPackage "NumericalAlgebraicGeometry"

FF = CC

CRN = reactionNetwork "A <--> 2B, A + C <--> D, B + E --> A + C, A+C --> D"
R = createRing(CRN, FF)
CE = flatten entries random(FF^1, FF^2) - conservationEquations(CRN,FF)
I = ideal CE
SSE = steadyStateEquations CRN
J = ideal SSE
F = I + J

T = transpose gens F
rM = sub(random(FF^5, FF^7),R)
G = polySystem(rM * T)
end
load "example-CRN.m2"
setUpPolysparse = G -> (
    C := coefficientRing ring G;
    M := sub(sub(G.PolyMap, apply(gens ring G, x -> x => 1)), C);
    N := numericalIrreducibleDecomposition flatten entries M;
    
    
    c0 = point{ 
    flatten apply(equations G,f->(
	    r := # exponents f;
	    t := apply(r-1, i->random CC);
	    t | { -sum t }
	    )) 
        }
    pre0 = point{toList(n:1_CC)}
    )







restart
needs "code/solveViaMonodromy.m2"
needs "examples/cyclic.m2"
plugin'c0 = map(R,AR,vars R | matrix c0) -- the actual polynomial system we solve
apply(polysP,p->plugin'c0 p) 
stop = (n,L)->n>3
getDefault Software
{*
setDefault(Software=>PHCPACK)
*}

-- two vertex

elapsedTime sols = twoNodes(SP,c0,{pre0},3,StoppingCriterion=>stop)

G = first sols
E = G.Edges
for e in E do (
    print peek e.Correspondence12;
    print peek e.Correspondence21;
    )

-- other strategies
elapsedTime sols = randomFlowerStrategy(SP,c0,{pre0},StoppingCriterion=>stop);
elapsedTime sols = solveViaMonodromy(SP,c0,{pre0},StoppingCriterion=>stop)
elapsedTime sols = flowerStrategy(SP,c0,{pre0},StoppingCriterion=>stop);
sols

-- currently not working?
elapsedTime sols = loopStrategy(SP,c0,{pre0},3,StoppingCriterion=>stop);





