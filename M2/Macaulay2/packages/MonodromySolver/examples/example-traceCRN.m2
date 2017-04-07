restart
load "example-CRN.m2"
setRandomSeed 2
(p0, x0) = createSeedPair(G,"initial parameters" => "one")
--(p0, x0) = createSeedPair G -- change createPolySystem to make this work
elapsedTime (V,npaths) = monodromySolve(G,p0,{x0},
    GraphInitFunction=>completeGraphInit,
    NumberOfEdges=>5,
--    TargetSolutionCount => 9,
    "new tracking routine"=>false,
    Verbose=>true)
assert(length V.PartialSols ==4)
Gr = V.Graph
W1 = apply(toList points (first Gr.Vertices).PartialSols, p -> matrix {p0.Coordinates|p.Coordinates})

R = ring Gr.Family
S = coefficientRing R
T =  CC[a, apply(numgens R, i -> (symbol WW)_i)][gens S, gens R]
mSysEqs = apply(flatten entries Gr.Family.PolyMap, i-> sub(i,T))

-- generate a random point on the solution variety
mSys = polySystem mSysEqs
svcodim = (mSys.NumberOfVariables - mSys.NumberOfPolys)
assert(svcodim == numgens S and svcodim == #coordinates p0)

-- make sure p0 lies on all khyperplanes
khyperplanes0' = (sub(vars S,T) - matrix p0) * random(CC^svcodim,CC^(svcodim))
khyperplanes0 = submatrix'(khyperplanes0',,{0})
-- ... and (x0,a0) satisfies the following. 
xcoeffs = random(CC^(numgens R),CC^1) 


xhyperplane = (sub(vars R, T) * transpose submatrix'(vars coefficientRing T,,{0}))_(0,0) - a 

seedCoeffs = (matrix x0 * xcoeffs)_(0,0)
a0 = {seedCoeffs}
xhyperplane0 = first specializeSystem(point {(a0 | flatten entries xcoeffs)}, polySystem {xhyperplane})

P' = polySystem transpose (matrix{mSysEqs} | khyperplanes0  | xhyperplane)
-- the max I got is 11. That seems correct: 15-4.
-- to get 11 more frequently, perhaps use more than one parameters?
-- of course, we should be using trace test to stop. then we stop only when we reach 11.
(V',npaths) = monodromySolve(P', point{a0|flatten entries xcoeffs}, {point{coordinates p0 | coordinates x0}},  NumberOfNodes => 4, NumberOfEdges => 2, Verbose=>true)
assert(length V'.PartialSols == 11)


--xhyperplane0 = last V'.SpecializedSystem
U = ring xhyperplane0
V0 = first V'.Graph.Vertices -- the seed vertex 
W2 = apply(points V0.PartialSols, p -> matrix p)
-- consolidate witness sets for trace test
sols = apply(W1 | W2, s -> point s);

curve = sub(matrix{mSysEqs}|khyperplanes0,U) 
Pquadric = polySystem transpose (curve|matrix{{xhyperplane0 * sub(first flatten entries khyperplanes0',U)}});
Plinear = polySystem transpose (curve|matrix{{random(1,U)+1}}) 

apply(sols, s->norm evaluate(Pquadric,s))
apply(sols, s->norm evaluate(curve,s))
apply(sols, s->norm evaluate(matrix{{xhyperplane0}},s))
apply(sols, s->norm evaluate(matrix{{sub(first flatten entries khyperplanes0',U)}},s))
apply(sols, s->numericalRank evaluate(jacobian Pquadric,s))

tracked = track(Pquadric,Plinear,sols)
failed = positions(tracked, x -> x.NumberOfSteps==0)

