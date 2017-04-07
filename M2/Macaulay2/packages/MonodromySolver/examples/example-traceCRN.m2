restart
load "example-CRN.m2"
setRandomSeed 0
(p0, x0) = createSeedPair(G,"initial parameters" => "one")
elapsedTime (V,npaths) = monodromySolve(G,p0,{x0},
    GraphInitFunction=>completeGraphInit,
    NumberOfEdges=>5,
--    TargetSolutionCount => 9,
    "new tracking routine"=>false,
    Verbose=>true)
assert(length V.PartialSols ==4)
Gr = V.Graph
W1 = apply(toList Gr.Vertices, v->  apply(toList points v.PartialSols, p -> matrix p) )

R = ring Gr.Family
S = coefficientRing R
T =  CC[a, apply(numgens R, i -> (symbol WW)_i)][gens S, gens R]
mSysEqs = apply(flatten entries Gr.Family.PolyMap, i-> sub(i,T))

-- generate a random point on the solution variety
mSys = polySystem mSysEqs
svcodim = (mSys.NumberOfVariables - mSys.NumberOfPolys)
assert(svcodim == numgens S and svcodim == #coordinates p0)

-- make sure p0 lies on all khyperplanes
khyperplanes = (sub(vars S,T) - matrix p0) * random(CC^svcodim,CC^(svcodim-1))
-- ... and (x0,a0) satisfies the following. 
xcoeffs = random(CC^(numgens R),CC^1) 


xhyperplane = (sub(vars R, T) * transpose submatrix'(vars coefficientRing T,,{0}))_(0,0) - a 

matrix x0 * xcoeffs
a0 = {(matrix x0 * xcoeffs)_(0,0)}

P' = polySystem transpose (matrix{mSysEqs} | khyperplanes  | xhyperplane)
-- the max I got is 11. That seems correct: 15-4.
-- to get 11 more frequently, perhaps use more than one parameters?
-- of course, we should be using trace test to stop. then we stop only when we reach 11.
(V',npaths) = monodromySolve(P', point{a0|flatten entries xcoeffs}, {point{coordinates p0 | coordinates x0}},  NumberOfNodes => 3, NumberOfEdges => 2, Verbose=>true)
length V'.PartialSols
-- end -- Anton's edits
G= V'.Graph
W2 = apply(toList G.Vertices, v->  apply(toList points v.PartialSols, p -> matrix p) )


-- todo: combine points 
