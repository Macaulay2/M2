load "example-CRN.m2"
setRandomSeed 0

-- toy network example in comments
-*(p0, x0) = createSeedPair(G,"initial parameters" => "one")
elapsedTime (V,npaths) = monodromySolve(G,p0,{x0},
    GraphInitFunction=>completeGraphInit,
    NumberOfEdges=>5,
    "new tracking routine"=>false,
    Verbose=>true)
assert(length V.PartialSols ==4)*-

W = wnt()
L = apply(numgens createRing(W,FF), i->random FF)
F = createPolySystem(W, FF, L)
(p0, x0) = createSeedPair(F,L)
elapsedTime (V,npaths) = monodromySolve(F,p0,{x0},
    GraphInitFunction=>completeGraphInit,
    NumberOfEdges=>5,
--    TargetSolutionCount => 9,
    "new tracking routine"=>false,
    Verbose=>true)
assert(length V.PartialSols == 9)

Gr = V.Graph
W1 = apply(toList points (first Gr.Vertices).PartialSols, p -> matrix {p0.Coordinates|p.Coordinates})

R = ring Gr.Family
S = coefficientRing R
T =  CC[a, apply(numgens R, i -> (symbol WW)_i)][gens S, gens R]
mSysEqs = apply(flatten entries Gr.Family.PolyMap, i-> sub(i,T))

-- generate a random point on the solution variety
mSys = polySystem mSysEqs;
svcodim = (mSys.NumberOfVariables - mSys.NumberOfPolys)
assert(svcodim == numgens S and svcodim == #coordinates p0)

-- make sure p0 lies on all khyperplanes
khyperplanes0' = (sub(vars S,T) - matrix p0) * random(CC^svcodim,CC^(svcodim));
khyperplanes0 = submatrix'(khyperplanes0',,{0});
-- ... and (x0,a0) satisfies the following. 
xcoeffs = random(CC^(numgens R),CC^1);



xhyperplane = (sub(vars R, T) * transpose submatrix'(vars coefficientRing T,,{0}))_(0,0) - a;
seedCoeffs = (matrix x0 * xcoeffs)_(0,0)
a0 = {seedCoeffs}

P' = polySystem transpose (matrix{mSysEqs} | khyperplanes0  | xhyperplane)

end ------------------------------------------------------------
restart
load "wnt-trace-test.m2"

-- second Wntness set: max I've gotten for Wnt model is 230 
elapsedTime (V',npaths) = monodromySolve(P', point{a0|flatten entries xcoeffs}, {point{coordinates p0 | coordinates x0}}, 
     NumberOfNodes => 3, NumberOfEdges => 3, GraphInitFunction=>completeGraphInit, "new tracking routine"=>false, Verbose=>true)

-- via Bertini
overdetEquations = (createPolySystem'overdetermined(W,FF,L)).PolyMap
R = ring overdetEquations
S = coefficientRing R
T =  CC[gens S, gens R]
overdetSysEqs = sub(overdetEquations, T)

khyperplanes0 = (sub(vars S,T) - matrix p0) * random(CC^svcodim,CC^(svcodim-1));
xhyperplane = (sub(vars R, T) - matrix x0) * random(CC^(numgens R),CC^1);

polys = khyperplanes0 | xhyperplane | transpose overdetSysEqs;
R = CC[x_1..x_(numgens ring polys)]
toR = map(R,ring polys,vars R)
NAGtrace 1
elapsedTime NV := numericalIrreducibleDecomposition(ideal toR polys,Software=>BERTINI)
-- assert(#NV#0 == 9)
