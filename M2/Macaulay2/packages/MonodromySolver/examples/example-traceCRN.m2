restart
load "example-CRN.m2"

setRandomSeed 0
(p0, x0) = createSeedPair(G,"initial parameters" => "one")  
elapsedTime (V,npaths) = monodromySolve(G,p0,{x0}, NumberOfEdges => 5, EdgesSaturated=>true)
assert(length V.PartialSols ==4)
Gr = V.Graph

R = ring Gr.Family
S = coefficientRing R
T =  CC[a][gens S, gens R]
mSysEqs = apply(flatten entries Gr.Family.PolyMap, i-> sub(i,T))

-- generate a random point on the solution variety
mSys = polySystem mSysEqs
svcodim = (mSys.NumberOfVariables - mSys.NumberOfPolys)
assert(svcodim == numgens S and svcodim == #coordinates p0)

-- make sure p0 lies on all khyperplanes
khyperplanes = (sub(vars S,T) - matrix p0) * random(CC^svcodim,CC^(svcodim-1))
-- ... and (x0,a0) satisfies the following. 
xcoeffs = random(CC^(numgens R),CC^1) 
xhyperplane = (sub(vars R,T) * xcoeffs)_(0,0) - a
a0 = (matrix x0 * xcoeffs)_(0,0)

P' = polySystem transpose (matrix{mSysEqs} | khyperplanes  | xhyperplane)

-- the max I got is 11. That seems correct: 15-4.
-- to get 11 more frequently, perhaps use more than one parameters?
-- of course, we should be using trace test to stop. then we stop only when we reach 11.
(V',npaths) = monodromySolve(P', point{{a0}}, {point{coordinates p0 | coordinates x0}},  NumberOfNodes => 7, NumberOfEdges => 2, Verbose=>true)
length V'.PartialSols

-- something is wrong with the setup... when NumberOfEdges=>1
(V',npaths) = monodromySolve(P', point{{a0}}, {point{coordinates p0 | coordinates x0}},  NumberOfNodes => 4, NumberOfEdges => 1, Verbose=>true)

-- end -- Anton's edits








nSliceParams = svcodim*(numgens T)
U = CC[apply(nSliceParams, i-> symbol ww_i)][gens T]
hyperplanes = apply(svcodim, j -> sum(apply(numgens T, i-> sub((gens T)#i,U)*(gens coefficientRing U)#(j*svcodim +i))))
svCut = polySystem( apply(mSysEqs, e-> sub(e,U))| hyperplanes)
setRandomSeed(3)
(q0,y0) = createSeedPair(svCut,"initial parameters" => "random")

-- check that it worked: for some seeds it doesn't	    
apply(mSysEqs, e -> sub(e, apply(numgens T, i -> (gens T)#i => (y0.Coordinates)#i)))

-- now slice with xes: might need to generate new hyperplanes 
specializedHyperplanes = specializeSystem(q0,polySystem drop(hyperplanes,-1))
xSliceOpts = apply(numgens S,  i -> (gens T)#i => 0)
xSlices = apply(specializedHyperplanes, h -> sub(sub(h,T), xSliceOpts))

-- set up system
kSliceOpts = apply(numgens S,  i -> (gens T)#i => (y0.Coordinates)#i)
kslice = sub(random(1,S),T)
a0 = point {{ sub(kslice, kSliceOpts) }}
P' = polySystem(mSysEqs | xSlices  | {kslice-a})

-- solve system: still failing
(V',npaths) = monodromySolve(P', a0, {y0})
ring P'

specializeSystem(a0,P')


(p0, x0) = createSeedPair(G,"initial parameters" => "one")  



m = numgens S
n = numgens R'
vs = V.BasePoint.Coordinates
cIndex = position(vs,i->i == max vs)
bps = apply(m, i->sub((gens S)#i, T) - vs#i)

bps#cIndex
-- next steps: create and solve the system "H" created by slicing with random hyperplanes in x vars

T0 = (coefficientRing S)[apply(n*m, i->(symbol W)_i)] -- W is just an unused symbol
T' = T0[gens T]
forms2 = apply(m, i -> sum(apply(n, 
	    j -> (gens T0)#(j+i*n)*sub((gens R)#j, T'))))
H = polySystem(apply(equations Gr.Family, p -> sub(p, T')) | drop(forms2,-1) | {sub(bps#cIndex, T')})
(q0,y0) = createSeedPair(H, "initial parameters" => "random")
elapsedTime (V',npaths') = monodromySolve(H,q0,{y0},NumberOfEdges=>4,Verbose=>true)


if min apply(toList (V'.Graph).Vertices, i->  length i.PartialSols) == 0 then (
    newPoints = {}
    )
else (
    -- TODO: need to modify V'.partialSols as list exluding W-coordinates
    )

-- now set up master system

masterSys = polySystem(apply(equations Gr.Family,i -> sub(i, T)) | apply(m,i->bps#i * sub(first specializeSystem(q0, polySystem {forms2#i}), T)))
masterSols = apply(length V.PartialSols, i -> point {((V.PartialSols)#i).Coordinates | p0.Coordinates}) | 
apply(length newPoints, i -> point {newPoints#i | q0.Coordinates})


-- now create random billinear forms

formsx = apply(m, i->sub(random(1,R'),T))
formsy= apply(m, i->sub(random(1,S),T))
patchesx= apply(m, i->sub(random(1,R'),T))
patchesy= apply(m, i->sub(random(1,S),T))

endSys = polySystem(mSys | apply(m, i-> formsx#i * patchesy#i + formsy#i * patchesx#i + patchesx#i * patchesy#i))
tracked = track(masterSys,endSys,masterSols)

-- now create parallel slices to endSys
-- tracking seems to fail periodically here
(r1,r2) = (random(CC), random(CC))
E1 = polySystem(mSys | apply(m, i-> formsx#i * patchesy#i + formsy#i * patchesx#i + r1* patchesx#i * patchesy#i))
E2 = polySystem(mSys | apply(m, i-> formsx#i * patchesy#i + formsy#i * patchesx#i + r2* patchesx#i * patchesy#i))
t1 = track(endSys,E1,tracked)
t2 = track(endSys,E2,tracked)

