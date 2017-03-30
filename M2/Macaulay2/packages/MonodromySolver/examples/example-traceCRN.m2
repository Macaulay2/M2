load "example-CRN.m2"

setRandomSeed 0
-- system for example from Elizabeth's talk
(p0, x0) = createSeedPair(G,"initial parameters" => "one")  
(V,npaths) = monodromySolve G
elapsedTime (V,npaths) = monodromySolve(G,p0,{x0}, NumberOfEdges => 4, EdgesSaturated=>true)
assert(length V.PartialSols ==5)
Gr = V.Graph

R = ring Gr.Family
S = coefficientRing R
T =  CC[a][gens S, gens R]
mSysEqs = apply(flatten entries Gr.Family.PolyMap, i-> sub(i,T))

-- generate a random point on the solution variety
mSys = polySystem mSysEqs
svcodim = (mSys.NumberOfVariables - mSys.NumberOfPolys)
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

-- solve System: still not working
kSliceOpts = apply(numgens S,  i -> (gens T)#i => (y0.Coordinates)#i)
kslice = sub(random(1,S),T)
a0 = point {{ sub(kslice, kSliceOpts) }}
P' = polySystem(mSysEqs | xSlices  | {kslice-a})
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

