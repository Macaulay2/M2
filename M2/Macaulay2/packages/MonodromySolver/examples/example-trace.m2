needsPackage "MonodromySolver"
setRandomSeed 0
m = 2; n = 4; d = 3;  
C = CC[a_(1,1)..a_(m,n),b_1..b_m]
R = C[x_1..x_n]
F = apply(n-m, i->sub(random(d,CC[x_1..x_n]),R)) -- V(F) = intersection of n-m hypersurfaces of degree d
A = genericMatrix(C,n,m)
B = genericMatrix(C,b_1,1,m)
L = flatten entries (vars R * A + B) -- slice of complimentary dimension 
G = polySystem(F|L)

{* clearAll()
C = CC[b]
R = C[x,y,z]
F = {y-x^2, z-x^3} 
L = { 2*x + 3*y + 5*z - b }
G = polySystem(F|L) *}
end


restart
load "example-trace.m2"
(p0, x0) = createSeedPair(G);
elapsedTime (V,npaths) = monodromySolve(G,p0,{x0},NumberOfEdges=>4,Verbose=>true)
Gr = V.Graph
R = ring Gr.Family
R' = CC[gens R]
dadRing =  (flattenRing R)#0
dadSys = apply(flatten entries Gr.Family.PolyMap, i-> sub(i,dadRing))
S = coefficientRing R
m = numgens S
n = numgens R'
vs = V.BasePoint.Coordinates
cIndex = position(vs,i->i == max vs)
bps = apply(m, i->sub((gens S)#i, dadRing) - vs#i)


-- next steps: create and solve the system "H" created by slicing with random hyperplanes in x vars

preGranDadRing = (coefficientRing S)[apply(n*m, i->(symbol W)_i)] -- W is just an unused symbol
granDadRing = preGranDadRing[gens dadRing]
forms2 = apply(m, i -> sum(apply(n, 
	    j -> (gens preGranDadRing)#(j+i*n)*sub((gens R)#j, granDadRing))))
H = polySystem(apply(equations Gr.Family, p -> sub(p, granDadRing)) | forms2)
(q0,y0) = createSeedPair(H, "initial parameters" => "random")
elapsedTime (V',npaths') = monodromySolve(H,q0,{y0},NumberOfEdges=>4,Verbose=>true)
 -- no points found, as expected since there are "too many equations" solely in xs
 npaths'
if min apply(toList (V'.Graph).Vertices, i->  length i.PartialSols) == 0 then (
    newPoints = {}
    )
else (
    -- TODO: need to modify V'.partialSols as list exluding W-coordinates
    )

-- now set up master system

masterSys = polySystem(apply(equations Gr.Family,i -> sub(i, dadRing)) | apply(m,i->bps#i * sub(first specializeSystem(q0, polySystem {forms2#i}), dadRing)))
masterSols = apply(length V.PartialSols, i -> point {((V.PartialSols)#i).Coordinates | p0.Coordinates}) | 
apply(length newPoints, i -> point {newPoints#i | q0.Coordinates})


-- now create random billinear forms

formsx = apply(m, i->sub(random(1,R'),dadRing))
formsy= apply(m, i->sub(random(1,S),dadRing))
patchesx= apply(m, i->sub(random(1,R'),dadRing))
patchesy= apply(m, i->sub(random(1,S),dadRing))

endSys = polySystem(dadSys | apply(m, i-> formsx#i * patchesy#i + formsy#i * patchesx#i + patchesx#i * patchesy#i))
tracked = track(masterSys,endSys,masterSols)

-- now create parallel slices to endSys
-- tracking seems to fail periodically here
(r1,r2) = (random(CC), random(CC))
E1 = polySystem(dadSys | apply(m, i-> formsx#i * patchesy#i + formsy#i * patchesx#i + r1* patchesx#i * patchesy#i))
E2 = polySystem(dadSys | apply(m, i-> formsx#i * patchesy#i + formsy#i * patchesx#i + r2* patchesx#i * patchesy#i))
t1 = track(endSys,E1,tracked)
t2 = track(endSys,E2,tracked)

-- rough affine trace test example contains functions for computing traces
H = V.Graph
sys = polySystem specializeSystem(V.BasePoint, H.Family)
sols = points V.PartialSols
computeTrace = L -> sum apply(L, t -> matrix t)
params = gens coefficientRing ring H.Family
peek sys
lastB = last params
traces = {transpose(computeTrace sols | matrix {{last coordinates p0}})}
linearSlice = apply(flatten entries G.Family.PolyMap, F -> sub(sub(F, ring G.Family), toList apply(0..(length params -2), i -> params#i => (p0.Coordinates)#i)))
for i from 0 to 2 do (
    b = random(RR);
    sys' = polySystem apply(linearSlice, F->sub(F, lastB => b));
    T = track(sys, sys', sols);
    print (T/matrix/transpose , transpose computeTrace T);
    traces = append(traces, transpose (computeTrace T | matrix{{b}}))
    );
first SVD(traces#2-traces#1|traces#3-traces#1)

--
restart
needsPackage "MonodromySolver"
R = CC[x_0..x_1]
S = R[y_0..y_1]

f = x_0*y_0^2 - x_1*y_1^2
l2 = sub(x_0*y_0 + 2*x_1,S)

system = polySystem {f,l2}
(p0,x0) = createSeedPair(system, "initial parameters" => "random")  
elapsedTime (V,npaths) = monodromySolve(system,p0,{x0},NumberOfEdges=>10,Verbose=>true,EdgesSaturated=>true,TargetSolutionCount=>2)
edgesSaturated V.Graph
