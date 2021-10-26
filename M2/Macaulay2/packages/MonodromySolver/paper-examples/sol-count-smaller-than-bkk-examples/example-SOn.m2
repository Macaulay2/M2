needsPackage "MonodromySolver"
LIEDIM=binomial(N,2)--This is the dimension of SO(N)
R=CC[c_1..c_LIEDIM,t_(1,1,1)..t_(LIEDIM,N,N)][x_(1,1)..x_(N,N)]--here c_i are the constants in the linear form indexed by i.
--and t_(i,j,k) is the coefficient of x_(j,k) in the linear form indexed by i
M=genericMatrix(R,N,N)--next three lines create equations for SO(N)
B=M*transpose(M)-id_(R^N)
polys=flatten for j from 0 to N-1 list for k from j to N-1 list B_(j,k)--Here they are
--below gives linear slices where the coefficients {c_i,t_(i,j,k)} form the parameter space
linearSlice=for i from 1 to LIEDIM list(c_i+sum(flatten for j from 1 to N list  for k from 1 to N list t_(i,j,k)*x_(j,k))); --this takes a random cut
G = polySystem join (polys,linearSlice)--the incidence variety.
end

restart
N=4;
N=5;
N=6;
N=7;
--where N is as in SO(N)
load "example-SOn.m2"
describe R
x0coords = flatten entries id_(CC^N)
setRandomSeed 0
(p0, x0) := createSeedPair(G,point{x0coords});
elapsedTime (V,npaths) = monodromySolve(G,p0,{x0},NumberOfNodes=>2,NumberOfEdges=>4,Verbose=>true)

-* 

-- N=4 -------------------------------------------------

  node1: 40
  node2: 40
trackedPaths 0
     -- 4.0872 seconds elapsed

o7 = (HomotopyNode{...5...}, 160)

-- N=5 -------------------------------------------------

  node1: 384
  node2: 384
trackedPaths 0
     -- 23.4456 seconds elapsed

o7 = (HomotopyNode{...5...}, 1537)

-- N=6 -------------------------------------------------

  node1: 4768
  node2: 4768
trackedPaths 0
      -- 528.439 seconds elapsed

o16 = (HomotopyNode{...5...}, 19087)

-- N=7 -----------------------------------------------                                                                              

  node1: 111616                                                                                                                
  node2: 111616                                                                                                                
trackedPaths 0                                                                                                                 
     -- 42790.9 seconds elapsed                                                                                                
                                                                                                                               
o7 = (HomotopyNode{...5...}, 447568)                                                                                           

-- N=8 ------------------------------------------------
*-

setRandomSeed 1
(p0, x0) := createSeedPair(G,x0coords);
elapsedTime (V,npaths) = monodromySolve(G,p0,{x0},NumberOfNodes=>4,NumberOfEdges=>1,Verbose=>true)

-*
node1: 111616
node2: 111616
trackedPaths 0
-- 58735.3 seconds elapsed
    	 
 o9 = (HomotopyNode{...5...}, 671313)
	 
*-

-- regeneration in Bertini 
specPolys = specializeSystem (p0, polySystem polys);
R = CC[x_1..x_(numgens ring first specPolys)]
describe R
toR = map(R,ring first specPolys,vars R)
elapsedTime NV := numericalIrreducibleDecomposition(ideal (specPolys/toR),Software=>BERTINI)

-*
-- N=4 -------------------------------------------------

i12 : elapsedTime NV := numericalIrreducibleDecomposition(ideal (specPolys/toR),Software=>BERTINI)
     -- 80.9053 seconds elapsed

o12 = a numerical variety with components in
      dim 6:  [dim=6,deg=40] [dim=6,deg=40]

-- N=5 -------------------------------------------------

i25 : elapsedTime NV := numericalIrreducibleDecomposition(ideal (specPolys/toR),Software=>BERTINI)
     -- 1248.64 seconds elapsed

o25 = a numerical variety with components in
      dim 10:  [dim=10,deg=384] [dim=10,deg=384]

-- N=6 ---------------------------------------------------

core dump by bertini
-- 10604.9 seconds elapsed

GNU MP: Cannot allocate memory (size=39959176) 

*-

-- PHCpack blackbox
specPolys = specializeSystem (p0, G);
elapsedTime sols = solveSystem(specPolys,Software=>PHCPACK);
#sols 
-*
-- N=4 -------------------------------------------------

-- 103.789 seconds elapsed       

-- N=5 -------------------------------------------------

-- killed after 1 day

*-
