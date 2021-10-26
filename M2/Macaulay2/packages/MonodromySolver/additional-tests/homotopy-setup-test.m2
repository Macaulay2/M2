needsPackage "MonodromySolver"
errorDepth = 0
R1 = CC[a,b,c,d][A,B];
polys = polySystem {A*a+B*b,A*B*c+d};
(p0,x0) = createSeedPair polys;
count = 2;
--NumberOfEdges=>1 (no randomization)
(V,npaths) = monodromySolve(polys,p0,{x0},
    NumberOfNodes=>4,
    NumberOfEdges=>1);
assert( length V.PartialSols == count );

end
restart 
load "bug-homotopy-setup.m2"
