///
--original
restart
needsPackage "MonodromySolver"
setRandomSeed 0;
R=CC[x,y,z];
F=random(3,R);
P=sum apply(gens R,g->diff(g,F)*random CC);
sparseMonodromySolve polySystem {F,P,random(1,R)-1}
///

-- reproducing...
needsPackage "MonodromySolver"
R=(CC_53[W_(0,{3, 0, 0}), W_(0,{2, 1, 0}), W_(0,{1, 2, 0}), W_(0,{0, 3, 0}), W_(0,{2, 0, 1}), W_(0,{1, 1, 1}), W_(0,{0,
       2, 1}), W_(0,{1, 0, 2}), W_(0,{0, 1, 2}), W_(0,{0, 0, 3}), W_(1,{2, 0, 0}), W_(1,{1, 1, 0}), W_(1,{0, 2, 0}), W_(1,{1,
       0, 1}), W_(1,{0, 1, 1}), W_(1,{0, 0, 2}), W_(2,{1, 0, 0}), W_(2,{0, 1, 0}), W_(2,{0, 0, 1}), W_(2,{0, 0, 0})])[x, y, z]
P = gateSystem matrix {{W_(0,{3, 0, 0})*x^3+W_(0,{2, 1, 0})*x^2*y+W_(0,{1, 2, 0})*x*y^2+W_(0,{0, 3, 0})*y^3+W_(0,{2, 0,
       1})*x^2*z+W_(0,{1, 1, 1})*x*y*z+W_(0,{0, 2, 1})*y^2*z+W_(0,{1, 0, 2})*x*z^2+W_(0,{0, 1, 2})*y*z^2+W_(0,{0, 0, 3})*z^3},
       {W_(1,{2, 0, 0})*x^2+W_(1,{1, 1, 0})*x*y+W_(1,{0, 2, 0})*y^2+W_(1,{1, 0, 1})*x*z+W_(1,{0, 1, 1})*y*z+W_(1,{0, 0,
       2})*z^2}, {W_(2,{1, 0, 0})*x+W_(2,{0, 1, 0})*y+W_(2,{0, 0, 1})*z+W_(2,{0, 0, 0})}},

(p0, x0) = createSeedPair P;
(V, npaths) = monodromySolve(P,p0,{x0});
print "monodromySolve done"
G = V.Graph;
H = G.Family;


(V, npaths) := monodromySolve(P,p0,{x0});
G' := V.Graph;
H' = G.Family;

for i to 10 do (
    print i;
    (p1, x1) := createSeedPair P;
    start = transpose matrix V.BasePoint;
    targ = transpose matrix p1;
    H01 = specialize(H, start||targ);
    trackHomotopy(H01,points V.PartialSols)
    )


for i to 10 do (
    print i;
    monodromySolve P
    )

end


debug SLPexpressions
end
restart
load "sparseMonodromySolve.m2"
keys H01
H01.Parameters
peek H01.ParameterHomotopy
H01#(CC_53) -- RawHomotopy

-- next steps: look at trackHomotopyM2engine, getRawHomotopy (something goes wrong for GateParameterHomotopy?xs)
