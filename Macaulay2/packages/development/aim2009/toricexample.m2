
loadPackage "NormalToricVarieties"
load "ToricResolve.m2"

Q = normalToricVariety({{-1,-1,-1},{1,-1,-1},{-1,1,-1},{1,1,-1},{-1,-1,1},{1,-1,1},{-1,1,1},{1,1,1}},{{0,2,4,6},{0,1,4,5},{0,1,2,3},{1,3,5,7},{2,3,6,7},{4,5,6,7}});

rays Q
max Q

isSimplicial Q
isSmooth Q

Y = resolveSingularities2 Q

isSmooth Y
isSimplicial Y
