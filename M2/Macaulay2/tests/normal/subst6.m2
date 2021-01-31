-- We have decided that this is the behavior we must use:

makeElem = () -> (
     x := local x;
     (QQ[x_1,x_2])_0)

f1 = makeElem()
f2 = makeElem()
F = map(ring f1, ring f2, vars ring f1)
G = map(ring f1, ring f2) 
assert(G.matrix == 0)
assert(F.matrix == vars ring f1)
