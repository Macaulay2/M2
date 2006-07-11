R = QQ[x,y,MonomialOrder => Position => Down ]
f = transpose matrix {{0,1-y,x+x^2}, {x,y,0}, {x,0,y}}
g = gens gb (f, StopWithMinimalGenerators=>true, Syzygies=>false, ChangeMatrix=>false)
g = mingens gb (f, StopWithMinimalGenerators=>true, Syzygies=>false, ChangeMatrix=>false)
assert(image g == image f)
