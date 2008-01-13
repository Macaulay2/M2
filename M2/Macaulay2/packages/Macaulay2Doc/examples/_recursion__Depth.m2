recursionDepth()
f = x -> recursionDepth()
f()
g = x -> f()
g()
r = i -> if i == 100 then recursionDepth() else r(i+1)
r 0
