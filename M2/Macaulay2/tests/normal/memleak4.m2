-- we can no longer test for memory leaks, because  engineHeap()  and   engineMemory()  are gone, oh well
end

n = 100
kk = ZZ/31
R = kk[x]
doit = i -> R/x
collectGarbage()
collectGarbage()
collectGarbage()
k = # netRows engineHeap()
k'= # netRows engineMemory()
time scan(n, doit)
collectGarbage()
collectGarbage()
collectGarbage()
K = # netRows engineHeap()
K'= # netRows engineMemory()
w = floor( 1/2 + (K-k)/n )
w' = floor( 1/2 + (K'-k')/n )
stderr << "average number of handles wasted per iteration : " << w << endl
stderr << "average number of memory types wasted per iteration : " << w' << endl
-- engineHeap
-- engineMemory
assert( w < 2 )
assert( w' < 2 )
