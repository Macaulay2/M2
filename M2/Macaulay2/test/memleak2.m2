-- make sure engine objects get freed!
k = # netRows engineHeap()
k'= # netRows engineMemory()
n = 100
f = i -> (x := local x; R := ZZ[x,y]; g := matrix{{x,y}}; M := coker g; N := image g; )
scan(n, f)
collectGarbage()
K = # netRows engineHeap()
K'= # netRows engineMemory()
stderr << "average number of handles wasted per iteration : " << floor( 1/2 + (K-k)/n ) << endl
stderr << "average number of memory types wasted per iteration : " << floor( 1/2 + (K'-k')/n ) << endl
assert ( K < k + n - 10 )
assert ( K'< k'+ n - 10 )
