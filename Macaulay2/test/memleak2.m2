-- make sure engine objects get freed!
S = ZZ[t,u];
k = # netRows engineHeap()
k'= # netRows engineMemory()
n = 200
f = i -> (
     x := local x; 
     y := local y;
     R := QQ[x,y];
     g := matrix{{x,y}};
     s := syz g;
     I := ideal(x,y);
     G := gb I;
     S := R/I;
     f := map(R,R,{y,x});
     M := coker g; 
     C := res M;
     N := image g;
     )
scan(n, f)
collectGarbage()
collectGarbage()
collectGarbage()
K = # netRows engineHeap()
K'= # netRows engineMemory()
-- these results are slightly unpredictable - the garbage collector doesn't collect
-- everything.
stderr << "average number of handles wasted per iteration : " << floor( 1/2 + (K-k)/n ) << endl
stderr << "average number of memory types wasted per iteration : " << floor( 1/2 + (K'-k')/n ) << endl
engineHeap
engineMemory
assert ( K < k + n - 10 )
assert ( K'< k'+ n - 10 )
