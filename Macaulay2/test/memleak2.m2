-- make sure engine objects get freed!
S = ZZ[t,u];
k = # netRows engineHeap()
k'= # netRows engineMemory()
n = 1000
f = i -> (
     x := local x; 
     y := local y;
     M := monoid[x,y];
     R := QQ[x,y];
     g := matrix{{x,y}};
     s := syz g;
     I := ideal(x,y);
     G := gb I;
     S := R/I; -- seems to be leaking 2 fractions (2022) [Dec 7,00]
     f := map(R,R,{y,x});  -- leaking 2 fractions? (4027)[Dec 7,00]
     M := coker g; 
     C := res M;  -- leaking one fraction? (5031)[Dec 7,00]
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
