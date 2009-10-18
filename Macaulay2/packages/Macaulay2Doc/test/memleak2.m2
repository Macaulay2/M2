stderr << "this test is no longer relevant" << endl
exit 0

-- make sure engine objects get freed!
S = ZZ[t,u];
k = # netRows engineHeap()
k'= # netRows engineMemory()
n = 500
f = i -> (
     x := local x; 
     y := local y;
     z := local z;
     --M := monoid[x,y];
     --R := QQ[x,y];
     --g := matrix{{x,y}};
     --s := syz g;
     --I := ideal(x,y);
     --G := gb I;
     --S := R/I; -- seems to be leaking 2 fractions (2022) [Dec 7,00]
     --f := map(R,R,{y,x});  -- leaking 2 fractions? (4027)[Dec 7,00]
     --M':= coker g; 
     --C := res M';  -- leaking one fraction? (5031)[Dec 7,00]
     --N := image g;
     B := ZZ[x,y,z];
     --g := matrix{{23*B_0}};
     a := 23_B; -- by itself, leaks nothing.
     b := B_0; -- by itself, leaks nothing.
     c := B_1;
     d := b+b; -- by itself, leaks one int.
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
end
-- Local Variables:
-- compile-command: "make -C $M2BUILDDIR/Macaulay2/packages/Macaulay2Doc/test memleak2.out"
-- End:
