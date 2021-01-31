-- an old bug from Eisenbud
kk=ZZ/32003
S=kk[a,b,c,d]
m=map(S^2,S^{-1,-1},matrix{{a,b},{c,d}})
jm=jacobian m
mt = transpose jm
jn=gens kernel mt
R=kk[A,B,C,D,SkewCommutative=>true]
ev=map(R,S,matrix{{A,B,C,D}})
JN = ev jn
q=vars(R)**id_(R^2)
p=q*JN
--The following line uses up all the memory; on my 64Mb machine
--it begins swapping, doesn't finish before I give up.
F=res(coker p, LengthLimit=>2)
G=res(coker p, LengthLimit=>3)
assert( apply(4,i->rank G_i) == {2, 6, 12, 20} )
end
-- Local Variables:
-- compile-command: "make -C $M2BUILDDIR/Macaulay2/packages/Macaulay2Doc/test res7.out"
-- End:
