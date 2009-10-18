setRandomSeed 5
k = GF(13,4)
p = char k
f = x -> assert( x^p == sub(x,a=>a^p) )
for i from 1 to 100 do f a^i

R = k[t,u]
f = x -> assert( x^p == sub(x,{a=>a^p,t=>t^p,u=>u^p}) )
for i from 1 to 100 do f (t+u^6+a^i)


-- # Local Variables:
-- # compile-command: "make -k -C $M2BUILDDIR/Macaulay2/packages/Macaulay2Doc/test ringmap6.out "
-- # End:
