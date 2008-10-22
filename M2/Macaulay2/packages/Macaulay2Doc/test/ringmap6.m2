setRandomSeed 5
k = GF 729
p = 3
assert( promote(k.PrimitiveElement,k) != a )		    -- just happens to be the case currently, making the next lines more interesting
f = x -> assert( x^p == sub(x,a=>a^p) )
for i from 1 to 100 do f a^i

R = k[t,u]
f = x -> assert( x^p == sub(x,{a=>a^p,t=>t^p,u=>u^p}) )
for i from 1 to 100 do f (t+u^6+a^i)

