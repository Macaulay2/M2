-- warm up first:
k=ZZ/5
R1=k[s,t]
R2=k[x,y,z,w]
f = map(R1,R2,{s^4,s^3*t-t^2*s^2,s*t^3,t^4})
ker f
isHomogeneous f
isHomogeneous ker f
assert(f (ker f) == 0)
P0 = projectiveHilbertPolynomial 0
P1 = projectiveHilbertPolynomial 1
assert ( hilbertPolynomial coker gens ker f == 4 * P1 - 3 * P0 )

-- now over a Galois field
k=GF(5,2,Variable=>a)
R1=k[s,t]
R2 = k[x,y,z,w]
assert( substitute(a*x-1,{x=>a}) == a^2 - 1 )
f = map(R1,R2,{s^4,s^3*t-a*t^2*s^2,s*t^3,t^4},DegreeMap => v -> 4*v)
ker f
assert(f (ker f) == 0)
P0 = hilbertPolynomial (R2/(x,y,z))
P1 = hilbertPolynomial (R2/(x,y))
assert ( hilbertPolynomial coker gens ker f == 4 * P1 - 3 * P0 )

-- some other stuff
k = GF(2,3,Variable=>x)
k = GF(2,3,Variable=>x)
k = GF(2,3,Variable=>x)
x+x^2
k = GF(2,3,Variable=>a)

assert( length degree id_(k^1) == 0 )

end
-- Local Variables:
-- compile-command: "make -C $M2BUILDDIR/Macaulay2/packages/Macaulay2Doc/test galois.out"
-- End:
