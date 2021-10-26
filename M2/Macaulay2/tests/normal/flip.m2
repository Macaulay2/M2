R = ZZ/101[x]
A = R^{0,1}
B = R^{0,2}
C = R^{0,4}
D = R^{0,8}

assert( A != B )
assert( A ** B != B ** A )
assert( R^{0,1} == R^{0,1} )

assert( source flip(A,B) == A ** B )
assert( target flip(A,B) == B ** A )

m = map(B,A,0)
n = map(D,C,0)
assert( source contract(m,n) == dual source m ** source n )
assert( target contract(m,n) == dual target m ** target n )
assert( source diff(m,n) == dual source m ** source n )
assert( target diff(m,n) == dual target m ** target n )

f = map(C,A**B,0)
assert( source adjoint(f,A,B) == A )
assert( target adjoint(f,A,B) == dual B ** C )

g = map(B**C,A,0)
assert( source adjoint'(g,dual B,C) == A ** dual B)
assert( target adjoint'(g,dual B,C) == C )

-- square matrices
assert(isHomogeneous flip(R^{0,1},R^{0,5}))
assert(isHomogeneous flip(R^{0,1,2},R^{0,5,10}))
assert(isHomogeneous flip(R^{0,1,2,3},R^{0,5,10,15}))

-- rectangular matrices
assert(isHomogeneous flip(R^{0,1},R^{0,5,10}))
assert(source flip(R^{0,1},R^{0,5,10}) == R^{0,1} ** R^{0,5,10})
assert(target flip(R^{0,1},R^{0,5,10}) == R^{0,5,10} ** R^{0,1})

assert(isHomogeneous flip(R^{0,1},R^{0,5,10,15}))
assert(isHomogeneous flip(R^{0,1,2,3},R^{0,5,10}))
assert(isHomogeneous flip(R^{0,1,2,3,4},R^{0,5,10}))
end
-- Local Variables:
-- compile-command: "make -C $M2BUILDDIR/Macaulay2/packages/Macaulay2Doc/test flip.out"
-- End:
