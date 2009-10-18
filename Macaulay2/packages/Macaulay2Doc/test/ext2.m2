-- Ezra Miller <enmiller@math.berkeley.edu>

R = ZZ/101[a,b,c,d]

f = map(
     R^{-1,-1,-1,0},
     R^   {-2, -2,   -2,-2,-2,-2,-2,-2,-2,-2,-2,-2,-1,-1,-1,-1},
     {    { d,  0,    c, 0, 0, b, a, 0, 0, 0, 0, 0, 0, 0, 0, 0},     -- -1
     	  { 0,  d,    0, c, 0, 0, 0, b, a, 0, 0, 0, 0, 0, 0, 0},     -- -1
     	  { 0,  0,    0, 0, d, 0, 0, 0, 0, c, b, a, 0, 0, 0, 0},     -- -1
     	  { 0,  0,  d^2, 0, 0, 0, 0, 0, 0, 0, 0, 0, d, c, b, a}      -- 0
	  })

assert( isHomogeneous f )

E = cokernel f

assert( HH_1 res E == 0 )
end
-- Local Variables:
-- compile-command: "make -C $M2BUILDDIR/Macaulay2/packages/Macaulay2Doc/test ext.out"
-- End:
