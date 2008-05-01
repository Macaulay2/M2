-- test kernel for module maps over ring maps

S = QQ[y]
R = QQ[x,Degrees=>{2}]
f = map(S,R,{y^2})

m = matrix {{1,y^2}}
n = map(S^1,R^(-degrees source m),f,m)
assert( n * gens ker n == 0 )
assert( ker n == image map(R^{{0}, {-2}}, R^{{-2}}, {{x}, {-1}}) )
assert( coimage n == cokernel map(R^{{0}, {-2}}, R^{{-2}}, {{x}, {-1}}) )

m = matrix {{1,y}}
n = map(S^1,R^(-degrees source m),f,m)
assert( n * gens ker n == 0 )
assert( ker n == 0 )
assert( coimage n == R^{{0}, {-1}} )

m = matrix {{1,y+1}}
n = map(S^1,R^(-degrees source m),f,m)
assert( n * gens ker n == 0 )
assert( ker n == 0 )
assert( coimage n == R^{{0}, {-1}} )

m = matrix {{1,y^2+1}}
n = map(S^1,R^(-degrees source m),f,m)
assert( n * gens ker n == 0 )
assert( ker n == image map(R^{{0}, {-2}}, R^{{-2}}, {{x+1}, {-1}}) )
assert( coimage n == cokernel map(R^{{0}, {-2}}, R^{{-2}}, {{x+1}, {-1}}) )

S = QQ[y]
R = QQ[x]
f = map(S,R,{y^2})

m = matrix {{1,y^2}}
n = map(S^1,R^(-degrees source m),f,m)
assert( n * gens ker n == 0 )
assert( ker n == image map(R^{{0}, {-2}}, R^{{-2}}, {{x}, {-1}}) )
assert( coimage n == cokernel map(R^{{0}, {-2}}, R^{{-2}}, {{x}, {-1}}) )

m = matrix {{1,y}}
n = map(S^1,R^(-degrees source m),f,m)
assert( n * gens ker n == 0 )
assert( ker n == 0 )
assert( coimage n == R^{{0}, {-1}} )

m = matrix {{1,y+1}}
n = map(S^1,R^(-degrees source m),f,m)
assert( n * gens ker n == 0 )
assert( ker n == 0 )
assert( coimage n == R^{{0}, {-1}} )

m = matrix {{1,y^2+1}}
n = map(S^1,R^(-degrees source m),f,m)
assert( n * gens ker n == 0 )
assert( ker n == image map(R^{{0}, {-2}}, R^{{-2}}, {{x+1}, {-1}}) )
assert( coimage n == cokernel map(R^{{0}, {-2}}, R^{{-2}}, {{x+1}, {-1}}) )


S = QQ[y]
R = QQ[x]
f = map(S,R,{y^2+1})

m = matrix {{1,y^2}}
n = map(S^1,R^(-degrees source m),f,m)
assert( n * gens ker n == 0 )
assert( ker n == image map(R^{{0}, {-2}}, R^{{-2}}, {{x-1}, {-1}}) )
assert( coimage n == cokernel map(R^{{0}, {-2}}, R^{{-2}}, {{x-1}, {-1}}) )

m = matrix {{1,y}}
n = map(S^1,R^(-degrees source m),f,m)
assert( n * gens ker n == 0 )
assert( ker n == 0 )
assert( coimage n == R^{{0}, {-1}} )

m = matrix {{1,y+1}}
n = map(S^1,R^(-degrees source m),f,m)
assert( n * gens ker n == 0 )
assert( ker n == 0 )
assert( coimage n == R^{{0}, {-1}} )

m = matrix {{1,y^2+1}}
n = map(S^1,R^(-degrees source m),f,m)
assert( n * gens ker n == 0 )
assert( ker n == image map(R^{{0}, {-2}}, R^{{-2}}, {{x}, {-1}}) )
assert( coimage n == cokernel map(R^{{0}, {-2}}, R^{{-2}}, {{x}, {-1}}) )

-- Local Variables:
-- compile-command: "make -C $M2BUILDDIR/Macaulay2/packages/Macaulay2Doc/test ker8.out"
-- End:
