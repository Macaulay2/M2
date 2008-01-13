kk=ZZ/101
R=kk[x,y,u,v,SkewCommutative=>true]
i=ideal(x+u*v)
M=coker gens i
F=res(M, LengthLimit=>5)
P = image F.dd_2
xP = ideal vars R * P
P1 = P/xP
presentation P1
prune P1
f = modulo ( gens P, gens xP)
assert( gens P * f % xP == 0 )
assert( image (gens P * f) == xP )
assert( rank source f == 21 )				    -- that's just what it happens to be...

--   It's too hard to keep track:
-- 
-- g = matrix {{1, -1, 0, 0, 0, -x, -y, -u, 0, 0, -v, 0, 0, 0, 0, 0, 0, 0, 1, 0, 0}, {0, 0, 0, 0, 1, 0, 0, 0, 0, 0, 0, x, -x, x, -x, x, -y, y, u, 0, v}, {0, 0, 1, -1, 0, 0, 0, 0, -x, -y, -u, -u, 0, 0, 0, 0, 0, 0, 0, -v, 1}, {0, 0, 0, 0, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0}}
-- 
-- g = matrix {{1, 0, 0, 0, 0, -v, 0, -u, -y, y, -x, x, -x, x, x, -x, 0, 0, 0, 0, 0}, {1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0}, {0, -1, 1, 0, 0, 1, -v, 0, 0, 0, -u, 0, 0, 0, 0, 0, -y, -x, 0, 0, 0}, {0, 0, 0, -1, 1, 0, 0, 1, 0, 0, 0, -v, 0, 0, 0, 0, 0, 0, -u, -y, -x}}
-- 
-- g' = matrix {{1, -1, 0, 0, 0, -x, -y, -u, 0, 0, -v, 0, 0, 0, 0, 0, 0, 0, 1, 0, 0}, {0, 0, 0, 0, 1, 0, 0, 0, 0, 0, 0, x, -x, x, -x, x, -y, y, u, 0, v}, {0, 0, 1, -1, 0, 0, 0, 0, -x, -y, -u, -u, 0, 0, 0, 0, 0, 0, 0, -v, 1}, {0, 0, 0, 0, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0}}
-- 
-- assert(f-g==0 or f-g'==0)

end
-- Local Variables:
-- compile-command: "make -C $M2BUILDDIR/Macaulay2/test tensor.out"
-- End:
