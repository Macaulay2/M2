-- oops : reported by Frank Schreyer, Mar 29, 2000

ZZ[x,y,SkewCommutative=>true]
assert( substitute(x*y,matrix {{y,x}}) == y*x )

-- new to 0.9.5:
E = ZZ/32003[x,y,z,a..f,SkewCommutative => {a,b,c,d,e,f}]

end
-- Local Variables:
-- compile-command: "make -C $M2BUILDDIR/Macaulay2/packages/Macaulay2Doc/test skew.out"
-- End:
