-- oops : reported by Frank Schreyer, Mar 29, 2000

ZZ[x,y,SkewCommutative=>true]
assert( substitute(x*y,matrix {{y,x}}) == y*x )
-- Local Variables:
-- compile-command: "make skew.okay "
-- End:
