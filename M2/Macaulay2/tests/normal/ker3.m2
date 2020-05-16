R = ZZ/101[a,b,c]
S = ZZ/101[x,y]
f = map(S,R,{x^2,x*y,y^2})
I = ker f
assert( {{2}} == degrees source generators I )
end
-- Local Variables:
-- compile-command: "make -C $M2BUILDDIR/Macaulay2/packages/Macaulay2Doc/test ker3.out"
-- End:
