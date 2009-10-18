-- nets

t = "   asdf qwer dfhh   xcvb  eryy wert"
t = t||t||t
wrap("-",10,t)

List#{Standard,BeforePrint} = x -> wrap("-", 80, net x)
toList ( 0 .. 100 )

-- net/string conversion

x = "asdf\nasdf\n"
assert ( toString net x === x )

x = "asdf\nasdf\nqwer"
assert ( toString net x === x )

x = "\nasdf\nqwer"
assert ( toString net x === x )

end
-- Local Variables:
-- compile-command: "make -C $M2BUILDDIR/Macaulay2/packages/Macaulay2Doc/test nets.out"
-- End:
