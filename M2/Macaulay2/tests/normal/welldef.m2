R = ZZ/101[x]
assert     isWellDefined inducedMap(R^1, image matrix {{x}})
assert( try ( isWellDefined inducedMap(image matrix {{x}}, R^1); false ) else true )

	  
end
-- Local Variables:
-- compile-command: "make -C $M2BUILDDIR/Macaulay2/packages/Macaulay2Doc/test welldef.out"
-- End:
