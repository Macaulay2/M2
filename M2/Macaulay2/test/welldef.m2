R = ZZ/101[x]
assert     isWellDefined inducedMap(R^1, image matrix {{x}})
assert( try ( isWellDefined inducedMap(image matrix {{x}}, R^1); false ) else true )

	  
-- Local Variables:
-- compile-command: "make welldef.okay "
-- End:
