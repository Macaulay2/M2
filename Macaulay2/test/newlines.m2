-- test whether lines ending with ^M or ^M^J work equally well.
--   line 2 ends with just ^M  
    --    line 3 ends with ^M^J  
f = x -> x	   --    line 4
assert( drop(locate f, 1) === (4, 4) )
assert ( "a\na" === "a
a" )
assert ( "a\na" === "a
a" )
assert ( "a\na" === "a
a" )
assert ( "a\n\na" === "a

a" )
assert ( "a\na" === ///a
a/// )
assert ( "a\na" === ///a
a/// )
assert ( "a\na" === ///a
a/// )
assert ( "a\n\na" === ///a

a/// )
assert ( "a\n\na" === "a

a" )
-- Local Variables:
-- compile-command: "make -C $M2BUILDDIR newlines.okay "
-- End:
