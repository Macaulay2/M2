-- test whether lines ending with ^M or ^M^J work equally well.
--   line 2 ends with just ^M      --    line 3 ends with ^M^J  
f = x -> x	   --    line 4
assert( drop(locate f, 1) === (4, 6, 4, 9) )		    -- we no longer give locations of function parameter lists.  Why??
-- assert( drop(locate f, 1) === (4, 4, 4, 9) )
ascii "a\na"
ascii "aa"
assert ( "a\na" === "aa" )
assert ( "a\na" === "a
a" )
assert ( "a\na" === "a
a" )
assert ( "a\n\na" === "a

a" )
assert ( "a\na" === ///aa/// )
assert ( "a\na" === ///a
a/// )
assert ( "a\na" === ///a
a/// )
assert ( "a\n\na" === ///a

a/// )
assert ( "a\n\na" === "a
a" )
end
-- Local Variables:
-- compile-command: "make -C $M2BUILDDIR/Macaulay2/test newlines.out"
-- End:
