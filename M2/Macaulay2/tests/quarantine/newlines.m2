-- test whether lines ending with ^M or ^M^J work equally well.
--   line 2 ends with just ^M      --    line 3 ends with ^M^J  
f = x -> x	   --    line 4
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
--status: this is a minor problem with how error messages display the location of code
assert( drop(locate f, 1) === (4, 6, 4, 9) )		    -- we no longer give locations of function parameter lists.  Why??
-- assert( drop(locate f, 1) === (4, 4, 4, 9) )
end
-- Local Variables:
-- compile-command: "make -C $M2BUILDDIR/Macaulay2/packages/Macaulay2Doc/test newlines.out"
-- End:
