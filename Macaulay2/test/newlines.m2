-- test whether lines ending with ^M or ^M^J work equally well.
--   line 2  
    --    line 3   
f = () -> ()	   --    line 4
assert( locate f === ("newlines.m2", 4, 4) )
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
